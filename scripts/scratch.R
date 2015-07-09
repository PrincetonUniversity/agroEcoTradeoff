library(agroEcoTradeoff)
source("Rmod/tradeoff_mod2.R")
source("Rmod/constraints_dt2.R")


cnames <- c("Ag", "C", "bd", "cost")

cblist <- NULL
j <- 1
myseq = seq(0,1,0.1)
for (k in myseq) {
    cblist[[j]] <- c(k, 1 - k, 0, 0)
    j <- j + 1
}

# yield modificationsmod space
yblist <- list(yb1 <- c(1, 1))
# prod_targ space
tnames <- c("maize", "cassava", "ground", "cotton", "soy", "pulse", "sunflower",
            "sugarcane", "wheat")
targlist <- list(targ1 <- rep(2, length(tnames)))

parms <- do.call(rbind, lapply(yblist, function(x) {
  do.call(rbind, lapply(targlist, function(y) {
    do.call(rbind, lapply(cblist, function(z) {
      v <- c(z, x, y)
      names(v) <- c(cnames, "y1", "y2", tnames)
      v
    }))
  }))
}))

input_key = "ZA"
silent = TRUE

carbon = NULL
land = NULL
count = 1

for(i in 1:nrow(parms)) {
  print(paste("running batch", i))
  if(i == 1) {
    if(!"ctype" %in% colnames(parms)) { 
      ctype <- "+"
    } else {
      ctype <- parms[i, "ctype"]
    }
    to <- tradeoff_mod2(prod_targ = parms[i, tnames], 
                       ybetas = list(parms[i, "y1"], parms[i, "y2"]), 
                       cbetas = parms[i, cnames], input_key = input_key, 
                       ybeta_update = 1, ctype = ctype,  silent = silent)
  } 
  if(i > 1) {
    ybup <- ifelse(all(parms[i, c("y1", "y2")] == parms[i - 1, c("y1", "y2")]), 
                   0, 1)
    if(!"ctype" %in% colnames(parms)) { 
      ctype <- "+"
    } else {
      ctype <- parms[i, "ctype"]
    }
    to <- tradeoff_mod2(prod_targ = parms[i, tnames], 
                       ybetas = list(parms[i, "y1"], parms[i, "y2"]), 
                       cbetas = parms[i, cnames], ybeta_update = ybup, 
                       input_key = input_key, exist_list = to$inputs,
                       ctype = ctype, silent = silent)
  }
  
  land[count] <- sum(to$impacts$conv_area, na.rm = TRUE)
  carbon[count] <- sum(to$impacts$C_tot, na.rm = TRUE)
  count <- count + 1
}
