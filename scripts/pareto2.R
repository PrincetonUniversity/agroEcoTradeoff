library(agroEcoTradeoff)
cnames <- c("Ag", "C", "bd", "cost")

cblist <- NULL
j <- 1
myseq = seq(0,1,0.1)
for (h in myseq) { 
  for (k in seq(0, 1 - h, 0.1)) {
    for (i in seq(0, 1 - (k + h), 0.1)) {
      cblist[[j]] <- c(k, i, 1 - i - k - h, h)
      j <- j + 1
    }
  }
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
todisk = TRUE
silent = TRUE

bcode <- run_code(input_key)
if(todisk == TRUE) {
  dnm <- paste0(full_path(set_base_path(), "external/output/batch/dt/"), bcode)
  dir.create(dnm)
} 
out_list <- list()

land = NULL
carbon = NULL
biodiversity = NULL
cost = NULL
count = 1

for(i in 1:nrow(parms)) {
  print(paste("running batch", i))
  if(i == 1) {
    if(!"ctype" %in% colnames(parms)) { 
      ctype <- "+"
    } else {
      ctype <- parms[i, "ctype"]
    }
    to <- tradeoff_mod(prod_targ = parms[i, tnames], 
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
    to <- tradeoff_mod(prod_targ = parms[i, tnames], 
                       ybetas = list(parms[i, "y1"], parms[i, "y2"]), 
                       cbetas = parms[i, cnames], ybeta_update = ybup, 
                       input_key = input_key, exist_list = to$inputs,
                       ctype = ctype, silent = silent)
  }
  ct <- 0
  for (ii in 1:length(to$conv$x)) {
    cvrtd <- FALSE
    for (j in 3:length(to$conv)) {
      if (to$conv[[ii, j]] == 1) {
        cvrtd <- TRUE
        break
      }
    }
    if (cvrtd == TRUE) {
      ct <- ct + sum(to$inputs$cost[ii])
    }
  }
  
  land[count] <- sum(to$impacts$conv_area, na.rm = TRUE)
  biodiversity[count] <- mean(to$impacts$rich_mu, na.rm = TRUE)
  carbon[count] <- sum(to$impacts$C_tot, na.rm = TRUE)
  cost[count] <- -ct
  count <- count + 1
}
outputtable <- as.data.table(cbind.data.frame(land, carbon, biodiversity, cost))
outputtable$ind <- seq(1, length(outputtable$land))

setorder(outputtable, land)
j <- 1
dominated = NULL
dominatedby = NULL
for (i in 1:(length(outputtable$land)-1)) {
  if ((outputtable$carbon[i + 1] > outputtable$carbon[i]) & 
      (outputtable$biodiversity[i + 1] > outputtable$biodiversity[i]) &
      (outputtable$cost[i + 1] > outputtable$cost[i])) {
    dominated[j] <- outputtable$ind[i + 1]
    dominatedby[j] <- outputtable$ind[i]
    j <- j + 1
  }
}

setorder(outputtable, carbon)
for (i in 1:(length(outputtable$land)-1)) {
  if ((outputtable$land[i + 1] > outputtable$land[i]) & 
      (outputtable$biodiversity[i + 1] > outputtable$biodiversity[i]) &
      (outputtable$cost[i + 1] > outputtable$cost[i])) {
    dominated[j] <- outputtable$ind[i + 1]
    dominatedby[j] <- outputtable$ind[i]
    j <- j + 1
  }
}

setorder(outputtable, biodiversity)
for (i in 1:(length(outputtable$land)-1)) {
  if ((outputtable$land[i + 1] > outputtable$land[i]) & 
      (outputtable$carbon[i + 1] > outputtable$carbon[i]) &
      (outputtable$cost[i + 1] > outputtable$cost[i])) {
    dominated[j] <- outputtable$ind[i + 1]
    dominatedby[j] <- outputtable$ind[i]
    j <- j + 1
  }
}

setorder(outputtable, cost)
for (i in 1:(length(outputtable$land)-1)) {
  if ((outputtable$land[i + 1] > outputtable$land[i]) & 
      (outputtable$carbon[i + 1] > outputtable$carbon[i]) &
      (outputtable$biodiversity[i + 1] > outputtable$biodiversity[i])) {
    dominated[j] <- outputtable$ind[i + 1]
    dominatedby[j] <- outputtable$ind[i]
    j <- j + 1
  }
}

ot <- outputtable[ !(ind %in% dominated)]


#s3d <-scatterplot3d(ot$land,ot$carbon,ot$biodiversity, 
#                    pch=16, highlight.3d=TRUE, type="h", main="3D Scatterplot", 
#                    xlab = "Converted Area (ha)", ylab = "Carbon Lost (tonnes)", 
#                    zlab = "Protected Area Lost (ha)")
#fit <- lm(ot$biodiversity ~ ot$land+ot$carbon) 
#s3d$plane3d(fit)

ot$carbon <- (ot$carbon - mean(ot$carbon))/sd(ot$carbon)
ot$land <- (ot$land - mean(ot$land))/sd(ot$land)
ot$biodiversity <- (ot$biodiversity - mean(ot$biodiversity))/sd(ot$biodiversity)
ot$cost <- (ot$cost - mean(ot$cost))/sd(ot$cost)

ot[, ind:=NULL]
ot$tot <- rowSums(ot)
setorder(ot, tot)





