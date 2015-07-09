library(agroEcoTradeoff)
cnames <- c("Ag", "C", "bd", "cost")

cblist <- NULL
j <- 1
myseq = seq(0,1,0.1)
for (k in myseq) {
for (i in seq(0, 1 - k, 0.1)) {
  cblist[[j]] <- c(0, i, 1 - i - k, k)
  j <- j + 1
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

#batch code
input_key = "ZA"
todisk = TRUE
silent = FALSE

bcode <- run_code(input_key)
if(todisk == TRUE) {
  dnm <- paste0(full_path(set_base_path(), "external/output/batch/dt/"), bcode)
  dir.create(dnm)
} 
out_list <- list()

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
    
    prod_targ = parms[i, tnames]
    ybetas = list(parms[i, "y1"], parms[i, "y2"])
    cbetas = parms[i, cnames]
    ybeta_update = 1
    input = "D"
    exist_list = NULL
    ctype = "+"
    silent = TRUE
    
    names(cbetas) <- c("Ag", "C", "bd", "cost")
    
    # set up 
    
    rnm <- full_path(set_base_path(), paste0("external/ext_data/", input_key, 
                                             "-mask.tif"))
    ha <- res(raster(rnm))[1]^2 / 10000  # nasty, hard-coded
    rc <- run_code(input_key)  # creates a once off code for any outputs
    #il <- input_handler(input_key = input_key, ybetas = ybetas, input = input, code = rc)
    il <- input_handler(input_key = input_key, ybetas = ybetas, input = input, 
                        code = rc, ybeta_update = ybeta_update, 
                        exist_list = exist_list, silent = silent)
    
    # target module
    if(input == "D") {
      target <- targets_dt(prod_targ = prod_targ, currprod = il$currprod, 
                           potprod = il$pp_curr)
    } else if(input == "R") {
      target <- targets_r(prod_targ = prod_targ, currprod = il$currprod, 
                          potprod = il$pp_curr, cropnames = il$cropnames)
    }
    
    # constraints module 
    if(input == "D") {
      c_prob <- constraints_dt(inlist = list("y_std" = il$y_std, "C" = il$carbon_p, 
                                             "bd" = il$cons_p, "cost" = il$cost), 
                               cbetas = cbetas, code = rc, 
                               cropnames = il$cropnames, ctype = ctype, 
                               silent = silent)
    } else if(input == "R") {
      rasterOptions(tmpdir = "external/output/temp")  # set tempfile directory
      c_prob <- constraints_r(inlist = list("y_std" = il$y_std, "C" = il$carbon_p, 
                                            "bd" = il$cons_p, "cost" = il$cost), 
                              cbetas = cbetas, code = rc, cropnames = il$cropnames, 
                              silent = silent)
    }
    
    # convert module
    if(input == "D") {
      converted <- convert_dt(conv_prob = c_prob, target = target, 
                              crop_frac = il$cropfrac, pot_yield = il$p_yield, 
                              cropnames = il$cropnames, base = il$mask, ha = ha, 
                              keep_index = FALSE)
    } else if(input == "R") {
      converted <- convert_r(conv_prob = c_prob, target = target, 
                             crop_frac = il$cropfrac, pot_yield = il$p_yield, 
                             cropnames = il$cropnames, code = rc) 
    }
  }
  
  if(i > 1) {
    ybup <- ifelse(all(parms[i, c("y1", "y2")] == parms[i - 1, c("y1", "y2")]), 
                   0, 1)
    if(!"ctype" %in% colnames(parms)) { 
      ctype <- "+"
    } else {
      ctype <- parms[i, "ctype"]
    }
    prod_targ = parms[i, tnames]
    ybetas = list(parms[i, "y1"], parms[i, "y2"])
    cbetas = parms[i, cnames]
    ybeta_update = ybup
  
    names(cbetas) <- c("Ag", "C", "bd", "cost")
    
    # set up 
    
    rnm <- full_path(set_base_path(), paste0("external/ext_data/", input_key, 
                                             "-mask.tif"))
    ha <- res(raster(rnm))[1]^2 / 10000  # nasty, hard-coded
    rc <- run_code(input_key)  # creates a once off code for any outputs
    #il <- input_handler(input_key = input_key, ybetas = ybetas, input = input, code = rc)
    il <- input_handler(input_key = input_key, ybetas = ybetas, input = input, 
                        code = rc, ybeta_update = ybeta_update, 
                        exist_list = exist_list, silent = silent)
    
    # target module
    if(input == "D") {
      target <- targets_dt(prod_targ = prod_targ, currprod = il$currprod, 
                           potprod = il$pp_curr)
    } else if(input == "R") {
      target <- targets_r(prod_targ = prod_targ, currprod = il$currprod, 
                          potprod = il$pp_curr, cropnames = il$cropnames)
    }
    
    # constraints module 
    if(input == "D") {
      c_prob <- constraints_dt(inlist = list("y_std" = il$y_std, "C" = il$carbon_p, 
                                             "bd" = il$cons_p, "cost" = il$cost), 
                               cbetas = cbetas, code = rc, 
                               cropnames = il$cropnames, ctype = ctype, 
                               silent = silent)
    } else if(input == "R") {
      rasterOptions(tmpdir = "external/output/temp")  # set tempfile directory
      c_prob <- constraints_r(inlist = list("y_std" = il$y_std, "C" = il$carbon_p, 
                                            "bd" = il$cons_p, "cost" = il$cost), 
                              cbetas = cbetas, code = rc, cropnames = il$cropnames, 
                              silent = silent)
    }
    
    # convert module
    if(input == "D") {
      converted <- convert_dt(conv_prob = c_prob, target = target, 
                              crop_frac = il$cropfrac, pot_yield = il$p_yield, 
                              cropnames = il$cropnames, base = il$mask, ha = ha, 
                              keep_index = FALSE)
    } else if(input == "R") {
      converted <- convert_r(conv_prob = c_prob, target = target, 
                             crop_frac = il$cropfrac, pot_yield = il$p_yield, 
                             cropnames = il$cropnames, code = rc) 
    }  
  }
  
  
  
  
  cn <- 0
  bd <- 0
  ct <- 0
  for (ii in 1:length(converted$x)) {
    cvrtd <- FALSE
    for (j in 3:length(converted)) {
      if (converted[[ii, j]] == 1) {
        cvrtd <- TRUE
        break
      }
    }
    if (cvrtd == TRUE) {
      bd <- bd + il$cons_p[ii]
      cn <- cn + il$carbon_p[ii]
      ct <- ct + il$cost[ii]
    }
  }
  carbon[count] <- cn
  biodiversity[count] <- bd
  cost[count] <- ct
  count <- count + 1
} 

  

