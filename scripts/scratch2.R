library(agroEcoTradeoff)
cnames <- c("Ag", "C", "bd", "cost")
cblist <- list(c(0.5,0.5,0,0))

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

input_key <- "ZA"
input = "D"
silent <- TRUE
ctype <- "+"
prod_targ <- parms[1, tnames]
ybetas = list(parms[1, "y1"], parms[1, "y2"])
cbetas = parms[1, cnames]
ybeta_update = 1
exist_list = NULL

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

inlist = list("y_std" = il$y_std, "C" = il$carbon_p, 
              "bd" = il$cons_p, "cost" = il$cost)
code = rc
cropnames = il$cropnames

if(length(cbetas) != 4) stop("cbetas must be a 4 element vector")
cbetas_gt0 <- ifelse(cbetas != 0, 1, 0)  # vector to select inputs
if(sum(cbetas_gt0) == 0) {
  shhh("No land use prioritized: defaulting to crop productivity as priority", 
       silent = silent)
  cbetas <- c(1, 0, 0, 0)
  cbetas_gt0 <- cbetas
}
rlist <- inlist[which(cbetas != 0)]
cbetas_r <- cbetas[which(cbetas != 0)]
rlist_mod <- lapply(1:length(rlist), function(x) {
  if(cbetas_r[x] == 1) {
    shhh(paste(names(rlist)[x], ": full constraint applied"), silent = silent)
    r <- rlist[[x]]
  } else {
    #cnm <- fname(paste0("external/output/prob-rast/temp/c", x, "-rst-"))
    shhh(paste(names(rlist)[x], ": partial constraint applied"), 
         silent = silent)
    #r <- rastTest(cbetas_r[x] * rlist[[x]], filename = cnm)
    r <- cbetas_r[x] * rlist[[x]]
  }
})
names(rlist_mod) <- names(rlist)

# create output
cnames <- names(rlist_mod)[which(!names(rlist_mod) %in% "y_std")]  
yname <- names(rlist_mod)[which(names(rlist_mod) == "y_std")]  # pp a constraint?
p_y <- data.table(ind = 1:nrow(inlist$y_std))  # set-up output data.table
if(length(cnames) > 0) {  # if constraints are in result,
  shhh(paste("processing constraints", paste(cnames, collapse = ", ")), 
       silent = silent)
  if(ctype == "X") {
    cp <- data.table(c_p = Reduce(`*`, do.call(cbind, rlist_mod[cnames])))
  } else if(ctype == "+") {
    cp <- data.table(c_p = Reduce(`+`, do.call(cbind, rlist_mod[cnames])))
  }
}
if(length(yname) == 1) {
  shhh("yield is...", silent = silent)
  p_y[, c(cropnames) := rlist_mod[[yname]]]  # add y_dt into out_df
  if(exists("cp")) {  # multiply by constraints, if exist
    shhh("...constrained", silent = silent)
    if(ctype == "X") {
      for(j in cropnames) set(p_y, i = NULL, j = j, p_y[[j]] * cp)
    } else if(ctype == "+") {
      for(j in cropnames) set(p_y, i = NULL, j = j, p_y[[j]] + cp)
    }
    rm(cp)  # free up memory
    p_y[, ind := NULL]  # drop index
  } else {
    shhh("...unconstrained", silent = silent)
  }
} else if(length(yname) == 0) {  # if y_std is not factor, then p_y becomes c_p
  shhh("p_y for each crop is equal to product of constraints", 
       silent = silent) 
  p_y[, c(cropnames) := do.call(cbind, lapply(1:9, function(x) cp))]
  p_y[, ind := NULL]  # drop index
}