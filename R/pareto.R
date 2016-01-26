#' Generate weight combinations for running pareto function
#' @param cnames A vector of constraint names to optimize over e.g. c("Y", "BD")
#' @param step The step interval over which to search for optimal solutions
#' #@param yblist A two element list giving yield modifications 
#' @keywords internal
#' @export
pareto_steps <- function(cnames, step = 0.1) {
  
  # checks 
  if (length(cnames) == 0) stop("select at least one constraint")
  if (1 %% step != 0) stop("step must divide evenly into 1", call. = FALSE)
  compnames <- c("Y", "C", "BD", "COST")
  incl <- compnames %in% cnames
  if (sum(incl) != length(cnames)) {
    stop("Constraint choices are Y, C, BD, and COST.", call. = FALSE)
  } 
 
  
  incl <- compnames %in% cnames
  if (length(cnames) > 0) {
    colone <- min(which(incl == TRUE))
    incl[colone] = FALSE
    cblist <- list(c(0,0,0,0))
    cblist[[1]][colone] = 1
  }
  
  steps <- 1 / step
  
  if (length(cnames) > 1) {
    coltwo <- min(which(incl == TRUE))
    incl[coltwo] = FALSE
    for (i in 2:(steps+1)) {
      cblist[[i]] <- c(0,0,0,0)
      cblist[[i]][coltwo] <- step * (i - 1)
      cblist[[i]][colone] <- 1 - cblist[[i]][coltwo]
    }
  }
  
  if(length(cnames) > 2) {
    colthree <- min(which(incl == TRUE))
    incl[colthree] = FALSE
    prevlength <- length(cblist)
    stepping <- step
    stepper <- 0
    for (i in (prevlength + 1):(prevlength + sum(1:steps))) {
      cblist[[i]] <- c(0,0,0,0)
      cblist[[i]][colthree] <- stepping 
      cblist[[i]][coltwo] <- stepper
      cblist[[i]][colone] <- 1 - (cblist[[i]][coltwo] + cblist[[i]][colthree])
      stepper <- stepper + step
      if (cblist[[i]][colone] < step/2) {
        stepping <- stepping + step
        stepper <- 0
      }
     # < step/2 would be == 0 if not for the small epsilon from numerical rep. error
    }
  }
  
  if(length(cnames) > 3) {
    colfour <- min(which(incl == TRUE))
    incl[colfour] = FALSE
    stepping2 <- step
    stepper <- 0
    steppingseq <- NULL
    prev <- 0
    for (k in 1:(steps+1)) {
      steppingseq[k] <- prev + sum(seq(1:(steps + 2 - k)))
      prev <- steppingseq[k]
    }
    for (j in 1:steps) {
      prevlength <- length(cblist)
      stepping <- 0
      for (i in (prevlength + 1):steppingseq[j + 1]) {
        cblist[[i]] <- c(0,0,0,0)
        cblist[[i]][colfour] <- stepping2
        cblist[[i]][colthree] <- stepping 
        cblist[[i]][coltwo] <- stepper
        cblist[[i]][colone] <- 1 - (cblist[[i]][coltwo] + cblist[[i]][colthree]
                                    + cblist[[i]][colfour])
        stepper <- stepper + step
        if(cblist[[i]][colone] < step/2) {
          stepping <- stepping + step
          stepper <- 0
        }
        # < step/2 would be == 0 if not for the small epsilon from numerical rep. error
      }
      stepping2 <- stepping2 + step
    }
  }
  return(cblist)
}

#' Approximates Pareto front for multi-objective optimization of AgroEcoTradeoff 
#' Model outputs
#' @description This function generates a series of possible outputs from the
#' AgroEcoTradeoff Model and retains those that are non-dominated, effectively
#' mimicking the traditional weighted sum method for multi-objective optimization.
#' @param cnames A list of the constraints to optimize over 
#' @param step The step interval over which to search for optimal solutions
#' @param yblist A two element list of yield modifiers (currently disabled) 
#' @param prod_targ Production targets passed as list. See examples for format.
#' @param Yv agricultural impact metric (new land required)
#' @param Cv Carbon impact metric
#' @param BDv Biodiversity impact metric
#' @param COSTc Cost impact metric
#' @param Yst Function to calculate impacts across crops (e.g. sum)
#' @param Cst Function to calculate impacts across carbon (e.g. sum)
#' @param BDst Function to calculate impacts across biodiversity (e.g. mean)
#' @param COSTst Function to calculate cost impact (e.g. mean)
#' @param todisk Write out batch results to disk? TRUE or FALSE (default)
#' @param silent Verbose simulations?  Doesn't really work with parallel process
#' @param ncl Number of cpus to use (default = 4)
#' @param path Input data path
#' @return data.table
#' @export
# pareto <- function(cnames, step = 0.1, yblist, targ) {
pareto <- function(cnames, step = 0.1, prod_targ, yblist = list(1, 1),
                   input_key = "ZA", Yv, Cv, BDv, COSTv, Yst, Cst, BDst, COSTst,
                   todisk = FALSE, silent = TRUE, ncl = 4, path) {

  # prod_targ <- c("maize" = 2, "soy" = 2)
  # cnames <- c("Y", "C", "BD"); step = 0.1
  # cnames <- c("Y", "C", "BD", "COST"); step = 0.1
  # cnames <- c("Y", "C"); step = 0.05
  # Yv = "conv_area"; Cv = "tot_C"; BDv = "priority"; COSTv = "mu_cost"
  # Yst = sum; Cst = sum; BDst = mean; COSTst = mean
  # input_key = "ZA"; ncl = 4
  # currprodmod <- c(1, 1); todisk <- FALSE; path = "external/data/dt/new/"

  # weight combinations
  cblist <- pareto_steps(cnames, step = step)

  # prod_targ space
  # LDE: this needs to changed so that crop names can be passed in as variable
#   tnames <- c("maize", "cassava", "ground", "cotton", "soy", "pulse", "sunflower",
#               "sugarcane", "wheat")
#   # targ <- 4
  #targlist <- list(targ1 <- rep(prod_targ, length(tnames)))
  
  
  # dummy for switched of yield modification
  yblist <- list(do.call(c, yblist))
  targlist <- list(prod_targ)
  # yblist <- list(yb1 <- c(1, 1))
  
  # compile parameters
  parms <- batch_params(yblist, targlist, cblist)
#   parms <- do.call(rbind, lapply(yblist, function(x) {
#     do.call(rbind, lapply(targlist, function(y) {
#       do.call(rbind, lapply(cblist, function(z) {
#         v <- c(z, x, y)
#         tlistnms <- names(targlist[[1]])
#         tlistnms <- ifelse(is.null(tlistnms), 
#                            paste0("c", 1:length(targlist[[1]])), tlistnms)
#         names(v) <- c(compnames, "y1", "y2", names(targlist[[1]]))
#         v
#       }))
#     }))
#   }))
  
  # run batch 
  tob <- tradeoff_batch(parms, input_key = input_key, silent = silent, 
                        todisk = todisk, ncl = ncl, path = path)
  if(todisk == TRUE) {
    tobimp <- tob
  } else {
    tobimp <- lapply(tob, function(x) x[[1]]) 
  }
  
  
  
  # Prepare for writing output table 
  # LDE: this needs to be passed in as variable also
#   bcode <- run_code(input_key)
#   if(todisk == TRUE) {
#     dnm <- paste0(full_path(set_base_path(), "external/output/batch/dt/"), 
#                   bcode)
#     dir.create(dnm)
#   } 
#   out_list <- list()
#   
#   input_key = "ZA"   # variabilize
#   silent = TRUE
#   
#   impact_list = NULL
#   count = 1
#   
#   tnames <- c("maize", "soy")
#   compnames <- c("Y", "C", "BD", "COST")
#   for(i in 1:nrow(parms)) {
#     # i <- 1; i <- 2; silent = FALSE
#     print(paste("running batch", i))
#     if(i == 1) {
#       to <- tradeoff_mod(prod_targ = parms[i, tnames], 
#                           ybetas = list(parms[i, "y1"], parms[i, "y2"]), 
#                           cbetas = parms[i, compnames], input_key = input_key, 
#                           ybeta_update = 0, silent = silent, path = path)
#     } 
#     if(i > 1) {
#       to <- tradeoff_mod(prod_targ = parms[i, tnames], 
#                          ybetas = list(parms[i, "y1"], parms[i, "y2"]), 
#                          cbetas = parms[i, compnames], ybeta_update = 0, 
#                          input_key = input_key, exist_list = to$inputs,
#                          silent = silent)
#     }
    # Write table
#     fnm <- full_path(set_base_path(), 
#                      paste0("external/output/batch/dt/", bcode, "/", 
#                             to$runcode, ".csv"))
#     # write.table(to$conv, file = fnm, sep = ",", col.names = TRUE, 
                # row.names = FALSE)
#     out_list[[i]] <- to[[c("impacts")]]
#     names(out_list)[i] <- to$runcode
    # dnm <- dir("external/output/batch/dt")
    
#     if ("Y" %in% cnames) {
#       impact_list$land[i] <- sum(to$impacts[, "conv_area"], na.rm = TRUE)
#     }
#     if ("C" %in% cnames) {
#       impact_list$carbon[i] <- sum(to$impacts[, "tot_C"], na.rm = TRUE)
#     }
#     if ("BD" %in% cnames) {
#       impact_list$biodiversity[i] <- mean(to$impacts[, "priority"], na.rm =TRUE)
#     }
#     if ("COST" %in% cnames) { 
#       impact_list$cost[i] <- mean(to$impacts[, "mu_cost"], na.rm = TRUE)
#     }
#     #count <- count + 1
#   }
#   save(out_list, file = paste0("external/output/batch/dt/", bcode, 
#                                "/out_tables.rda"))
  # save(parms, file = paste0("external/output/batch/dt/", bcode, "/parms.rda"))

#   outputtable <- as.data.table(do.call(cbind.data.frame, impact_list))
#   outputtable <- as.data.table(outputtable)
#   otnames <- names(outputtable)
#   setorderv(outputtable, otnames)
#   outputtable$ind <- seq(1, length(outputtable[[1]]))
#   
#   vars <- list(Yv = "conv_area", Cv = "tot_C", BDv = "priority", 
#                COSTv = "mu_cost")
#   ostat <- list("Y" = sum, "C" = sum, "BD" = mean, "COST" = mean)
#   out_dt <- do.call(rbind, lapply(tobimp, function(x) {  # x <- tobimp[[1]]
#     land <- ostat$Y(x[, vars$Yv], na.rm = TRUE)
#     carbon <- ostat$C(x[, vars$Cv], na.rm = TRUE)
#     bd <- ostat$BD(x[, vars$BDv], na.rm = TRUE)
#     cost <- ostat$COST(x[, vars$COSTv], na.rm = TRUE)
#     odt <- data.table(cbind("Y" = land, "C" = carbon, "BD" = bd, "COST" = cost, 
#                             "ind" = x$iter[1]))
#     setorderv(odt, names(odt))
#     odt
#   }))
#   
  # summarize impacts across crops
  impacts <- batch_stat(tobimp, Yv, Cv, BDv, COSTv, Yst, Cst, BDst, COSTst)
  
  # remove dominated solutions
  optimal <- non_dominator(impacts, cnames = c("Y", "C"))
  
  return(optimal)
#   #Brute force elimination of dominated solutions
#   dom = NULL
#   h = 1
#   for(i in 1:(nrow(outputtable) - 1)) {
#   # for(i in 1:(length(outputtable[[1]]) - 1)) {
#     for(j in (i + 1):nrow(outputtable)){ # j = 2
#     # for(j in (i + 1):length(outputtable[[1]])){
#       dominated = TRUE
#       for(k in 1:(ncol(outputtable) - 1)){  # k = 1
#       #for(k in 1:(length(outputtable)-1)){
#         if(outputtable[[k]][j] < outputtable[[k]][i]) {
#           dominated = FALSE
#           break
#         }
#       }
#       if (dominated == TRUE) {
#         dom[h] = j
#         h = h + 1
#       }
#     }
#   }
#   outputtable <- list("bcode" = bcode, "table" = outputtable[!(ind %in% dom)])
}