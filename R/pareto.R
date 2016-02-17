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
#' @param prod_targ Production targets passed as list. See examples for format.
#' @param yblist A two element list of yield modifiers (currently disabled) 
#' @param currprodmod Modifier of current production
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
#' @return data.table
#' @export
# pareto <- function(cnames, step = 0.1, yblist, targ) {
pareto <- function(cnames, step = 0.1, prod_targ, yblist = list(1, 1),
                   currprodmod, input_key = "ZA", Yv, Cv, BDv, COSTv, Yst, Cst, 
                   BDst, COSTst, todisk = FALSE, silent = TRUE, ncl = 4) { 
                   #keepconv = FALSE) {

#   prod_targ <- c("maize" = 2, "soy" = 2); yblist = list(1, 1)
#   cnames <- c("Y", "C", "BD"); step = 0.25
#   cnames <- c("Y", "C", "BD", "COST"); step = 0.1
#   cnames <- c("Y", "C"); step = 0.25; silent = FALSE
#   Yv = "conv_area"; Cv = "tot_C"; 
#   BDv = c(Yv, "int_prior"); COSTv = c(Yv, "mu_cost")
#   wmu <- function(x, na.rm = na.rm) {
#     weighted.mean(x = x[, 2], w = x[, 1], na.rm = na.rm)
#   }
#   Yst = sum; Cst = sum; BDst = wmu; COSTst = wmu
#   input_key = "ZA"; ncl = 4
#   todisk = FALSE 
#   currprodmod <- c(1, 1); todisk <- TRUE; #path = "external/data/dt/latest/"

  # weight combinations
  cblist <- pareto_steps(cnames, step = step)

  # dummy for switched of yield modification
  yblist <- list(do.call(c, yblist))
  targlist <- list(prod_targ)
  currprodmodlist <- list(currprodmod)
  # yblist <- list(yb1 <- c(1, 1))
  
  # compile parameters
  parms <- batch_params(yblist, targlist, cblist, currprodmodlist)

  # run batch 
  tob <- tradeoff_batch(parms, input_key = input_key, silent = silent, 
                        todisk = todisk, ncl = ncl)
#   if(todisk == TRUE) {
#     tobimp <- tob$imp_list$impacts
#   } else if(todisk == FALSE) {
#     tobimp <- lapply(tob$imp_list, function(x) x$impacts) 
#   } 
  
  # summarize impacts across crops
  tobimp <- tob$imp_list$impacts
  impacts <- batch_stat(tobimp, Yv, Cv, BDv, COSTv, Yst, Cst, BDst, COSTst)
  
  # remove dominated solutions
  optimal <- non_dominator(impacts, cnames = cnames)
  
  if(todisk == TRUE) {
   bpath <- set_base_path()
   bdnm <- full_path(bpath, paste0("external/output/", tob$bcode))
   save(optimal, file = full_path(bdnm, "optitab.rda"))
  }
  return(list("optitab" = optimal, "params" = parms))
}