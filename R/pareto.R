#' Approximates Pareto front for multi-objective optimization of AgroEcoTradeoff 
#' Model outputs
#' @description This function generates a series of possible outputs from the
#' AgroEcoTradeoff Model and retains those that are non-dominated, effectively
#' mimicking the traditional weighted sum method for multi-objective optimization.
#' @param cnames A list of the constraints to optimize over 
#' @param step The step interval over which to search for optimal solutions
#' @param yblist A two element list giving yield modifications 
#' @param targ The target multiplier for production
#' 

pareto <- function(cnames, step = 0.1, yblist, targ) {
  
  
  compnames <- c("Ag", "C", "bd", "cost")
  if (length(cnames) == 0)
    stop("select at least one constraint")
  
  if (1 %% step != 0)
    stop("step must divide evenly into 1")
  
  incl <- compnames%in%cnames
  
  if (sum(incl) != length(cnames))
    stop("Constraint choices are Ag, C, bd, and cost.")
  
  
  if (length(cnames) > 0) {
    colone <- min(which(incl == TRUE))
    incl[colone] = FALSE
    cblist <- list(c(0,0,0,0))
    cblist[[1]][colone] = 1
  }
  
  steps <- 1/step
  
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
        if (cblist[[i]][colone] < step/2) {
          stepping <- stepping + step
          stepper <- 0
        }
        # < step/2 would be == 0 if not for the small epsilon from numerical rep. error
      }
      stepping2 <- stepping2 + step
    }
  }
  
  # prod_targ space
  tnames <- c("maize", "cassava", "ground", "cotton", "soy", "pulse", "sunflower",
              "sugarcane", "wheat")
  targlist <- list(targ1 <- rep(targ, length(tnames)))
  
  parms <- do.call(rbind, lapply(yblist, function(x) {
    do.call(rbind, lapply(targlist, function(y) {
      do.call(rbind, lapply(cblist, function(z) {
        v <- c(z, x, y)
        names(v) <- c(compnames, "y1", "y2", tnames)
        v
      }))
    }))
  }))
  
  #Prepare for writing output table 
  bcode <- run_code("ZA")
  dnm <- paste0(full_path(set_base_path(), "external/output/batch/dt/"), bcode)
  dir.create(dnm)
  out_list <- list()
  
  input_key = "ZA"
  silent = TRUE
  
  outputtable = NULL
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
                          cbetas = parms[i, compnames], input_key = input_key, 
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
                          cbetas = parms[i, compnames], ybeta_update = ybup, 
                          input_key = input_key, exist_list = to$inputs,
                          ctype = ctype, silent = silent)
    }
    #Write table
    fnm <- full_path(set_base_path(), 
                     paste0("external/output/batch/dt/", bcode, "/", 
                            to$runcode, ".csv"))
    write.table(to$conv, file = fnm, sep = ",", col.names = TRUE, 
                row.names = FALSE)
    out_list[[i]] <- to[[c("impacts")]]
    names(out_list)[i] <- to$runcode
    dnm <- dir("external/output/batch/dt")
    save(out_list, file = paste0("external/output/batch/dt/", dnm, 
                                   "/out_tables.rda"))
    save(parms, file = paste0("external/output/batch/dt/", dnm, "/parms.rda"))
    
    
    
    if ("Ag" %in% cnames)
      outputtable$land[count] <- sum(to$impacts$conv_area, na.rm = TRUE)
    if ("C" %in% cnames)
      outputtable$carbon[count] <- sum(to$impacts$C_tot, na.rm = TRUE)
    if ("bd" %in% cnames)
      outputtable$biodiversity[count] <- sum(to$impacts$pa_loss, na.rm = TRUE)
    if ("cost" %in% cnames)
      outputtable$cost[count] <- sum(to$impacts$cost_tot, na.rm = TRUE)
    count <- count + 1
  }
  
  outputtable <- as.data.table(outputtable)
  otnames <- names(outputtable)
  setorderv(outputtable, otnames)
  outputtable$ind <- seq(1, length(outputtable[[1]]))
  
  
  #Brute force elimination of dominated solutions
  dom = NULL
  h = 1
  for (i in 1:(length(outputtable[[1]])-1)) {
    for (j in (i+1):length(outputtable[[1]])){
      dominated = TRUE
      for (k in 1:(length(outputtable)-1)){
        if (outputtable[[k]][j] < outputtable[[k]][i]) {
          dominated = FALSE
          break
        }
      }
      if (dominated == TRUE) {
        dom[h] = j
        h = h + 1
      }
    }
  }
  outputtable <- outputtable[!(ind %in% dom)]
}
  
  