#' Runs tradeoff_mod in batch mode
#' @description Function to drive large numbers of iteration of trademod
#' @param parms Matrix or data.frame providing input parameters permutations
#' @param input_key Two letter location code indicating input data to load.  
#' @param todisk TRUE, in which case output data.table is written to disk
#' @param silent Silent or verbose model. TRUE (default) or FALSE 
#' @return List of tradeoff_mod impact data.frames and conversion data.table if 
#' todisk is FALSE
#' @note This is one place where parallelizing could be useful, replacing for 
#' loop with foreach.
#' @examples
#' # Define parameter sets
#' # Constraint space
#' cnames <- c("Ag", "C", "bd", "cost")
#' cblist <- list(cb1 <- c(1, 0, 0, 0), 
#'                cb2 <- c(1, 1, 0, 0), 
#'                cb3 <- c(1, 1, 1, 0), 
#'                cb1r <- c(1, 0, 0, 1), 
#'                cb2r <- c(1, 1, 0, 1), 
#'                cb3r <- c(1, 1, 1, 1))
#'                
#' # ymod space
#' yblist <- list(yb1 <- c(1, 1), 
#'                yb2 <- c(0.8, 1),
#'                yb3 <- c(1.2, 1))
#' # prod_targ space
#' tnames <- c("maize", "cassava", "ground", "cotton", "soy", "pulse", 
#'             "sunflower", "sugarcane", "wheat")
#' targlist <- list(targ1 <- rep(2, length(tnames)), 
#'                  targ2 <- rep(3, length(tnames)), 
#'                  targ3 <- rep(4, length(tnames)))
#'                  
#' parms <- do.call(rbind, lapply(yblist, function(x) {
#'   do.call(rbind, lapply(targlist, function(y) {
#'     do.call(rbind, lapply(cblist, function(z) {
#'       v <- c(z, x, y)
#'       names(v) <- c(cnames, "y1", "y2", tnames)
#'       v
#'    }))
#'  }))
#'}))
#' parms <- parms[1:5, ]
#' batch_test <- tradeoff_batch(parms = parms, input_key = "ZA", todisk = FALSE, 
#'                              silent = TRUE)
#' @export
tradeoff_batch <- function(parms, input_key = "ZA", todisk = TRUE, 
                           silent = FALSE) {
  bcode <- run_code(input_key)
  if(todisk == TRUE) {
   dnm <- paste0(full_path(set_base_path(), "external/output/batch/dt/"), bcode)
   dir.create(dnm)
  } 
  out_list <- list()
  for(i in 1:nrow(parms)) {
    print(paste("running batch", i))
    if(i == 1) {
      if(!"ctype" %in% colnames(parms)) { 
        ctype <- "X"
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
       ctype <- "X"
      } else {
       ctype <- parms[i, "ctype"]
      }
      to <- tradeoff_mod(prod_targ = parms[i, tnames], 
                         ybetas = list(parms[i, "y1"], parms[i, "y2"]), 
                         cbetas = parms[i, cnames], ybeta_update = ybup, 
                         input_key = input_key, exist_list = to$inputs,
                         ctype = ctype, silent = silent)
    }
    if(todisk == TRUE) {
      fnm <- full_path(set_base_path(), 
                       paste0("external/output/batch/dt/", bcode, "/", 
                              to$runcode, ".csv"))
      write.table(to$conv, file = fnm, sep = ",", col.names = TRUE, 
                  row.names = FALSE)
      out_list[[i]] <- to[[c("impacts")]]
      names(out_list)[i] <- to$runcode
    } else if(todisk == FALSE) {
      out_list[[i]] <- to[c("impacts", "conv")]
      names(out_list)[i] <- to$runcode
    }   
  }
  return(out_list)
}


