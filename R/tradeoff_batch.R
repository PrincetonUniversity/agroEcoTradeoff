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
tradeoff_batch <- function(parms, input_key = "ZA", todisk = FALSE, 
                           silent = FALSE, ncl = 4, path) {
  # prod_targ <- c("maize" = 2, "soy" = 2)
  # cblist <- pareto_steps(c("Y", "C"), step = 0.25)
  # parms <- batch_params(list(c(1, 1)), targlist = list(prod_targ), cblist)
  # todisk <- TRUE; FALSE
  # input_key = "ZA"; ncl = 4
  # currprodmod <- c(1, 1); todisk <- FALSE; path = "external/data/dt/new/"
  bcode <- run_code(input_key)
  dnm <- full_path(set_base_path(), 
                   paste0("external/output/batch/dt/", bcode, "/"))
  if(todisk == TRUE) dir.create(dnm)  # create batch directory
  
  # Read in inputs
  il <- input_handler(input_key = input_key, path = path, ybeta_update = 0)
  
  # fetch inputs first, since we are in batch mode and we aren't modifying 
  # yields
  # path <- "external/data/dt/new/"
  # plot(dt_to_raster(il$mask, CRSobj = CRS(il$sp$crs)))
  
  # library(foreach)
  # library(doMC)

  registerDoMC(ncl)
  out_list <- foreach(i = 1:nrow(parms)) %dopar% { # i <- 1
    tof <- tradeoff_mod(prod_targ = parms[i, il$cropnames], 
                        cbetas = parms[i, c("Y", "C", "BD", "COST")], 
                        ybetas = list(1, 1), 
                        currprodmod = 1,  # put in option for batch mod her 
                        input_key = "ZA", exist_list = il,
                        ybeta_update = 0, silent = silent, path = path)
    odf <- cbind.data.frame("iter" = i, "rc" = tof$runcode, tof$impacts)
    
    # write out
    if(todisk == TRUE) {
      fnm <- fname(dnm, tof$runcode, ".csv")
      write.table(tof$conv[, 3:ncol(tof$conv), with = FALSE], file = fnm, 
                  sep = ",", col.names = TRUE, row.names = FALSE)
      out <- odf
    } else if(todisk == FALSE) {
      out <- list("impacts" = odf, "conv" = tof$conv)
    }
    return(out)
  }
  if(todisk == TRUE) { 
    save(out_list, file = fname(dnm, "impacts_tab.rda"))
  }
  return(out_list)  
}
#   for(i in 1:nrow(parms)) { # i <- 1
#     print(paste("running batch", i))
#     if(i == 1) {
#       lval <- NULL
#     } else {
#       lval <- "to"
#     } 
#     get(lval)
#      
#       # if(!"ctype" %in% colnames(parms)) { 
#         # ctype <- "X"
#       # } else {
#         # ctype <- parms[i, "ctype"]
#       # }
#       to <- tradeoff_mod(prod_targ = parms[i, tnames], 
#                          ybetas = list(parms[i, "y1"], parms[i, "y2"]), 
#                          cbetas = parms[i, cnames], input_key = input_key, 
#                          ybeta_update = 1, silent = silent)
#     } 
#     if(i > 1) {
#       ybup <- ifelse(all(parms[i, c("y1", "y2")] == parms[i - 1, c("y1", "y2")]), 
#                      0, 1)
#       if(!"ctype" %in% colnames(parms)) { 
#        ctype <- "X"
#       } else {
#        ctype <- parms[i, "ctype"]
#       }
#       to <- tradeoff_mod(prod_targ = parms[i, tnames], 
#                          ybetas = list(parms[i, "y1"], parms[i, "y2"]), 
#                          cbetas = parms[i, cnames], ybeta_update = ybup, 
#                          input_key = input_key, exist_list = to$inputs,
#                          ctype = ctype, silent = silent)
#       if(todisk == TRUE) {
#        fnm <- full_path(set_base_path(), 
#                         paste0("external/output/batch/dt/", bcode, "/", 
#                                to$runcode, ".csv"))
#        write.table(to$conv, file = fnm, sep = ",", col.names = TRUE, 
#                    row.names = FALSE)
#        out_list[[i]] <- to[[c("impacts")]]
#        names(out_list)[i] <- to$runcode
#        
#     }
#     if(todisk == TRUE) {
#       fnm <- full_path(set_base_path(), 
#                        paste0("external/output/batch/dt/", bcode, "/", 
#                               to$runcode, ".csv"))
#       write.table(to$conv, file = fnm, sep = ",", col.names = TRUE, 
#                   row.names = FALSE)
#       out_list[[i]] <- to[[c("impacts")]]
#       names(out_list)[i] <- to$runcode
#     } else if(todisk == FALSE) {
#       out_list[[i]] <- to[c("impacts", "conv")]
#       names(out_list)[i] <- to$runcode
#     }   
#   }
#   return(out_list)
# }
# 
# 
