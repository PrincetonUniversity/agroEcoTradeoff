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
#' targlist <- list(c("maize" = 2, "soy" = 2))
#' #targlist <- list(c("maize" = 2))
#' cpmlist <- list(c(1, 1))
#' cblist <- pareto_steps(c("Y", "C"), step = 0.25)
#' parms <- batch_params(targlist = targlist, cblist = cblist, 
#'                       currprodmodlist = list(c(1, 1)))
#' batch_test <- tradeoff_batch(parms = parms, input_key = "ZA", todisk = FALSE, 
#'                              silent = TRUE)
#' @export
tradeoff_batch <- function(parms, input_key = "ZA", todisk = FALSE, 
                           silent = FALSE, ncl = 4) {
#   prod_targ <- c("maize" = 2, "soy" = 2)
#   cblist <- pareto_steps(c("Y", "C"), step = 0.25)
#   parms <- batch_params(list(c(1, 1)), targlist = list(prod_targ), 
#                       cblist, list(c(1, 1)))
#   todisk <- TRUE; # todisk <- FALSE
#   input_key = "ZA"; ncl = 4
#   currprodmod <- c(1, 1); silent = FALSE

  bpath <- set_base_path()
  path <- full_path(bpath, full_path("external/data", input_key)) 
  bcode <- run_code(input_key, it = "B")
  bdnm <- full_path(bpath, "external/output/")
  # if(!dir.exists(bdnm)) dir.create(bdnm)
  dnm <- full_path(bdnm, bcode)
  if(todisk == TRUE) dir.create(dnm, recursive = TRUE)  # create batch directory
  
  # Read in inputs
  parmnames <- colnames(parms)
  crops <- gsub("f", "", parmnames[grep("f", parmnames)])  # select cropnames
  il <- input_handler(path = path, crops = crops, ybeta_update = 0)
  
  # fetch inputs first, since we are in batch mode and we aren't modifying 
  # yields
  # path <- "external/data/dt/new/"
  # plot(dt_to_raster(il$mask, CRSobj = CRS(il$sp$crs)))
  
  # library(foreach)
  # library(doMC)

  registerDoMC(ncl)
  out_list <- foreach(i = 1:nrow(parms)) %dopar% { # i <- 1
    tof <- tradeoff_mod(prod_targ = parms[i, paste0("f", il$cropnames)], 
                        cbetas = parms[i, c("Y", "C", "BD", "COST")], 
                        ybetas = list(1, 1), # hard-coded this to switch off
                        currprodmod = parms[i, paste0("c", il$cropnames)],
                        it = i, input_key = input_key, exist_list = il,
                        ybeta_update = 0, silent = silent)
    odf <- cbind.data.frame("iter" = i, "rc" = tof$runcode, tof$impacts)
    
    # write out
    if(todisk == TRUE) {
      fnm <- fname(dnm, paste0("/", tof$runcode), ".csv")
      write.table(tof$conv[, 3:ncol(tof$conv), with = FALSE], file = fnm, 
                  sep = ",", col.names = TRUE, row.names = FALSE)
      out <- list("impacts" = odf)
    } else if(todisk == FALSE) {
      out <- list("impacts" = odf, "conv" = tof$conv)
    }
    return(out)
  }
  
  outtab <- do.call(rbind, lapply(out_list, function(x) x$impacts))
  if(todisk == TRUE) {
    save(bcode, file = full_path(bdnm, "bcode.rda"))
    print(paste("Batch code is", bcode))
    save(outtab, file = full_path(dnm, "impacts_tab.rda"))
    save(parms, file = full_path(dnm, "parms.rda"))
    outf <- list("bcode" = bcode, "imp_list" = list("impacts" = outtab))
  }
  if(todisk == FALSE) {
    convlist <- lapply(out_list, function(x) x$conv)
    convnames <- sapply(out_list, function(x) as.character(x$impacts$rc[1]))
    names(convlist) <- convnames
    outf <- list("bcode" = bcode, 
                 "imp_list" = list("impacts" = outtab, "conv" = convlist))
    
  }
  return(outf)  
}
