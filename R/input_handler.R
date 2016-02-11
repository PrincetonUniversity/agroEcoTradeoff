#' Raster ybetas to data.tables (deprecated)
#' @param ybetas list of either 2 rasters or 2 vectors providing yield 
#' modifications for climate & irrigation.
#' @param cropnames Vector of cropnames in model
#' @keywords internal
#' @export
ybeta_rast_to_dt <- function(ybetas, cropnames, base) {
  for(i in 1:length(ybetas)) {
    if(tolower(gsub("Brick|Stack|Layer", "", 
                    class(ybetas[[i]])[1])) == "raster") {
      if(nlayers(ybetas[[i]]) != length(cropnames)) {
        stop("Yield modifying rasters should have one layer per crop", 
             call. = FALSE)
      }
      valind <- base$ind
      ybetas[[i]] <- as.data.table(ybetas[[i]])[valind, ]
    }
  }
  return(ybetas)
}

#' Data preparation function
#' @param ybetas list of either 2 rasters or 2 vectors providing yield 
#' modifications for climate & irrigation.
#' @param input "D" or "R". "D" runs the (faster) data.table version of the 
#' model, "R" the raster version.
#' @param code Unique simulation code.
#' @param ybeta_update 1 or 0; determines whether yield_mod_* is run or not.
#' @param exist_list Default is NULL, other an existing list of processed inputs
#' @param silent Hide or show print statements describing processing steps 
#' (TRUE [default] or FALSE)
#' @return List of input variables for model 
#' @details This function reads in the necessary raster or data.table inputs for 
#' tradeoff_mod, but will pass through an existing list if they are provided in 
#' exist_list. Yield modifications and standardizations are performed when 
#' ybeta_update = 1 and/or exist_list = NULL. In exist_list is NULL, then the 
#' full routine will always be run.  
#' @note It is currently used internally by tradeoff_mod, but could well be 
#' pulled outside of it. 
#' @examples
#' # Outdated and removed, need to be added
#' il <- input_handler(path = path)
#' @export
input_handler <- function(path, ybetas = list(1, 1), ybeta_update = 0, 
                          exist_list = NULL, silent = TRUE, code = NULL) {
#   input_key = "ZA"; exist_list = NULL; silent = TRUE; 
  
  # path = "external/data/ZA"
#   ybetas <- list(1, 1); code = run_code(input_key); ybeta_update <- 0
# exist_list = toff$inputs 
  
  lnms <- c("p_yield", "carbon", "mask", "cost", "bd", "cons", "cropnames", 
            "convertible", "intpa")
  
  if(ybeta_update == 1) {
    stop("The yield modification facility is currently switched off", 
         call. = FALSE)
  }

  if(!is.null(exist_list) & any(!lnms %in% names(exist_list))) {
    stop("Input list must have all variables", call. = FALSE) 
  }
  # no existing data list provided 
  if(is.null(exist_list)) {  # il_y
    il <- fetch_inputs(path = path)  # fetch inputs
    
    # check for zeros in yields
    zerocheck <- function(x) if(any(x == 0)) {
      stop("Yields can't have zeros because normalization fails", call. = FALSE)
    }
    il$p_yield[, zerocheck(.SD)]  
    # ybetas <- ybeta_rast_to_dt(ybetas, cropnames = il$cropnames, base = il$mask)
    # ybeta <- yield_mod(inlist = il["p_yield"], ybetas = ybetas, 
    #                   code = code, cropnames = il$cropnames, 
    #                   silent = silent)
    # il[["p_yield"]] <- ybeta[["p_yield"]]
    
    # have to make a second mask now of convertible areas
    newmask <- cbind(il$mask, il$convertible)
    valinds <- which(newmask$convertible > 0)  # values to keep
    newmask <- newmask[valinds, ]  # reduce mask to just farmable area
    setkey(newmask, "ind") 
    # plot(dt_to_raster(outlist$mask, CRSobj = CRS(il$sp$crs)))
    
    # remove unfarmable areas from all data.tables
    il_nms <- c("p_yield", "carbon", "cost", "bd", "cons", "intpa")
    il[il_nms] <- lapply(il_nms, function(x) il[[x]][valinds, ])
    
    # standardize
    # Crops
    # Standardize over all values, not by crop
    il$y_std <- 1 - standardize(1 / il$p_yield)  
    
    # carbon
    carbonperyield <- 1 / il$p_yield
    carbon <- il$carbon$veg + il$carbon$soil * 0.25  # hard-code loss
    for(j in il$cropnames) {
     set(carbonperyield, i = NULL, j = j, carbonperyield[[j]] * carbon)
    }
    il$carbon_p <- 1 - standardize(carbonperyield)
    
    # biodiversity 
    bdperyield <- 1 / il$p_yield
    for(j in il$cropnames) {
     set(bdperyield, i = NULL, j = j, bdperyield[[j]] * 
          il$cons[, grep("cons", names(il$cons)), with = FALSE])  
    }
    il$cons_p <- 1 - standardize(bdperyield)
    
    # cost
    costperyield <- 1 / il$p_yield
    for(j in il$cropnames) {
     set(costperyield, i = NULL, j = j, costperyield[[j]] * 
          il$cost[, grep("cost", names(il$cost)), with = FALSE])
    }
    il$cost_p <- 1 - standardize(costperyield)
    
    il[["mask"]] <- newmask
    
  }
  
  # if existing list is provided but ybeta needs adjustment
  # if(!is.null(exist_list) & ybeta_update == 1) {
  #  stop(paste("Modifying potential yields in an existing list is switched",
  #             "off for the time being"), call. = FALSE)
  #  ybetas <- ybeta_rast_to_dt(ybetas, cropnames = il$cropnames, base = il$mask)
  #  ybeta <- yield_mod(inlist = exist_list[c("p_yield", "pp_curr")], 
  #                     ybetas = ybetas, code = code, 
  #                     cropnames = il$cropnames)
  #  outlist <- exist_list
  #  outlist$y_std <- ybeta$y_std
  #  outlist[c("p_yield", "pp_curr")] <- ybeta[c("p_yield", "pp_curr")]
  # }

  if(!is.null(exist_list) & ybeta_update == 0) {
    il <- exist_list
  }
  return(il)
}

