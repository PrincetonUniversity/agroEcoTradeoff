#' Raster ybetas to data.tables
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
      ybetas[[i]] <- as.data.table.raster(ybetas[[i]])[valind, ]
    }
  }
  return(ybetas)
}

#' Data preparation function
#' @param input_key Two letter country/location code (but possibly arbitrary) 
#' indicating input data to load.  
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
#' load("data/cropnames.rda")
#' rc <- run_code(input_key = "ZA")  
#' dfact <- c(0.9, 1.2)
#' ybetas <- lapply(1:2, function(x) {
#'   m <- brick("external/ext_data/ZA-crop-areas.tif")
#'   r <- m
#'   r <- setValues(r, values = rnorm(n = ncell(r) * nlayers(r), mean = dfact[x], 
#'                                    sd = 0.05))
#'   nm_up(mask(r, m, maskvalue = 0), cropnames)
#' })
#' 
#' input_handler(input_key = "ZA", ybetas = ybetas, input = "D", code = rc, 
#'               ybeta_update = 1)
#' input_handler(input_key = "ZA", ybetas = ybetas, input = "R", code = rc, 
#'               ybeta_update = 1)
#' il <- fetch_inputs(input_key = "ZA")  #' fetch all necessary inputs 
#' input_handler(input_key = "ZA", ybetas = ybetas, input = "D", code = rc, 
#'               ybeta_update = 0, exist_list = il)
#' input_handler(input_key = "ZA", ybetas = ybetas, input = "D", code = rc, 
#'               ybeta_update = 0, exist_list = il[-1])
#' input_handler(input_key = "ZA", ybetas = ybetas, input = "D", code = rc, 
#'               ybeta_update = 1, exist_list = il)
#' input_handler(input_key = "ZA", ybetas = list(1, 1), input = "D", code = rc, 
#'               ybeta_update = 1, exist_list = il)
#' 
#' il <- fetch_inputs(input_key = "ZA", input = "R") 
#' input_handler(input_key = "ZA", ybetas = ybetas, input = "R", code = rc, 
#'               ybeta_update = 0, exist_list = il)
#' input_handler(input_key = "ZA", ybetas = ybetas, input = "R", code = rc, 
#'               ybeta_update = 0, exist_list = il[-1])
#' input_handler(input_key = "ZA", ybetas = ybetas, input = "R", code = rc, 
#'               ybeta_update = 1, exist_list = il)
#' @export
input_handler <- function(input_key = "ZA", ybetas, input = "D", code, 
                          ybeta_update, exist_list = NULL, silent = TRUE) {
  lnms <- c("currprod", "pp_curr", "p_yield", "cropfrac", "carbon", "mask", 
            "cons_p", "cost", "carbon_p", "richness", "pas", "cropnames")
  #ha <- res(il$currprod)[2]^2 / 10000
  impnames <- c("carbon", "richness", "pas")
  if(!is.null(exist_list) & any(!lnms %in% names(exist_list))) {
    stop("Input list must have all variables", call. = FALSE) 
  }
  # no existing data list provided 
  if(input == "D" & is.null(exist_list)) {  # il_y
    il <- fetch_inputs(input_key = input_key)  # fetch all necessary inputs 
    ybetas <- ybeta_rast_to_dt(ybetas, cropnames = il$cropnames, base = il$mask)
    ybeta <- yield_mod_dt(inlist = il[c("p_yield", "pp_curr")], ybetas = ybetas, 
                          code = code, cropnames = il$cropnames, silent = silent)
    outlist <- il
    outlist$y_std <- ybeta$y_std
    outlist[c("p_yield", "pp_curr")] <- ybeta[c("p_yield", "pp_curr")]
  }
  # if existing list is provided but ybeta needs adjustment
  if(input == "D" & !is.null(exist_list) & ybeta_update == 1) {
    ybetas <- ybeta_rast_to_dt(ybetas, cropnames = il$cropnames, base = il$mask)
    ybeta <- yield_mod_dt(inlist = exist_list[c("p_yield", "pp_curr")], 
                          ybetas = ybetas, code = code, cropnames = il$cropnames)
    outlist <- exist_list
    outlist$y_std <- ybeta$y_std
    outlist[c("p_yield", "pp_curr")] <- ybeta[c("p_yield", "pp_curr")]
  }
  if(input == "D" & !is.null(exist_list) & ybeta_update == 0) {
    outlist <- exist_list
  }
  if(input == "R" & is.null(exist_list)) {  # il_y
    il <- fetch_inputs(input_key = input_key, input = "R") 
    ybeta <- yield_mod_r(inlist = il[c("p_yield", "pp_curr")], ybetas = ybetas, 
                         code = code, cropnames = il$cropnames, silent = silent)
    outlist <- il
    outlist$y_std <- ybeta$y_std
    outlist[c("p_yield", "pp_curr")] <- ybeta[c("p_yield", "pp_curr")]
  } 
  if(input == "R" & !is.null(exist_list) & ybeta_update == 1) {
    if(any(!lnms %in% names(exist_list))) {
       stop("Input list must have all variables", call. = FALSE)
    }
    ybeta <- yield_mod_r(inlist = exist_list[c("p_yield", "pp_curr")], 
                         ybetas = ybetas, code = code, cropnames = il$cropnames)
    outlist <- exist_list
    outlist$y_std <- ybeta$y_std
    outlist[c("p_yield", "pp_curr")] <- ybeta[c("p_yield", "pp_curr")]
  }
  if(input == "R" & !is.null(exist_list) & ybeta_update == 0) {
    outlist <- exist_list
  }
  return(outlist)
}

