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
      ybetas[[i]] <- as.data.table(ybetas[[i]])[valind, ]
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
#' input_handler(input_key = "ZA", ybetas = ybetas, code = rc, 
#'               ybeta_update = 1)
#' il <- fetch_inputs(input_key = "ZA")  #' fetch all necessary inputs 
#' input_handler(input_key = "ZA", ybetas = ybetas, code = rc, 
#'               ybeta_update = 0, exist_list = il)
#' input_handler(input_key = "ZA", ybetas = ybetas, code = rc, 
#'               ybeta_update = 0, exist_list = il[-1])
#' input_handler(input_key = "ZA", ybetas = ybetas, code = rc, 
#'               ybeta_update = 1, exist_list = il)
#' input_handler(input_key = "ZA", ybetas = list(1, 1), code = rc, 
#'               ybeta_update = 1, exist_list = il)
#' @export
input_handler <- function(input_key = "ZA", ybetas, code, ybeta_update, 
                          exist_list = NULL, silent = TRUE, 
                          path = "external/data/dt") {
#   input_key = "ZA"; exist_list = NULL; silent = TRUE; 
#   path = "external/data/dt/new/"
#   ybetas <- list(1, 1); code = run_code(input_key); ybeta_update <- 0
  lnms <- c("p_yield", "carbon", "mask", "cost", "bd", "cons", "cropnames", 
            "convertible", "intpa")

  if(!is.null(exist_list) & any(!lnms %in% names(exist_list))) {
    stop("Input list must have all variables", call. = FALSE) 
  }
  # no existing data list provided 
  if(is.null(exist_list)) {  # il_y
    il <- fetch_inputs(path = path, input_key = input_key)  # fetch inputs 
    ybetas <- ybeta_rast_to_dt(ybetas, cropnames = il$cropnames, base = il$mask)
    ybeta <- yield_mod(inlist = il["p_yield"], ybetas = ybetas, 
                       code = code, cropnames = il$cropnames, 
                       silent = silent)
    outlist <- il
    # outlist$y_std <- ybeta$y_std
    # outlist[c("p_yield", "pp_curr")] <- ybeta[c("p_yield", "pp_curr")]
    outlist[["p_yield"]] <- ybeta[["p_yield"]]
  }
  # if existing list is provided but ybeta needs adjustment
  if(!is.null(exist_list) & ybeta_update == 1) {
    stop(paste("Modifying potential yields in an existing list is switched",
               "off for the time being"), call. = FALSE)
  #  ybetas <- ybeta_rast_to_dt(ybetas, cropnames = il$cropnames, base = il$mask)
  #  ybeta <- yield_mod(inlist = exist_list[c("p_yield", "pp_curr")], 
  #                     ybetas = ybetas, code = code, 
  #                     cropnames = il$cropnames)
  #  outlist <- exist_list
  #  outlist$y_std <- ybeta$y_std
  #  outlist[c("p_yield", "pp_curr")] <- ybeta[c("p_yield", "pp_curr")]
  }
  if(!is.null(exist_list) & ybeta_update == 0) {
    outlist <- exist_list
  }
  
#   plot(dt_list_to_raster(base = newmask, 
#                          outlist["p_yield"], CRSobj = CRS(il$sp$crs)))
#   plot(dt_to_raster(cbind(newmask, outlist$p_yield), CRSobj = CRS(il$sp$crs)))
  

  # have to make a second mask now of convertible areas
  newmask <- cbind(outlist$mask, outlist$convertible)
  valinds <- which(newmask$convertible > 0)  # values to keep
  newmask <- newmask[valinds, ]  # reduce mask to just farmable area
  setkey(newmask, "ind") 
  # plot(dt_to_raster(outlist$mask, CRSobj = CRS(il$sp$crs)))
  
  # remove unfarmable areas from all data.tables
  ol_nms <- c("p_yield", "carbon", "cost", "bd", "cons", "intpa")
  outlist[ol_nms] <- lapply(ol_nms, function(x) outlist[[x]][valinds, ])
  
  # standardize
  # Crops
  # Standardize over all values, not by crop
  ### this standardization assumption needs to be validated 
  outlist$y_std <- 1 - standardize(1 / outlist$p_yield)  
  
  # calculate farmable area
  # farmha <- newmask[["convertible"]] * outlist$sp$ha  # farmable area
  
  # hist(outlist$p_yield$soy)

  # production <- outlist$p_yield[, lapply(.SD, function(x) x * farmha)]
  # y_std <- standardize(production)  # and standardize
  
#   par(mfrow = c(1, 2))
#   hist(standardize(production$maize))
#   hist(standardize(production$soy))
#   hist(y_std$maize)
#   hist(y_std$soy)
#   
#   hist(outlist$p_yield$soy)
#   hist(production$maize)
#   hist(production$soy)
#   

  # y_std2 <- 1 - standardize(1 / production)  # this assumption needs fixing 
  
  # Calculate conversion probabilities so they are done by impact/production
  # for division
  # carbon
  # carbonperyield <- copy(outlist$p_yield)[, c(outlist$cropnames) := 
                                     # lapply(.SD, function(x) 1 / x), 
                                     # .SDcols = outlist$cropnames]
  carbonperyield <- 1 / outlist$p_yield
  carbon <- outlist$carbon$veg + outlist$carbon$soil * 0.25  # hard-code loss
  for(j in outlist$cropnames) {
    set(carbonperyield, i = NULL, j = j, carbonperyield[[j]] * carbon)
  }
  outlist$carbon_p <- 1 - standardize(carbonperyield)
  
  # biodiversity 
  bdperyield <- 1 / outlist$p_yield
  for(j in outlist$cropnames) {
   set(bdperyield, i = NULL, j = j, bdperyield[[j]] * 
        outlist$cons[, grep("cons", names(outlist$cons)), with = FALSE])  
  }
  outlist$cons_p <- standardize(bdperyield)
  
  # cost
  costperyield <- 1 / outlist$p_yield
  for(j in outlist$cropnames) {
    set(costperyield, i = NULL, j = j, costperyield[[j]] * 
         outlist$cost[, grep("cost", names(outlist$cost)), with = FALSE])
  }
  outlist$cost_p <- 1 - standardize(costperyield)

  outlist[["mask"]] <- newmask
  return(outlist)
}

