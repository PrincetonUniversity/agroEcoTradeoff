#' Spatial tradeoffs model
#' @description Master module that calls four major modules and associated 
#' helper functions, to create a single land use tradeoff scenario.  
#' @param prod_targ Production targets passed as list. See examples for format.
#' @param ybetas list of 2 rasters or 2 vectors providing yield modifications
#' @param cbetas Vector of constraints to apply to land use. See examples. 
#' @param ybeta_update 1 (default) or 0 - determines whether yield_mod_* is run.
#' @param input_key Country/location code indicating input data to load.  
#' @param input "D" or "R" for data.table or raster version of the model.
#' @param ctype Specific multiplicative ("X") or additive ("+") constraints 
#' @param silent Silent or verbose mode (TRUE [default] or FALSE) 
#' @details Much of the detail of running the model, and examples that help to 
#' illustrate it, are in the examples for the individual model components, 
#' including checks for consistency between "D" and "R" versions.
#' @return Data.frame of impacts, and data.table referencing conversions.
#' @note The impacts module has to be fixed still to not double-count impacts 
#' resulting from multi-season cropping in converted pixels. 
#' @examples
#' # Some inputs 
#' load("data/cropnames.rda")
#' rc <- run_code(input_key = "ZA")  
#' 
#' # dummy climate/irrigation modifiers
#' dfact <- c(0.9, 1.2)
#' ybetas <- lapply(1:2, function(x) {
#'   m <- brick("external/ext_data/ZA-crop-areas.tif")
#'   r <- m
#'   r <- setValues(r, values = rnorm(n = ncell(r) * nlayers(r), mean = dfact[x], 
#'                  sd = 0.05))
#'   nm_up(mask(r, m, maskvalue = 0), cropnames)
#' })
#' ybetas <- list(1, 1)
#' # production target list
#' prod_targ <- c("maize" = 4, "cassava" = 2, "ground" = 2, "cotton" = 2, 
#'                "soy" = 2, "pulse" = 2, "sunflower" = 2, "sugarcane" = 2, 
#'                "wheat" = 2)
#' 
#' # cbeta vector
#' cbetas <- c(1, 1, 0.5, 0)
#' names(cbetas) <- c("Ag", "C", "bd", "cost")
#' 
#' # Comparing the two versions
#' system.time(tdt <- tradeoff_mod(prod_targ, ybetas, cbetas))  # 2.5 seconds
#' system.time(tdr <- tradeoff_mod(prod_targ, ybetas, cbetas, input = "R"))  # 6 
#' identical(tdt$impacts, tdr$impacts)  # check output stats for consistency
#' 
#' CRSobj <- projection(raster("external/ext_data/ZA-carbon-priorities.tif"))
#' plot(dt_to_raster(tdt$conv, CRSobj = CRSobj) - tdr$conv)  # check maps
#' @export 
#input_key = "ZA"; input = "D"; ybeta_update = 1; exist_list = NULL
tradeoff_mod2 <- function(prod_targ, ybetas, cbetas, input_key = "ZA", 
                         input = "D", ybeta_update = 1, exist_list = NULL, 
                         ctype = "X", silent = TRUE) {
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
    target <- targets_dt2(prod_targ = prod_targ, currprod = il$currprod, 
                         potprod = il$pp_curr)
  } else if(input == "R") {
    target <- targets_r(prod_targ = prod_targ, currprod = il$currprod, 
                        potprod = il$pp_curr, cropnames = il$cropnames)
  }
  
  
  yield <- il$p_yield
  carbonperyield <- 1/yield #for division
  carbon <- il$carbon$veg + il$carbon$soil * 0.25
  for(j in il$cropnames) set(carbonperyield, i = NULL, j = j, carbonperyield[[j]] * 
                            carbon)
  carbonperyield <- 1 - (carbonperyield - min(carbonperyield))/diff(range(carbonperyield))
  
  # Temporarily use protected areas as sole biodiversity metric
  
  bdperyield <- 1/yield
  bd <- il$pas
  bd[(is.na(bd))] <- 0
  for(j in il$cropnames) set(bdperyield, i = NULL, j = j, bdperyield[[j]] * 
                               bd)
  bdperyield <- 1 - (bdperyield - min(bdperyield))/diff(range(bdperyield))
  
  # New standardized yields
  
  y_std_tot <- (il$p_yield - min(il$p_yield))/diff(range(il$p_yield))
  
  # constraints module 
  if(input == "D") {
    c_prob <- constraints_dt2(inlist = list("y_std" = y_std_tot, "C" = carbonperyield, 
                                            "bd" = bdperyield, "cost" = il$cost), 
                              cbetas = cbetas, code = rc, 
                              cropnames = il$cropnames, ctype = ctype, 
                              silent = silent)
  } else if(input == "R") {
    rasterOptions(tmpdir = "external/output/temp")  # set tempfile directory
    c_prob <- constraints_r(inlist = list("y_std" = il$y_std, "C" = il$carbon_p, 
                                          "bd" = il$cons_p, "cost" = il$cost), 
                            cbetas = cbetas, code = rc, cropnames = il$cropnames, 
                            silent = silent)
  }
  
  # convert module
  if(input == "D") {
    converted <- convert_dt4(conv_prob = c_prob, target = target, 
                            crop_frac = il$cropfrac, pot_yield = il$p_yield, 
                            cropnames = il$cropnames, base = il$mask, ha = ha, 
                            keep_index = FALSE)
  } else if(input == "R") {
    converted <- convert_r(conv_prob = c_prob, target = target, 
                           crop_frac = il$cropfrac, pot_yield = il$p_yield, 
                           cropnames = il$cropnames, code = rc) 
  }
  
  # impacts
  if(input == "D") {
    impacts <- impact_dt2(conv = converted, 
                         carbon = il$carbon[, c("veg", "soil"), with = FALSE], 
                         pot_yield = il$p_yield, 
                         div_list = il[c("richness", "pas")], 
                         crop_frac = il$cropfrac, 
                         cropnames = il$cropnames, ha = ha)
  } else if(input == "R") {
    impacts <- impact_r(conv = converted, 
                        carbon = il$carbon[[c("veg", "soil")]], 
                        pot_yield = il$p_yield, 
                        div_list = il[c("richness", "pas")], 
                        crop_frac = il$cropfrac, cropnames = il$cropnames, 
                        ha = ha)
  }
  out <- list("conv" = converted, "impacts" = impacts, "inputs" = il, 
              "runcode" = rc)
}
