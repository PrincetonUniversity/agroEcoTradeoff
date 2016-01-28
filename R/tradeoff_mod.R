#' Spatial tradeoffs model
#' @description Master module that calls four major modules and associated 
#' helper functions, to create a single land use tradeoff scenario.  
#' @param prod_targ Production targets passed as list. See examples for format.
#' @param ybetas list of 2 rasters or 2 vectors providing yield modifications
#' @param cbetas Vector of constraints to apply to land use. See examples. 
#' @param currprodmod Modifiers of current yield/production (default 1)
#' @param input_key Country/location code indicating input data to load.  
#' @param ybeta_update 1 or 0 (default) - determines whether yield_mod_* is run.
#' @param silent Silent or verbose mode (TRUE [default] or FALSE) 
#' @details Much of the detail of running the model, and examples that help to 
#' illustrate it, are in the examples for the individual model components, 
#' including checks for consistency between "D" and "R" versions.
#' @return Data.frame of impacts, and data.table referencing conversions.
#' @note The impacts module has to be fixed still to not double-count impacts 
#' resulting from multi-season cropping in converted pixels. 
#' @examples
# # production target list
#' prod_targ <- c("maize" = 4, "soy" = 2)
#' 
#' # cbeta vector
#' cbetas <- c(0.5, 0.5, 0, 0)
#' names(cbetas) <- c("Y", "C", "BD", "COST")
#' 
#' tdt <- tradeoff_mod("ZA", prod_targ, ybetas, cbetas)  
#' CRSobj <- projection(raster("external/ext_data/ZA-carbon-priorities.tif"))
#' plot(dt_to_raster(tdt$conv, CRSobj = CRSobj) - tdr$conv)  # check maps
#' @export 
# input_key = "ZA"; ybeta_update = 0; exist_list = NULL; 
# path = "external/data/dt/"
tradeoff_mod <- function(prod_targ, ybetas, cbetas, currprodmod = 1, 
                         input_key = "ZA", ybeta_update = 0, exist_list = NULL, 
                         silent = TRUE) {
  names(cbetas) <- c("Y", "C", "BD", "COST")
  
  # set up 
  bpath <- set_base_path()
  path <- full_path(bpath, full_path("external/data", input_key)) 
  # ha <- spatial_meta(input_key)$ha
  rc <- run_code(input_key)  # creates a once off code for any outputs
  il <- input_handler(path = path, ybetas = ybetas, ybeta_update = ybeta_update, 
                      exist_list = exist_list, silent = silent)
  
  # target module
  target <- targets(prod_targ = prod_targ, currprod = il$currprod, 
                    currprodmod = currprodmod)
  
  # constraints module 
  conv <- constraints(inlist = list("Y" = il$y_std, "C" = il$carbon_p, 
                                    "BD" = il$cons_p, "COST" = il$cost_p), 
                        cbetas = cbetas, # code = rc, cropnames = il$cropnames,
                        silent = silent)
  
  # convert module
  converted <- convert(conv_prob = conv, target = target, 
                       pot_yield = il$p_yield, cropnames = il$cropnames, 
                       base = il$mask, ha = il$sp$ha, keep_index = FALSE)

  # impacts
  impacts <- impact(conv = converted, il = il)
  
  out <- list("conv" = converted, "impacts" = impacts, "inputs" = il, 
              "runcode" = rc)
}