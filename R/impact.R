

#' Calculates impacts of cropland conversions
#' @param conv data.table of cropland conversions
#' @param il List of inputs created by input_handler
#' @details Calculates summary statistics of the impacts of cropland 
#' conversions.
#' @note Some estimate of fragmentation might be useful here. 
#' @examples
#' rc <- run_code(input_key = "ZA")  
#' il <- input_handler(input_key = "ZA", ybetas = c(1, 1), code = rc, 
#'                     ybeta_update = 1)
#' CRSobj <- projection(raster("external/ext_data/ZA-carbon.tif"))
#' prod_targ <- c("maize" = 4,  "soy" = 2)
#' ybetas <- list(1, 1)
#' cbetas <- c(1, 1, 0, 0)
#' names(cbetas) <- c("Y", "C", "BD", "COST")
#' impnames <- c("carbon", "richness", "pas")
#' 
#' # target
#' target <- targets(prod_targ = prod_targ, currprod = il$currprod, 
#'                      potprod = il$pp_curr)
#' # constraints module 
#' c_prob <- constraints(inlist = il[c("y_std", "carbon_p", "cons_p", 
#'                          "cost")], 
#'                          cbetas = cbetas, code = rc, 
#'                          cropnames = il$cropnames)
#' 
#' # convert module
#' converted <- convert(conv_prob = c_prob, target = target, 
#'                     crop_frac = il$cropfrac, 
#'                         pot_yield = il$p_yield, cropnames = il$cropnames, 
#'                         base = il$mask, ha = ha, keep_index = FALSE)
#' # impacts
#' impacts <- impact(conv = converted, il = il)

#' @export
impact <- function(conv, il) {
  
  cn <- il$cropnames
  
  # area converted to each crop (ha)
  conv_area <- conv[, lapply(.SD, function(x) {
    x * il$mask[, convertible * il$sp$ha]
   }), .SDcols = cn]  
  conv_area_tot <- round(conv_area[, colSums(.SD), .SDcol = cn])  # total
  conv_area_tot <- data.frame(t(conv_area_tot))
  rownames(conv_area_tot) <- "conv_area" 

  # impact module 1 (carbon)
  impact1 <- impact_mod1(il, conv, conv_area)
  
  # impact module 2 (cost)
  impact2 <- impact_mod2(il, conv, conv_area)
  
  # impact module 3 (biodiversity)
  impact3 <- impact_mod3(il, conv_area)
  
  # assemble and return
  impacts <- t(rbind(conv_area_tot, impact1, impact2, impact3))
  return(impacts)
}

