#' Calculates impacts of cropland conversions
#' @param conv data.table of cropland conversions
#' @param il List of inputs created by input_handler
#' @details Calculates summary statistics of the impacts of cropland 
#' conversions.
#' @note Some estimate of fragmentation might be useful here. 
#' @examples
#' # To be updated
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

