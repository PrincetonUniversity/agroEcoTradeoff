#' Calculates impacts of cropland conversions
#' @param conv Raster* of cropland conversions
#' @param carbon RasterBrick of veg and soil carbon stocks
#' @param pot_yield RasterBrick of potential crop yields (modified as needed)
#' @param div_list list of diversity rasters
#' @param crop_frac Raster* of cropland fractions
#' @param cropnames Vector of crop names in analysis 
#' @param ha Pixel area
#' @details Calculates summary statistics of the impacts of cropland 
#' conversions, including total tons carbon lost, carbon loss/ton crop yield, 
#' total area converted per crop, total area loss of PAs, mean diversity of
#' converted pixels, mean diversity/ton crop yield
#' @note Some estimate of fragmentation might be useful here. I have to fix
#' impacts to not double-count where winter-crops are grown (wheat here)
#' @export
impact_r <- function(conv, carbon, pot_yield, div_list, crop_frac, cropnames, 
                     ha) {
  conv_frac <- nm_up(rast_math(crop_frac * conv), cropnames)  
  conv_area <- nm_up(rast_math(conv * crop_frac * ha), cropnames)  
  conv_area_tot <- calc(conv_area, sum)  # total area of conversion
  closs <- (carbon$veg + carbon$soil * 0.25)
  mymean <- function(b) {
  o <- round(sapply(1:nlayers(b), function(x) { 
      v <- values(b[[x]])
      mu <- mean(v[v > 0], na.rm = TRUE)
    }), 1)
    names(o) <- names(b)
    return(o) 
  }
 
  b <- nm_up(rast_math(closs / conv_frac), cropnames) 
  cyld <- mymean(b)  # mean closs/yld ratios
  closs_sum <- cellStats(nm_up(rast_math(conv_area * closs), cropnames), sum)  
 
  # Biodiversity loss - species richness
  bdmu <- mymean(b <- nm_up(rast_math(conv * div_list[["richness"]]), 
                            cropnames))
  bdyld <- mymean(nm_up(rast_math(b / pot_yield), cropnames))
  pas <- div_list[["pas"]]
  pas[which(!is.na(values(pas)))] <- 1
  paloss <- round(cellStats(nm_up(rast_math(pas * conv_area), cropnames), sum))
  conv_areat <- round(cellStats(conv_area, sum))
  crop_impacts <- cbind.data.frame("conv_area" = conv_areat,"rich_mu" = bdmu,
                                   "rich/t_yld" = bdyld, "pa_loss" = paloss, 
                                   "tC/t_yld" = cyld, "C_tot" = closs_sum)
  return(crop_impacts)
}
