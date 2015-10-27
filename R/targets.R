#' Calculates crop production targets
#' @description This module calculates the amount of production (in tonnes) for each modelled crop, given
#' user-specified targets, how much of it can be met on existing cropland, and how much needs to come from 
#' new cropland. This version works with rasters.  
#' @param prod_targ A named list given the production targets for each crop as a multiple (greater than 1)
#' @param currprod A RasterBrick of the current production coming from existing croplands
#' @param potprod A RasterBrick of the potential production on existing croplands
#' @param filename Output filename. Default is "default", in which case a system generated name is created
#' @seealso \code{\link{targets_dt}} for data.table version
#' @examples
#' rc <- run_code(input_key = "ZA")  # creates a once off code for any outputs saved from this simulation
#' ilr <- fetch_inputs(input_key = "ZA", input = "R")  # fetch all necessary inputs
#' ybetas <- c(1, 1)
#' ybetar <- yield_mod_r(inlist = ilr[c("p_yield", "pp_curr")], ybetas = ybetas, code = rc, 
#'                      cropnames = ilr$cropnames)
#' prod_targr <- c("maize" = 2, "cassava" = 2, "ground" = 2, "cotton" = 2, "soy" = 2, "pulse" = 2, 
#'                "sunflower" = 2, "sugarcane" = 2, "wheat" = 2)
#' targetr <- targets_r(prod_targ, currprod = ilr$currprod, potprod = ybetar$pp_curr, code = rc, 
#'                     cropnames = ilr$cropnames)
#' @export
targets_r <- function(prod_targ, currprod, potprod, cropnames) {#, code) {
   #fpath <- "external/output/yield-mod/"
   #pdnm <- fname(paste0(fpath, "yb-rst-"), code)
   prod_diff <- nm_up(rast_math(potprod - currprod), cropnames)
   prod_diff_m <- raster::as.matrix(prod_diff)
   prod_diff_m[prod_diff_m < 0] <- 0  # set any less to zero to zero
   t_df <- cbind.data.frame("current" = cellStats(currprod, sum), 
                            "potential" = colSums(prod_diff_m, na.rm = TRUE))
   t_df <- round(t_df)
   t_df$target <- t_df$current * prod_targ
   t_df$target_newland <- t_df$target - t_df$potential
   t_df$target_newland <- ifelse(t_df$target_newland < 0, 0, t_df$target_newland)
   t_df$t_pct_currland <- round(t_df$potential / t_df$target * 100, 1)
   t_df$t_pct_newland <- round(t_df$target_newland / t_df$target * 100, 1)
   #file.remove(list.files("external/output/targets/temp/", full.names = TRUE))
   return(t_df)
}

#' Calculates crop production targets
#' @description This module calculates the amount of production (in tonnes) for each modelled crop, given
#' user-specified targets, how much of it can be met on existing cropland, and how much needs to come from 
#' new cropland. This version works with dta.tables.  
#' @param prod_targ A named list given the production targets for each crop as a multiple (greater than 1)
#' @param currprod A data.table of the current production coming from existing croplands
#' @param potprod A data.table of the potential production on existing croplands
#' @details If there is a modification to yield potentials due to climate change or irrigation, potprod 
#' (which is again only the potential production on existing croplands) should be extracted from the results
#' of the \code{\link{yield_mod_dt}} function. 
#' @seealso \code{\link{targets_r}} for raster-based version
#' @examples
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' il_dt <- raster_list_to_dt(inlist = il[c("p_yield", "pp_curr")])
#' ybetas <- c(1, 1)
#' ybeta <- yield_mod_dt(inlist = il_dt[[2]], ybetas = ybetas, code = rc, cropnames = il$cropnames)
#' ybeta_r <- yield_mod_r(inlist = il[c("p_yield", "pp_curr")], ybetas = ybetas, code = rc, 
#'                        cropnames = il$cropnames)
#' potprod <- ybeta$pp_curr
#' currprod <- raster_list_to_dt(inlist = il["currprod"], base = FALSE)$currprod
#' prod_targ <- c("maize" = 2, "cassava" = 2, "ground" = 2, "cotton" = 2, "soy" = 2, "pulse" = 2, 
#'                "sunflower" = 2, "sugarcane" = 2, "wheat" = 2)
#' t_dt <- targets_dt(prod_targ, currprod, potprod)
#' rc <- run_code(input_key = "ZA")  # creates a once off code for any outputs saved from this simulation
#' t_r <- targets_r(prod_targ, il$currprod, ybeta_r$pp_curr, code = rc, cropnames = il$cropnames)
#' identical(t_dt, t_r)
#' @export
targets_dt <- function(prod_targ, currprod, potprod) {#, cropnames)
  # For disaggregation of the input grids, we need to similarly downsize the production of each pixel
  original_length <- 8783
  scale_factor <- nrow(currprod) / original_length
  currprod <- currprod / scale_factor
  potprod <- potprod / scale_factor
  
  prod_diff <- potprod - currprod
  mysum <- function(x) round(sum(x, na.rm = TRUE))
  for(i in seq_along(prod_diff)) {
    set(prod_diff, i = which(prod_diff[[i]] < 0), j = i, value = 0)
  }
  t_df <- cbind.data.frame("current" = t(currprod[, lapply(.SD, mysum)]), 
                           "potential" = t(prod_diff[, lapply(.SD, mysum)]))
  t_df$target <- t_df$current * prod_targ
  t_df$target_newland <- t_df$target - t_df$potential
  t_df$target_newland <- ifelse(t_df$target_newland < 0, 0, t_df$target_newland)
  t_df$t_pct_currland <- round(t_df$potential / t_df$target * 100, 1)
  t_df$t_pct_newland <- round(t_df$target_newland / t_df$target * 100, 1)
  return(t_df)
}

