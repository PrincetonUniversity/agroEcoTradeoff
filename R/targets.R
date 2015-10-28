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
#' ybetas <- list(1, 1)
#' ybeta <- yield_mod_dt(inlist = il[c("p_yield", "pp_curr")], ybetas = ybetas, 
#'                       code = rc, cropnames = il$cropnames)
#' potprod <- ybeta$pp_curr
#' currprod <- il$currprod
#' prod_targ <- c("maize" = 2, "cassava" = 2, "ground" = 2, "cotton" = 2, "soy" = 2, "pulse" = 2, 
#'                "sunflower" = 2, "sugarcane" = 2, "wheat" = 2)
#' t_dt <- targets_dt(prod_targ, currprod, potprod)
#' @export
targets_dt <- function(prod_targ, currprod, potprod) {#, cropnames)

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

