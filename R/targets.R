#' Calculates crop production targets
#' @description This module calculates the amount of production (in tonnes) for each modelled crop, given
#' user-specified targets, how much of it can be met on existing cropland, and how much needs to come from 
#' new cropland. This version works with dta.tables.  
#' @param prod_targ A named list given the production targets for each crop as a multiple (greater than 1)
#' @param currprod A data.table of the current production coming from existing croplands
#' @param currprodmod A vector to modify production levels on existing cropland. 
#' @details This determines how much cropland has to be converted. The argument 
#' currprodmod can be used to simulate an assumption that yield gaps on current
#' cropland will be closed before new land is needed (or that a greater amount of 
#' new land will be needed if current production falls, if currprodmod < 1).
#' @examples
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' currprodmod <- 1
#' currprod <- il$currprod
#' prod_targ <- c("maize" = 2, "soy" = 2)
#' targets(prod_targ, currprod, currprodmod)
#' currprodmod <- c(1.2, 0.9)
#' targets(prod_targ, currprod, currprodmod)
#' currprodmod <- c(1.2, 0.9, 0.8)
#' targets(prod_targ, currprod, currprodmod)  # fails
#' currprodmod <- NULL
#' targets(prod_targ, currprod, currprodmod)  # fails
#' currprodmod <- c(1, 1)
#' targets(prod_targ, currprod, currprodmod)  # fails
#' currprodmod <- c(2, 2)
#' targets(prod_targ, currprod, currprodmod)  # fails
#' currprodmod <- c(0.5, 0.5)
#' targets(prod_targ, currprod, currprodmod)
#' @export
targets <- function(prod_targ, currprod, currprodmod) {#, cropnames)
 
  if((length(currprodmod) != 1) & (length(currprodmod) != length(currprod))) {
    stop(paste("currprodmod should be a single value, or the same length", 
               "as currprod (i.e. one value for each crop being modified)"), 
         call. = FALSE)
  } 
  potprod <- currprodmod * currprod  # modify current production levels
  prod_diff <- potprod - currprod  # is there a difference
  # mysum <- function(x) round(sum(x, na.rm = TRUE))
#   for(i in seq_along(prod_diff)) {
#     set(prod_diff, i = which(prod_diff[[i]] < 0), j = i, value = 0)
#   }
#   t_df <- cbind.data.frame("current" = t(currprod[, lapply(.SD, mysum)]), 
#                            "potential" = t(prod_diff[, lapply(.SD, mysum)]))
  t_df <- cbind.data.frame("current" = currprod, "potential" = prod_diff)
  t_df$target <- t_df$current * prod_targ
  t_df$target_newland <- t_df$target - t_df$potential
  t_df$target_newland <- ifelse(t_df$target_newland < 0, 0, t_df$target_newland)
  t_df$t_pct_currland <- round(t_df$potential / t_df$target * 100, 1)
  t_df$t_pct_newland <- round(t_df$target_newland / t_df$target * 100, 1)
  return(t_df)
}

