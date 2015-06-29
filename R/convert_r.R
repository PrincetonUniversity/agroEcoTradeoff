#' Land conversion module
#' @description This module takes inputs from the targets and constraints 
#' modules and determines which pixels are converted.  
#' @param conv_prob Conversion probability brick (from \code{\link{constraints*}})
#' @param target Production target data.frame 
#' @param crop_frac RasterBrick of crop allocation fraction
#' @param pot_yield RasterBrick of potential crop yields (modified by ybeta)
#' @param code Unique simulation code resulting from run_code function
#' @param cropnames Vector of crop names in analysis
#' @param write_out FALSE (default) or TRUE. If TRUE, brick is written to disk
#' @details This function relies on vectorization of input rasters to rank 
#' pixels for each crop by their conversion probability, calculate the cumulative 
#' production from most likely to least likely to be converted, and then finding 
#' the pixels which are less than the production target. These are marked as  
#' converted. 
#' @examples
#' rc <- run_code(input_key = "ZA")  
#' il <- fetch_inputs(input_key = "ZA", input = "R")  
#' ybetas <- list(1, 1)
#' ybeta <- yield_mod_r(inlist = il[c("p_yield", "pp_curr")], ybetas = ybetas, 
#'                      cropnames = il$cropnames) 
#' prod_targ <- c("maize" = 2, "cassava" = 2, "ground" = 2, "cotton" = 2, 
#'                "soy" = 2, "pulse" = 2, "sunflower" = 2, "sugarcane" = 2, 
#'                "wheat" = 2)
#' target <- targets_r(prod_targ, currprod = il$currprod, 
#'                     potprod = ybeta$pp_curr, cropnames = il$cropnames)
#' cbetas <- c("y_std" = 0, "C" = 1, "bd" = 1, "cost" = 1)
#' clist <- list("y_std" = ybeta$y_std, "C" = il$carbon_p, "bd" = il$cons_p, 
#'               "cost" = il$cost)
#' conv_prob <- constraints_r(inlist = clist, cbetas = cbetas, code = rc, 
#'                            cropnames = il$cropnames)
#' conv <- convert_r(conv_prob = conv_prob, target = target, 
#'                   crop_frac = il$cropfrac, pot_yield = ybeta$p_yield, 
#'                   code = il$code, cropnames = il$cropnames)
#' plot(conv)
#' @export
convert_r <- function(conv_prob, target, crop_frac, pot_yield, code, cropnames,
                      write_out = FALSE) {
  ha  <- res(crop_frac)[1]^2 / 10000
  pnm <- fname(full_path(set_base_path(), 
                         "external/output/convert/temp/prod-r-"), code)
  p_prod <- nm_up(rast_math(pot_yield * (crop_frac * ha)), cropnames)
  ind <- 1:ncell(conv_prob[[1]])
  r <- conv_prob[[1]]
  r[] <- NA
  converted <- lapply(1:nlayers(conv_prob), function(x) {
    targ <- target$target_newland[x]
    pv <- values(conv_prob[[x]])
    prodv <- values(p_prod[[x]])
    convert_df <- cbind(ind, pv, prodv)
    convert_df <- convert_df[order(convert_df[, 2], decreasing = TRUE), ]
    convert_df <- convert_df[!is.na(rowSums(convert_df)), ]
    converted <- ifelse(cumsum(convert_df[, 3]) < targ, 1, 0)
    test <- cbind(convert_df, converted)
    r2 <- r
    r2[convert_df[, "ind"]] <- converted
    r2
  })
  if(write_out == TRUE) {
    fnm <- fname(full_path(set_base_path(), "external/output/convert/conv-"), 
                 code)
    b <- nm_up(brick(stack(converted), filename = fnm, overwrite = TRUE), 
               cropnames)
  } else {
    b <- nm_up(brick(stack(converted)), cropnames)
  }
  return(b)
}

