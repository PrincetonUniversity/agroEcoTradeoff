#' Land conversion module
#' @description This module takes inputs from the targets and constraints modules 
#' and determines which pixels are converted. This is an alternate version of 
#' convert, which is relies on data.table functions. 
#' @param conv_prob Conversion probability brick
#' @param prod_targ Production target data.frame
#' @param crop_frac RasterBrick of crop allocation fraction
#' @param pot_yield RasterBrick of potential crop yields (modified, if necessary)
#' @param cropnames Vector of crop names in analysis
#' @param base Name of base data.table that provides grid numbers and coordinates
#' @param keep_index FALSE, otherwise valind is retained in the output
#' @details This function relies on vectorization of input rasters to rank pixels 
#' for each crop by their conversion probability, calculate the cumulative 
#' production from most likely to least likely to be converted, and then finding 
#' the pixels which are less than the production target. These are marked as  
#' converted. This function uses the as.data.table.raster function by etiennebr, 
#' the link to which is shown in the note below. 
#' @note https://gist.github.com/etiennebr/9515738 
#' @examples
#' # We'll compare both raster and data.table methods here
#' rc <- run_code(input_key = "ZA")  
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' ybetas <- list(1, 1)
#' ybeta <- yield_mod_dt(inlist = il[c("p_yield", "pp_curr")], ybetas = ybetas, 
#'                       code = rc, cropnames = il$cropnames) 
#' clist <- list("y_std" = ybeta$y_std, "C" = il$carbon_p, "bd" = il$cons_p, 
#'               "cost" = il$cost)
#' CRSobj <- projection(raster("external/ext_data/ZA-carbon.tif"))
#' ha <- res(raster("external/ext_data/ZA-carbon.tif"))[2]^2 / 10000
#' prod_targ <- c("maize" = 4, "cassava" = 2, "ground" = 2, "cotton" = 2, 
#'                "soy" = 2, "pulse" = 2, "sunflower" = 2, "sugarcane" = 2, 
#'                "wheat" = 2)
#' target <- targets_dt(prod_targ = prod_targ, currprod = il$currprod, 
#'                      potprod = ybeta$pp_curr)
#' #targetr <- targets_r(prod_targ, currprod = il$currprod, 
#'                       potprod = ybetar$pp_curr, code = rc, 
#'                       cropnames = il$cropnames)
#' #identical(target, targetr)
#' 
#' cbetas <- c("y_std" = 0, "C" = 1, "bd" = 1, "cost" = 1)
#' conv_prob <- constraints_dt(inlist = clist, cbetas = cbetas, code = rc, 
#'                             cropnames = il$cropnames)
#' valind <- which(values(il$mask) == 1) 
#' system.time(conv <- convert_dt(conv_prob, target = target, 
#'                                crop_frac = il$cropfrac, 
#'                                pot_yield = ybeta$p_yield, valind = valind, 
#'                                cropnames = il$cropnames, 
#'                                base = il$grid, ha = ha))  # 0.135 seconds
#' system.time(convr <- convert_r(conv_prob = conv_probr2, target = targetr, 
#'                                crop_frac = il$cropfrac, 
#'                                pot_yield = ybetar$p_yield, 
#'                                code = il$code, cropnames = il$cropnames))  # 2.9s
#' convr2 <- dt_to_raster(dt = conv, CRSobj = projection(il$currprod))
#' plot(convr - convr2)  # both approaches equal
#' 
#' cbetas <- c("y_std" = 1, "C" = 1, "bd" = 1, "cost" = 0)
#' conv_prob <- constraints_dt(inlist = clist, cbetas = cbetas, code = rc, 
#'                             cropnames = il$cropnames, base = il$mask)
#' conv_probr1 <- dt_list_to_raster(base = base, inlist = list(conv_prob), 
#'                                  CRSobj = prj)[[1]]
#' conv_probr2 <- constraints_r(inlist = clistr, cbetas = cbetas, code = rc, 
#'                              cropnames = il$cropnames)
#' 
#' valind <- which(values(il$mask) == 1) 
#' system.time(conv <- convert_dt(conv_prob, target = target, 
#'                                crop_frac = il$cropfrac,
#'                                pot_yield = ybeta$p_yield, 
#'                                cropnames = il$cropnames, 
#'                                base = il$mask, ha = ha))  # 0.135 seconds
#' convr2 <- dt_to_raster(dt = conv, CRSobj = CRSobj)
#' #plot(convr - convr2)  # both approaches equal
#' plot(convr2)

#' @export
convert_dt2 <- function(conv_prob, target, crop_frac, pot_yield, cropnames, base, 
                       ha, keep_index = FALSE) {
  #conv_prob[, ind := base[, ind]]
  #setkey(conv_prob, ind)
  setkey(base, ind)
  #prod_dt <- pot_yield[valind, ] * crop_frac[valind, ] * ha
  #prod_dt <- pot_yield * crop_frac * ha
  prod_dt <- data.table::as.data.table(as.matrix(pot_yield) * 
                                       as.matrix(crop_frac) * ha)  
  prod_dt[, ind := base$ind]
  if(any(is.na(prod_dt)) | any(is.na(conv_prob))) {
    stop("NAs are not allowed", call. = FALSE)
  }
  conv_dt <- data.table(ind = base$ind, key = "ind")
  inds <- sapply(conv_prob[, cropnames, with = FALSE], function(x) {
    order(x, decreasing = TRUE)
  })
  for(i in colnames(inds)) {
    targ <- target[i, "target_newland"]
    conv_dt[inds[, i], c(i) := ifelse(cumsum(prod_dt[inds[, i], i, with = FALSE]) < targ, 1, 0)]
  }
  
  # Use a fraction of the next highest probability pixel to hit target
  for (i in colnames(inds)) {
    remaining <- target[i, "target_newland"] - conv_dt[[i]] %*% prod_dt[[i]]
    nextind <- inds[sum(conv_dt[[i]])+1, i]
    conv_dt[[i]][nextind] <- remaining/prod_dt[[i]][nextind]
  }
  
  out_dt <- merge(base, conv_dt, all = TRUE)
  if(keep_index == FALSE) out_dt[, ind := NULL]
  return(out_dt)
}

