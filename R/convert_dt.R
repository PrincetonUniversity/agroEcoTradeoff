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
convert_dt <- function(conv_prob, target, crop_frac, pot_yield, cropnames, base, 
                       ha, keep_index = FALSE) {
  #conv_prob[, ind := base[, ind]]
  #setkey(conv_prob, ind)
  setkey(base, ind)
  #prod_dt <- pot_yield[valind, ] * crop_frac[valind, ] * ha
  #prod_dt <- pot_yield * crop_frac * ha
  # temporarily removed crop_frac
  prod_dt <- data.table::as.data.table(as.matrix(pot_yield) * ha) 
  prod_dt[, ind := base$ind]
  if(any(is.na(prod_dt)) | any(is.na(conv_prob))) {
    stop("NAs are not allowed", call. = FALSE)
  }
  conv_dt <- data.table(ind = base$ind, key = "ind")
  
  conv_prob$ind <- 1:nrow(conv_prob) #use inds to keep track of unallocated pixels
  sequence = c(1:length(cropnames)) #sequence by which crops are considered
  
  # 26/10/2015 - To-fix: bad practice to use T as object name. This is variable
  # (shorthand) for TRUE in R.
  TT <- rep(-1, nrow(conv_prob)) #vector representing allocation of pixels to crops
  targ_rem <- NULL
  
  for (k in 1:length(cropnames)) {
    for (i in 1:length(cropnames))
      # remaining production to be met only use unallocated pixels
      targ_rem[i] <- target$target_newland[i] - sum(prod_dt[[i]][TT == i]) 
    conv_prob_u <- conv_prob[TT == -1]   
    prod_dt_u <- prod_dt[TT == -1] 
    # vector of residual profit for each pixel in its current allocation
    alloc_rp <- rep(0, nrow(conv_prob_u)) 
    for(i in sequence) {
      targ <- targ_rem[i]
      if (targ > 0) {
        #subtract the residual profit of pixels already allocated
        conv_prob_u[[i]] <- conv_prob_u[[i]] - alloc_rp 
        # sort the probabilities in descending order
        inds <- order(conv_prob_u[, i, with = FALSE], decreasing = TRUE)
        # map the index for unallocated pixels to their original indices
        inds_map <- conv_prob_u$ind[inds] 
        for (j in 0:(sum(ifelse(cumsum(prod_dt_u[inds, i, with = FALSE]) < targ, 1, 0))+1)) #pixels to exceed targ
          TT[inds_map[j]] <- i
        # Here's where we could alter the algorithm. Instead of using just the 
        # one with the highest probability pixel that wasn't
        # selected, maybe it makes sense to take a mean of the next % pixels 
        # if we assume that a lot of pixels are going
        # to be allocated elsewhere
        nextp <- conv_prob_u[[i]][inds[j + 1]] # highest prob pixel that wasn't selected
        alloc_rp[inds[1:j]] <- alloc_rp[inds[1:j]] + 
         (conv_prob_u[[i]][inds[1:j]] - nextp) #update resid profit vector
      }
    }
    #Eliminate the last crop considered from the sequence. Its target has been met.
    sequence <- sequence[1:(length(sequence)-1)]
  }
  
  for (i in 1:length(cropnames)) {
    # initialize a column for each crop filled with zeroes
    conv_dt[, cropnames[i]] <- 0 
    # for a crop, put a 1 in all the rows where TT indicates that crop
    conv_dt[TT == i, cropnames[i]] <- 1 
  }
  
  out_dt <- merge(base, conv_dt, all = TRUE)
  if(keep_index == FALSE) out_dt[, ind := NULL]
  return(out_dt)
}

