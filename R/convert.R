#' Land conversion module
#' @description This module takes inputs from the targets and constraints modules 
#' and determines which pixels are converted. This is an alternate version of 
#' convert, which is relies on data.table functions. 
#' @param conv_prob Conversion probability brick
#' @param prod_targ Production target data.frame
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
#' ybeta <- yield_mod(inlist = il[c("p_yield")], ybetas = ybetas, 
#'                    code = rc, cropnames = il$cropnames) 
#' clist <- list("y_std" = ybeta$y_std, "C" = il$carbon_p, "bd" = il$cons_p, 
#'               "cost" = il$cost)
#' CRSobj <- projection(raster("external/ext_data/ZA-carbon.tif"))
#' ha <- res(raster("external/ext_data/ZA-carbon.tif"))[2]^2 / 10000
#' prod_targ <- c("maize" = 4, "soy" = 2)
#' target <- targets(prod_targ = prod_targ, currprod = il$currprod, 
#'                   currprodmod = 1)
#' 
#' cbetas <- c("y_std" = 0, "C" = 1, "bd" = 1, "cost" = 1)
#' conv_prob <- constraints(inlist = clist, cbetas = cbetas, code = rc, 
#'                          cropnames = il$cropnames)
#' valind <- which(values(il$mask) == 1) 
#' system.time(conv <- convert(conv_prob, target = target, 
#'                             pot_yield = ybeta$p_yield, valind = valind, 
#'                             cropnames = il$cropnames, 
#'                             base = il$grid, ha = ha))  # 0.135 seconds
#' convr2 <- dt_to_raster(dt = conv, CRSobj = projection(il$currprod))
#' plot(convr - convr2)  # both approaches equal
#' 
#' cbetas <- c("y_std" = 1, "C" = 1, "bd" = 1, "cost" = 0)
#' conv_prob <- constraints(inlist = clist, cbetas = cbetas, code = rc, 
#'                          cropnames = il$cropnames, base = il$mask)
#' conv_probr1 <- dt_list_to_raster(base = base, inlist = list(conv_prob), 
#'                                  CRSobj = prj)[[1]]
#' 
#' valind <- which(values(il$mask) == 1) 
#' system.time(conv <- convert(conv_prob, target = target, 
#'                             pot_yield = ybeta$p_yield, 
#'                             cropnames = il$cropnames, 
#'                             base = il$mask, ha = ha))  # 0.135 seconds
#' convr2 <- dt_to_raster(dt = conv, CRSobj = CRSobj)
#' #plot(convr - convr2)  # both approaches equal
#' plot(convr2)

#' @export
convert <- function(conv_prob, target, pot_yield, cropnames, base, 
                    ha, keep_index = FALSE) {
  
  # conv_prob = c_prob; target = target; pot_yield = il$p_yield; 
  # cropnames = il$cropnames; base = il$mask; ha = il$sp$ha; 
  # keep_index = FALSE
  # key(il$mask)
  #conv_prob[, ind := base[, ind]]
  #setkey(conv_prob, ind)
  #prod_dt <- pot_yield[valind, ] * crop_frac[valind, ] * ha
  #prod_dt <- pot_yield * crop_frac * ha
  # temporarily removed crop_frac
  # all(testmat[, 2] == pot_yield$soy * base$convertible)
 
  # convert to production (yield must be in tons/ha)
  prod_dt <- data.table(as.matrix(pot_yield) * base$convertible * ha)
  # prod_dt <- data.table::as.data.table(as.matrix(pot_yield) * ha) 
  
  prod_dt[, ind := base$ind]
  if(any(is.na(prod_dt)) | any(is.na(conv_prob))) {
    stop("NAs are not allowed", call. = FALSE)
  }
  
  # crop allocation begins
  conv_prob$ind <- 1:nrow(conv_prob) # inds to keep track of unallocated pixels
  sequence <- 1:length(cropnames) # sequence by which crops are considered
  TT <- rep(-1, nrow(conv_prob)) # temporary pixel allocation vector
  targ_rem <- NULL  # remaining target vector
  
  for(k in 1:length(cropnames)) {  # k <- 1
    # remaining production to be met - only use unallocated pixels
    for(i in 1:length(cropnames)) {  # i <- 1
      targ_rem[i] <- target$target_newland[i] - sum(prod_dt[[i]][TT == i])
    }
    conv_prob_u <- conv_prob[TT == -1]   
    prod_dt_u <- prod_dt[TT == -1] 
    # vector of residual profit for each pixel in its current allocation
    alloc_rp <- rep(0, nrow(conv_prob_u))  # alloc_rp[1:10]
    for(i in sequence) {  # i <- 1
      targ <- targ_rem[i]
      if(targ > 0) {
        # subtract the residual profit of pixels already allocated
        conv_prob_u[[i]] <- conv_prob_u[[i]] - alloc_rp # 
        # sort the probabilities in descending order
        inds <- order(conv_prob_u[, i, with = FALSE], decreasing = TRUE)
        # map the index for unallocated pixels to their original indices
        inds_map <- conv_prob_u$ind[inds]
        
        # find the cell that provide cumulative production up to the target 
        # amount
        cumsumfun <- function(x, y) 0:sum(ifelse(cumsum(x) < y, 1, 0)) + 1
        convind <- prod_dt_u[inds, cumsumfun(.SD, targ), .SD = i]
        TT[inds_map[convind]] <- i
        j <- length(convind)
        
        # replaced with more data.table centric version above
        # for(j in 0:(sum(ifelse(cumsum(prod_dt_u[inds, i, with = FALSE]) < 
                               # targ, 1, 0))+1)) { # pixels to exceed targ
          # TT[inds_map[j]] <- i
        # }

        # Here's where we could alter the algorithm. Instead of using just the 
        # one with the highest probability pixel that wasn't
        # selected, maybe it makes sense to take a mean of the next % pixels 
        # if we assume that a lot of pixels are going
        # to be allocated elsewhere
        nextp <- conv_prob_u[[i]][inds[j + 1]] # highest p that wasn't selected
        alloc_rp[inds[1:j]] <- alloc_rp[inds[1:j]] + 
         (conv_prob_u[[i]][inds[1:j]] - nextp) # update resid profit vector
      }
    }
    # Eliminate the last crop considered from the sequence. 
    # Its target has been met.
    sequence <- sequence[1:(length(sequence) - 1)]
  }
  
  # allocate land to each crop
  conv_dt <- data.table(ind = base$ind, key = "ind")  # conversion table
  for(i in 1:length(cropnames)) {
    # initialize a column for each crop filled with zeroes
    conv_dt[, c(cropnames[i]) := 0]  #<- 0 
    # for a crop, put a 1 in all the rows where TT indicates that crop
    conv_dt[TT == i, c(cropnames[i]) := 1]  # <- 1
  }
  
  # merge with base
  out_dt <- merge(base, conv_dt, all = TRUE)
  remvars <- names(out_dt)[!names(out_dt) %in% c("x", "y", cropnames)]
  if(keep_index == FALSE) out_dt[, c(remvars) := NULL]
  return(out_dt)
}


