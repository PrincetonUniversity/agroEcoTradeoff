#' Modifies input rasters by provided multipliers
#' @description This is a raster-based version of this function. 
#' @param inlist Named list of input RasterBricks
#' @param ybetas list of 2 rasters or 2 vectors providing yield modifications
#' @param code Unique simulation code resulting from run_code function
#' @param cropnames Names of simulated crops
#' @param write_out FALSE (default) or TRUE: write modified standardized yields
#' to disk
#' @param silent Hide or show print statements (TRUE [default] or FALSE)
#' @details For ybetas, if rasters are provided, they should be as a single
#' brick that maps the yield impacts of climate change/irrigation, or both,
#' with 1 layer for each crop. Vectors can alternatively be provided 
#' that apply a uniform yield modification. A single value can be provided, 
#' in which case it will be recycled across all assessed crops, or a vector
#' equal in length to the number of crops being analyzed can be used. The first
#' element of ybeta can be treated as a climate change modifier, as a way to
#' modify the potential yield of each crop to investigate, say, the spatial
#' patterns that will result if yield achieve only half their potential. The
#' second element should be reserved for testing irrigation effects, for the
#' sake of good practice. 
#' @note ybetas could also be used in future to test fertilizer-related yield
#' modifications, thereby becoming a three parameter term. The write_out logic
#' will need to be fixed for theified pp_curr and p_yield also 
#' @seealso \code{\link{yield_mod_dt}} for data.table based version
#' @examples
#' rc <- run_code(input_key = "ZA")  
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' inlist <- il[c("p_yield", "pp_curr")]
#' 
#' # Wrong ybetas length
#' ybetas <- list(rep(0.75, nlayers(inlist[[1]])), c(0.9, 1))
#' ybeta <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc)  
#' 
#' # Correct ybetas (passed as vectors)
#' ybetas <- list(rep(0.75, nlayers(inlist[[1]])), 
#'                rep(0.75, nlayers(inlist[[1]])))
#' ybeta <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc, 
#'                      cropnames = il$cropnames)
#' 
#' # Null modification passed
#' ybetas <- list(1, rep(1, 9))
#' ybeta2 <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc, 
#'                       cropnames = il$cropnames)  # wrong lengths
#' ybetas <- list(1, 1)
#' ybeta2 <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc, 
#'                       cropnames = il$cropnames)  # right lengths
#' # ybeta2$p_yield - inlist$p_yield  # zeroes, as they should be
#' ybetas <- list(rep(1, 9), rep(1, 9))
#' ybeta2 <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc, 
#'                       cropnames = il$cropnames)
#' # ybeta2$p_yield - inlist$p_yield  # zeroes, as they should be
#' 
#' # ybetas as rasters
#' dfact <- c(0.9, 1.2)
#' ybetas <- lapply(1:2, function(x) {
#'  r <- inlist[[1]]  # recycling
#'  r[] <- rnorm(n = ncell(r), mean = dfact[x], sd = 0.05)
#'  mask(r, inlist[[1]])
#' })
#' ybeta3 <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc, 
#'                       cropnames = il$cropnames) 
#' ybeta3$p_yield - (ybetas[[1]] * ybetas[[2]] * inlist$p_yield)  # zeroes
#' 
#' # Raster modifier X vector
#' ybetas[[2]] <- c(0.9, 0.9)
#' ybeta3 <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc, cropnames = il$cropnames)  # wrong lengths
#' ybetas[[2]] <- c(1)
#' ybeta3 <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc, 
#'                       cropnames = il$cropnames)  # wrong lengths
#' ybetas[[2]] <- rep(1.25, nlayers(inlist[[1]])) 
#' ybeta4 <- yield_mod_r(inlist = inlist, ybetas = ybetas, code = rc, 
#'                       cropnames = il$cropnames)  
#' ybeta4$p_yield - (ybetas[[1]] * 1.25 * inlist$p_yield)  # zeroes
#' 
#' # Compare yield_mod_dt with yield_mod_r
#' # set up dt inputs first
#' dang <- Sys.time()
#' inlist2 <- raster_list_to_dt(inlist = inlist)
#' ybetas <- list(rep(0.75, nlayers(inlist[[1]])), 
#'                rep(0.75, nlayers(inlist[[1]])))
#' ybeta_dt1 <- yield_mod_dt(inlist = inlist2[[2]], ybetas = ybetas, 
#'                           code = rc, cropnames = il$cropnames)
#' ybeta_dt1_y_std <- dt_to_raster(cbind(inlist2[[1]], ybeta_dt1$y_std), 
#'                                 CRSobj = projection(il$currprod))
#' Sys.time() - dang  # 0.92 seconds
#' 
#' # versus 6 seconds for raster based version (probably much faster in memory,
#' # to be fair)
#' system.time(ybeta <- yield_mod_r(inlist = inlist, ybetas = ybetas, 
#'                                  code = rc, cropnames = il$cropnames))
#' round(ybeta_dt1_y_std[[2:10]], 7) - round(ybeta$y_std, 7)  
#' # the same, but for very small rounding (R Inferno)
#' plot(round(ybeta_dt1_y_std[[2:10]], 9) - round(ybeta$y_std, 9))  
#' # ...due to very minor rounding (R Inferno)
#' @export
yield_mod_r <- function(inlist, ybetas, code, cropnames, write_out = FALSE, silent = TRUE) {
  fpath <- "external/output/yield-mod/"
  #ybnm <- fname(paste0(fpath, "yb-rst-"), code)
  ybeta_l <- lapply(1:length(ybetas), function(x) {
    check_length(ybetas[[x]], nlayers(inlist[[1]]), paste("ybetas", x))
  })
  if(Reduce("!=", ybeta_l)) stop("Each ybeta must be of equal length", 
                                 call. = FALSE)
  vcheck <- sapply(ybetas, is.vector)
  vind <- ifelse(vcheck == TRUE, 1, 0)
  if(sum(vind) == 1) {  # if one ybeta is a raster
    t1 <- any((ybetas[[which(vind == 1)]] == 1) | 
               (sum(ybetas[[which(vind == 1)]]) == nlayers(inlist[[1]])))
    if(t1) {  # if the vector part of ybetas is just 1 (or a series of 1s), indicating no yield mod
      shhh(paste("ybeta becomes a raster, from ybetas", which(vind == 0)),
           silent = silent)
      ybeta <- ybetas[[which(vind == 0)]]
    } else {  # otherwise, if vector part is not 1s, multiply vector and raster
      shhh("multiplying ybeta raster with ybeta vector", silent = silent)
      ybeta <- nm_up(rast_math(ybetas[[1]] * ybetas[[2]]), cropnames)
    }
  } else if(sum(vind) == 0) {
    shhh("multiplying ybeta rasters together", silent = silent)
    ybeta <- nm_up(rast_math(ybetas[[1]] * ybetas[[2]]), cropnames)
  } else {  # otherwise, multiply the two vectors together
    shhh("ybeta is a vector", silent = silent)
    ybeta <- ybetas[[1]] * ybetas[[2]]
  }
  
  # Modify data, if needed
  ybeta_vl <- ifelse(is.vector(ybeta), sum(ybeta), 0)
  if(is.vector(ybeta) & (ybeta_vl == 1 | ybeta_vl == nlayers(inlist[[1]]))) {
    shhh("No yield modifications made", silent = silent)
    outlist <- inlist
  } else {
    shhh("Modifying yields", silent = silent)
    outlist <- lapply(1:length(inlist), function(x) {
      fnm <- fname(paste0(fpath, "y-mod-", names(inlist)[x], "-"), code)
      r <- nm_up(rast_math(ybeta * inlist[[x]], filename = fnm), cropnames)
      return(r)
    })
    names(outlist) <- names(inlist)
  }
  
  # Standardize potential yield raster
  fnm <- fname(paste0(fpath, "y-std-"), code)
  y_std <- lapply(1:nlayers(outlist$p_yield), function(x) {
    standardize(outlist$p_yield[[x]], "R")
  }) 
  r <- raster(nrow = 100, ncol = 100, res = 1)
  
  if(write_out == TRUE) {
    y_std <- nm_up(brick(stack(y_std), filename = fnm), cropnames)
  } else {
    y_std <- nm_up(brick(stack(y_std)), cropnames)
  }
  outlistf <- list(y_std, outlist[[1]], outlist[[2]])
  names(outlistf) <- c("y_std", names(inlist))
  return(outlistf)
} 

#' Modifies input data.tables by provided multipliers
#' @description This is a data.table-based version of this function. 
#' @param inlist Named list of input data.tables
#' @param ybetas list of either 2 data.tables or 2 vectors providing 
#' modifications for climate & irrigation 
#' @param code Unique simulation code resulting from run_code function
#' @param cropnames Names of simulated crops
#' @param silent Hide or show print statements (TRUE [default] or FALSE)
#' @details For ybetas, if data.tables are provided, each data.table should 
#' provide one column to represent the impacts to each crop, or a single column
#' that will apply to all crops. Vectors can alternatively be provided that
#' apply a uniform yield modification. See further details in documentation
#' for \code{\link{yield_mod_r}}
#' @seealso \code{\link{yield_mod_r}} for raster-based version
#' @examples 
#' rc <- run_code(input_key = "ZA")  
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' inlist_dt <- raster_list_to_dt(inlist = il[c("p_yield", "pp_curr")])
#' 
#' # no yield modification
#' ybetas <- list(1, 1)
#' ybeta <- yield_mod_dt(inlist = inlist_dt[[2]], ybetas = ybetas, 
#'                       code = rc, cropnames = il$cropnames)
#' 
#' # yield modification with vectors
#' ybetas <- list(rep(0.75, nlayers(il[[3]])), rep(0.75, nlayers(il[[3]])))
#' ybeta2 <- yield_mod_dt(inlist = inlist_dt[[2]], ybetas = ybetas, code = rc,
#'                        cropnames = il$cropnames)
#' ybeta2r <- dt_list_to_raster(inlist_dt[[1]][, c("x", "y"), with = FALSE],
#'                              ybeta2, projection(il$currprod))
#' ybeta_r <- yield_mod_r(il[c("p_yield", "pp_curr")], ybetas = ybetas, 
#'                        code = rc, cropnames = il$cropnames)
#' plot(ybeta2r$y_std - ybeta_r$y_std)  # equal
#' plot(ybeta2r$p_yield - ybeta_r$p_yield)  # equal
#' plot(ybeta2r$pp_curr - ybeta_r$pp_curr)  # equal
#' 
#' # yield modification with rasters
#' # ybetas as rasters
#' dfact <- c(0.9, 1.2)
#' ybetas_l <- lapply(1:2, function(x) {
#'  r <- il[[3]]  # recycling
#'  r[] <- rnorm(n = ncell(r), mean = dfact[x], sd = 0.05)
#'  mask(r, il[[3]])
#' })
#' ybetas_dt <- raster_list_to_dt(ybetas_l)
#' 
#' ybeta3 <- yield_mod_dt(inlist = inlist_dt[[2]], ybetas = ybetas_dt[[2]],
#'                        code = rc, cropnames = il$cropnames)
#' ybeta3r <- dt_list_to_raster(inlist_dt[[1]][, c("x", "y"), with = FALSE],
#'                              ybeta3, projection(il$currprod))
#' ybeta_r <- yield_mod_r(inlist = il[c("p_yield", "pp_curr")], 
#'                        ybetas = ybetas_l, code = rc, cropnames = il$cropnames)
#' plot(ybeta3r$y_std - ybeta_r$y_std)  # equal
#' plot(ybeta3r$p_yield - ybeta_r$p_yield)  # equal
#' plot(ybeta3r$pp_curr - ybeta_r$pp_curr)  # equal
#' 
#' # Raster modifier X vector
#' ybetas_dt[[2]][[2]] <- c(0.9, 0.9)
#' ybeta4 <- yield_mod_dt(inlist = inlist_dt[[2]], ybetas = ybetas_dt[[2]],
#'                        code = rc, cropnames = il$cropnames)  # wrong length
#' ybetas_dt[[2]][[2]] <- c(0.9, 0.9) <- c(1)
#' ybeta4 <- yield_mod_dt(inlist = inlist_dt[[2]], ybetas = ybetas_dt[[2]],
#'                        code = rc, cropnames = il$cropnames)  # wrong length
#' ybetas_dt[[2]][[2]] <- rep(1.25, nlayers(il$currprod)) 
#' ybeta5 <- yield_mod_dt(inlist = inlist_dt[[2]], ybetas = ybetas_dt[[2]],
#'                        code = rc, cropnames = il$cropnames)
#' ybeta5r <- dt_list_to_raster(inlist_dt[[1]][, c("x", "y"), with = FALSE],
#'                              ybeta5, projection(il$currprod))
#' ybetas_l[[2]] <- rep(1.25, nlayers(il$currprod)) 
#' ybeta_r <- yield_mod_r(inlist = il[c("p_yield", "pp_curr")], 
#'                        ybetas = ybetas_l, code = rc, 
#'                        cropnames = il$cropnames)
#' plot(ybeta5r$y_std - ybeta_r$y_std)  # equal
#' plot(ybeta5r$p_yield - ybeta_r$p_yield)  # equal
#' plot(ybeta5r$pp_curr - ybeta_r$pp_curr)  # equal
#' @export
yield_mod_dt2 <- function(inlist, ybetas, code, cropnames, silent = TRUE) {
  ybeta_l <- lapply(1:length(ybetas), function(x) {
    check_length(ybetas[[x]], ncol(inlist[[1]]), paste("ybetas", x))
  })
  if(Reduce("!=", ybeta_l)) stop("Each ybeta must be of equal length", 
                                 call. = FALSE)
  vcheck <- sapply(ybetas, is.vector)
  vind <- ifelse(vcheck == TRUE, 1, 0)
  if(sum(vind) == 1) {  # if one ybeta is a raster
    t1 <- any((ybetas[[which(vind == 1)]] == 1) | 
               (sum(ybetas[[which(vind == 1)]]) == ncol(inlist[[1]])))
    if(t1) {  # if vector part of ybetas is just 1(s), no yield mod
      shhh(paste("ybeta becomes a data.table, from ybetas", which(vind == 0)),
           silent = silent)
      ybeta <- ybetas[[which(vind == 0)]]
    } else {  # otherwise, if vector part is not 1s, multiple vector & raster
      shhh("multiplying ybeta data.table with ybeta vector", silent = silent)
      ybeta <- ybetas[[which(vind == 0)]] * ybetas[[which(vind == 1)]]
    }
  } else if(sum(vind) == 0) {
    shhh("multiplying ybeta data.tables", silent = silent)
    ybeta <- ybetas[[1]] * ybetas[[2]] 
  } else {  # otherwise, multiply the two vectors together
    shhh("ybeta is a vector", silent = silent)
    ybeta <- ybetas[[1]] * ybetas[[2]]
  }
 
 # Modify data, if needed
  ybeta_vl <- ifelse(is.vector(ybeta), sum(ybeta), 0)
  if(is.vector(ybeta) & (ybeta_vl == 1 | ybeta_vl == ncol(inlist[[1]]))) {
    shhh("No yield modifications made", silent = silent)
    outlist <- inlist
  } else {
    shhh("Modifying yields", silent = silent)
    outlist <- lapply(inlist, function(x) ybeta * x)
    names(outlist) <- names(inlist)
  }
 
  # Standardize potential yields (by area/production)
  y_std <- 1-standardize(1/outlist$p_yield) # Standardize over all values, not by crop
  outlistf <- list(y_std, outlist[[1]], outlist[[2]])
  names(outlistf) <- c("y_std", names(inlist))
  return(outlistf)
} 




