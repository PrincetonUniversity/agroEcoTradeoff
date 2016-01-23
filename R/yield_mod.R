#' Modifies input data.tables by provided multipliers
#' @description This is a data.table-based version of this function. 
#' @param inlist Named list of crop yield data.table
#' @param ybetas list of either 2 data.tables or 2 vectors providing 
#' modifications for climate & irrigation 
#' @param code Unique simulation code resulting from run_code function
#' @param cropnames Names of simulated crops
#' @param silent Hide or show print statements (TRUE [default] or FALSE)
#' @details For ybetas, if rasters are provided, they should be as a single
#' brick that maps the yield impacts of climate change/irrigation, or both,
#' with 1 layer for each crop. For ybetas, if data.tables are provided, 
#' each data.table should 
#' provide one column to represent the impacts to each crop, or a single column
#' that will apply to all crops. Vectors can alternatively be provided that
#' apply a uniform yield modification. Vectors can alternatively be provided 
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
#' @examples 
#' rc <- run_code(input_key = "ZA")  
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' inlist_dt <- raster_list_to_dt(inlist = il[c("p_yield", "pp_curr")])
#' 
#' # no yield modification
#' ybetas <- list(1, 1)
#' ybeta <- yield_mod(inlist = inlist_dt[[2]], ybetas = ybetas, 
#'                    code = rc, cropnames = il$cropnames)
#' 
#' # yield modification with vectors
#' ybetas <- list(rep(0.75, nlayers(il[[3]])), rep(0.75, nlayers(il[[3]])))
#' ybeta2 <- yield_mod(inlist = inlist_dt[[2]], ybetas = ybetas, code = rc,
#'                     cropnames = il$cropnames)
#' ybeta2r <- dt_list_to_raster(inlist_dt[[1]][, c("x", "y"), with = FALSE],
#'                              ybeta2, projection(il$currprod))
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
#' ybeta3 <- yield_mod(inlist = inlist_dt[[2]], ybetas = ybetas_dt[[2]],
#'                     code = rc, cropnames = il$cropnames)
#' ybeta3r <- dt_list_to_raster(inlist_dt[[1]][, c("x", "y"), with = FALSE],
#'                              ybeta3, projection(il$currprod))
#' 
#' # Raster modifier X vector
#' ybetas_dt[[2]][[2]] <- c(0.9, 0.9)
#' ybeta4 <- yield_mod(inlist = inlist_dt[[2]], ybetas = ybetas_dt[[2]],
#'                     code = rc, cropnames = il$cropnames)  # wrong length
#' ybetas_dt[[2]][[2]] <- c(0.9, 0.9) <- c(1)
#' ybeta4 <- yield_mod(inlist = inlist_dt[[2]], ybetas = ybetas_dt[[2]],
#'                     code = rc, cropnames = il$cropnames)  # wrong length
#' ybetas_dt[[2]][[2]] <- rep(1.25, nlayers(il$currprod)) 
#' ybeta5 <- yield_mod(inlist = inlist_dt[[2]], ybetas = ybetas_dt[[2]],
#'                     code = rc, cropnames = il$cropnames)
#' ybeta5r <- dt_list_to_raster(inlist_dt[[1]][, c("x", "y"), with = FALSE],
#'                              ybeta5, projection(il$currprod))
#' ybetas_l[[2]] <- rep(1.25, nlayers(il$currprod)) 
#' @export
yield_mod <- function(inlist, ybetas, code, cropnames, silent = TRUE) {
#   inlist = il["p_yield"]; cropnames = il$cropnames
#   silent = FALSE
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
  # Standardize over all values, not by crop
  # y_std <- 1 - standardize(1 / outlist$p_yield)  # this assumption needs fixing 
  # outlistf <- list(y_std, outlist[[1]]) # , outlist[[2]])
  # names(outlistf) <- c("y_std", names(inlist))
  # return(outlistf)
  return(outlist)
} 
