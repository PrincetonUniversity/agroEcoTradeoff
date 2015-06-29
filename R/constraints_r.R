#' Establishes per pixel probability of fields being converted to cropland
#' @description This function determines the per pixel probability of a field 
#' being converted for a crop as a function of that crop's yield, as well as any 
#' constraints related to carbon, biodiversity, and travel costs, factoring in 
#' any prior yield modifications made (e.g. due to climate change or added 
#' irrigation)
#' @param inlist A list of the four constraint Raster*s
#' @param cbetas 4 element vector with values 0-1 for land use weights
#' @param code Unique simulation code resulting from run_code function
#' @param cropnames Vector of crop names in analysis 
#' @param ctype Specific multiplicative ("X") or additive ("+") constraints 
#' @param silent Silent or verbose mode (TRUE [default] or FALSE)
#' @return Brick of conversion probabilities for each crop
#' @details For inlist, the input should be a named list, with the first element 
#' named "y_std, then "C", "bd", and "cost".
#' @examples 
#' rc <- run_code(input_key = "ZA")  
#' #code <- rc
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' ybetas <- list(1, 1)
#' ybeta <- yield_mod_r(inlist = il[c("p_yield", "pp_curr")], ybetas = ybetas, 
#'                      code = rc, cropnames = il$cropnames) 
#' cbetas <- c("y_std" = 0, "C" = 1, "bd" = 1, "cost" = 1)
#' clist <- list("y_std" = ybeta$y_std, "C" = il$carbon_p, "bd" = il$cons_p, 
#'               "cost" = il$cost)
#' p_y <- constraints_r(inlist = clist, cbetas = cbetas, code = rc, 
#'                      cropnames = il$cropnames)
#' plot(p_y - Reduce("*", clist[2:4]))
#'
#' cbetas <- c("y_std" = 0, "C" = 0, "bd" = 0, "cost" = 0)
#' p_y <- constraints_r(inlist = clist, cbetas = cbetas, code = rc, 
#'                      cropnames = il$cropnames)
#' plot(p_y - clist$y_std)
#' 
#' cbetas <- c("y_std" = 0, "C" = 0.5, "bd" = 0, "cost" = 0)
#' p_y <- constraints_r(inlist = clist, cbetas = cbetas, code = rc, 
#'                      cropnames = il$cropnames)
#' plot(clist$C * 0.5) - p_y$maize
#' 
#' cbetas <- c("y_std" = 1, "C" = 0.5, "bd" = 0, "cost" = 0)
#' p_y <- constraints_r(inlist = clist, cbetas = cbetas, code = rc, 
#'                      cropnames = il$cropnames)
#' plot(p_y - (clist$y_std * clist$C * 0.5))
#' 
#' cbetas <- c("y_std" = 1, "C" = 1, "bd" = 0, "cost" = 1)
#' p_y <- constraints_r(inlist = clist, cbetas = cbetas, code = rc, 
#'                      cropnames = il$cropnames)
#' plot(p_y - (clist$y_std * clist$C * clist$cost))
#' 
#' cbetas <- c("y_std" = 0, "C" = 1, "bd" = 0.5, "cost" = 1)
#' p_y <- constraints_r(inlist = clist, cbetas = cbetas, code = rc, 
#'                      cropnames = il$cropnames)
#' plot(p_y - (clist$C * clist$bd * 0.5 * clist$cost))
#' @export
constraints_r <- function(inlist, cbetas, code, cropnames, ctype = "X", 
                          silent = TRUE) {
  if(length(cbetas) != 4) stop("cbetas must be a 4 element vector")
  #cbetas_gt0 <- which(cbetas != 0)
  cbetas_gt0 <- ifelse(cbetas != 0, 1, 0)  # vector to select inputs
  if(sum(cbetas_gt0) == 0) {
    shhh("No land use prioritized: defaulting to crop productivity as priority", 
         silent = silent)
    cbetas <- c(1, 0, 0, 0)
    cbetas_gt0 <- cbetas
  }
  rlist <- inlist[which(cbetas != 0)]
  cbetas_r <- cbetas[which(cbetas != 0)]
  rlist_mod <- lapply(1:length(rlist), function(x) {
    if(cbetas_r[x] == 1) {
      shhh(paste(names(rlist)[x], ": full constraint applied"), silent = silent)
      r <- rlist[[x]]
    } else {
      #cnm <- fname(paste0("external/output/prob-rast/temp/c", x, "-rst-"))
      shhh(paste(names(rlist)[x], ": partial constraint applied"), 
           silent = silent)
      #r <- rastTest(cbetas_r[x] * rlist[[x]], filename = cnm)
      r <- cbetas_r[x] * rlist[[x]]
    }
  })
  names(rlist_mod) <- names(rlist)
  pnm <- fname(full_path(set_base_path(), "external/output/prob-rast/p-rst-"), 
               code)
  if(length(rlist_mod) == 1) {
    if(nlayers(rlist_mod[[1]]) == 1) {
      p_y <- nm_up(brick(stack(lapply(1:9, function(x) rlist_mod[[1]])), 
                         filename = pnm, overwrite = TRUE), cropnames)
    } else {
      p_y <- nm_up(rlist_mod[[1]], cropnames)
    }
  } else {
    if(ctype == "X") {
      p_y <- rast_math(Reduce("*", rlist_mod))
    } else if(ctype == "+") {
      p_y <- rast_math(Reduce("+", rlist_mod))
    }
   
    p_y <- rast_math(Reduce("*", rlist_mod))
    if(nlayers(p_y) == 1) {
      p_y <- nm_up(brick(stack(lapply(1:9, function(x) p_y))), cropnames)
    } else {
      p_y <- nm_up(p_y, cropnames)
    }
  } 
  #file.remove(list.files("external/output/prob_rast/temp/", full.names = TRUE))
  return(p_y)
}
