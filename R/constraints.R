#' Establishes per pixel probability of fields being converted to cropland
#' @description This function determines the per pixel probability of a field 
#' being converted for a crop as a function of that crop's yield, as well as any 
#' constraints related to carbon, biodiversity, and travel costs, factoring in 
#' any prior yield modifications made (e.g. due to climate change or added 
#' irrigation). This version uses data.tables rather than raster and reframes the
#' constraints as per yield potential. 
#' @param inlist A list of data.tables for the four constraints 
#' @param cbetas 4 element vector (values 0-1, summing to 1) of land use weights
#' @param code Unique simulation code resulting from run_code function
#' @param cropnames Vector of crop names in analysis 
#' @param silent TRUE, otherwise FALSE gives verbose mode
#' @return data.table of conversion probabilities for each crop
#' @details For inlist, the input should be a named list, with the first element 
#' named "Y, then "C", "BD", and "COST". The weights must sum to 1, 
#' regardless of how they are apportioned. For weights of 1/3, write them to at 
#' least the third decimal place (code tolerates summing to 0.999) 
#' @examples 
#' rc <- run_code(input_key = "ZA")
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' # ybetas <- list(1, 1)
#' # ybeta <- yield_mod(inlist = il_dt[[2]][c("p_yield")], 
#' #                   ybetas = ybetas, code = rc, cropnames = il$cropnames)
#'                    
#' inlist <- list("y_std" = il$y_std, "C" = il$carbon_p, "bd" = il$cons_p, 
#'                "cost" = il$cost_p)
#' CRSobj <- spatial_meta("ZA")$crs
#' 
#' # Checking case where yields are excluded
#' cbetas <- c("y_std" = 0, "C" = 0.333, "bd" = 0.333, "cost" = 0.333)
#' con1 <- constraints(inlist = inlist, cbetas = cbetas, code = rc, 
#'                     cropnames = il$cropnames)
#' #con1[, lapply(.SD, max, na.rm = TRUE)]
#' 
#' # all four constraints
#' cbetas <- c("Y" = 0.25, "C" = 0.25, "BD" = 0.25, "COST" = 0.25)
#' con2 <- constraints(inlist = inlist, cbetas = cbetas, code = rc, 
#'                     cropnames = il$cropnames)
#' con2r <- dt_list_to_raster(base = il$mask, inlist = list(con2), 
#'                            CRSobj = CRSobj)[[1]]
#' plot(con2r)
#' # current function versus shorter code (current version much faster)
#' system.time(con2 <- constraints(inlist = inlist, cbetas = cbetas, code = rc, 
#'                                 cropnames = il$cropnames))
#' @export
constraints <- function(inlist, cbetas, silent = TRUE) {
                        # code, # cropnames, #ctype = "+", 
  # code <- run_code(input_key = "ZA"); ctype = "+"; silent = TRUE
  # inlist = list("Y" = il$y_std, "C" = il$carbon_p, "BD" = il$cons_p, 
  #                "COST" = il$cost_p)
  if(round(sum(cbetas), 2) != 1) stop("cbetas must sum to one", call. = FALSE)
  if(length(cbetas) != 4) {
    stop("cbetas must be a 4 element vector", call. = FALSE)
  }
  # cbetas_gt0 <- ifelse(cbetas != 0, 1, 0)  # vector to select inputs
  
#   if(sum(cbetas_gt0) == 0) {
#     shhh("No land use prioritized: defaulting to crop productivity as priority", 
#          silent = silent)
#     cbetas <- c(1, 0, 0, 0)
#     cbetas_gt0 <- cbetas
#   }
  rlist <- inlist[names(cbetas)[which(cbetas != 0)]]
  cbetas_r <- cbetas[which(cbetas != 0)]
  rlist_mod <- lapply(1:length(rlist), function(x) {  # x <- 1
    if(cbetas_r[x] == 1) {
      shhh(paste(names(rlist)[x], ": full constraint applied"), silent = silent)
      r <- rlist[[x]]
    } else {
      shhh(paste(names(rlist)[x], ": partial constraint applied"), 
           silent = silent)
      r <- cbetas_r[x] * rlist[[x]]
    }
  })
  names(rlist_mod) <- names(rlist)
  
  # create output
  shhh(paste("processing constraints", paste(cnames, collapse = ", ")), 
       silent = silent)
  # modified to account for unique constraint values for each crop
  i <- 1
  p_y <- rlist_mod[[i]]
  
  while(i < length(rlist_mod)) {
    i <- i + 1
#     if(ctype == "X") {  ## this needs to be removed, as it is duplicated
#       p_y <- p_y + rlist_mod[[i]]
#     } else if(ctype == "+") {
#       p_y <- p_y + rlist_mod[[i]]
#     }
    p_y <- p_y + rlist_mod[[i]]
  }
  return(p_y)
}










