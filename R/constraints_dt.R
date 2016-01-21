#' Establishes per pixel probability of fields being converted to cropland
#' @description This function determines the per pixel probability of a field 
#' being converted for a crop as a function of that crop's yield, as well as any 
#' constraints related to carbon, biodiversity, and travel costs, factoring in 
#' any prior yield modifications made (e.g. due to climate change or added 
#' irrigation). This version uses data.tables rather than raster and reframes the
#' constraints as per yield potential. 
#' @param inlist A list of data.tables for the four constraints 
#' @param cbetas 4 element vector (values 0-1) containing land use weights
#' @param code Unique simulation code resulting from run_code function
#' @param cropnames Vector of crop names in analysis 
#' @param ctype Specific multiplicative ("X") or additive ("+") constraints 
#' @param silent TRUE, otherwise FALSE gives verbose mode
#' @return data.table of conversion probabilities for each crop
#' @details For inlist, the input should be a named list, with the first element 
#' named "y_std, then "C", "bd", and "cost".
#' @examples 
#' rc <- run_code(input_key = "ZA")
#' il <- fetch_inputs(input_key = "ZA")  # fetch all necessary inputs
#' il_dt <- raster_list_to_dt(inlist = il[c("p_yield", "pp_curr", "carbon_p", 
#'                                          "cons_p", "cost")], base = TRUE)
#' ybetas <- list(1, 1)
#' ybeta <- yield_mod_dt(inlist = il_dt[[2]][c("p_yield", "pp_curr")], 
#'                       ybetas = ybetas, code = rc, cropnames = il$cropnames) 
#' clist <- list("y_std" = ybeta$y_std, "C" = il_dt[[2]]$carbon_p, 
#'               "bd" = il_dt[[2]]$cons_p, "cost" = il_dt[[2]]$cost)
#' prj <- projection(il$pp_curr)
#' base <- il_dt[[1]][, c("x", "y"), with = FALSE]
#' 
#' # Checking case where yields are excluded
#' cbetas <- c("y_std" = 0, "C" = 1, "bd" = 1, "cost" = 1)
#' con1 <- constraints_dt(inlist = clist, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)
#' #con1[, lapply(.SD, max, na.rm = TRUE)]
#' 
#' # all four constraints
#' cbetas <- c("y_std" = 1, "C" = 1, "bd" = 1, "cost" = 1)
#' con2 <- constraints_dt(inlist = clist, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)
#' con2r <- dt_list_to_raster(base = base, inlist = list(con2), 
#'                            CRSobj = prj)[[1]]
#' plot(con2r - ybetar$y_std * il$carbon_p * il$cons_p * il$cost))  
#' round(cellStats(con2r - (ybetar$y_std * il$carbon_p * il$cons_p * il$cost), 
#'                 range), 7)  #' R inferno
#' con2b <- constraints_r(inlist = clistr, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)  
#' plot(con2r - con2b)  # equivalent to constraints_r
#' 
#' # yield and carbon
#' cbetas <- c("y_std" = 1, "C" = 1, "bd" = 0, "cost" = 0)
#' con3 <- constraints_dt(inlist = clist, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)
#' con3r <- dt_list_to_raster(base = base, inlist = list(con3), 
#'                            CRSobj = prj)[[1]]
#' plot(con3r - ybetar$y_std * il$carbon_p)  #' stacks up against basic raster math
#' con3b <- constraints_r(inlist = clistr, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)  
#' plot(con3r - con3b)  # equivalent to constraints_r
#' 
#' # carbon and bd
#' cbetas <- c("y_std" = 0, "C" = 1, "bd" = 1, "cost" = 0)
#' con4 <- constraints_dt(inlist = clist, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)
#' con4r <- dt_list_to_raster(base = base, inlist = list(con4), 
#'                            CRSobj = prj)[[1]]
#' plot(con4r - il$carbon_p * il$cons_p)  #' stacks up against basic raster math
#' con4b <- constraints_r(inlist = clistr, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)  
#' plot(con4r - con4b)  # equivalent to constraints_r
#' 
#' # all four, but cost partial
#' cbetas <- c("y_std" = 1, "C" = 1, "bd" = 1, "cost" = 0.5)
#' con5 <- constraints_dt(inlist = clist, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)
#' con5r <- dt_list_to_raster(base = base, inlist = list(con5), 
#'                            CRSobj = prj)[[1]]
#' plot(con5r - ybetar$y_std * il$carbon_p * il$cons_p * il$cost * 0.5) 
#' round(cellStats(con5r - ybetar$y_std * il$carbon_p * il$cons_p * il$cost*0.5, 
#'                 range), 10)  #' R inferno
#' con5b <- constraints_r(inlist = clistr, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)  
#' plot(con5r - con5b)  # equivalent to constraints_r
#' 
#' # no yield, all 3 constrains, but cost partial
#' cbetas <- c("y_std" = 0, "C" = 1, "bd" = 0.5, "cost" = 1)
#' con6 <- constraints_dt(inlist = clist, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)
#' con6r <- dt_list_to_raster(base = base, inlist = list(con6), 
#'                            CRSobj = prj)[[1]]
#' plot(con6r - il$carbon_p * il$cons_p * 0.5 * il$cost)  
#' con6b <- constraints_r(inlist = clistr, cbetas = cbetas, code = rc, 
#'                        cropnames = il$cropnames)  
#' plot(con6r - con6b)  # equivalent to constraints_r
#' @export
constraints_dt <- function(inlist, cbetas, code, cropnames, ctype = "X", 
                           silent = TRUE) {
  if(length(cbetas) != 4) stop("cbetas must be a 4 element vector")
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
  
  # create output
  shhh(paste("processing constraints", paste(cnames, collapse = ", ")), 
       silent = silent)
  # modified to account for unique constraint values for each crop
  i <- 1
  p_y <- rlist_mod[[i]]
  while (i < length(rlist_mod)) {
    i <- i + 1
    if(ctype == "X") {  ## this needs to be removed, as it is duplicated
      p_y <- p_y + rlist_mod[[i]]
    } else if(ctype == "+") {
      p_y <- p_y + rlist_mod[[i]]
    }
  }
  return(p_y)
}










