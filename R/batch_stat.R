#' Extract and summarize batch impact statistics
#' @param inlist List of impact tables from batch run
#' @param Yv agricultural impact metric (new land required)
#' @param Cv Carbon impact metric
#' @param BDv Biodiversity impact metric
#' @param COSTc Cost impact metric
#' @param Yst Function to calculate impacts across crops (e.g. sum)
#' @param Cst Function to calculate impacts across carbon (e.g. sum)
#' @param BDst Function to calculate impacts across biodiversity (e.g. mean)
#' @param COSTst Function to calculate cost impact (e.g. mean)
#' @return data.table
#' @keywords internal
#' @export
batch_stat <- function(inlist, Yv = "conv_area", Cv, BDv, COSTv, Yst, Cst, BDst, 
                       COSTst) {
 
  # inlist <- tobimp
  # Yv = "conv_area"; Cv = "tot_C"; BDv = "priority"; COSTv = "mu_cost"
  # Yst = sum; Cst = sum; BDst = mean; COSTst = mean
  vars <- list("Y" = Yv, "C" = Cv, "BD" = BDv, "COST" = COSTv)
  ostat <- list("Y" = Yst, "C" = Cst, "BD" = BDst, "COST" = COSTst)
  out_dt <- do.call(rbind, lapply(inlist, function(x) {  # x <- tobimp[[1]]
    land <- ostat$Y(x[, vars$Y], na.rm = TRUE)
    carbon <- ostat$C(x[, vars$C], na.rm = TRUE)
    bd <- ostat$BD(x[, vars$BD], na.rm = TRUE)
    cost <- ostat$COST(x[, vars$COST], na.rm = TRUE)
    odt <- data.table(cbind("Y" = land, "C" = carbon, "BD" = bd, "COST" = cost, 
                            "ind" = x$iter[1]))
    setorderv(odt, names(odt))
    odt
  }))
  return(out_dt)
}