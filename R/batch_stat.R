#' Extract and summarize batch impact statistics
#' @param intab Impact table from batch run
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
batch_stat <- function(intab, Yv = "conv_area", Cv, BDv, COSTv, Yst, Cst, BDst, 
                       COSTst) {
 
  # intab <- tobimp
  # Yv = "conv_area"; Cv = "tot_C"; 
  # BDv = c(Yv, "int_prior"); COSTv = c(Yv, "mu_cost")
  # wmu <- function(x, na.rm = na.rm) {
    # weighted.mean(x = x[, 2], w = x[, 1], na.rm = na.rm)
  # }
  # Yst = sum; Cst = sum; BDst = wmu; COSTst = wmu
  vars <- list("Y" = Yv, "C" = Cv, "BD" = BDv, "COST" = COSTv)
  ostat <- list("Y" = Yst, "C" = Cst, "BD" = BDst, "COST" = COSTst)
  out_dt <- do.call(rbind, lapply(unique(intab$iter), function(i) {  # i <- 1
    x <- intab[intab$iter == i, ]
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