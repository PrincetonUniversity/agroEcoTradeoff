#' Create parameter table for batch run
#' @param yblist List of yield modification parameters (option switched off now)
#' @param targlist List of crop target vectors
#' @param cblist List of weight parameters
#' @param currprodmodlist List of current production-level modifiers
#' @return data.frame of input parameters
#' @details For use in pareto and tradeoff_batch
#' @keywords internal
#' @export
batch_params <- function(yblist = list(c(1, 1)), targlist, cblist, 
                         currprodmodlist) {
  parms <- do.call(rbind, lapply(yblist, function(x) {
    do.call(rbind, lapply(targlist, function(y) {
      do.call(rbind, lapply(currprodmodlist, function(z) {
        do.call(rbind, lapply(cblist, function(zz) {
         v <- c(x, y, z, zz)
         tlistnms <- names(targlist[[1]])
         tlistnms <- ifelse(is.null(tlistnms), 
                            paste0("c", 1:length(targlist[[1]])), tlistnms)
         names(v) <- c("y1", "y2", paste0("f", names(targlist[[1]])), 
                       paste0("c", names(targlist[[1]])),
                       "Y", "C", "BD", "COST")
         v
        }))
      }))
    }))
  }))
  return(parms)
}
