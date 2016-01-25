#' Create parameter table for batch run
#' @param yblist List of yield modification parameters (option switched off now)
#' @param targlist List of crop target vectors
#' @param cblist List of weight parameters
#' @return data.frame of input parameters
#' @details For use in pareto and tradeoff_batch
#' @keywords internal
#' @export
batch_params <- function(yblist, targlist, cblist) {
  parms <- do.call(rbind, lapply(yblist, function(x) {
    do.call(rbind, lapply(targlist, function(y) {
      do.call(rbind, lapply(cblist, function(z) {
        v <- c(z, x, y)
        tlistnms <- names(targlist[[1]])
        tlistnms <- ifelse(is.null(tlistnms), 
                           paste0("c", 1:length(targlist[[1]])), tlistnms)
        names(v) <- c("Y", "C", "BD", "COST", "y1", "y2", names(targlist[[1]]))
        v
      }))
    }))
  }))
  return(parms)
}