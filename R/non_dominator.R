#' Remove dominated solutions
#' @param intable Input data.table with impact results
#' @param cnames Which parameters were given weight values
#' @details Provides brute force elimination of dominated solutions
#' @keywords internal
#' @export
non_dominator <- function(intable, cnames, bcode) {
  # intable <- out_dt
  # cnames = c("BD", "COST")
  cnames <- c(cnames, "ind")
  itab <- intable[, which(names(intable) %in% cnames), with = FALSE]
  
  dom = NULL
  h = 1
  for(i in 1:(nrow(itab) - 1)) {
   # for(i in 1:(length(outputtable[[1]]) - 1)) {
    for(j in (i + 1):nrow(itab)) { # j = 2
    # for(j in (i + 1):length(outputtable[[1]])){
      dominated = TRUE
      for(k in 1:(ncol(itab) - 1)){  # k = 1
      #for(k in 1:(length(outputtable)-1)){
        if(itab[[k]][j] < itab[[k]][i]) {
         dominated = FALSE
         break
        }
      }
      if(dominated == TRUE) {
        dom[h] = j
        h = h + 1
      }
    }
  }
  #otab <- list("bcode" = bcode, "table" = itab[!(ind %in% dom)])
  otab <- itab[!(ind %in% dom)]
  return(otab)
}

# itab[, plot(BD, COST)]
# otab[, points(BD, COST, pch = 20, col = "red")]
