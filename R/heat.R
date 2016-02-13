#' Create heat map from group of conversion map data.table
#' @param bpath Path to the batch run you want to make
#' @param basedt The base (mask) data.table for conversion to raster
#' @param todisk TRUE/FALSE, whether to write raster to disk
#' @return A raster, optionally written to disk in the batch folder, containing 
#' the frequency (in percent) of conversions for each pixel in the map. 
#' @export
heat <- function(bpath, basedt, todisk = TRUE) {
  fnames <- dir(bpath, pattern = "csv", full.names = TRUE)
  DT <- lapply(fnames, function(x) {
    DTi <- fread(x)
    if(ncol(DTi) > 1) {
      DTi[, rowSums(.SD)]
    } else {
      DTi
    }
    DTi
  })
  DTbind <- data.table(do.call(cbind, DT))
  DTsum <- data.table(DTbind[, rowSums(.SD)])
  DTspat <- cbind(basedt[, .(x, y)], DTsum)
  DTb <- dt_to_raster(DTspat, CRSobj)
  DTbpct <- DTb / maxValue(DTb) * 100
  writeRaster(DTbpct, filename = fp(bpath, "heat.tif"))
  return(DTbpct)
}
