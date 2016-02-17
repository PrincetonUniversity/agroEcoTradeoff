#' Raster ybetas to data.tables (deprecated)
#' @param ybetas list of either 2 rasters or 2 vectors providing yield 
#' modifications for climate & irrigation.
#' @param cropnames Vector of cropnames in model
#' @keywords internal
#' @export
ybeta_rast_to_dt <- function(ybetas, cropnames, base) {
  for(i in 1:length(ybetas)) {
    if(tolower(gsub("Brick|Stack|Layer", "", 
                    class(ybetas[[i]])[1])) == "raster") {
     if(nlayers(ybetas[[i]]) != length(cropnames)) {
      stop("Yield modifying rasters should have one layer per crop", 
           call. = FALSE)
     }
     valind <- base$ind
     ybetas[[i]] <- as.data.table(ybetas[[i]])[valind, ]
    }
  }
  return(ybetas)
}
