#' Evaluates a raster or raster using logical tests 
#' @param x Any raster calculation statement
#' @param tmpdir Location raster will be written to if too big to handle in memory
#' @param format Default format for rasters, here geotiff 
#' @param NAval Value to set to NA, default is NULL
#' @param filename Output raster name, with gdal compliant extension
#' @return Raster* resulting from raster algebra statement
#' @details The file writing part is a bit complicated. The name you provide the function will force a write
#' to disk only if the calculation can be done in memory. Otherwise, if raster wrote a temporary file to 
#' disk because it was too big for memory, the function returns that object.  This is why the function gives 
#' control as to where you can write a filename.  
#' @note This function was modified from one found in my old ldefuncs.rs.R. As noted there, this should be 
#' improved to run in blocks, and perhaps parallelize, while calculation is being done. In future this 
#' function, if it proves useful, should go into separate R library for raster functions. 
#' @export
rast_math <- function(x, tmpdir = NULL, format = "GTiff", NAval = NULL, filename = NULL) {
  if(!is.null(tmpdir)) rasterOptions(tmpdir = tmpdir)
  rasterOptions(format = format)
  rexp <- expression(x)
  r <- eval(rexp)
  if(!is.null(NAval)) {
    NAvalue(r) <- NAval
  }
  if(!is.null(filename) & inMemory(r)) {
    r <- writeRaster(r, filename = filename, overwrite = TRUE)
  } else
  return(r)
}

#' Checks class of raster object
#' @param x raster* object to check
#' @details Internal function
#' @keywords internal
#' @export
rclass_chk <- function(x) any(sapply(c("RasterLayer", "RasterBrick", "RasterStack"), function(j) is(x, j)))

#' @title Run gdal_calc.py
#' 
#' @description Allows gdal_calc.py to be run, with limited options, for arbitrary calculations
#' @param cstr String providing the raster calculation to be performed, e.g. "(A == 1) * 10"
#' @param x a list of rasters, or filenames of rasters, where list element names match names in cstr
#' @param filename output filename to write, with gdal compliant filename extension
#' @param type The gdal data type, e.g. "UInt16" (default), "Byte", etc.
#' @param gdformat The gdal format for output. Default if "GTiff" for geotiff
#' @param overwrite Option to overwrite existing file name. Default is TRUE
#' @param robject Name of robject to return. Default is FALSE, meaning raster is simply written to disk
#' @return The calculated raster
#' @keywords internal
#' @note This still has to be fixed to handle multi-band rasters gracefully
#' @export
gdal_calc <- function(cstr, x, filename, type = "UInt16", gdformat = "GTiff", overwrite = TRUE, 
                      robject = FALSE) {
  dang <- Sys.time()
  rnm <- function(j) if(is.vector(j)) j else if(rclass_chk(j)) j@file@name
  rnms <- paste("-", names(x), " ",  sapply(x, rnm), sep = "", collapse = " ")
  calc_str <- paste0("gdal_calc.py ", rnms, " --outfile=", filename, " --type=", type, " --format=", 
                     gdformat, paste(ifelse(overwrite == TRUE, " --overwrite ", " ")), "--calc='", cstr, "'")
  print(paste("Running calculation", cstr))
  system(calc_str)
  print(paste("finished in", Sys.time() - dang))
  if(robject == TRUE) r <- raster(filename)
  return(r)
} 

