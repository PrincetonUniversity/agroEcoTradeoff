#' Checks and sets object length to correct length
#' @param x object to check
#' @param lx length it should be
#' @param name Optional name to add in for output message
#' @details Internal function that checks whether object is either of length 1 or equal to some longer length,
#' such as the number of layers in a RasterBrick. If it is between those numbers, it sets the object equal to 
#' the first element.
#' @keywords internal
#' @export
set_length <- function(x, lx = y, name = "") {
  rtypes <- c("RasterLayer", "RasterBrick", "RasterStack")
  if(any(sapply(rtypes, function(j) is(x, j)))) {
    if((nlayers(x) > 1) & (nlayers(x) < lx)) {
      o <- x[[1]]
      print(paste("Length of object", name, 
                  "is incorrect, only the first element will be applied"))
    } else {
      o <- x 
    }
  } else if(is.vector(x)) {
    if((length(x) > 1) & (length(x) < lx)) {
      o <- x[1]
      print(paste("Length of object", name, 
                  "is incorrect, only the first element will be applied"))
    } else {
      o <- x
    }
  }
  return(o)
}

#' Checks and object length to make sure it is equal to 1 or max number of input layers
#' @param x object to check
#' @param lx length it should be
#' @param name Optional name to add in for output message
#' @details Internal function that checks whether object is either of length 1 or equal to some longer length,
#' such as the number of layers in a RasterBrick. If it is not equal to those numbers, it causes a failure.
#' @keywords internal
#' @export
check_length <- function(x, lx, name = "") {
  rtypes <- c("RasterLayer", "RasterBrick", "RasterStack")
  if(any(sapply(rtypes, function(j) is(x, j)))) {
    if((nlayers(x) != 1) & (nlayers(x) != lx)) {
      stop(paste("Number of layers in", name, "must be equal to 1 or", lx), 
           call. = FALSE)
    }
    o <- nlayers(x)
  } else if(is.vector(x)) {
    if((length(x) != 1) & (length(x) != lx)) {
      stop(paste("Length of vector", name, "must be equal to 1 or", lx), 
           call. = FALSE)
    }
    o <- length(x)
  } else if(is.data.table(x)) {
    if((ncol(x) != 1) & (ncol(x) != lx)) {
    stop(paste("Number of columns in", name, "must be equal to 1 or", lx), 
         call. = FALSE)
   }
   o <- length(x)
  }
  return(o)
}


#' Creates unique code for outputs from each simulation
#' @param input_key Input key (country code) passed through trademod function
#' @parm it Iteration number, for code suffix
#' @keywords internal
#' @note This borrow the file-naming bit of raster::rasterTmpFile()
#' @export
run_code <- function(input_key, it) {
  # it = 1
  #code <- paste0(input_key, "-", gsub("\\-|\\:|\\ ", "", strptime(Sys.time(), "%Y-%m-%d %H:%M:%S")))
#   code <- paste0(input_key, "_", 
#                  paste0(gsub(" ", "_", 
#                              format(Sys.time(), format = "%Y_%j_%H%M%S"), 
#                              #gsub(":|-", "", as.character(Sys.time()))), 
#                         # "_", Sys.getpid(), "_", 
#                         paste0(sample(0:9, 5, replace = TRUE), collapse = ""))))
  op <- options(digits.secs = 6)
  tme <- gsub("\\.", "", format(Sys.time(), format = "%y%j_%H%M%OS"))
  options(op)
  code <- paste0(input_key, "_", tme, "_", it)
  return(code)
}


#' Filenaming function for rasters
#' @param path folder and name root for file
#' @param code Result of run_code
#' @keywords internal
#' @export
fname <- function(path, code, ext = ".tif") {
  fnm <- paste0(path, code, ext)
  return(fnm)
}

#' Layer assigning name for rasterStackBrick
#' @param x RasterStackBrick
#' @param namevec vector of layer names
#' @keywords internal
#' @export
nm_up <- function(x, namevec) {
  if(length(namevec)!= nlayers(x)) {
    stop("Layer names and name vector are of different length")
  }
  names(x) <- namevec
  return(x)
}

#' Gives model base path
#' @keywords internal
#' @note This sets the working directory for the model to the highest level
#' directory containing the model name. This won't run the model if you set it 
#' up in a lower level directory of the same name
#' @export
#' 
set_base_path <- function() {
  dpath <- getwd()
  dpathrt <- strsplit(dpath, .Platform$file.sep)[[1]]
  bnames <- c("agroEcoTradeoff", "agroecotradeoff")
  if(!any(dpathrt %in% bnames)) {
    stop("You need to setwd() into agroEcoTradeoff")
  }
  wpath <- full_path(gsub("(agroEcoTradeoff.*)", "", dpath, ignore.case = TRUE, 
                          perl = TRUE), "agroEcoTradeoff")
  return(wpath)
}

# #' Fetches inputs from file structure for trademod
# #' @param path Base path to data
# #' @param input_key A unique file identifier for simulation-specific inputs
# #' @keywords internal
# #' @export
# fetch_inputs <- function(path = "external/data/dt", input_key = "ZA") {
#   path <- full_path(set_base_path(), path) 
# 
#   # Don't read in conversion probability tables. Derive them instead from 
#   # constraints.
#   fp <- full_path(set_base_path(), 
#                   c("data/cropnames.rda", "data/carbon-names.rda"))
#   for (i in fp) load(i)
#   lnms <- c("currprod", "pp_curr", "p_yield", "carbon", 
#             "mask", "cost", "richness", "pas", "cons")
#   bnms <- c("current-production", "production-current", "potential-yields\\.", 
#             "carbon\\.")
#   rnms <- c("mask", "cost", "-div", "pas", "cons")
#   innms <- c(bnms, rnms)
#   in_files <- list.files(path, pattern = input_key, full.names = TRUE)
#   in_files <- unlist(lapply(innms, function(x) in_files[grep(x, in_files)]))
#   disksize <- sum(file.info(in_files)$size) * 0.00098^2
#   if (disksize > 2048) {
#     stop(paste0("The data.table version of tradeoff_mod must still", 
#                 "needs to have an intelligent system for dealing with", 
#                 "very large file sizes", call. = FALSE))
#   }
#   l <- lapply(in_files, function(x) fread(x))
#   names(l) <- lnms
#   l[[length(l) + 1]] <- cropnames
#   names(l) <- c(lnms, "cropnames")
#   return(l)
# }

#' Function to standardize values from 0-1, for raster or data.table
#' @param x A data.table
#' @keywords internal
#' @export
standardize <- function(x) {
  o <- (x - min(x, na.rm = TRUE)) / diff(range(x, na.rm = TRUE))
  return(o)
}

#' Converts rasters to data.tables
#' @param inlist List of input Raster*s
#' @param base TRUE (default), in which case data.table with coordinates and
#' index is created, not if FALSE.
#' @keywords internal
#' @details This assumes that rasters are all of equal size, because it creates
#' a coordinate DT
#' @export
raster_list_to_dt <- function(inlist, base = TRUE) {
  if(base == TRUE) {
    base_dt <- as.data.table.raster(inlist[[1]], xy = TRUE)[, c("x", "y"), 
                                                            with = FALSE]
    base_dt[, ind := 1:nrow(base_dt)]
  }
  dts <- lapply(inlist, function(x) {
    dt <- as.data.table.raster(x)
  })
  if(!is.null(names(inlist))) names(dts) <- names(inlist)
  if(base == TRUE) outlist <- list(base_dt, dts)
  if(base == FALSE) outlist <- dts
  return(outlist)
}

#' Converts rasters to data.tables
#' @param inlist List of input data.tables in same format as output by
#' \code{\link{raster_list_to_dt}}
#' @param CRSobj projected coordinate system
#' @keywords internal
#' @details This assumes that rasters are all of equal size, because it creates
#' a coordinate DT
#' @export
dt_list_to_raster <- function(base, inlist, CRSobj) {
  dts <- lapply(inlist, function(x) {
    dt <- cbind(base, x)
    dtr <- dt_to_raster(dt, CRSobj = CRSobj)
  })
  if(!is.null(names(inlist))) names(dts) <- names(inlist)
  return(dts)
}

#' Silences print statements or not
#' @param x print statement
#' @param silent TRUE or FALSE
#' @keywords internal
#' @keywords internal
#' @export
shhh <- function(x, silent) {
  if(silent == FALSE) print(x)
} 

#' Fetches raster meta data for study area
#' @param path input path
#' @param input_key Location code
#' @keywords internal
#' @note This will likely be replaced by upgrades to dtraster package, which 
#' will provide header data for raster tables. Currently this reads metadata 
#' from a raster mask of the study area.
#' @export
spatial_meta <- function(path, input_key) {
  rnm <- full_path(path, paste0(input_key, "-mask.tif"))
  r <- raster(rnm)
  ha <- res(r)[1]^2 / 10000  # hectares for study area
  CRSobj <- projection(r)
  list("ha" = ha, "crs" = CRSobj)
}
 
# #' Cumulative sum, ignoring NA.
# #' @param x numeric vector
# #' @return: cumulative sum of x, but NA values are ignored
# #' @keywords internal
# #' @note Function code from https://github.com/google/CausalImpact/blob/master/R/impact_misc.R 
# #' @examples:
# #' cumsum.na.rm(c(1, NA, 2))
# #' cumsum(c(1, NA, 2))
# cumsum.na.rm <- function(x) {
#   if (is.null(x)) {
#     return(x)
#   }
#   s <- cumsum(ifelse(is.na(x), 0, x))
#   s[is.na(x)] <- NA
#   return(s)
# }
