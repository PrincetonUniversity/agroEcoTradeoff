#' Fetches inputs from file structure for trademod
#' @param path Base path to data
#' @details Loads in model inputs
#' @examples  
#' ip <- fetch_inputs(path = "external/data/ZA/")
#' @export
fetch_inputs <- function(path) {
  # path = "external/data/ZA"
  
  # Don't read in conversion probability tables. Derive them instead from 
  # constraints.
  # ds_nms <- c("cropnames", "carbon-names", "currprod")
  ds_nms <- c("currprod", paste0(basename(path), "-bdprops"))
  fp <- full_path(path, paste0(ds_nms, ".rda"))
  for (i in fp) load(i)
  cropnames <- names(currprod)
  
  # spatial metadata
  sp_parms <- spatial_meta(path, input_key = basename(path))
  
  # data.tables
  dtnms <- c("potential-yields", "carbon", "mask", "cost", "bd", "cons", 
             "convertible", "intpa")
  lnms <- dtnms
  lnms[1] <- "p_yield"
  in_files <- list.files(path, pattern = paste0(basename(path), ".*.csv"), 
                         full.names = TRUE)
  in_files <- unlist(lapply(dtnms, function(x) in_files[grep(x, in_files)]))
  disksize <- sum(file.info(in_files)$size) * 0.00098^2
  if (disksize > 2048) {
    stop(paste0("The data.table version of tradeoff_mod still", 
                "needs to have an intelligent system for dealing with", 
                "very large file sizes", call. = FALSE))
  }
  l <- lapply(in_files, function(x) fread(x))
  names(l) <- lnms
  
  # Check on equal number of rows
  if(diff(range(sapply(l, function(x) nrow(x)))) != 0) {
    stop("All inputs data.table need to have the same number of rows", 
         call. = FALSE)
  }
  # Check on having NAs
  if(any(sapply(l, function(x) any(is.na(x))))) {
    stop("Input data.tables can't have NAs", call. = FALSE)
  }
  
  ii <- length(l)
  rda_list <- list(cropnames, currprod, bdprops, sp_parms)
  for(i in 1:length(rda_list)) l[[ii + i]] <- rda_list[[i]]
  names(l) <- c(lnms, "cropnames", "currprod", "bdprops", "sp")
  return(l)
}
