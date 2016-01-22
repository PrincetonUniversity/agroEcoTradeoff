#' Fetches inputs from file structure for trademod
#' @param path Base path to data
#' @param input_key A unique file identifier for simulation-specific inputs
#' @details Loads in model inputs
#' @examples  
#' ip <- fetch_inputs(path = "external/data/dt/new", input_key = "ZA")
#' @export
fetch_inputs <- function(path = "external/data/dt", input_key = "ZA") {
  bpath <- set_base_path()
  path <- full_path(bpath, path) 
  
  # Don't read in conversion probability tables. Derive them instead from 
  # constraints.
  # ds_nms <- c("cropnames", "carbon-names", "currprod")
  ds_nms <- c("carbon-names", "currprod")
  fp <- full_path(bpath, paste0("data/", ds_nms, ".rda"))
  for (i in fp) load(i)
  cropnames <- names(currprod)
  
  dtnms <- c("potential-yields", "carbon", "mask", "cost", "bd", "cons")
  lnms <- c("p_yield", "carbon", "mask", "cost", "bd", "cons")
  in_files <- list.files(path, pattern = input_key, full.names = TRUE)
  in_files <- unlist(lapply(dtnms, function(x) in_files[grep(x, in_files)]))
  disksize <- sum(file.info(in_files)$size) * 0.00098^2
  if (disksize > 2048) {
    stop(paste0("The data.table version of tradeoff_mod must still", 
                "needs to have an intelligent system for dealing with", 
                "very large file sizes", call. = FALSE))
  }
  l <- lapply(in_files, function(x) fread(x))
  names(l) <- lnms
  ii <- length(l)
  rda_list <- list(cropnames, currprod)
  for(i in 1:length(rda_list)) l[[ii + i]] <- rda_list[[i]]
  names(l) <- c(lnms, "cropnames", "currprod")
  return(l)
}