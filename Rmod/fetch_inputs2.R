fetch_inputs2 <- function(path = "external/ext_data", input_key = "ZA", input = "D") 
{
  path <- ifelse(input == "D", full_path(set_base_path(), "external/ext_data/dt"), 
                 full_path(set_base_path(), "external/ext_data/"))
  
  # Don't read in conversion probability tables. Derive them instead from constraints.
  for (i in full_path(set_base_path(), c("data/cropnames.rda", 
                                         "data/carbon-names.rda"))) load(i)
  lnms <- c("currprod", "pp_curr", "p_yield", "cropfrac", "carbon", 
            "mask", "cost", "richness", "pas")
  bnms <- c("current-production", "production-current", "potential-yields\\.", 
            "crop-convert-fractions", "carbon\\.")
  rnms <- c("mask", "cost", "-div", "pas")
  nmupnms <- c(rep("cropnames", 4), "carbon_names")
  innms <- c(bnms, rnms)
  if (input == "D") {
    in_files <- list.files(path, pattern = input_key, full.names = TRUE)
    in_files <- unlist(lapply(innms, function(x) in_files[grep(x, 
                                                               in_files)]))
    disksize <- sum(file.info(in_files)$size) * 0.00098^2
    if (disksize > 2048) {
      stop(paste0("The data.table version of tradeoff_mod must still", 
                  "needs to have an intelligent system for dealing with", 
                  "very large file sizes", call. = FALSE))
    }
    l <- lapply(in_files, function(x) fread(x))
    names(l) <- lnms
  }
  else if (input == "R") {
    in_files <- list.files(path, pattern = input_key, full.names = TRUE)
    bin_files <- unlist(lapply(bnms, function(x) in_files[grep(x, 
                                                               in_files)]))
    rin_files <- unlist(lapply(rnms, function(x) in_files[grep(x, 
                                                               in_files)]))
    lb <- lapply(1:length(bin_files), function(x) {
      nm_up(brick(bin_files[x]), get(nmupnms[x]))
    })
    lr <- lapply(rin_files, function(x) raster(x))
    l <- c(lb, lr)
    names(l) <- lnms
  }
  l[[length(l) + 1]] <- cropnames
  names(l) <- c(lnms, "cropnames")
  return(l)
}
