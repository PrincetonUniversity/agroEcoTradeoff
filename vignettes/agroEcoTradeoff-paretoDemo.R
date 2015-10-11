library(agroEcoTradeoff)
source("R/tradeoff_mod.R")
source("R/constraints_dt.R")
source("R/convert_dt.R")
source("R/pareto.R")
source("R/targets.R")
source("R/impact_dt.R")
source("R/input_handler.R")
source("R/fetch_inputs.R")
source("R/yield_mod.R")

# 2 Constraints
# Yield modification
yblist <- list(yb1 <- c(1, 1))
# Target multiplier
targ <- 2
# Refine the step interval (divisible evenly into 1) over which to search for optimal solutions
step <- 0.25
# Pick the constraints to optimize over
cnames <- c("Ag", "C")

ot <- pareto(cnames = cnames, step = step, yblist = yblist, targ = targ)
# Plot the Pareto front
plot(ot$land, ot$carbon, xlab = "Total Area Converted (ha)", ylab = "Total Carbon Lost (t)")

# Load the output tables
setwd(full_path(proj_root("agroEcoTradeoff"), "agroEcoTradeoff"))
dnm <- dir("external/output/batch/dt")
load(paste0("external/output/batch/dt/", dnm, "/out_tables.rda"))
load(paste0("external/output/batch/dt/", dnm, "/parms.rda"))

# Set projection
il <- fetch_inputs(input_key = "ZA", input = "R")
CRSobj <- projection(il$currprod)

# Fetch Inputs using data table format
il <- fetch_inputs(input_key = "ZA")

# View targets
target <- targets_dt(prod_targ = parms[1, il$cropnames], 
                    currprod = il$currprod, potprod = il$pp_curr)
colnames(target)[5:6]  <- c("%existing", "%newland")
knitr::kable(target[, -4])

# Load up some background data
library(RColorBrewer)
zam <- raster("external/ext_data/ZA-mask.tif")
cropland <- raster("external/ext_data/ZA-crop-areas.tif")
fblock <- raster("external/ext_data/farm-blocks.tif")

dtrs <- lapply(1:5, function(x) {
  dt <- fread(paste0("external/output/batch/dt/", dnm, "/", 
                     names(out_list)[x], ".csv"))
  dto <- cbind(dt[, c("x", "y"), with = FALSE], 
               do.call(cbind, lapply(colnames(dt)[-c(1:2)], function(i) {
                 dt[, c(i), with = FALSE]
               })))
  dtr <- dt_to_raster(dto, CRSobj = CRSobj)
})

brks <- c(0, 0.01, 0.05, 0.1, 0.15, 0.2)
brks2 <- c(0, seq(0.001, 0.601, 0.15))
yor <- c("transparent", brewer.pal(9, name = "YlOrRd")[5:8])
gr <-  c("transparent", brewer.pal(9, name = "Greens")[5:8])
bp <- c("grey90", brewer.pal(9, name = "Blues")[c(4, 6, 7, 9)])
ldim = c(0.4, 0.75)
sdim = c(0.03, 0.12, 0.25)
legdim = c(0.8, 0.15)
tdim = c(0.58, 0.18)

# Plot the cropland, farmblocks, and converted land under the chosen scenario

# Select the scenario based upon ranking of total area converted (1 is scenario with least area converted)
scen = ot$ind[1] #least area converted
conv_plot(scen = scen)


# Plotted values of the 5 scenarios, aggregated across all crops
scenRange <- 1:5
cl <- 1.5
cols <- bpy.colors(5)
par(mfrow = c(3, 2), mar = c(2, 4, 1, 2), mgp = c(2, 1, 0))
v <- sapply(scenRange, function(x) sum(out_list[[x]]$conv_area) / 1000)
plot(v, pch = 20, cex = 4, col = cols, ylab = "ha/1000", xaxt = "n", xlab = "", ylim = range(v), cex.lab = cl) 
v <- sapply(scenRange, function(x) sum(out_list[[x]]$pa_loss) / 1000)
plot(v, pch = 20, cex = 4, col = cols, ylab = "ha/1000", xaxt = "n", xlab = "", ylim = range(v) * 1.1, 
     cex.lab = cl) 
v <- sapply(scenRange, function(x) mean(out_list[[x]][, 3], na.rm = TRUE))
plot(v, pch = 20, cex = 4, col = cols, ylab = "spp/tY", xaxt = "n", xlab = "", ylim = range(v), cex.lab = cl) 
v <- sapply(scenRange, function(x) mean(out_list[[x]][, 5], na.rm = TRUE))
plot(v, pch = 20, cex = 4, col = cols, ylab = "tC/tY", xaxt = "n", xlab = "", ylim = range(v), cex.lab = cl) 
v <- sapply(scenRange, function(x) sum(out_list[[x]][, 6], na.rm = TRUE) / 1000)
plot(v, pch = 20, cex = 4, col = cols, ylab = "C loss (tonnes / 1000)", xaxt = "n", xlab = "",
     ylim = range(v), cex.lab = cl)
plot(1:1000, 1:1000, pch = "", axes = FALSE, ylab = "")
points(rep(400, 5), seq(50, 950, 200), pch = 20, col = cols, cex = 4)
text(rep(440, 5), seq(50, 950, 200), labels = c("Scen 1", "Scen 2", "Scen 3", "Scen 4", "Scen 5"), 
     adj = 0)

