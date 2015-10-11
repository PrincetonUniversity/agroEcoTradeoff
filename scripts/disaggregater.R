fname <- full_path("external/ext_data2/", paste0(x, "-carbon.tif"))
c10 <- brick("external/ext_data/ZA-carbon.tif")
c <- disaggregate(c10, fact = 9)
writeRaster(c, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-carbon-priorities.tif"))
cp10 <- brick("external/ext_data/ZA-carbon-priorities.tif")
cp <- disaggregate(cp10, fact = 9)
writeRaster(cp, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-cons-priorities.tif"))
cop10 <- brick("external/ext_data/ZA-cons-priorities.tif")
cop <- disaggregate(cop10, fact = 9)
writeRaster(cop, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-cost.tif"))
cost10 <- brick("external/ext_data/ZA-cost.tif")
cost <- disaggregate(cost10, fact = 9)
writeRaster(cost, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-crop-convert-fractions-base.tif"))
ccfb10 <- brick("external/ext_data/ZA-crop-convert-fractions-base.tif")
ccfb <- disaggregate(ccfb10, fact = 9)
writeRaster(ccfb, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-current-production.tif"))
cup10 <- brick("external/ext_data/ZA-current-production.tif")
cup <- disaggregate(cup10, fact = 9)
writeRaster(cup, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-mammal-div.tif"))
md10 <- brick("external/ext_data/ZA-mammal-div.tif")
md <- disaggregate(md10, fact = 9)
writeRaster(md, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-mask.tif"))
mask10 <- brick("external/ext_data/ZA-mask.tif")
mask <- disaggregate(mask10, fact = 9)
writeRaster(mask, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-pas-10km.tif"))
pas10 <- brick("external/ext_data/ZA-pas-10km.tif")
pas <- disaggregate(pas10, fact = 9)
writeRaster(pas, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-potential-production-current-area.tif"))
ppca10 <- brick("external/ext_data/ZA-potential-production-current-area.tif")
ppca <- disaggregate(ppca10, fact = 9)
writeRaster(ppca, filename = fname, overwrite = TRUE)

fname <- full_path("external/ext_data2/", paste0(x, "-potential-yields.tif"))
py10 <- brick("external/ext_data/ZA-potential-yields.tif")
py <- disaggregate(py10, fact = 9)
writeRaster(py, filename = fname, overwrite = TRUE)

