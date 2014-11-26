setwd("D:/tian/GIS/DEM/")
library(raster)
hgts  <- list.files(pattern = "*.hgt")
rsts.l  <-  lapply(hgts,raster)
merge  <- do.call(merge, rsts.l)
writeRaster(merge, "hkdSRTM90.tif")
plot(merge)
summary(merge)
merge
