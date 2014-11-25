## ggplotKMZ
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd("~/Dropbox/2data/data/")

kmz  <- "plate-boundaries.kmz"
sldf <- readOGR(kmz,  "Plate Boundaries")
kmz  <- "plateboundaries.kmz"
sldf <- readOGR(kmz,  "Plate Convergence")
kmz  <- "Tectonic_Plate_Boundaries.kml"
sldf <- readOGR(kmz,  "Tectonic Plate Boundaries")
kmz  <- "TectonicPlates.kmz"
ogrListLayers(kmz)
plot(sldf)
library(plotKML)
jpBbox  <- ge.xy2bboxSPDF(128, 147, 30, 46, wgs84GRS) 
jpPlateBoundary  <- crop(sldf, jpBbox)
plot(jpPlateBoundary)
class(jpPlateBoundary)
ge.sp2shpGeo(jpPlateBoundary)
