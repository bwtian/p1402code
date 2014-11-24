## ggplotKMZ
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd("~")

kmz  <- "plate-boundaries.kmz"
kmz  <- "plateboundaries.kmz"
kmz  <- "Tectonic_Plate_Boundaries.kml"
kmz  <- "TectonicPlates.kmz"
ogr2ogr(kmz)
ogrListLayers(kmz)
  
plot(kmz.spdf)
ggplot(kmz.spdf@data) + geom_line()
rt  <- readOGR(kmz,  "Plate Boundaries")
class(rt)
library(plotKML)
ge.sp2shpGeo(obj = rt)
xlimJP
ylimJP
jpBbox  <- ge.xy2bboxSPDF(128, 147, 30, 46, wgs84GRS) 

plot(jpBbox)
jpPlateBoundary  <- crop(rt, jpBbox)
plot(jpPlateBoundary)
class(jpPlateBoundary)
ge.sp2shpGeo(jpPlateBoundary)
ogrListLayers(kmz)
