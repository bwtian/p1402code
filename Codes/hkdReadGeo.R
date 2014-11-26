source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dir = "~/Share/iData/Hokkaido/")
dsnHkdLine  <- "hokkaido_line.shp"
dsnHkdGeo  <- "hokkaido_geo.shp"
ogrListLayers(dsnHkdLine)
ogrListLayers(dsnHkdGeo)
hkdGeoLine.sldf  <- readOGR(dsnHkdLine, "hokkaido_line")
hkdGeoLine.sldf
hkdGeoLine.sldf <- spTransform(hkdGeoLine.sldf, CRS(wgs84GRS))
levels(factor(hkdGeoLine@data$Legend_E))
hkdFault.sldf  <- hkdGeoLine.sldf[
                        hkdGeoLine.sldf@data$Legend_E == "confirmed fault" |     ## 25Mb
                        hkdGeoLine.sldf@data$Legend_E == "concealed fault" |
                        hkdGeoLine.sldf@data$Legend_E == "inferred fault" ,]     ## 26.4MB
#ge.sp2shpGeo(hkdFault.sldf)
#plot(hkdFault.sldf)
hkdGeoPoly.SPDF  <- readOGR(dsnHkdGeo, "hokkaido_geo")
head(hkdGeoPoly.SPDF@data)
hkdGeoPoly.SPDF <- spTransform(hkdGeoPoly.SPDF, CRS(wgs84GRS))
names(hkdGeoPoly.SPDF)
levels(factor(hkdGeoPoly@data$Division_E))
#ge.sp2shpGeo(hkdGeoPoly.SPDF)
