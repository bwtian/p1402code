### hkdExtent

xmin <- 139
xmax <- 146
ymin <- 41
ymax <- 46
bbox.SPDF <- ge.xy2bboxSPDF(xmin,xmax,ymin,ymax,wgs84GRS)
hkdExtent  <- bbox.SPDF
ge.sp2shpGeo(hkdExtent)
getwd()


library(raster)
a  <- getData("SRTM", lat = 42.5, lon = 142.5)
plot(a)
getData("ISO3")
japan  <- getData('alt', country='JPN')
plot(japan)
hkd  <- crop(japan, hkdExtent)
plot(hkd)
writeRaster(hkd, "hkdDEM.tif")
contour(hkd, levels = c(500,1000,1500))
