source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd("~/D/tian/greenTuff/")
### Make study Boundary

jp1.SPDF  <- getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
hkdLand.SPDF  <- ge.LargestPolys(jp1.SPDF, Polygon=T)
plot(hkdLand)

kml  <- "GreenTuff/doc.kml"
ogrListLayers(kml)
greenTuff.sldf  <-readOGR(kml,  "GreenTuff")
greenTuff.sldf
proj4string(greenTuff.sldf)  <- CRS(wgs84GRS)
dx  <- ge.splitPoly(greenTuff.sldf, hkdLand.SPDF )
plot(dx)
df  <- fortify(dx)
df  <- df[!df$group == 1.4,]
levels(factor(df$group))
#ge.sp2shpGeo(hkdLand)
gTuff  <- ggplot(df) + geom_polygon(aes(x = long, y = lat, group=group, fill = group))
