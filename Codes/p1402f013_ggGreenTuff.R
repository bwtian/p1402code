source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd("~/D/tian/greenTuff/")
### Make study Boundary

jp1.SPDF  <- getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
hkdLand.SPDF  <- ge.LargestPolys(jp1.SPDF, Polygon=T)
plot(hkdLand)

### Plot GreenTuff
GreenTuff  <- "GreenTuff/doc.kml"
ogrListLayers(GreenTuff)
greenTuff.sldf  <-readOGR(GreenTuff,  "GreenTuff")
greenTuff.sldf
proj4string(greenTuff.sldf)  <- CRS(wgs84GRS)
greenTuff.SP  <- ge.splitPoly(greenTuff.sldf, hkdLand.SPDF )

greenTuff.df  <- fortify(greenTuff.SP)
greenTuff.df   <- greenTuff.df [!greenTuff.df $group == 1.4,]
levels(factor(greenTuff.df$group))
#ge.sp2shpGeo(hkdLand)
#ggplot(greenTuff.df) + geom_polygon(aes(x = long, y = lat, group=group, fill = group))

### Plot Belt
Belt  <- "Belt/doc.kml"
ogrListLayers(Belt)
belt.sldf  <-readOGR(Belt, "Belt")
belt.sldf
proj4string(belt.sldf)  <- CRS(wgs84GRS)
belt.SP <- ge.splitPoly(belt.sldf, hkdLand.SPDF )
plot(belt.SP)
belt.df  <- fortify(belt.SP)
df  <- df[!df$group == 1.4,]
levels(factor(belt.df$group))
#ge.sp2shpGeo(hkdLand)

ggplot(belt.df) + geom_polygon(aes(x = long, y = lat, group=group, fill = group),
                               data = belt.df) +
        geom_path(aes(x = long, y = lat, group=piece, color = group), greenTuff.df,
                  size = 2) +

