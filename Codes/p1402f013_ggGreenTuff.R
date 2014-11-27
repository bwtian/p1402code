source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd("~/D/tian/greenTuff/")
### Make study Boundary

jp1.SPDF  <- getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
hkdLand.SPDF  <- ge.LargestPolys(jp1.SPDF, Polygon=T)
plot(hkdLand.SPDF)

### Plot GreenTuff
GreenTuff  <- "GreenTuff/doc.kml"
ogrListLayers(GreenTuff)
greenTuff.sldf  <-readOGR(GreenTuff,  "GreenTuff")
greenTuff.sldf
proj4string(greenTuff.sldf)  <- CRS(wgs84GRS)
greenTuff.SP  <- ge.splitPoly(greenTuff.sldf, hkdLand.SPDF )

greenTuff.df  <- fortify(greenTuff.SP)
greenTuff.df   <- greenTuff.df [!greenTuff.df $group == 1.4,] # eliminate
greenTuff.df$gtuff  <- 0
greenTuff.df[greenTuff.df$group == 1.2,]$gtuff  <- 1
greenTuff.df[greenTuff.df$group == 1.3,]$gtuff  <- 1
levels(factor(greenTuff.df$gtuff))
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
belt.dfr  <- fortify(belt.SP)
ggplot(belt.df) + geom_polygon(aes(x = long, y = lat, group=group, fill = group))
belt.df  <- belt.dfr[!(belt.dfr$group == "1.7" |
                       belt.dfr$group == "1.8" |
                       belt.dfr$group == "1.9" |
                       belt.dfr$group == "1.10"),]
belt.df[belt.df$group == 1.6,]$group = "1.5"
belt.df$name  <-  "Hidaka & Tokoro Belt"
belt.df[belt.df$group == 1.2,]$name  <- "North Kitakami Belt"
belt.df[belt.df$group == 1.3,]$name  <- "Nemuro Belt"
belt.df[belt.df$group == 1.4,]$name  <- "Kamuikotan Belt"
belt.df[belt.df$group == 1.5,]$name  <- "Sorachi-Yezo Belt"


levels(factor(belt.df$group))

#ge.sp2shpGeo(hkdLand)

ggplot(belt.df) + geom_polygon(aes(x = long, y = lat, group=group, fill = group),
                               data = belt.df) +
        geom_path(aes(x = long, y = lat, group=piece, color = factor(gtuff)), greenTuff.df,
                  size = 2)

