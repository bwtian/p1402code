## Lastedit: 20150107
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")

### Make study Boundary

jp1.SPDF  <- getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
basemap.r  <- readRDS("~/Dropbox/2data/dataProduct/jp/jp_google_hybrid_137.5_35.5_zoom5_140825_1326.Rds")
jpVolA  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
jpVolQ  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol455_140812_172148.Rds")

ggVol  <- ggmap(basemap.r, extent = "panel") +
        geom_point(data = jpVolQ@data,
                   aes(as.numeric(lon), as.numeric(lat),
                       color="blue"), shape = 17, alpha = 0.7) +
        geom_point(data = jpVolA@data,
                   aes(as.numeric(lon), as.numeric(lat),
                      color="red"),  shape = 17, size = 2)  +
        scale_color_manual(name =  "Volcanoes", values = c("orange","red"), labels = c("Quaternary volcanoes","Active volcanoes"))
ggVol
library(wrspathrow)
wrs2.SPDF  <- pathrow_num(x = jp1.SPDF, as_polys = TRUE)
#plot(wrs2.SPDF,col = "red")
#wrs2.SPDF@data
wrs2.df  <- fortify(wrs2.SPDF)

ggWRS  <-ggSap + geom_polygon(aes(long,lat,group=group),
                    color = "grey", alpha = 0.3, fill = NA,
                    linetype = 3,
                    data=wrs2.df) +
        geom_point(data = wrs2.SPDF@data, color = "black",
                   aes(x = centroid_x, y = centroid_y, shape = MODE ))

ggWRS2  <- ggWRS +  geom_text(data = wrs2.SPDF@data,
                  aes(x = centroid_x, y = centroid_y),
                  label = paste(wrs2.SPDF@data$PATH, wrs2.SPDF@data$ROW," "),
                  family="Times", face = "Italic", colour="black", size = 4) +
        scale_shape_manual(name =  " WRS 2", values = 20 , labels = c("Path and Row"))
### Plate Boundaries

jpArc.sldf  <- readRDS("~/Dropbox/2data/dataProduct/jp/jpPlateBoundary_141124_223221.Rds")

ggPlate  <- ggWRS2 + geom_path(aes(long,lat,group=piece),
                    color = "red",
                    linetype = 1,
                    size = 1,
                    hkdArc.df) +
  geom_text(aes(x = 144.5, y = 41.4, label = "Kuril Trench"),
            hjust = -0.1, angle = 35, family="Times", colour="white",
            size = 4) +
  geom_text(aes(x = 143.5, y = 40, label = " Japan \n Trench"),
            hjust = -0.1, angle = 90, family="Times", colour="white",
            size = 4) +
  geom_text(aes(x = 139.4, y = 44, label = "Plate Boundary"),
            hjust = -0.1, angle = 78, family="Times", colour="white",
            size = 4)

jpTlines.sldf  <- readRDS("~/Dropbox/2data/dataProduct/jp/jpTlines_141125_221917.Rds")
ggTlines  <- ggPlate + geom_line(aes(long,lat,group=group, linetype=factor(id2)),
  color = "red",
  #linetype = 2,
  size = 1,
  hkdTlines.df) +
  scale_linetype_manual(name =  "Tectonic lines", values = c(1,3),
                                    labels = c("Tectonic lines","Volcanic front"))
ggBar  <- ggTlines  +
  scaleBar(lon = 139, lat = 40, distanceLon = 100,
           distanceLegend = 30, distanceLat = 15,
           dist.unit = "km", arrow.length = 60,
           arrow.distance = 680, arrow.North.size = 4,
           legend.colour = "white", arrow.North.color = "white", arrow.colour = "blue")

ggFont  <- ggBar +
  #coord_equal() +
  theme_bw(base_family = "Times")
hkd  <-ggFont
# ge.ggsave(hkd)
# ge.ggsave
ggsave(plot = hkd, "hkd.pdf", width = 7, height =5.3)
# getwd()
