##### hkd
## Lastedit: 20141210
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
getwd()
setwd("~/Dropbox/2data/data/greenTuff")
# ge.getGoogleMap(142.5,43.5,4)
### Make study Boundary

jp1.SPDF  <- getData('GADM', country='JPN', level=1, path = "~/Dropbox/2data//dataRaw/gadm2")
basemap.r  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_google_satellite_142.5_43.5_zoom6_141018_2339.Rds")
jpVolA.spdf  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
jpVolQ.spdf  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol455_140812_172148.Rds")
sap.spdf  <- readRDS("~/Dropbox/2data/data/Sapporo_140817_162919.Rds")
xmin <- 139
xmax <- 146
ymin <- 41.4
ymax <- 45.8
bbox.SPDF <- ge.xy2bboxSPDF(xmin,xmax,ymin,ymax,wgs84GRS)
bh <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
bh_xy  <- bh[!duplicated(bh$ID),]
bh_xy$grp <- cut(bh_xy$TD, breaks = c(0,500,1000,1500,2000,2200),
                 labels = c("0-500","500-1000","1000-1500","1500-2000","2000-2200"))

proj4string(jpVolQ.spdf) <- proj4string(bbox.SPDF)
volQ <- jpVolQ.spdf[bbox.SPDF,]
volA <- jpVolA.spdf[bbox.SPDF,]
hkdLand  <- ge.LargestPolys(jp1.SPDF, Polygon=T)
#plot(hkdLand)
volQ2  <- jpVolQ.spdf[hkdLand,]
#volA@data
A  <- c(1,7,10,12,15,18)
volAA  <- volA[volA$ID %in% A,]
#volAA@data
limitsX  <- c(138,147)
breaksX  <- seq(limitsX[1], limitsX[2],1)
labelsX=parse(text=paste(breaksX, "^o ", "*E", sep=""))
##limitsY  <- c(41,47)
limitsY  <- c(40,47)
breaksY  <- seq(limitsY[1],limitsY[2],1)
labelsY=parse(text=paste(breaksY, "^o ", "*N", sep=""))
## Layer0: Base map
ggBH  <-  ggmap(basemap.r, extent = "panel") +
        ### Layers
        geom_point(data = bh_xy, aes(Lon, Lat,fill = grp, size = grp),
                   shape = 21, alpha = 0.9) +

        scale_x_continuous(name = " Longitude",
                           breaks=breaksX,
                           labels=labelsX,
                           limits=limitsX,
                           expand = c(0.01,0.01)) +

        scale_y_continuous(name = " Latitude",
                           breaks=breaksY,
                           labels=labelsY,
                           limits=limitsY,
                           expand = c(0.01,0.01)) +
        theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0),
              axis.title.x = element_text(vjust = 0)) +
        # labs(size = "Borehole depth (m)") +
        scale_size_manual("Borehole depth (m)",values=c(1,1.5,2,3,4)) +
        scale_fill_brewer("Borehole depth (m)", palette="Blues")
ggVol  <- ggBH  +
        geom_point(data = volQ@data,
                   aes(as.numeric(lon), as.numeric(lat),
                       color="blue"), shape = 17, alpha = 0.7) +
        geom_point(data = volA@data,
                   aes(as.numeric(lon), as.numeric(lat),
                       color="red"),  shape = 17, size = 2)  +
        geom_point(data = volAA@data,
                   aes(as.numeric(lon), as.numeric(lat),color="white"),
                   shape = 17, size = 2) +
        scale_color_manual(name =  "Volcanoes", values = c("orange","red", "green"), labels = c("Quaternary volcanoes","Active volcanoes", "Most active volcanoes")) +
        geom_path(data = volQ2@data, aes(as.numeric(lon), as.numeric(lat)),size = 12, alpha = 0.2, colour = "yellow",lineend = "round")

ggSap  <- ggVol + geom_point(data = sap.spdf, aes(x = lon, y = lat), colour = "White")  +
        geom_text(data = sap.spdf, aes(x = lon, y = lat, label = name), hjust = -0.1,family="Times", face="italic", colour="white")


library(wrspathrow)
wrs2.SPDF  <- pathrow_num(x = hkdLand, as_polys = TRUE)
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
#plot(jpArc.sldf)
bbox2.SPDF <- ge.xy2bboxSPDF(138,147,40,47,wgs84GRS)
hkdArc.sldf  <- crop(jpArc.sldf, bbox2.SPDF)
hkdArc.df  <- fortify(hkdArc.sldf )
hkdArc.df  <- hkdArc.df[order(hkdArc.df$lat),]
rownames(hkdArc.df)  <- seq_along(hkdArc.df$lat)
# summary(hkdArc.df)
# ggWRS2 + geom_point(aes(long,lat,group=group),
#                       color = "red",
#                       linetype = 1,
#                       hkdArc.df) +


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
hkdTlines.sldf  <- crop(jpTlines.sldf, bbox2.SPDF)
#plot(hkdTlines.sldf)
hkdTlines.df  <- fortify(hkdTlines.sldf)
## regroup
hkdTlines.df$id2 <- 2
hkdTlines.df[hkdTlines.df$id == 1,]$id2 <- 1
hkdTlines.df[hkdTlines.df$id == 3,]$id2 <- 1
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
        theme_bw(base_family = "Times") +
        theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0),
              axis.title.x = element_text(vjust = 0.25))
hkd  <-ggFont
### inset
basein  <- readRDS("google_google_satellite_142.5_43.5_zoom4_150130_1545.Rds")
p  <- ggmap(basein) + xlim(120,165) + ylim(24,55) +
        geom_rect(aes(xmin = 139, xmax = 147, ymin = 40, ymax = 47),alpha=0.05, colour="cyan", fill = "cyan",linetype=1, size = 0.2) +
        geom_text(aes(x = 150, y = 35, label = "Pacific \n Ocean"),
                  hjust = -0.1, angle = 0, family="Times", colour="white",
                  size = 3) +
        geom_text(aes(x = 136, y = 35, label = "Japan"),
                  hjust = -0.1, angle = 30, family="Times", colour="white",
                  size = 3)
#         geom_point(data = jpVolA.spdf@data,
#                    aes(as.numeric(lon), as.numeric(lat)),
#                        color="red",  shape = 17, size = 2)
p
# p = qplot(1:10, 1:10, log='x')
# g = ggplotGrob(qplot(1, 1))
# Error: annotation_custom only works with Cartesian coordinates
# hkd + annotation_map(grob = p, xmin = 144, xmax =147, ymin = 45, ymax = 47)
box= data.frame(lon = c(143,147.5), lat= c(44.7, 47))
# geom_rect(aes(xmin = 143.5, xmax = 147, ymin = 44.7, ymax = 47.7), colour = "white", size =1) +
fullmap <- hkd + geom_segment(aes(x=143.45,xend=147, y=44.75,yend=44.75), color = "white", alpha = 0.8) +
        geom_segment(aes(x=143.45,xend=143.45, y=44.75,yend=47), color = "white", alpha = 0.8) +
inset(grob = ggplotGrob(p + theme_nothing()), xmin = 143, xmax = 147.5, ymin = 44.75, ymax = 47)
ggsave(plot = fullmap, "hkd.pdf", width =7, height = 5)
getwd()
# print(fullMap)
#
# #Any old plot
# a_plot <- ggplot(cars, aes(speed, dist)) + geom_line()
#
# #A viewport taking up a fraction of the plot area
# vp <- viewport(width = 3, height = 3, x = 144, y = 45)

#Just draw the plot twice
# png("test.png")
# print(hkd)
# print(p, vp = vp)
# dev.off()
# v1<-viewport(width = 1, height = 1, x = 0.5, y = 0.5) #plot area for the main map
# v2<-viewport(width = 0.3, height = 0.3, x = 0.86, y = 0.28) #plot area for the inset map
# print(hkd,vp=v1)
# print(p,vp=v2)
