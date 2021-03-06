source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dir.hkd)
ge.raster2df  <- function(rst){
        rst  <- raster(rst)
        rst.spdf  <- rasterToPoints(rst, spatial=TRUE)
        rst.df  <- as.data.frame(rst.spdf)
}
# lulc.df  <- ge.raster2df("hkdBigLULCver1402Merge.tif")
# lulc.df2  <- ge.crsTransform(lulc.df, x, y, xlcc,ylcc,wgs84GRS,lccWgs84)
# lulc.rst  <- raster("hkdBigLULCver1402Merge.tif")
# levelplot(lulc.rst)
# lst.rst  <- raster("hkdL8B10CenterMos.tif")
# lulc.rsp <- projectRaster(lulc.rst, lst.rst,method="ngb")
# writeRaster(lulc.rsp, "lulc100.tif")
### Start herr
# hkdKT  <- readRDS("hkd_kt3dlcc_140530_114352.Rds")
# hkdKT$t <- 10^(hkdKT$KT)
# hkdXyzt  <- hkdKT[,c(1:3,9)]
# names(hkdXyzt)  <- c("x","y","z","t")
# #
# lulc.df  <- ge.raster2df("lulc100.tif")
# lst.df  <- ge.raster2df("hkdL8B10CenterMos.tif")
# sst.df  <- hkdXyzt[hkdXyzt$z == 1500,]
# hkdFault.sldf  <- readRDS("hkdFault.sldf_141126_221926.Rds")
# ft.df  <- fortify(hkdFault.sldf)
# ft.lcc  <- ge.crsTransform(ft.df, long, lat, x, y, wgs84GRS,lccWgs84)

# summary(sst.df)
# summary(hkdKT)
# hkdSST1500  <- hkdKT[hkdKT$Z == 1500,]
# d  <- as.data.frame(rbind(c(41.91, 140.87),
#                           c(42.23, 139.94),
#                           c(42.88, 141.29),
#                           c(43.47, 144.19)))
d  <- as.data.frame(rbind(c(41.91, 140.87),
                          c(42.23, 139.93),
                          #c(42.82, 141.31),
                          c(43.454, 144.158)))
names(d)  <- c("lat", "lon")

dlcc  <- ge.crsTransform(d, lon, lat, xlcc, ylcc, wgs84GRS,lccWgs84)
# dlcc  <- ge.crsTransform(d, lon, lat, xlcc, ylcc, wgs84GRS,lccWgs84)
#dlcc  <- maxids[-4,]
# dlcc$xlcc  <- dlcc$X
# dlcc$ylcc  <- dlcc$Y
# point  <- ge.df2spwgs84(dlcc, lon ,lat)
# ge.sp2shpGeo(point)
points  <- "Onshin/doc.kml"
points.spdf  <- readOGR(points,  "Onshin")
points.df  <- as.data.frame(points.spdf)
points.df$Name
points.lcc  <- ge.crsTransform(d, lon, lat, xlcc, ylcc, wgs84GRS,lccWgs84)
points.lcc$name  <-  c("Epicenter","Usubetsu \n hot spring", "Oakan \n volcano")
points.name  <- points.lcc[order(points.lcc$ylcc),]

### Google map

# type = "terrain"
# nameX=expression(Longitude~(degree*E))
# nameY=expression(Longitude~(degree*N))
# a  <- ggmap(get_map(location = c(lon = 140.87, lat = 41.91),maptype = type, zoom = 13), extent = "panel") +
#         scale_x_continuous(name = nameX) +   scale_y_continuous(name = nameY) +
#         theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0),
#               axis.title.x = element_text(vjust = 0)) + +
#         annotate("point", x=140.87, y=41.91,
#                  color = "white", cex =4) +
#         annotate("text", label=data[i,]$name,
#                  x=data[i,]$lon, y=data$lat,
#                  vjust= -0.1, fontfamily = "times")
#         theme_bw(base_size = 12, base_family = "Times")
#            a
# ggmap(get_map(location = c(lon = 139.93, lat = 42.23),maptype = type, zoom = 13), extent = "panel")
#ggmap(get_map(location = c(lon = 141.31, lat = 42.82),maptype = type, zoom = 13), extent = "panel")
# ggmap(get_map(location = c(lon = 144.158, lat = 43.45),maptype = type, zoom = 13), extent = "panel")

rad  <- 5000
dlcc$xmin  <- round(dlcc$xlcc, -3) -rad
dlcc$xmax  <- round(dlcc$xlcc, -3) +rad
dlcc$ymin  <- round(dlcc$ylcc, -3) -rad
dlcc$ymax  <- round(dlcc$ylcc, -3) +rad
dlcc$id  <- 1:nrow(dlcc)



sub  <- dlcc
# head(sub)
ge.subdf  <- function(df,x,y,sub){
        out.l  <- list() # a list of dataframe
        for (i in 1:nrow(sub)){
                xmin  <- sub[i,]$xmin
                xmax  <- sub[i,]$xmax
                ymin  <- sub[i,]$ymin
                ymax  <- sub[i,]$ymax
                x  <- df$x
                y  <- df$y
                out.l[[i]]  <-  df[x >= xmin & x <= xmax & y  >= ymin & y <= ymax,]
        }
        #         out.spdf  <- do.call(rbind, out.l)
        #         out.df  <- as.data.frame(out.spdf)
        return(out.l)
}
sst.clip.l <- ge.subdf(sst.df, x, y, sub)
#head(sst.clip.l[[1]])
lst.clip.l <- ge.subdf(lst.df, x,y,sub)
#head(lst.clip.l[[1]])
lulc.clip.l <- ge.subdf(lulc.df,x,y,sub)
ft.clip.l  <- ge.subdf(ft.lcc,x,y,sub)
cols = oceColorsJet(10)
lst.col.brks  <- seq(-20, 20, 2)
lst.col.labs  <- as.character(lst.col.brks)
lst.name  <- expression(~(degree*C))

gglst  <- function(df){
        ggplot(df) +
                geom_raster(aes(x,y, fill = hkdL8B10CenterMos)) +
                scale_x_continuous(labels = function(x) x/1000 -1200) +
                scale_y_continuous(labels = function(x) x/1000 -1400) +
                xlab("Easting (km)") +
                ylab("Northing (km)") +
                scale_fill_gradientn(colours = cols,
                                     na.value="white",
                                     breaks = lst.col.brks,
                                     labels = lst.col.labs,
                                     name = lst.name) +
                coord_equal() +
                theme_bw(base_size = 12, base_family = "Times") +
                theme(plot.margin = unit(c(1,0,0.2,0), "lines")) +
                theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0),
                      axis.title.x = element_text(vjust = 0))
}

lst.grobs  <- list()
for (i in 1:length(lst.clip.l)) {
        lst.grobs[[i]]  <-  gglst(lst.clip.l[[i]]) +
                annotate("point", x=points.name[i,]$xlcc, y=points.name[i,]$ylcc,
                         color = "white", cex =4) +
                #geom_point(points.name[i,], aes(x =xlcc, y = ylcc), color = "white", cex =4) +
                annotate("text", label=points.name[i,]$name,
                         x=points.name[i,]$xlcc, y=points.name[i,]$ylcc,
                         vjust= -0.1, fontfamily = "times")
        #                 annotate("text",label=paste("LST", LETTERS[i],sep=":"), x=points.name$xlcc, y=points.name$ylcc, hjust= -0.4, vjust=2,fontface = "bold")
}
# sst.col  <-  cols
# sst.col.brks  <- seq(0, 400, 10)
# sst.col.labs  <- as.character(sst.col.brks)
# sst.name  <- expression(~(degree*C))
# ggsst  <- function(df){
#         ggplot(df) +
#         geom_raster(aes(x, y, fill = t)) +
#                 scale_x_continuous(labels = function(x) x/1000 -1200) +
#                 scale_y_continuous(labels = function(x) x/1000 -1400) +
#                 xlab("") +
#                 ylab("") +
#                 scale_fill_gradientn(colours = sst.col,
#                                      na.value="white",
#                                      breaks = sst.col.brks,
#                                      labels = sst.col.labs,
#                                      name = sst.name) +
#                 coord_equal() +
#                 theme_bw(base_size = 12, base_family = "Times")  +
#
#                 theme(plot.margin = unit(c(0.5,-1,0,-1.5), "lines"))
# }
# sst.grobs  <- list()
# for (i in 1:length(sst.clip.l)) {
#         sst.grobs[[i]]  <-  ggsst(sst.clip.l[[i]]) +
#                 annotate("text",label=paste("SST", LETTERS[i],sep=":"), x=-Inf, y=Inf, hjust=-0.4, vjust=2,fontface = "bold")
# }
lulc.col.brks  <- c(1,2,3,4,5,6,8,10,11)
lulc.col.labs  <- c("Water", "Urban", "Paddy", "Crop","Grass", "DeciduousForest",
                    "EvergreenForest", "Bare", "SnowAndIce")
lulc.cols  <- c("blue", "red", "purple", "yellow", "yellowgreen", "springgreen", "forestgreen", "saddlebrown", "white")
lulc.name  <- "LULC"
gglulc  <- function(df){
        ggplot(df) +
                geom_raster(aes(x,y, fill = factor(lulc100))) +
                scale_x_continuous(labels = function(x) x/1000 -1200) +
                scale_y_continuous(labels = function(x) x/1000 -1400) +
                xlab("Easting (km)") +
                ylab("Northing (km)") +
                scale_fill_manual(values = lulc.cols,
                                  na.value="white",
                                  #breaks = lulc.col.brks,
                                  labels = lulc.col.labs,
                                  name = lulc.name) +
                coord_equal() +
                theme_bw(base_size = 12, base_family = "Times") +
                theme(legend.position="none")  +
                theme(plot.margin = unit(c(1,0,0.2,0), "lines")) +
                theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0),
                      axis.title.x = element_text(vjust = 0))

}
lulc.grobs  <- list()
for (i in 1:length(lulc.clip.l)) {
        lulc.grobs[[i]]  <-  gglulc(lulc.clip.l[[i]]) +
                geom_path(aes(x, y, group=id), data = ft.clip.l[[i]],
                          color = "red", size =1,
                          alpha = 0.9) +
                annotate("point", x=points.name[i,]$xlcc, y=points.name[i,]$ylcc,
                         color = "white", cex =4) +
                #geom_point(points.name[i,], aes(x =xlcc, y = ylcc), color = "white", cex =4) +
                annotate("text", label=points.name[i,]$name,
                         x=points.name[i,]$xlcc, y=points.name[i,]$ylcc,
                         vjust= -0.1, fontfamily = "times")
        #annotate("path", )
}

# grid.newpage()
# grid.draw(rbind(
#         cbind(ggplotGrob(grobs[[1]]), ggplotGrob(grobs[[2]]), size="last"),
#         cbind(ggplotGrob(grobs[[3]]), ggplotGrob(grobs[[4]]), size="last"),
#         size = "last"))
lst.col  <- rbind(ggplotGrob(lst.grobs[[1]]),
                  ggplotGrob(lst.grobs[[2]]),
                  ggplotGrob(lst.grobs[[3]]),
                  size = "last")
#grid.draw(lst.col)
# sst.col  <-rbind(ggplotGrob(sst.grobs[[1]]),
#                  ggplotGrob(sst.grobs[[2]]),
#                  ggplotGrob(sst.grobs[[3]]),
#                  ggplotGrob(sst.grobs[[4]]),
#                  size = "last")
lulc.col  <-rbind(ggplotGrob(lulc.grobs[[1]]),
                  ggplotGrob(lulc.grobs[[2]]),
                  ggplotGrob(lulc.grobs[[3]]),
                  size = "last")



fmt <- function(){
        f <- function(x) as.character(round(x,2))
        f
}
ggterrain  <- function(df){
        type = "terrain"
        zoom = 13
        #nameX=parse(text=paste("Longitude ", "(", "^o ", "*E", sep=""))
        nameX=expression(Longitude~(degree*E))
        #nameY=parse(text=paste("Latitude ", "(","^o ", "*N", sep=""))
        nameY=expression(Latitude~(degree*N))
        limitsX  <- c(df$lon - 0.05, df$lon + 0.05)
        limitsY  <- c(df$lat - 0.05, df$lat + 0.05)
        ggmap(get_map(location = c(lon = df$lon, lat = df$lat),maptype = type, zoom = zoom), extent = "device") +
                xlab(nameX) + ylab(nameY) +  scale_x_continuous(labels = fmt()) +
                theme_bw(base_size = 12, base_family = "Times") +
                theme(plot.margin = unit(c(1,0,0,0), "lines")) +
                theme(axis.text.y = element_text(angle = 90, hjust = 0.5, vjust = 0),
                                             axis.title.x = element_text(vjust = 0))
}
#ggterrain(data[1,])
terrain.grobs  <- list()
data  <- points.name
for (i in 1:length(data)) {
        terrain.grobs[[i]]  <-  ggterrain(data[i,]) +
                annotate("point", x=data[i,]$lon, y=data[i,]$lat,
                         color = "white", cex =4) +
                annotate("text", label=data[i,]$name,
                         x=data[i,]$lon, y=data$lat,
                         vjust= -0.1, fontfamily = "times")
}
# terrain.grobs
terrain.col  <- rbind(ggplotGrob(terrain.grobs[[1]]),
                  ggplotGrob(terrain.grobs[[2]]),
                  ggplotGrob(terrain.grobs[[3]]),
                                   size = "last")

# sst.col$widths  <- lst.col$widths
# lulc.col$widths  <- lst.col$widths
# sst.col$heights  <- lst.col$heights
# lulc.col$heights  <- lst.col$heights
# grid.draw(cbind(lst.col,sst.col))
# pdf("hkdSub.pdf", width = 7)
# grid.arrange(lst.col,lulc.col, sst.col ,ncol = 3)
grid.arrange(lst.col, lulc.col,terrain.col, ncol = 3,
              main = textGrob(c("LST","LULC", "Location"), x = unit(c(0.14,0.51, 0.85), "npc"), y = unit(c(0.12,0.12, 0.12), "npc"),
                             gp=gpar(font=2,fontfamily = "times")),
             left =  textGrob(c("A","B","C"), y = unit(c(0.85,0.51,0.18), "npc"),
                              gp=gpar(font=2,fontfamily = "times")))
# dev.off()
# getwd()
#grid.arrange(lst.col,lulc.col,ncol = 2)

