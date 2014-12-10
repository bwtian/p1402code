source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dir.hkd)
hkdKT  <- readRDS("hkd_kt3dlcc_140530_114352.Rds")
hkdKT$t <- 10^(hkdKT$KT)
# summary(hkdKT)
# hkdSST1500  <- hkdKT[hkdKT$Z == 1500,]
#
# ge.df2spLccWgs84  <-
# function(df, x, y, crs) {
#         # spatialize a data frame to spdf
#         d  <- df
#         d$x  <- d[,which(colnames(d) == as.character(substitute(x)))]
#         d$y  <- d[,which(colnames(d) == as.character(substitute(y)))]
#         crs <- "+proj=lcc +lat_1=32.8 +lat_2=43.2 +lat_0=38 +lon_0=137.5 +x_0=1000000 +y_0=1000000 +datum=WGS84 +units=m +no_defs"
#         coords  <- d[, c("x","y")]
#         m  <- as.matrix(coords) #sp need numeric matrix
#         mode(m)  <- "numeric"
#         sp  <- sp::SpatialPoints(m, proj4string = sp::CRS(crs))
#         spdf <- sp::SpatialPointsDataFrame(m, data = d, proj4string=sp::CRS(crs))
#         return(spdf)
# }
# hkdSST1500.spdf  <- ge.df2spLccWgs84(hkdSST1500, X,Y)
# ge.sp2shpPrj(hkdSST1500.spdf)
# getwd()
#df  <- subset(hkdKT, Z %% 200 == 0)

facet_labels <- function(variable, value) {
        labels <- as.character(value)
        labnew  <- paste("Depth", labels, "m")

        return (labnew)
}

df  <- subset(hkdKT, Z == 100 | Z == 300 | Z == 500 | Z == 700 | Z == 900 |
                 Z == 1100 |  Z == 1300 | Z == 1500 )
df$ZZ  <- facet_labels("Z", df$Z)
class(df$ZZ)
df$ZZ  <- factor(df$ZZ, levels = c("Depth 100 m", "Depth 300 m", "Depth 500 m",  "Depth 700 m",
                                   "Depth 900 m","Depth 1100 m", "Depth 1300 m", "Depth 1500 m"))

g1 <- ggplot(df) +
        geom_raster(aes(x = X, y =Y, fill = t)) +
        facet_wrap(~ZZ, ncol =2)
#g1
g2  <- g1 + scale_x_continuous(label = function(x) x/1000 -1200) +
scale_y_continuous(label = function(x) x/1000 -1400) +
xlab("Easting (km)") +
ylab("Northing (km)")
#g2
# y  <- as.numeric(df$Temperatrue)
# max(y)
#breaksY = c(0,100,200,250,300,350,400, max(y))
# breaksY = c(0,100,150,200,250,300,350,400,450,515)
breaksY = c(0,50,100,150,200,250,300,350)
labelsY = as.character(breaksY)
#cols  <- oceColorsJet(255)
cols  <- ge.rainbow(255)
#paste0(parse(text=paste("Temperature ", "^o ", "*C", sep=""))
bold.text <- element_text(face = "bold", color = "black")
strip.text  <- element_text(face = "bold", color = "black")
g3  <- g2 +  scale_fill_gradientn(name = expression(Temperature~(degree*C)),
                                    colours = cols,
                                    breaks = breaksY,
                                    labels = labelsY) +
        coord_equal() +
        theme_bw(base_size = 12, base_family = "Times") +
        theme(title = bold.text,
              strip.text = strip.text)
#g3
####
jpVolA.spdf  <- readRDS("~/Dropbox/2data/dataProduct/jpVolcanoes/jpVol110_140812_174525.Rds")
xmin <- 139
xmax <- 146
ymin <- 41.4
ymax <- 45.8
bbox.SPDF <- ge.xy2bboxSPDF(xmin,xmax,ymin,ymax,wgs84GRS)
proj4string(jpVolA.spdf) <- proj4string(bbox.SPDF)
volA <- jpVolA.spdf[bbox.SPDF,]
volAl  <- spTransform(volA, CRS(lccWgs84))

volAl.df  <- data.frame(coordinates(volAl))
ggVol  <- g3  +
  geom_point(data = volAl.df,
             aes(as.numeric(lon), as.numeric(lat), alpha=factor(0)),
             shape = 17, alpha = 0.3)  +
  scale_alpha_manual(name =  "Volcanoes",
                     values = c(0.3,0.5), labels = c("Active volcanoes"))
ggVol
jpTlines.sldf  <- readRDS("~/Dropbox/2data/dataProduct/jp/jpTlines_141125_221917.Rds")
hkdTlines.sldf  <- crop(jpTlines.sldf, bbox.SPDF)
hkdTlines.sldfl  <- spTransform(hkdTlines.sldf, CRS(lccWgs84))
#plot(hkdTlines.sldf)
hkdTlines.df  <- fortify(hkdTlines.sldfl)
## regroup
hkdTlines.df$id2 <- 2
hkdTlines.df[hkdTlines.df$id == 1,]$id2 <- 1
hkdTlines.df[hkdTlines.df$id == 3,]$id2 <- 1
ggTlines  <-ggVol + geom_line(aes(long,lat,group=group, linetype=factor(id2)),
                              color = "red",
                              #linetype = 2,
                              size = 1,
                              alpha = 0.5,
                              hkdTlines.df) +
  scale_linetype_manual(name =  "Tectonic lines", values = c(1,2),
                        labels = c("Tectonic lines","Volcanic front"))

### Circles
D15  <- hkdKT[hkdKT$Z == 1500,]
class(hkdKT$Z)
A  <- D15[D15$Temperatrue > 300,]
A$class  <- 1
A[A$X > 1400000 & A$X < 1500000,]$class  <- 2
A[A$X > 1500000,]$class  <- 3
centroids <- aggregate(cbind(X,Y)~class,A, mean)
maxT <- aggregate(cbind(Temperatrue)~class,A, max)
maxids  <- A[A$Temperatrue %in% maxT$Temperatrue,]
maxids
#ggplot(maxids, aes(X,Y)) +  geom_point(size=50, shape=1, color="gold4")
ggCirles  <- ggTlines + geom_point(data =maxids, aes(X,Y),size=6, shape=1, color="white")

hkdHeatflow  <- readRDS("hkdHeatflow.lcc_141210_114009.Rds")
hkdHeatflow.df  <- as.data.frame(hkdHeatflow)
hkdHeatflow.df$ZZ  <- factor("Depth 1500 m", levels = c("Depth 100 m", "Depth 300 m", "Depth 500 m",  "Depth 700 m",
                                                        "Depth 900 m","Depth 1100 m", "Depth 1300 m", "Depth 1500 m"))
summary(hkdHeatflow.df)
# hkdHeatflow.d  <- hkdHeatflow.df[order(hkdHeatflow.df$x, hkdHeatflow.df$y),]
# d <- with(hkdHeatflow.df, hkdHeatflow.df[rep(1:nrow(hkdHeatflow.df), Heat.Flow),])
breaksH  <- seq(0,300,50)
labelsH  <- as.character(breaksH)
ggHeatflow  <-
        ggCirles +
        geom_point(data =hkdHeatflow.df, aes(x, y, colour = Heat.Flow),  shape = 21) +
        stat_density2d(data = hkdHeatflow.df, aes(x, y, z = Heat.Flow, weight=Heat.Flow),color = "gold") +
        scale_colour_continuous(name = expression("Heat flow"~(mW/m^2)),
                           #low="orange", high="red",
                           breaks = breaksH,
                           labels = labelsH)
scale_
#ggplot(data = hkdHeatflow.df, aes(x, y, z = Heat.Flow))+
        #stat_density2d(data = hkdHeatflow.df, aes(x, y, z = Heat.Flow,alpha=..level.., fill=..level.., weight=Heat.Flow), size=2)

        #aes(colour=..level..), breaks=c(160, 170, 180)
        #geom_contour(data = hkdHeatflow.df, aes(x, y, z = as.numeric(Heat.Flow)))
        #geom_density2d(data = hkdHeatflow.df, aes(x, y),col )

hkd3D  <-  ggHeatflow
hkd3D
# library(directlabels)
# direct.label(hkd3D)
# ge.ggsave(hkd3D)
#ggsave(plot = hkd3D, "hkd3D.pdf", width =7, height = 9)
# getwd()

