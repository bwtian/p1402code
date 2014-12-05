source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dataDir)
# getwd()
hkdKT  <- readRDS("hkd/hkd_kt3dlcc_140530_114352.Rds")
summary(hkdKT)
hkdKT$t <- exp(hkdKT$KTb10 + 0.5*hkdKT$KTb10_krig_var)
#df  <- subset(hkdKT, Z %% 200 == 0)
df  <- subset(hkdKT, Z == 100 | Z == 300 | Z == 500 | Z == 700 | Z == 900 |
                 Z == 1100 |  Z == 1200 | Z == 1300 | Z == 1400 )
g1 <- ggplot(df) +
geom_raster(aes(x = X, y =Y, fill = Temperatrue)) +
facet_wrap(~Z)
g2  <- g1 + scale_x_continuous(label = function(x) x/1000) +
scale_y_continuous(label = function(x) x/1000) +
xlab("Easting (km)") +
ylab("Northing (km)")

exp(6)
# y  <- as.numeric(df$Temperatrue)
# max(y)
#breaksY = c(0,100,200,250,300,350,400, max(y))
breaksY = c(0,100,150,200,250,300,350,400,450,515)
labelsY = as.character(breaksY)
cols  <- oceColorsJet(255)
#paste0(parse(text=paste("Temperature ", "^o ", "*C", sep=""))
bold.text <- element_text(face = "bold", color = "black")

g3  <- g2 +  scale_fill_gradientn(name = expression(Temperature~(degree*C)),
                                    colours = cols,
                                    breaks = breaksY,
                                    labels = labelsY) +
        theme(title = bold.text) +
        theme_bw(base_size = 12, base_family = "Times") + coord_equal()

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
             aes(as.numeric(lon), as.numeric(lat), color="red"),
             shape = 17, alpha = 0.3)  +
  scale_color_manual(name =  "Volcanoes",
                     values = c("red"), labels = c("Active volcanoes"))
#ggVol
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

ggCirles  <-
ggTlines + geom_point(data =maxids, aes(X,Y),size=5, shape=1, color="white")

D14  <-subset(hkdKT,  Z == 1400)

A  <- D14[D14$Temperatrue > 300,]
A$class  <- 1
A[A$X > 1400000 & A$X < 1500000,]$class  <- 2
A[A$X > 1500000,]$class  <- 3
centroids <- aggregate(cbind(X,Y)~class,A, mean)
maxT <- aggregate(cbind(Temperatrue)~class,A, max)
maxids  <- A[A$Temperatrue %in% maxT$Temperatrue,]
ggplot(A, aes(X, Y, fill = class)) +
  geom_point() +
  stat_ellipse(geom = "polygon", alpha = 1/2, aes(fill = class)) +
ggplot(maxids, aes(X,Y)) +  geom_point(size=50, shape=1, color="gold4")

circleFun <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}
dat <- circleFun(c(1,-1),2.3,npoints = 100)
ggplot(dat,aes(x,y)) + geom_path()
g<-g+annotate("path",
              x=xc+r*cos(seq(0,2*pi,length.out=100)),
              y=yc+r*sin(seq(0,2*pi,length.out=100)))
veganCovEllipse <- function (cov, center = c(0, 0), scale = 1, npoints = 100)
{
  theta <- (0:npoints) * 2 * pi/npoints
  Circle <- cbind(cos(theta), sin(theta))
  t(center + scale * t(Circle %*% chol(cov)))
}
hkd3D  <-ggCirles
ge.ggsave(hkd3D)
# getwd()

