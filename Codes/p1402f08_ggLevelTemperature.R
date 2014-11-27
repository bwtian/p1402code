source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dataDir)
# getwd()
hkdKT  <- readRDS("hkd/hkd_kt3dlcc_140530_114352.Rds")
head(hkdKT)
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


# y  <- as.numeric(df$Temperatrue)
# max(y)
#breaksY = c(0,100,200,250,300,350,400, max(y))
breaksY = c(0,100,150,200,250,300,350,400,450,515)
labelsY = as.character(breaksY)
cols  <- oceColorsJet(255)
#paste0(parse(text=paste("Temperature ", "^o ", "*C", sep=""))
g3  <- g2 +  scale_fill_gradientn(name = parse(text =paste0("(Temperature",expression(~(degree*C))),
                                    colours = cols,
                                    breaks = breaksY,
                                    labels = labelsY) +
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

ggTlines
threeD  <-ggTlines
ge.ggsave(threeD)
getwd()

