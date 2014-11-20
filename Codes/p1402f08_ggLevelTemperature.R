source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dataDir)
getwd()
hkdKT  <- readRDS("hkd/hkd_kt3dlcc_140530_114352.Rds")
head(hkdKT)
#df  <- subset(hkdKT, Z %% 200 == 0)
df  <- subset(hkdKT, Z == 100 | Z == 300 | Z == 500 | Z == 700 | Z == 900 | Z == 1000 |
                 Z == 1100 |  Z == 1200 | Z == 1300 | Z == 1400 ) 
g1 <- ggplot(df) +
geom_raster(aes(x = X, y =Y, fill = Temperatrue)) +
facet_wrap(~Z)

g2  <- g1 + scale_x_continuous(label = function(x) x/1000) +
scale_y_continuous(label = function(x) x/1000) +
xlab("Easting (km)") +
ylab("Northing (km)")


y  <- as.numeric(df$Temperatrue)
hist(y)
#breaksY = c(0,100,200,250,300,350,400, max(y))
breaksY = c(0,100,200,250,300,350,400, 515)
labelsY = as.character(breaksY)
g3  <- g2 +  scale_fill_gradientn(name = expression(Temperature~(degree*C)),
                                    colours = rev(rainbow(7)),
                                    breaks = breaksY,
                                    labels = labelsY) +
        theme_bw(base_size = 12, base_family = "Times")
g3


