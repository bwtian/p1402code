source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd("~/Share/iData/")
arc  <- "arcs.kmz" 
ogrListLayers(arc)
sldf <- readOGR(arc,  "arcs")
plot(sldf)
jpTlines  <- sldf
ge.sp2shpGeo(jpTlines)
