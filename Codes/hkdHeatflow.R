source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dir.hkd)
heatflow.spdf  <- readRDS("flow.spdf_141209_224154.Rds")
heatflow.lcc  <- spTransform(heatflow.spdf, CRS(lccWgs84))
### Clip
hkdBigPoly  <- readRDS("hkdBigLccWgs84_141210_113600.Rds")
proj4string(hkdBigPoly)  <- CRS(lccWgs84)
hkdHeatflow  <- crop(heatflow.lcc, hkdBigPoly)
hkdHeatflow.lcc  <- hkdHeatflow
#ge.sp2shpPrj(hkdHeatflow.lcc)
plot(hkdHeatflow)
