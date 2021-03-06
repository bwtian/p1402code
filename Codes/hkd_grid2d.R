#' Make 2D 100m * 100m Grid with lccWgs84 CRS
#'
source("./tirSettings.R")
## BBOX Raster
# P  <- Polygon(cbind(c(1152500, 1152500, 1673500, 1673500, 1152500),
#                     c(1372500, 1843500, 1843500, 1372500, 1372500)))
P  <- Polygon(cbind(c(1152550, 1152550, 1673550, 1673550, 1152550),
                    c(1372550, 1843550, 1843550, 1372550, 1372550)))
Ps  <- Polygons(list(P), "P")
SpP  <- SpatialPolygons(list(Ps))
bbgrid1h  <- spsample(SpP,type = "regular", cellsize = c(100,100),
                      offset = c(0.5, 0.5))
proj4string(bbgrid1h) <- CRS(lccWgs84)
bbraster1h  <- rasterFromXYZ(bbgrid1h)
bbraster1h[]  <- 1: ncell(bbraster1h)
proj4string(bbraster1h) <- CRS(lccWgs84)
proj4string(bbraster1h)

# plot(bbraster1h)
# saveRDS(bbraster1h, file = "~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
hkd1h  <- readRDS("~/SparkleShare/TIR/hkdbb_grdi2d1h.Rds")
proj4string(hkd1h)
## Hokkaido Shape Raster

shp  <- "~/Dropbox/2data/dataRaw/japan_ver71/HokkaidoUnion_lccWgs84.shp"
str(hkdshp)
hkdshp  <- readShapePoly(shp)
plot(hkdshpproj4string(hkdshp) <- CRS(lccWgs84)
hkdshpb  <- ge.LargestPolys(hkdshp, Polygon = T)
str(hkdshpb)
hkdBigLccWgs84  <- hkdshpb
ge.sp2shpPrj(hkdBigLccWgs84)
getwd()
hkdmaskb  <- mask(hkd1h, hkdshpb)
saveRDS(hkdmaskb, file = "~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
hkdmaskb  <- readRDS("~/SparkleShare/TIR/hkdmskb_grdi2d1h.Rds")
plot(hkdmaskb)
plot(hkdshp, add = T)
