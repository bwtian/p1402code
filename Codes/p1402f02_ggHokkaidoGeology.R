### p1402fig2 Geology setting
### boxplot by depth
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dir = "~/Share/iData/Hokkaido/")
hkdFault.sldf  <- readRDS("hkdFault.sldf_141126_221926.Rds")
hkdGeoPoly.SPDF  <- readRDS("hkdGeoPoly.SPDF_141126_222720.Rds")
hkdFault.df  <- fortify(hkdFault.sldf)
