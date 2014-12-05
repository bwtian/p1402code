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

# hkdKT  <- readRDS("hkd_kt3dlcc_140530_114352.Rds")
# hkdKT$t <- 10^(hkdKT$KT)
# hkdXyzt  <- hkdKT[,c(1:3,9)]
# names(hkdXyzt)  <- c("x","y","z","t")

# lulc.df  <- ge.raster2df("lulc100.tif")
# lst.df  <- ge.raster2df("hkdL8B10CenterMos.tif")
# sst.df  <- hkdXyzt[hkdXyzt$z == 1500,]
summary(sst.df)
# summary(hkdKT)
# hkdSST1500  <- hkdKT[hkdKT$Z == 1500,]
d  <- as.data.frame(rbind(c(41.91, 140.87),
                          c(42.23, 139.94),
                          c(42.816, 141.299),
                          c(43.485, 144.159)))
names(d)  <- c("lat", "lon")
dlcc  <- ge.crsTransform(d, lon, lat, xlcc, ylcc, wgs84GRS,lccWgs84)
rad  <- 3000
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
# sst.clip.l <- ge.subdf(sst.df, x, y, sub)
# #head(sst.clip.l[[1]])
# lst.clip.l <- ge.subdf(lst.df, x,y,sub)
# #head(lst.clip.l[[1]])
# lulc.clip.l <- ge.subdf(lulc.df,x,y,sub)



# summary(lst.clip.l[[1]])
cols = oceColorsJet(10)
lst.col.brks  <- seq(-20, 20, 2)
lst.col.labs  <- as.character(lst.col.brks)
lst.name  <- expression(~(degree*C))
names(lst.clip.l) <- c("A","B","C","D")
class(lst.clip.l["A"])
dimnames(lst.clip.l[[2]])
lst.grobs  <- lapply(seq_along(lst.clip.l), function(i) {
        df  <- lst.clip.l[[i]]
        ggplot(df) +
                        geom_raster(aes(x,y, fill = hkdL8B10CenterMos)) +
                        scale_x_continuous(labels = function(x) x/1000 -1200) +
                        scale_y_continuous(labels = function(x) x/1000 -1400) +
                        xlab("") +
                        ylab("") +
                        scale_fill_gradientn(colours = cols,
                                             na.value="white",
                                             breaks = lst.col.brks,
                                             labels = lst.col.labs,
                                             name = lst.name) +
                        coord_equal() +
                        theme_bw(base_size = 12, base_family = "Times") +
                        theme(plot.margin = unit(c(0,-0.5,0,0), "lines")) +
                        geom_text(data=df, aes(label=paste("LST", 1:4),
                          x=-Inf, y=Inf, hjust=-0.4, vjust=2, col = "red",fontface = "bold")
#                         annotate("text", x = -Inf, y = Inf, label ="LST",
#                                  hjust=-0.4, vjust=2, col="black", cex=6,
#                                  fontface = "bold")
        })


sst.col  <-  cols
sst.col.brks  <- seq(0, 400, 5)
sst.col.labs  <- as.character(sst.col.brks)
sst.name  <- expression(~(degree*C))
sst.grobs  <- lapply(sst.clip.l, function(df) {
        ggplot(df) +
                geom_raster(aes(x,y, fill = t)) +
                scale_x_continuous(labels = function(x) x/1000 -1200) +
                scale_y_continuous(labels = function(x) x/1000 -1400) +
                xlab("") +
                ylab("") +
                scale_fill_gradientn(colours = sst.col,
                                     na.value="white",
                                     breaks = sst.col.brks,
                                     labels = sst.col.labs,
                                     name = sst.name) +
                coord_equal() +
                theme_bw(base_size = 12, base_family = "Times")  +
                theme(plot.margin = unit(c(0,0,0,-0.5), "lines")) +
                annotate("text", x = -Inf, y = Inf, label = "SST",
                         hjust=-0.4, vjust=2, col="black", cex=6,
                         fontface = "bold")

        #theme(legend.position="left",legend.justification = "right")
})
lulc.col.brks  <- c(1,2,3,4,5,6,8,10,11)
lulc.col.labs  <- c("Water", "Urban", "Paddy", "Crop","Grass", "DeciduousForest",
           "EvergreenForest", "Bare", "SnowAndIce")
lulc.cols  <- c("blue", "red", "purple", "yellow", "yellowgreen", "springgreen", "forestgreen", "saddlebrown", "white")
lulc.name  <- "LULC"
lulc.grobs  <- lapply(lulc.clip.l, function(df) {
        ggplot(df) +
                geom_raster(aes(x,y, fill = factor(lulc100))) +
                scale_x_continuous(labels = function(x) x/1000 -1200) +
                scale_y_continuous(labels = function(x) x/1000 -1400) +
                xlab("") +
                ylab("") +
                scale_fill_manual(values = lulc.cols,
                                     na.value="white",
                                     #breaks = lulc.col.brks,
                                     labels = lulc.col.labs,
                                     name = lulc.name) +
                coord_equal() +
                theme_bw(base_size = 12, base_family = "Times") +
                theme(legend.position="none")  +
                theme(plot.margin = unit(c(0,-0.5,0,-0.5), "lines")) +
                annotate("text", x = -Inf, y = Inf, label = "LULC",
                         hjust=-0.4, vjust=2, col="black", cex=6,
                         fontface = "bold")
})

### Better
library(gridExtra)
grid.newpage()
# grid.draw(rbind(
#         cbind(ggplotGrob(grobs[[1]]), ggplotGrob(grobs[[2]]), size="last"),
#         cbind(ggplotGrob(grobs[[3]]), ggplotGrob(grobs[[4]]), size="last"),
#         size = "last"))
lst.col  <- rbind(ggplotGrob(lst.grobs[[1]]),
                   ggplotGrob(lst.grobs[[2]]),
                   ggplotGrob(lst.grobs[[3]]),
                   ggplotGrob(lst.grobs[[4]]),
                   size = "last")
# grid.draw(lst.col)
sst.col  <-rbind(ggplotGrob(sst.grobs[[1]]),
                ggplotGrob(sst.grobs[[2]]),
                ggplotGrob(sst.grobs[[3]]),
                ggplotGrob(sst.grobs[[4]]),
                size = "last")
lulc.col  <-rbind(ggplotGrob(lulc.grobs[[1]]),
                 ggplotGrob(lulc.grobs[[2]]),
                 ggplotGrob(lulc.grobs[[3]]),
                 ggplotGrob(lulc.grobs[[4]]),
                 size = "last")


# sst.col$widths  <- lst.col$widths
# lulc.col$widths  <- lst.col$widths
# sst.col$heights  <- lst.col$heights
# lulc.col$heights  <- lst.col$heights
# grid.draw(cbind(lst.col,sst.col))
grid.arrange(lst.col,lulc.col, sst.col ,ncol = 3)

#grid.arrange(lst.col,lulc.col,ncol = 2)

