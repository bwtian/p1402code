source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
setwd(dir.hkd)
ge.raster2df  <- function(rst){
        rst  <- raster(rst)
        rst.spdf  <- rasterToPoints(rst, spatial=TRUE)
        rst.df  <- as.data.frame(rst.spdf)
}
# lulc.df  <- ge.raster2df("hkdBigLULCver1402Merge.tif")
# lst.df  <- ge.raster2df("hkdL8B10CenterMos.tif")
# hkdKT  <- readRDS("hkd_kt3dlcc_140530_114352.Rds")
# hkdKT$t <- 10^(hkdKT$KT)
sst.df  <- hkdKT[hkdKT$Z == 1400,]

# summary(hkdKT)
# hkdSST1500  <- hkdKT[hkdKT$Z == 1500,]

d  <- as.data.frame(rbind(c(41.91, 140.87),
                          c(42.23, 139.94),
                          c(42.816, 141.299),
                          c(43.485, 144.159)))
names(d)  <- c("lat", "lon")
dlcc  <- ge.crsTransform(d, lon, lat, xlcc, ylcc, wgs84GRS,lccWgs84)
rad  <- 5000
dlcc$xmin  <- round(dlcc$xlcc, -3) -rad
dlcc$xmax  <- round(dlcc$xlcc, -3) +rad
dlcc$ymin  <- round(dlcc$ylcc, -3) -rad
dlcc$ymax  <- round(dlcc$ylcc, -3) +rad
dlcc$id  <- 1:nrow(dlcc)
sub  <- dlcc
# head(sub)
ge.subdf  <- function(df,x,y,sub){
        # return a list of
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
ge.crsTransform
# head(sst.df)
head(sst.df)
sst.clip.l <- ge.subdf(sst.df, X,Y, sub)
head(sst.clip.l[[1]])
# lst.clip.l <- ge.subdf(lst.df, x,y,sub)
# # head(lulc.df)
# lulc.clip.l <- ge.subdf(lulc.df,x,y,sub)

# names(clipper.l)  <- c("A", "B", "C", "D")
# ggplot(clipper.df,aes(x,y, fill = tCenter)) + geom_raster() +
# facet_wrap(~ id)


# summary(lst.clip.l[[1]])
cols = oceColorsJet(10)
lst.col.brks  <- seq(-20, 20, 2)
lst.col.labs  <- as.character(lst.col.brks)
lst.name  <- expression(~(degree*C))
lst.grobs  <- lapply(lst.clip.l, function(df) {
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
                theme_bw(base_size = 10, base_family = "Times") #+
                #theme(legend.position="left",legend.justification = "right")
        })
summary(sst.clip.l[[1]])
sst.col.brks  <- seq(0, 400, 10)
sst.col.labs  <- as.character(sst.col.brks)
sst.name  <- expression(~(degree*C))
sst.grobs  <- lapply(sst.clip.l, function(df) {
        ggplot(df) +
                geom_raster(aes(X,Y, fill = t)) +
                scale_x_continuous(labels = function(x) x/1000 -1200) +
                scale_y_continuous(labels = function(x) x/1000 -1400) +
                xlab("") +
                ylab("") +
                scale_fill_gradientn(colours = cols,
                                     na.value="white",
                                     breaks = sst.col.brks,
                                     labels = sst.col.labs,
                                     name = sst.name) +
                coord_equal() +
                theme_bw(base_size = 10, base_family = "Times") #+
        #theme(legend.position="left",legend.justification = "right")
})

sst.grobs
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
#
# grid.draw(sst.col)
# sst.col$widths  <- lst.col$widths
# sst.col$heights  <- lst.col$heights
# sst.col$
# grid.draw(cbind(lst.col,sst.col))
grid.arrange(lst.col,sst.col ,ncol = 2)
