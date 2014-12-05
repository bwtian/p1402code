#' clip a data frame by a xmin, xmax
# dsubd  <- function(data, sub){
#         out  <- list() # a list of dataframe
#         for (i in 1:nrow(sub)){
#                 xmin  <- sub[i,]$xmin
#                 xmax  <- sub[i,]$xmax
#                 ymin  <- sub[i,]$ymin
#                 ymax  <- sub[i,]$ymax
#                 out[[i]]  <-  data[x >= xmin & x <= xmax & y  >= ymin & y <= ymax,]
#         }
#         return(out)
#
# }
source("~/SparkleShare/TIR/demo/tirSettings.R")
setwd(dir.hkd)
#setwd("~/toaTbKlccCenterMos/")
# mos  <- raster("L8B10CenterMos.tif")
# mos.spdf  <- rasterToPoints(mos, spatial=TRUE)
# mos.df  <- as.data.frame(mos.spdf)
# names(mos.df)  <- c("x", "y", "tCenter")
# head(mos.df)

ge.raster2df  <- function(rst){
        rst  <- raster(rst)
        rst.spdf  <- rasterToPoints(rst, spatial=TRUE)
        rst.df  <- as.data.frame(rst.spdf)
}
lulc.df  <- ge.raster2df("hkdBigLULCver1402Merge.tif")
lst.df  <- ge.raster2df("hkdL8B10CenterMos.tif")
hkdKT  <- readRDS("hkd_kt3dlcc_140530_114352.Rds")
hkdKT$t <- 10^(hkdKT$KT)
sst  <- hkdKT[hkdKT$Z == 1500,]
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
head(sub)
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
head(sst.df)
sst.clip.l <- ge.subdf(sst.df, X,Y, sub)
head(lst.df.l)
lst.clip.l <- ge.subdf(lst.df, x,y,sub)
head(lulc.df)
lulc.clip.l <- ge.subdf(lulc.df,x,y,sub)

# names(clipper.l)  <- c("A", "B", "C", "D")
# ggplot(clipper.df,aes(x,y, fill = tCenter)) + geom_raster() +
# facet_wrap(~ id)


summary(lst.clip.l[[1]])
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
                geom_raster(aes(x,y, fill = t)) +
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
grid.draw(lst.col)
sst.col  <-rbind(ggplotGrob(sst.grobs[[1]]),
                ggplotGrob(sst.grobs[[2]]),
                ggplotGrob(sst.grobs[[3]]),
                ggplotGrob(sst.grobs[[4]]),
                size = "last")

grid.draw(sst.col)
# sst.col$widths  <- lst.col$widths
# sst.col$heights  <- lst.col$heights
# sst.col$
# grid.draw(cbind(lst.col,sst.col))
grid.arrange(lst.col,sst.col ,ncol = 2)
# tiff("clipper.tiff", h = 2000, w = 2000, res = 300)
# png("clipper.png")
# do.call(grid.arrange, c(grobs, nrow =2))

# Extracxt the legend from p1 !!!but that is just for p1
# legend = gtable_filter(ggplot_gtable(ggplot_build(grobs[[1]])), "guide-box")
#
# grid.draw(legend)    # Make sure the legend has been extracted
# grid.newpage()
# # Arrange and draw the plot as before
# grid.arrange(arrangeGrob(grobs[[1]] + theme(legend.position="none"),
#                          grobs[[2]] + theme(legend.position="none"),
#                          grobs[[3]] + theme(legend.position="none"),
#                          grobs[[4]] + theme(legend.position="none"),
#                          nrow = 2,
#                          main = textGrob("Main Title", vjust = 1, gp = gpar(fontface = "bold", cex = 1.5)),
#                          left = textGrob("Global Y-axis Label", rot = 90, vjust = 1)),
#              legend,
#              widths=unit.c(unit(1, "npc") - legend$width, legend$width),
#              nrow=1)
#dev.off()
# set.seed(1011)
# x  <- rnorm(100,mean  =50, sd = 25)
# y  <- rnorm(100,mean  =50, sd = 25)
# data  <- as.data.frame(cbind(x,y))
# data
# ### get rectangle map
# sub  <- data.frame(rbind(c(20,60,30,70),
#                          c(80,90,80,90)))
# sub
# names(sub)  <- c("xmin","xmax","ymin","ymax")
# library(ggplot2)
# g1  <- ggplot(data) + geom_point(aes(x =x,y =y)) +
#         geom_rect(data = sub, aes(xmin= xmin, xmax =xmax, ymin = ymin, ymax =ymax),
#                   col ="red", fill = NA)
#
# # subset
# g1 + xlim(80,90) + ylim(80,90)
# g1 + scale_x_continuous(limits = c(80, 90)) + scale_y_continuous(limits = c(80, 90))
# ## zoom out
# g1 + coord_cartesian(xlim = c(80,90), ylim = c(80,90))
# # subdata
# out  <- dsubd(data,sub)
# lapply(out,class)
# library(gridExtra)
# gl  <- lapply(out, function(df){
#         ggplot(df) + geom_point(aes(x =x,y =y))
# })
# do.call(grid.arrange, c(gl, list(ncol = 2)))
#

# Get the widths
gA <- ggplot_gtable(ggplot_build(p1))
gB <- ggplot_gtable(ggplot_build(p2))

# The parts that differs in width
leg1 <- with(gA$grobs[[8]], grobs[[1]]$widths[[4]])
leg2 <- with(gB$grobs[[8]], grobs[[1]]$widths[[4]])

# Set the widths
gA$widths <- gB$widths

# Add an empty column of "abs(diff(widths)) mm" width on the right of
# legend box for gA (the smaller legend box)
gA$grobs[[8]] <- gtable_add_cols(gA$grobs[[8]], unit(abs(diff(c(leg1, leg2))), "mm"))

# Arrange the two charts
grid.newpage()
grid.arrange(gA, gB, nrow = 2)

library(ggplot2)
library(gtable)
library(gridExtra)

# Your plots
p1 <- ggplot(data.frame(x=c("a","b","c"),y=c("happy","sad","ambivalent about life")),aes(x=factor(0),fill=x)) + geom_bar()
p2 <- ggplot(data.frame(x=c("a","b","c"),y=c("happy","sad","ambivalent about life")),aes(x=factor(0),fill=y)) + geom_bar()

# Get the gtables
gA <- ggplot_gtable(ggplot_build(p1))
gB <- ggplot_gtable(ggplot_build(p2))

# Set the widths
gA$widths <- gB$widths

# Arrange the two charts.
# The legend boxes are centered
grid.newpage()
grid.arrange(gA, gB, nrow = 2)
