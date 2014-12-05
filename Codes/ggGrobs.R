
# names(clipper.l)  <- c("A", "B", "C", "D")
# ggplot(clipper.df,aes(x,y, fill = tCenter)) + geom_raster() +
# facet_wrap(~ id)



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
