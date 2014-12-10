library(plyr)
library(ggplot2)
library(directlabels)
library(reshape)
library(reshape2)
volcano<-melt(volcano)
v<-ggplot(volcano, aes(x=X1,y=X2,z=value))  # specify the mapping properly
e
e<-v + stat_contour(aes(colour=..level..), breaks=c(160, 170, 180))
f  <- direct.label(e)
class(f)

p <- ggplot(volcano, aes(x, y, z = z)) +
        geom_contour()

tmp <- ggplot_build(p)$data[[1]]

p + geom_text(aes(label=level, z=NULL), data=subset(tmp, x==50))


ggplot(d, aes(x, y)) +
        stat_density2d(aes(alpha=..level.., fill=..level.., weight=Heat.Flow), size=2,
                       bins=10, geom="polygon") +
        scale_fill_gradient(low = "yellow", high = "red")
+
        scale_alpha(range = c(0.00, 0.5), guide = FALSE) +
        geom_density2d(colour="black", bins=10) +
        geom_point(data = dataset) +
        guides(alpha=FALSE) + xlim(c(10, 160)) + ylim(c(120, 280))
contour(d$x, d$y, d$Heat.Flow)
