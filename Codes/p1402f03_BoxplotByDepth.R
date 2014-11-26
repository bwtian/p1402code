hkdBH  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
hkd100  <- subset(hkdBH, (Depths >=100 ))
hkdxyz  <-  unique(hkdBH[,c(1:3,13)])
head(hkd100)
library(ggplot2)
plot(hkd100$Depths, hkd100$Temperature, type = "p", pch = 20)
bh  <- ggplot(data = hkd100, aes(x= Depths, y = Temperature)) +
        geom_point(aes(color = Temperature), shape = ".") +
        #geom_smooth(color = "blue") + 
        stat_smooth(method = "lm", color = "red") +
        annotate("text", x = 1100, y = 3, label = r2label, colour="red", 
                 size = 4, parse=TRUE, font = "times") + 
        xlab("Depth (m)") +
        geom_hline(yintercept = 15, linetype = 2,color = "green") +
        geom_hline(yintercept = 100, linetype = 3,color = "blue") +
        scale_x_continuous(breaks = breaksX, labels = labelsX) +
        scale_y_continuous(breaks = breaksY, labels = labelsY) +
        ylab(expression(Temperature~(degree*C))) +
        scale_colour_gradientn(name = expression(Temperature~(degree*C)), colours = rev(rainbow(7)), breaks = breaksY, labels = format(breaksY)) +
        theme_bw(base_size = 11, base_family = "Times")
setwd(figsDir)
bh
