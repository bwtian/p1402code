## f04
source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
hkdBH  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
hkd100  <- subset(hkdBH, (Depths >=100 ))
hkdxyz  <-  unique(hkdBH[,c(1:3,13)])
#plot(hkd100$Depths, hkd100$Temperature, type = "p", pch = 20)
lm_eqn = function(m) {
        l <- list(a = format(coef(m)[1], digits = 3),
                  b = format(abs(coef(m)[2]), digits = 3),
                  r2 = format(summary(m)$r.squared, digits = 3));
        #eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, l)
        if (coef(m)[2] >= 0)  {
                eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        } else {
                eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
        }
        as.character(as.expression(eq))
}
r2label = lm_eqn(lm(Temperature ~ Depths, data =  hkd100))


namesY = "Depth (m)"
breaksY = c(-100, -500, -1000, -1500, -2000, -2200)
labelsY = as.character(breaksY)

namesX = expression(Temperature~(degree*C))
breaksX = c(0,15,50,90,150,200, 250, 300)
labelsX = as.character(breaksX)
colorsX =  rev(rainbow(7))


hkdBH  <-
        ggplot(data = hkd100, aes(y= -Depths, x = Temperature)) +
        geom_point(aes(color = Temperature), shape = ".") + geom_smooth() +
        #geom_smooth(color = "blue") +
        stat_smooth(method = "lm", color = "red")
        #stat_smooth(method="lm", colour = "red",se=FALSE)  # show lm
        annotate("text", x = 100, y = -2000, label = r2label, colour="red",
                 size = 4, parse=TRUE, font = "Times") +
        geom_vline(xintercept = 15, linetype = 2,color = "green") +
        geom_vline(xintercept = 100, linetype = 3,color = "blue") +
        scale_x_continuous(name = namesX,
                           breaks = breaksX,
                           labels = labelsX) +
        scale_y_continuous(name = namesY,
                           breaks = breaksY,
                           labels = labelsY) +
        scale_colour_gradientn(name = namesY,
                               colours = colorsY,
                               breaks = breaksY,
                               labels = format(breaksY)) +
        theme_bw(base_size = 12, base_family = "Times") +
        theme(axis.title.x=element_text(vjust = -0.5, hjust =0.5))

hkdBH
# ggsave(plot =hkdBH, "hkdBH.pdf", width = 7, height = 5)
#ge.ggsave(hkdBH)
