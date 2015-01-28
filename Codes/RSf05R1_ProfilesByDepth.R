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
      eq <- substitute(italic(T) == a + b %.% italic(D)*","~~italic(r)^2~"="~r2,l)
    } else {
      eq <- substitute(italic(T) == a - b %.% italic(D)*","~~italic(r)^2~"="~r2,l)
    }
    as.character(as.expression(eq))
  }
r2label = lm_eqn(lm(Temperature ~ Depths, data =  hkd100))
labelsY = c("100","500","1000","1500","2000","2200")
breaksY = -as.numeric(labelsY)
labelsX = c("0","15","50","90","150","200", "250", "300")
breaksX = as.numeric(labelsX)
hkdBH  <-
  #ggplot(data = hkd100, aes(x= Depths, y = Temperature)) +
  ggplot(data = hkd100, aes(y = -Depths, x = Temperature)) +
  geom_point(aes(color = Temperature), shape = ".") +
  #geom_smooth(color = "blue") +
  stat_smooth(method = "lm", color = "red") +
  annotate("text", x = 300, y = -2000, label = r2label, colour="red",
           size = 4, parse=TRUE, font = "Times") +
  geom_hline(yintercept = 15, linetype = 2,color = "green") +
  geom_hline(yintercept = 100, linetype = 3,color = "blue") +
  scale_y_continuous(name ="Depth (m)", breaks = breaksY, labels = labelsY) +
  scale_x_continuous(name = expression(Temperature~(degree*C)),
                     breaks = breaksX, labels = labelsX) +
  scale_colour_gradientn(name = expression(Temperature~(degree*C)), colours = rev(rainbow(7)),
                         breaks = breaksY, labels = format(breaksY)) +
  theme_bw(base_size = 12, base_family = "Times") +
  theme(axis.title.x=element_text(vjust = -0.5, hjust =0.5))

hkdBH
#ggsave(plot =hkdBH, "hkdBH.pdf", width = 7, height = 5)
#ge.ggsave(hkdBH)
