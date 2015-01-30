source("~/SparkleShare/Rprofile/R/Rsettings/phdRsettings.R")
bhdir  <- "~/Dropbox/2data/dataRaw/boreholes/" #1250
setwd(bhdir)
hkdBH  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
hkd100  <- subset(hkdBH, (Depths >=100 ))
hkdxyz  <-  unique(hkdBH[,c(1:3,13)])
bhok.df  <- hkd100
raw.df  <- bhok.df
id  <- raw.df$ID
x  <- as.numeric(raw.df$Depths)
y  <- as.numeric(raw.df$Temperature)
td.df  <- as.data.frame(x = tapply(x, id, max))
td.df$ID  <- rownames(td.df)
colnames(td.df)   <- c("TD","ID")
d  <- td.df
data   <- as.numeric(d$TD)
count  <- nrow(d)
per  <- quantile(data, probs = seq(0, 1, 0.25))
names(per)  <- NULL
IQ25  <- per[2]
IQ75  <- per[4]
IQR  <- IQR(data)
min  <- min(data)
max  <- max(data)
median  <- median(data)
bmin  <- per[2] - 1.5*IQR(data)
bmax  <- per[4] + 1.5*IQR(data)
mean  <- mean(data)
sd  <- sd(data)
sdlow  <- mean(data) - sd(data)
sdup  <- mean(data) + sd(data)
skewness  <- mean((data-mean(data))^3)/(sd(data)^3)
kurtosis  <- mean((x-mean(x))^4)/(sd(x)^4)
#breaksX = c(0, min, IQ25, median, IQ75, bmax, seq(3000, 6000, 500),max)
#labelsX = as.character(round(breaksY,-1))
breaksX = seq(200,2200,200)
labelsX = as.character(breaksX)
limitsX = c(200,2200)
breaksY  <- seq(0,75,15)
labelsY  <- as.character(breaksY)
limitsY  <- c(0,75)
labelSummary  <- paste(
        paste0("Count of boreholes: ", round(count, 1)),
        paste0("Minumum Depth (m): ",round(min, 1)),
        paste0("Mean Depth (m): ",round(mean, 1)),
        paste0("1st Quartile (m): ",round(IQ25, 1)),
        paste0("Median Depth (m): ", round(median, 1)),
        paste0("3rd Quartile (m): ",round(IQ75, 1)),
        paste0("Maxumum Depth (m): ",round(max, 1)),
        #paste0("Interquartile Range (m): ",round(IQR, 1)),
        paste0("St. Dev.: ", round(sd, 1)),
        paste0("Skewness: ",round(skewness, 1)),
        paste0("Kurtosis: ",round(kurtosis, 1)),
        sep = "\n")

boxp  <- function(){
        ggplot(d,aes(factor(0),data)) +
        geom_boxplot(fill = "green",outlier.colour = "red") +
        geom_jitter(shape = ".", size  = 1) +
        #geom_boxplot(fill = "green", outlier.size = 0 )
        #geom_jitter(shape = ".", size  = 1) +
        stat_summary(fun.y="mean",geom="point",color="red", shape = 4) +
        #ylab("Total depth of each borehole (m)") +
        #geom_hline(yintercept = sdlow, linetype = 3,color = "blue") +
        #geom_hline(yintercept = mean, linetype = 4,color = "red") +
        #geom_hline(yintercept = sdup, linetype = 3,color = "blue") +
        scale_y_continuous(breaks = breaksX, labels = labelsX,
                           limits = limitsX) +

        #xlab("") + ylab("Boxplot of total depth") +
        xlab("") + ylab("") +
        theme_bw(base_size = 12, base_family = "Times") +
        theme(axis.text.y=element_blank(),axis.ticks.y=element_blank(),
              aspect.ratio = 1/8) +
        coord_flip()
}
boxp()

# hist0  <- ggplot(d, aes(TD)) +
#         geom_histogram(aes(y= ..density..),
#                        fill = "green", binwidth = 100, color = "black") +
#         geom_density(alpha=.2,fill="red")
# hist0
hist1  <- ggplot(d, aes(TD)) +
        geom_histogram(aes(y= ..count..),
                       fill = "green", binwidth = 100, color = "black") +
        geom_density(aes(y = 100*..count..),alpha=.2,fill="red")
hist2  <- hist1 +  annotate("text", x = Inf, y = Inf,
                         label = labelSummary, colour="blue",
                         size = 4,  hjust=1, vjust = 1,font = "Times") +
         xlab("Depth of boreholes (m)") +
         ylab("Frequency") +
         scale_x_continuous(breaks = breaksX, labels = labelsX,
                            limits = limitsX) +
         scale_y_continuous(breaks = breaksY, labels = labelsY,
                            limits = limitsY)+
         geom_vline(xintercept = median, linetype = 4,color = "red") +
         geom_vline(xintercept = IQ25, linetype = 3,color = "blue") +
         geom_vline(xintercept = IQ75, linetype = 3,color = "blue") +
         theme_bw(base_size = 12, base_family = "Times")
p1  <- boxp()
# + theme(
#         axis.text.x  = element_blank(),
#         axis.title.x = element_blank(),
#         axis.ticks.x = element_blank()
# )
p2  <- hist2
ggsave(plot = p1, "boxTD.pdf", width =7, height = 1.5)
ggsave(plot = p2, "histTD.pdf", width =7, height = 3.5)
getwd()
grid.arrange(p1,p2)
# png(
#         "test.png",
#         width     = 3.25,
#         height    = 3.25,
#         units     = "in",
#         res       = 96,
#         pointsize = 2
# )
# 3.27*300*
# pdf("test2.pdf", h = 7 , w = 7, pointsize = 3, fonts = "Times")
# grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size="last"))
# dev.off()
# png("test2.png", 640, 640, res = 96)
# grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size="last"))
# dev.off()
# grid.draw(rbind(ggplotGrob(dt_p),ggplotGrob(dh_p),ggplotGrob(dslp_p),ggplotGrob(dws_p),ggplotGrob(dwd_p),ggplotGrob(dhi_p),size="first"))
#
# dev.off()
# grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size="last"))
# grid.draw(rbind(ggplotGrob(p1), ggplotGrob(p2), size="max"))
# # no alignment (plots are centered)
# grid.arrange(set_panel_size(p1, h=unit(0.3,"npc"),w=unit(0.8,"npc")),
#              set_panel_size(p2, h=unit(0.8,"npc"),w=unit(0.8,"npc")))
#
# # with alignment
# align_plots(p1, p2, width=unit(0.8,"npc"))
# grid.arrange(p1, p2, ncol =1, heights(1:4))
# plots <- list(hist2, boxp2)
# grobs <- list()
# widths <- list()
# for (i in 1:length(plots)){
#         grobs[[i]] <- ggplotGrob(plots[[i]])
#         widths[[i]] <- grobs[[i]]$widths[2:5]
# }
# maxwidth <- do.call(grid::unit.pmax, widths)
# for (i in 1:length(grobs)){
#         grobs[[i]]$widths[2:5] <- as.list(maxwidth)
# }
# do.call("grid.arrange", c(grobs, ncol=1))
# grid.arrange(heights = c(1, 4), grobs[[2]], grobs[[1]])
