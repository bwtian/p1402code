hkdBH  <- readRDS("~/Dropbox/2data/dataProduct/hkd/hkd_profiles_140806_164333.Rds")
hkd11k  <- subset(hkdBH, (Depths >=100) & (Depths <=1100))
plot(hkdBH$Depths, hkdBH$Temperature_l)
boxplot(hkd11k$Temperature~hkd11k$ID)
hist(hkd11k$Temperature_l)
hkd11k  <- hkdBH
temp  <- by(hkd11k$Temperature, hkd11k$ID, function(x) (max(x)-min(x)))
depths  <- by(hkd11k$Depths, hkd11k$ID, function(x) (max(x)-min(x)))

grad  <- (temp/depths)*1000
summary(grad)
table(grad)
grad1  <- grad[grad>50]
length(grad1)
# most of boreholes are located in convection area
