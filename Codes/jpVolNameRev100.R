v1  <-  read.csv2("~/Dropbox/2data/data/jpVol110_spdf_140704_150719.csv")
v1$ID  <- 1:110
url2  <- "~/wget/gbank.gsj.jp/volcano/Act_Vol/activev.html"
d  <- phd.url.table(url2,2)
v2  <- d[complete.cases(d),]
names(v2)  <- c("jname", "Name", "r1998", "jma03", "jma09", "Prefecture")
#id2  <- c(100:110,1:99)
#v2$ID  <- id2
v3  <- merge(v1, v2, by = "jname", all =T)
nas   <- v3[!complete.cases(v3),]
nname  <- v2$jname
nname <- gsub("高原","高原山",nname, fixed = T)
nname <- gsub("鳴子火山群","鳴子",nname, fixed = T)
nname <- gsub("鶴見岳","鶴見岳・伽藍岳",nname, fixed = T)
nname <- gsub("指臼山","指臼岳",nname, fixed = T)
nname <- gsub("鳥島","伊豆鳥島",nname, fixed = T, useBytes = T)
nname <- gsub("硫黄伊豆鳥島","硫黄鳥島",nname, fixed = T, useBytes = T)
v2$jname  <- nname
#v2[grep("鳥島",v2$jname),]
v4  <- merge(v1, v2, by.x = "jname", all =T)
names(v4)
v5  <- v4[,c("ID", "Name", "jname", "yomi", "Prefecture", "londms", "latdms","lon", "lat", "elve",   "r1998", "jma03", "jma09")]
v6  <- v5[order(v4$ID),]
names(v6)  <- c("ID", "Name", "Jname", "Yomi", "Prefecture","Longitude", "Latitude",  "Lon", "Lat","Elevation", "V1998", "JMA03", "JMA09")
# jpVol110 <- v6 
# setwd(dataDir)
phd.write.csv(jpVol110)
v7  <- v6[,c("ID", "Name", "Jname", "Longitude", "Latitude", "Elevation", "JMA03", "JMA09")]
library(xtable)
print(xtable(v7),sanitize.text.function=NULL)
