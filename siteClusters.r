library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(gmt)
library(reshape2)

setwd("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/")

load("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/receiver.location.RData")

receiver.location$Latitude = ifelse(receiver.location$Latitude > 0, 
                                    -receiver.location$Latitude, 
                                    receiver.location$Latitude)

receiver.location.orig = receiver.location
receiver.location.orig$inshore = ifelse(receiver.location.orig$Classification == "inshore",1,0)
# do some joins to get lat and long co-ords of prevsite and nextsite (sitecode)
# into edgelist

receiver.location = receiver.location[,c("SiteCode","Latitude","Longitude")]
names(receiver.location)[1] = "sitecode"

receiver.location = receiver.location[complete.cases(receiver.location),]

# note RECIFE receiver on land
qmplot(Longitude, Latitude, data = receiver.location) +
  geom_point()

allres = data.frame(site1 = NULL,site2 = NULL, s12dist = NULL)
for(i in 1:nrow(receiver.location)){
  for(j in 1:nrow(receiver.location)){
    site1 = receiver.location[i,1]
    site2 = receiver.location[j,1]
    s12dist = geodist(receiver.location[i,2],receiver.location[i,3],receiver.location[j,2],receiver.location[j,3])
    res = data.frame(site1,site2,s12dist)
    allres = rbind.data.frame(allres,res)
  }
}

allres_lt2 = filter(allres,s12dist<2)

dmat = dcast(allres_lt2, site1 ~ site2, drop=F)
dmat = dmat[,-1]
dmat[is.na(dmat)] = 5
# rowmn = (apply(dmat,2,mean) < 5)
# dmat = dmat[rowmn,rowmn]
sites = colnames(dmat)
dmat = as.dist(dmat)
clusters = hclust(dmat,method="complete")
plot(clusters,labels=sites)
clusterCut <- cutree(clusters, h=2)
siteClusters <- data.frame(sitecode=sites,newsitecode=clusterCut)

siteClusters_LL = join(receiver.location,siteClusters,by="sitecode")

siteClusters_LL$sitecode = siteClusters_LL$sitecode %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), GB = c("GBI","GBO"),
                                                   KB = c("KBI","KBO"), KLB = c("KLBI","KLBO","MBC"), 
                                                   MI = c("MI","MIA","MIB","MJB"), MSZ = c("MSB","MZV"),
                                                   NH = c("NHI","NHO"), PB = c("PBI","PBO"), PP = c("PPN","PPS"),
                                                   RK = c("RKI","RKO"), SFA = c("SF2","SFO1"), SFB = c("SF3","SFI4"),
                                                   SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                                          "SISE","SIW"), ST = c("STI","STO"))


siteClusters_LL <- siteClusters_LL %>% group_by(sitecode) %>% dplyr::summarise(Latitude = mean(Latitude),
                                                         Longitude = mean(Longitude))

write.csv(siteClusters,"siteClusters.csv")
write.csv(siteClusters_LL,"siteClusters_latlong.csv")
