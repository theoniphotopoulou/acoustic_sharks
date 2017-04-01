library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(gmt)
library(reshape2)
library(forcats)

# setwd("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/")
setwd("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore")

# load("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/receiver.location.RData")
load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/receiver.location.RData")

names(receiver.location); dim(receiver.location)
receiver.location$Latitude = ifelse(receiver.location$Latitude > 0, 
                                    -receiver.location$Latitude, 
                                    receiver.location$Latitude)

# receiver.location.orig = receiver.location
# receiver.location.orig$inshore = ifelse(receiver.location.orig$Classification == "inshore",1,0)
# # do some joins to get lat and long co-ords of prevsite and nextsite (sitecode)
# # into edgelist

receiver.location = receiver.location[,-c(7,8,11)]
names(receiver.location); dim(receiver.location)
# names(receiver.location)[1] = "sitecode"

# this essentially removes all sites outside False Bay
receiver.location = receiver.location[complete.cases(receiver.location),]
names(receiver.location); dim(receiver.location)

# map of receivers
qmplot(Longitude, Latitude, data = receiver.location) +
  geom_point()

lon.col <- which(names(receiver.location)=="Longitude")
lat.col <- which(names(receiver.location)=="Latitude")

# work out all the pairwise distances between receivers
allres = data.frame(site1 = NULL,site2 = NULL, s12dist = NULL)
for(i in 1:nrow(receiver.location)){
  for(j in 1:nrow(receiver.location)){
    site1 = receiver.location[i,1]
    site2 = receiver.location[j,1]
    s12dist = geodist(receiver.location[i,lat.col],receiver.location[i, lon.col],receiver.location[j,lat.col],receiver.location[j,lon.col])
    res = data.frame(site1,site2,s12dist)
    allres = rbind.data.frame(allres,res)
  }
}

head(allres)

# filter out receivers that are less than 2km apart
allres_lt2 = filter(allres,s12dist<2)
head(allres_lt2, n=30)

# turn into a matrix
dmat = dcast(allres_lt2, site1 ~ site2, drop=F)
# drop the first column
dmat = dmat[,-1]
# replace NAs with the number 5
dmat[is.na(dmat)] = 5
# rowmn = (apply(dmat,2,mean) < 5)
# dmat = dmat[rowmn,rowmn]
sites = colnames(dmat)
dmat = as.dist(dmat)
clusters = hclust(dmat,method="complete")
plot(clusters,labels=sites)
clusterCut <- cutree(clusters, h=2)
siteClusters <- data.frame(sitecode=sites, newsitecode=clusterCut)

# add clusters to main receiver.location dataframe
siteClusters_LL = join(receiver.location,siteClusters,by="sitecode")

# collapse individual sites into clusters
siteClusters_LL$sitecode = siteClusters_LL$sitecode %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), GB = c("GBI","GBO"),
                                                   KB = c("KBI","KBO"), KLB = c("KLBI","KLBO","MBC"), 
                                                   MI = c("MI","MIA","MIB","MJB"), MSZ = c("MSB","MZV"),
                                                   NH = c("NHI","NHO"), PB = c("PBI","PBO"), PP = c("PPN","PPS"),
                                                   RK = c("RKI","RKO"), SFA = c("SF2","SFO1"), SFB = c("SF3","SFI4"),
                                                   SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                                          "SISE","SIW"), ST = c("STI","STO"))

# create mean LAT and LON for new clusters
sc_LatLon <- siteClusters_LL %>% group_by(sitecode) %>% dplyr::summarise(Latitude = mean(Latitude), Longitude = mean(Longitude))

# add LAT and LON of clusters to new dataframe
siteClusters_LatLon = join(siteClusters_LL, sc_LatLon, by="sitecode")
names(siteClusters_LatLon) # you have two sets of Lat and Lon now, rename below
names(siteClusters_LatLon)[7:8] <- c("Lat_site", "Lon_site")
names(siteClusters_LatLon)[10:11]<- c("Lat_clust", "Lon_clust")
names(siteClusters_LatLon) 

save(siteClusters_LatLon, file="/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/siteClusters_latlong.RData")

write.csv(siteClusters,"siteClusters.csv")
write.csv(siteClusters_LL,"siteClusters_latlong.csv")




