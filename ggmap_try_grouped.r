library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(forcats)

setwd("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/")

receiver.location.all = read.csv("siteClusters_latlong.csv")[,-1]

receiver.location = filter(receiver.location.all,!(sitecode == "RECIFE"|sitecode=="SFB"))

receiver.location$Latitude = ifelse(receiver.location$Latitude > 0, 
                                    -receiver.location$Latitude, 
                                    receiver.location$Latitude)
seas = "autumnwinter"

rec_ee = read.csv(paste("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/receiver_edgelist_",seas,".csv",sep=""))[,-1]

allfactorlevels = c(levels(rec_ee$prevsite),levels(rec_ee$sitecode))
rec_ee$prevsite = factor(rec_ee$prevsite,levels=allfactorlevels)
rec_ee$sitecode = factor(rec_ee$sitecode,levels=allfactorlevels)

rec_ee$prevsite = rec_ee$prevsite %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), GB = c("GBI","GBO"),
                        KB = c("KBI","KBO"), KLB = c("KLBI","KLBO","MBC"), 
                        MI = c("MI","MIA","MIB","MJB"), MSZ = c("MSB","MZV"),
                        NH = c("NHI","NHO"), PB = c("PBI","PBO"), PP = c("PPN","PPS"),
                        RK = c("RKI","RKO"), SFA = c("SF2","SFO1"), SFB = c("SF3","SFI4"),
                        SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                               "SISE","SIW"), ST = c("STI","STO"))

rec_ee$sitecode = rec_ee$sitecode %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), GB = c("GBI","GBO"),
                                 KB = c("KBI","KBO"), KLB = c("KLBI","KLBO","MBC"), 
                                 MI = c("MI","MIA","MIB","MJB"), MSZ = c("MSB","MZV"),
                                 NH = c("NHI","NHO"), PB = c("PBI","PBO"), PP = c("PPN","PPS"),
                                 RK = c("RKI","RKO"), SFA = c("SF2","SFO1"), SFB = c("SF3","SFI4"),
                                 SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                        "SISE","SIW"), ST = c("STI","STO"))


rec_ee = rec_ee %>% group_by(prevsite,sitecode) %>% dplyr::summarise(count = sum(count),
                                                            wt = sum(wt))
rec_ee = arrange(rec_ee,desc(count))
rec_ee = filter(rec_ee, prevsite != sitecode)
rec_ee = data.frame(rec_ee)

rec_ee = join(rec_ee, receiver.location, by="sitecode")

rec_ee = plyr::rename(x = rec_ee, replace = c("Latitude" = "site2lat","Longitude"="site2long"))

names(receiver.location)[1] = "prevsite"
rec_ee = join(rec_ee, receiver.location, by="prevsite")
rec_ee = plyr::rename(x = rec_ee, replace = c("Latitude" = "site1lat","Longitude"="site1long"))

rec_ee = rec_ee[complete.cases(rec_ee),]
rec_ee2 = rec_ee[rec_ee$count > 0,]

# non-directional edgelist
rec_ee3 = rec_ee2
for(i in 1:nrow(rec_ee2)){
  ps = rec_ee2$prevsite[i]
  ns = rec_ee2$sitecode[i]
  s1lat = rec_ee2$site1lat[i]
  s1long = rec_ee2$site1long[i]
  s2lat = rec_ee2$site2lat[i]
  s2long = rec_ee2$site2long[i]
  
  if(s2lat < s1lat){
    rec_ee3$prevsite[i] = ns
    rec_ee3$sitecode[i] = ps
    rec_ee3$site1lat[i] = s2lat
    rec_ee3$site2lat[i] = s1lat
    rec_ee3$site1long[i] = s2long
    rec_ee3$site2long[i] = s1long
  }
}

rec_ee3 = rec_ee3 %>% group_by(sitecode,prevsite) %>% dplyr::summarise(count = sum(count), 
                                                             wt = sum(count*wt)/sum(count),
                                                             site1lat = mean(site1lat),
                                                             site1long = mean(site1long),
                                                             site2lat = mean(site2lat),
                                                             site2long = mean(site2long))
rec_ee3 = rec_ee3[order(rec_ee3$count,decreasing=T),]
head(rec_ee3)                                                             

#map = get_googlemap(center = c(lon = 18, lat = -34), zoom = 8)

cscale = scale_colour_gradientn(colors=terrain.colors(10)[10:1], 
                                breaks = c(1,5,50,500,5000), 
                                limits = c(1,5000),
                                trans="log10",
                                name = "Transitions")

# directed graph

#rec_ee2 = rec_ee2[order(rec_ee2$count,decreasing=F),]

#qmplot(Longitude, Latitude, data = receiver.location.orig) +
#  geom_point(data = receiver.location.orig,aes(x = Longitude, y = Latitude),
#             colour = I("black")) +
#  geom_segment(data = rec_ee2, aes(x = site1long, y = site1lat,
#               xend = site2long, yend = site2lat, colour = count, 
#               size = count),
#             alpha=0.7) + cscale
  
#geom_path(data=, aes(x=Longitude, y=Latitude), color="black", size=1)

# undirected graph

rec_ee3 = rec_ee3[order(rec_ee3$count,decreasing=F),]

qmplot(Longitude, Latitude, data = receiver.location.all, geom="blank") +
#  geom_blank(data = receiver.location.all,aes(x = Longitude, y = Latitude)) +
  geom_segment(data = rec_ee3, aes(x = site1long, y = site1lat,
                                   xend = site2long, yend = site2lat, 
                                   colour = count, size = count),
               alpha=0.7) + 
  geom_point(data = receiver.location,aes(x = Longitude, y = Latitude),
             colour = I("black"), shape = 1, size = 2) +
  scale_size_continuous(trans="log10",range=c(0.5,2.5),name="") + 
  cscale + guides(size=FALSE)

ggsave(paste("FalseBay_"seas,".png",sep=""),width=8, height=6)
dev.off()
       