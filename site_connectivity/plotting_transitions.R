# Constructs plots and tables recording frequency of transitions between 
# cluster (group of receivers) pairs and visits to each cluster.
# ----
# Inputs: receiver_edgelist_<season>.csv (see create_edgelist.r) 
#         siteClusters_latlong.csv 
# Outputs: fig_network.png
#          aggregate_transitions_<season>.csv
#          aggregate_visits_<season>.csv
# ----
# Ian Durbach and Theoni Photopoulou 20170601

library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(forcats)
library(gridExtra)

setwd("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/")

############################
## spring/summer
############################

seas = "springsummer"

receiver.location.all = read.csv("siteClusters_latlong.csv")[,-1]

receiver.location = filter(receiver.location.all,!(sitecode == "NOTFB"|sitecode=="NH"))
receiver.location = droplevels(receiver.location)

receiver.location$Latitude = ifelse(receiver.location$Latitude > 0, 
                                    -receiver.location$Latitude, 
                                    receiver.location$Latitude)

rec_ee = read.csv(paste("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/receiver_edgelist_",seas,".csv",sep=""))[,-1]

allfactorlevels = c(levels(rec_ee$prevsite),levels(rec_ee$sitecode))
rec_ee$prevsite = factor(rec_ee$prevsite,levels=allfactorlevels)
rec_ee$sitecode = factor(rec_ee$sitecode,levels=allfactorlevels)

# group sites
rec_ee$prevsite = rec_ee$prevsite %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), # North shore
                                                   KLB = c("KLBI","KLBO","MBC"), 
                                                   MI = c("MO", "MI","MIA","MIB"),
                                                   MSZ = c("MSB","MZV","MJB"),
                                                   SFB = c("SF3","SFI4"),
                                                   SFA = c("SF2","SFO1"), # Middle of the Bay
                                                   SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                                          "SISE","SIW"), 
                                                   WR = c("WR","WRA","WRB"),
                                                   HK = c("HKI", "HKO"), # East shore
                                                   GB = c("GBI","GBO"),
                                                   KB = c("KBI","KBO"),
                                                   PB = c("PBI","PBO"),
                                                   NH = c("NHI","NHO","NHDI","NHDO"), # West Coast
                                                   ST = c("STI","STO"), # West shore
                                                   PP = c("PPN","PPS"), 
                                                   RK = c("RKI","RKO"), 
                                                   NOTFB = c("RECIFE","MOSSEL","ALGOA","EELSKIN","RBGB","GSGB","GEELBEK","CAMERA","GANS"))


rec_ee$sitecode = rec_ee$sitecode %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), # North shore
                                                   KLB = c("KLBI","KLBO","MBC"), 
                                                   MI = c("MO", "MI","MIA","MIB"),
                                                   MSZ = c("MSB","MZV","MJB"),
                                                   SFB = c("SF3","SFI4"),
                                                   SFA = c("SF2","SFO1"), # Middle of the Bay
                                                   SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                                          "SISE","SIW"), 
                                                   WR = c("WR","WRA","WRB"),
                                                   HK = c("HKI", "HKO"), # East shore
                                                   GB = c("GBI","GBO"),
                                                   KB = c("KBI","KBO"),
                                                   PB = c("PBI","PBO"),
                                                   NH = c("NHI","NHO","NHDI","NHDO"), # West Coast
                                                   ST = c("STI","STO"), # West shore
                                                   PP = c("PPN","PPS"), 
                                                   RK = c("RKI","RKO"), 
                                                   NOTFB = c("RECIFE","MOSSEL","ALGOA","EELSKIN","RBGB","GSGB","GEELBEK","CAMERA","GANS"))

# prevsite contains one site (WR) not in sitecode, so make factor levels same in both
levels(rec_ee$sitecode) <- levels(rec_ee$prevsite)

# count number of transitions between site pairs (unit: per 30 days)
rec_ee = rec_ee %>% group_by(prevsite,sitecode) %>% dplyr::summarise(count = 30*sum(count/min_effort),
                                                                     wt = sum(wt))
rec_ee = arrange(rec_ee,desc(count))
rec_ee = filter(rec_ee, prevsite != sitecode)
rec_ee = data.frame(rec_ee)

# add receiver locations
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

# colour scale for number of transitions
cscale = scale_colour_gradientn(colors=terrain.colors(10)[10:1], 
                                breaks = c(0.15,1.5,15,150), 
                                limits = c(0.03,160),
                                trans="log10",
                                name = "Transitions")

# size scale for number of visits
sscale = scale_size_area(breaks = c(50,100,150,200), 
                                limits = c(1,200),
                                name = "Total visits")

## add in total number of visits to each site
vis1 = rec_ee3 %>% group_by(sitecode) %>% dplyr::summarize(count1 = sum(count))
vis2 = rec_ee3 %>% group_by(prevsite) %>% dplyr::summarize(count2 = sum(count))
names(vis1)[1] = "prevsite"
vis = join(vis1,vis2,by="prevsite")
vis$count1[is.na(vis$count1)] = 0
vis$count2[is.na(vis$count2)] = 0
vis$totalvis = vis$count1 + vis$count2
receiver.location = join(receiver.location,vis,by="prevsite")
receiver.location$totalvis[is.na(receiver.location$totalvis)] = 0

# undirected graph

rec_ee3 = rec_ee3[order(rec_ee3$count,decreasing=F),]

#E.g. Seal Island, Strandfontein, Koeel Bay, Cape Point, Muizenberg
rec_to_text = receiver.location.all[c(2,5,8,12,14),]
rec_to_text$prevsite = c("Fish Hoek","Koeel Bay","Muizenberg","Cape Point","Strandfontein")
rec_to_text$Latitude = rec_to_text$Latitude + c(0,-0.01,0.02,-0.01,0.025)
rec_to_text$Longitude = rec_to_text$Longitude + c(-0.04,0,0,0,0)

fig_ss <- qmplot(Longitude, Latitude, data = receiver.location.all, geom="blank") +
  #  geom_blank(data = receiver.location.all,aes(x = Longitude, y = Latitude)) +
  geom_segment(data = rec_ee3, aes(x = site1long, y = site1lat,
                                   xend = site2long, yend = site2lat, 
                                   colour = count, size = count),
               alpha=0.7) + 
  geom_point(data = receiver.location,aes(x = Longitude, y = Latitude, size = totalvis),
             colour = I("black"), shape = 1) +
  # scale_size_continuous(trans="log10",range=c(0.5,2.5),name="") + 
  cscale + sscale + guides(size = guide_legend(override.aes = list(linetype=0))) +
  geom_text(data = rec_to_text, aes(x=Longitude,y=Latitude,label=prevsite),size=3.5) +
  annotate("text",x=18.38,y=-34.38,label="(a)~italic(Spring/Summer)",parse=TRUE)

#ggsave(paste("FalseBay_",seas,".png",sep=""),width=8, height=6)
#dev.off()

write.csv(rec_ee3,paste("aggregate_transitions_",seas,".csv",sep=""))
write.csv(receiver.location,paste("aggregate_visits_",seas,".csv",sep=""))

############################
## autumn/winter
############################

seas = "autumnwinter"

receiver.location.all = read.csv("siteClusters_latlong.csv")[,-1]

receiver.location = filter(receiver.location.all,!(sitecode == "NOTFB"|sitecode=="NH"))
receiver.location = droplevels(receiver.location)

receiver.location$Latitude = ifelse(receiver.location$Latitude > 0, 
                                    -receiver.location$Latitude, 
                                    receiver.location$Latitude)

rec_ee = read.csv(paste("/Users/iandurbach/Documents/Research/161208_AlisonMEPS/receiver_edgelist_",seas,".csv",sep=""))[,-1]

allfactorlevels = c(levels(rec_ee$prevsite),levels(rec_ee$sitecode))
rec_ee$prevsite = factor(rec_ee$prevsite,levels=allfactorlevels)
rec_ee$sitecode = factor(rec_ee$sitecode,levels=allfactorlevels)

rec_ee$prevsite = rec_ee$prevsite %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), # North shore
                                                   KLB = c("KLBI","KLBO","MBC"), 
                                                   MI = c("MO", "MI","MIA","MIB"),
                                                   MSZ = c("MSB","MZV","MJB"),
                                                   SFB = c("SF3","SFI4"),
                                                   SFA = c("SF2","SFO1"), # Middle of the Bay
                                                   SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                                          "SISE","SIW"), 
                                                   WR = c("WR","WRA","WRB"),
                                                   HK = c("HKI", "HKO"), # East shore
                                                   GB = c("GBI","GBO"),
                                                   KB = c("KBI","KBO"),
                                                   PB = c("PBI","PBO"),
                                                   NH = c("NHI","NHO","NHDI","NHDO"), # West Coast
                                                   ST = c("STI","STO"), # West shore
                                                   PP = c("PPN","PPS"), 
                                                   RK = c("RKI","RKO"), 
                                                   NOTFB = c("RECIFE","MOSSEL","ALGOA","EELSKIN","RBGB","GSGB","GEELBEK","CAMERA","GANS"))


rec_ee$sitecode = rec_ee$sitecode %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), # North shore
                                                   KLB = c("KLBI","KLBO","MBC"), 
                                                   MI = c("MO", "MI","MIA","MIB"),
                                                   MSZ = c("MSB","MZV","MJB"),
                                                   SFB = c("SF3","SFI4"),
                                                   SFA = c("SF2","SFO1"), # Middle of the Bay
                                                   SI = c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                                          "SISE","SIW"), 
                                                   WR = c("WR","WRA","WRB"),
                                                   HK = c("HKI", "HKO"), # East shore
                                                   GB = c("GBI","GBO"),
                                                   KB = c("KBI","KBO"),
                                                   PB = c("PBI","PBO"),
                                                   NH = c("NHI","NHO","NHDI","NHDO"), # West Coast
                                                   ST = c("STI","STO"), # West shore
                                                   PP = c("PPN","PPS"), 
                                                   RK = c("RKI","RKO"), 
                                                   NOTFB = c("RECIFE","MOSSEL","ALGOA","EELSKIN","RBGB","GSGB","GEELBEK","CAMERA","GANS"))

rec_ee = rec_ee %>% group_by(prevsite,sitecode) %>% dplyr::summarise(count = 30*sum(count/min_effort),
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
                                breaks = c(0.15,1.5,15,150), 
                                limits = c(0.03,160),
                                trans="log10",
                                name = "Transitions")

sscale = scale_size_area(breaks = c(50,100,150,200), 
                         limits = c(1,200),
                         name = "Total visits")

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

## add in total number of visits to each site
vis1 = rec_ee3 %>% group_by(sitecode) %>% dplyr::summarize(count1 = sum(count))
vis2 = rec_ee3 %>% group_by(prevsite) %>% dplyr::summarize(count2 = sum(count))
names(vis1)[1] = "prevsite"
vis = join(vis1,vis2,by="prevsite")
vis$count1[is.na(vis$count1)] = 0
vis$count2[is.na(vis$count2)] = 0
vis$totalvis = vis$count1 + vis$count2
receiver.location = join(receiver.location,vis,by="prevsite")
receiver.location$totalvis[is.na(receiver.location$totalvis)] = 0

# undirected graph#

rec_ee3 = rec_ee3[order(rec_ee3$count,decreasing=F),]

#E.g. Seal Island, Strandfontein, Koeel Bay, Cape Point, Muizenberg
rec_to_text = receiver.location.all[c(2,5,8,12,14),]
rec_to_text$prevsite = c("Fish Hoek","Koeel Bay","Muizenberg","Cape Point","Strandfontein")
rec_to_text$Latitude = rec_to_text$Latitude + c(0,-0.01,0.02,-0.01,0.025)
rec_to_text$Longitude = rec_to_text$Longitude + c(-0.04,0,0,0,0)

fig_aw <- qmplot(Longitude, Latitude, data = receiver.location.all, geom="blank") +
  #  geom_blank(data = receiver.location.all,aes(x = Longitude, y = Latitude)) +
  geom_segment(data = rec_ee3, aes(x = site1long, y = site1lat,
                                   xend = site2long, yend = site2lat, 
                                   colour = count, size = count),
               alpha=0.7) + 
  geom_point(data = receiver.location,aes(x = Longitude, y = Latitude, size = totalvis),
             colour = I("black"), shape = 1) +
  # scale_size_continuous(trans="log10",range=c(0.5,2.5),name="") + 
  cscale + sscale + guides(size = guide_legend(override.aes = list(linetype=0))) +
  geom_text(data = rec_to_text, aes(x=Longitude,y=Latitude,label=prevsite),size=3.5) +
  annotate("text",x=18.38,y=-34.38,label="(b)~italic(Autumn/Winter)",parse=TRUE)

write.csv(rec_ee3,paste("aggregate_transitions_",seas,".csv",sep=""))
write.csv(receiver.location,paste("aggregate_visits_",seas,".csv",sep=""))

ggsave("fig_network.png", arrangeGrob(fig_ss, fig_aw,ncol=1),width=8,height=12,dpi=200)
