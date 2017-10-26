# Acoustic telemetry data processing
# Data from Alison Kock
# Ian Durbach and Theoni Photopoulou 20161210

# set your working directory
setwd("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore")

Sys.setenv(TZ='Africa/Johannesburg')

list.files()
library(plyr)
library(dplyr)
library(lubridate)
library(igraph)
library(reshape2)
library(spnet)
library(sp)
library(ggmap)
library(ggplot2)
library(data.table)
library(reshape2)
library(stringr)
library(foreach)
library(forcats)

options(dplyr.print_max = 1e9)

# input your data
# I have a Stata file, if you have another type of file, e.g., csv, you will need to use a different function to read it in
library(foreign)
mydata <- read.dta("data_in_use.dta")
dim(mydata)
str(mydata)
head(mydata)
names(mydata)

head(mydata$datetime)
head(mydata$first_daily_time)
head(mydata$last_daily_time)

# select a few meaningful columns - these will be different depending on the data
mdf <- mydata[,-c(3,4,6:8,16,19:21,23:25,27,29:33,37:38,41,43,45:49,53,55)]
head(mdf)
dim(mdf)

# there was a problem with a duplicate deployment in 2006 and 2007 which we remove below
# remove 439 observations at KBO between 10/2006 and 11/2007 (Alison deployment typo)
mdf = filter(mdf,!(sitecode=="KBO" & ((year==2006 & month>=10)|(year==2007 & month<=11))))

##### Notes on the raw data 
# 1. datetime is not usually a time object at this stage and often gives time in GMT. it gives the time of each detection
# 2. first daily time is a time object and it's in SAST but it only gives the time of each new detection
# 3. I want the time given by datetime but I want it as a time object and in SAST
#####

# order data and work out visits
x <- mdf
head(x$datetime)

# format datetime object
mydatetime <- dmy_hms(x$datetime, tz = "UTC"); head(mydatetime)
mdt <- format(mydatetime, format="%Y-%m-%d %H:%M:%S", tz="Africa/Johannesburg", usetz=T); head(mdt)
x$datetime <- as.POSIXct(mdt, format="%Y-%m-%d %H:%M:%S", tz="Africa/Johannesburg", usetz=T)
head(x$datetime)
typeof(x$datetime)

# get date from datetime object
mydate <- as.POSIXct(mdt, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T)
head(mydate)
x$date <- mydate

# reformat datereciverlastdeployed
mydrld <- ymd(as.character(x$datereciverlastdeployed), tz = "UTC"); head(mydrld)
drld <- format(mydrld, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T); head(drld)
x$datereciverlastdeployed <- drld

# reformat date_tagged
mydttg <- ymd(as.character(x$date_tagged), tz = "UTC"); head(mydttg)
dttg <- format(mydttg, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T); head(dttg)
dttg1 <- as.POSIXct(dttg, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T); head(dttg1)
dttg2 <- as.Date(dttg1, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T); head(dttg2)
x$date_tagged <- dttg2

# reformat date_last
mydtl <- ymd(as.character(x$date_last), tz = "UTC"); head(mydtl)
dtl <- format(mydtl, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T); head(dtl)
dtl1 <- as.POSIXct(dtl, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T); head(dtl1)
dtl2 <- as.Date(dtl1, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T); head(dtl2)
x$date_last <- dtl2

# reformat time_recorded
mytr <- as.character(x$time_recorded); head(mytr)
trposix <- as.POSIXct(mytr, format="%H:%M:%S", tz="UTC"); head(trposix)
attr(trposix, "tzone") <- "Africa/Johannesburg"; head(trposix)
# with_tz(trposix, "Africa/Johannesburg") also works
trlt <- str_split(trposix, " ", 2); head(trlt)
rectime <- ldply(trlt)[,2]; head(rectime)
rtht <- hms(as.factor(rectime)); head(rtht)
rth <- lubridate::hour(rtht); head(rth)
x$time_recorded <- rectime
x$hr <- rth

head(x)

# order data by shark_id, datetime and sitecode
x = x[order(x$shark_id, x$datetime, x$sitecode),]
n = nrow(x)
head(x)

# work out the time period each tag was active
tfirst <- x[match(unique(x$shark_id), x$shark_id),c("shark_id","date_tagged","date_last")]
dseq <- foreach(j=1:nrow(tfirst)) %do% {
	seq.Date(from=tfirst$date_tagged[j], to=tfirst$date_last[j], by="days")
	}
# turn this into a long vector of dates but keep the date formats
udseq <- unlist(dseq); head(udseq)
date.vec <- as.Date(udseq, format="%Y-%m-%d", origin = "1970-01-01", tz="Africa/Johannesburg", usetz=T); head(date.vec)
date.table <- table(date.vec)
nrow(date.table) 

# work out quarters of the day
mytod <- rep(NA, length=nrow(x))
test <- ifelse(x$hr >= 4 & x$hr < 10, "Morning", ifelse(x$hr >= 10  & x$hr < 16, "Day", ifelse(x$hr >= 16 & x$hr < 22, "Evening", "Night")))
table(test)
table(x$hr) # check it's correct: sum between 4 and 10 for morning - sum(11344, 12931, 14530, 17140, 19575, 22971)
x$mytod <- as.factor(test)

# create a dataframe that has unique dates along with the number of tags active on that date, plus month and year
dtdf <- ldply(date.table)
names(dtdf) <- c("date", "active.tags") # the number of active tags on each day could be used as an offset in models of shark occurrence
head(dtdf)
dtdf$month <- month(dtdf$date)
dtdf$year <- year(dtdf$date)
head(dtdf)

# group by month and year and get a mean number of active tags
mcounts = dtdf %>% group_by(year,month) %>% dplyr::summarise(active.tags = mean(active.tags))
test <- join(x, mcounts, by=c("month","year")) 
head(test)

x <- test
head(x)

# FROM HERE, YOU CAN EITHER KEEP SITECODE AS THE SPATIAL GROUPING OR COLLAPSE TO CLUSTER (eg. all receivers at Fish Hoek get the same code), OR AREA (eg. North shore of False Bay)

###########
save(x, file="/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_data.RData")
###########

# PLOTTING THE LOCATIONS OF RECEIVERS FOR PUBLICATION
load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/receiver.location.RData")
head(receiver.location); names(receiver.location)
any(is.na(receiver.location$Latitude))
any(is.na(receiver.location$Longitude))
receiver.location <- receiver.location[,-11]
head(receiver.location)
receiver.location$FB = receiver.location$sitecode %>% forcats::fct_collapse(FB = c("FHNI","FHNO","FHSI","FHSO", "KLBI","KLBO","MBC", "MO", "MI", "MSB","MZV","MJB","SF3","SFI4","SF2","SFO1","SIE","SINA","SISA","SIE","SIN","SINB","SINE","SIS","SISB","SISE","SIW","WR","WRA","WRB","HKI","HKO","GBI","GBO","KBI","KBO","PBI","PBO","STI","STO","PPN","PPS","RKI","RKO"), 
# Ref = c("SF2","SFO1","SIE","SIN","SINB","SINE","SIS","SISB","SISE","SIW","WR"), # these receivers act as offshore reference areas for the descriptive inshore network analysis Ian did, which is why I am plotting them here.
NearFB = "NHI",
NOTFB = c("MIA","MIB","NHO","NHDI","NHDO","RECIFE","MOSSEL","ALGOA","EELSKIN","RBGB","GSGB","GEELBEK","CAMERA","GANS"))
receiver.location

receiver.locationFB <- filter(receiver.location, FB!="NOTFB")
receiver.locationFB <- droplevels(receiver.locationFB)
receiver.locationFB$map = receiver.locationFB$sitecode %>% forcats::fct_collapse(inshore = c("FHNI","FHNO","FHSI","FHSO", "KLBI","KLBO","MBC", "MO", "MI", "MSB","MZV","MJB","SF3","SFI4","HKI","HKO","GBI","GBO","KBI","KBO","PBI","PBO","STI","STO","PPN","PPS","RKI","RKO"), 
nomap = c("SIE","SINB","SINE","SISB","SISE","SIW","SINA","SISA","WRA","WRB"),
ref = c("NHI","SIN","SIS","WR","SF2","SFO1") # these receivers act as offshore reference areas for the descriptive inshore network analysis Ian did, which is why I am plotting them here.
)

# receiver.locationFB <- filter(receiver.locationFB, InshoreVSIsland=="coastal")
receiver.locationFB <- filter(receiver.locationFB, map!="nomap")

# plot receiver locations
qmplot(Longitude, Latitude, data = receiver.locationFB, geom="blank") + geom_point(data = receiver.locationFB, aes(x = Longitude, y = Latitude, shape = map)) + scale_shape_manual(values = c("inshore" = 1, "ref" = 16)) + theme(legend.position="none")
ggsave(paste("FalseBay_rec.png",sep=""), width=8, height=6)
dev.off()

# I then added place names in Preview and erased the point for Noordhoek in MAC OSX program Photos :)

  # # scale_size_continuous(trans="log10",range=c(0.5,2.5),name="") + 
  # # scale_size_area(name="Total visits") + 
  # cscale + guides(size = guide_legend(override.aes = list(linetype=0)))

# PLOTTING SOUTH AFRICA FOR MEPS PUBLICATION

# South Africa map
latRange <- c(-35, -22)
lonRange <- c(14, 35)

lonlat.ratio <- cos((min(latRange)+(max(latRange)-min(latRange))/2)*pi/180) # the length of 1 degree Lon at a given Lat can be found by taking the cosine of Lat (in radians, so I have to convert to degrees by multiplying by pi/180)

png(filename="sa_map.png", height=480, width=480*lonlat.ratio*((lonRange[2]-lonRange[1])/(latRange[2]-latRange[1])), res=100)

cex.text <- 0.9

map('worldHires', xlab="lon",ylab="lat", xlim=lonRange, ylim=latRange, interior=T, mar=c(10,10,0,0))

points(y=-34.2, x=18.6, pch=22, cex=3, col=2) # False Bay box
text(y=-34.2, x=16.8, "  False \n Bay", cex=cex.text, col=2)

text(y=-30, x=23, "South Africa", cex=1.5)
text(y=-32, x=15.5, pch=22, "  Atlantic \n Ocean", cex=cex.text)
text(y=-32, x=31.5, pch=22, " Indian \n Ocean", cex=cex.text)

axis(side=1, line=1, cex.axis=1.1, at=axTicks(1, axp=c(-25,52,20)), tck=-0.02, col="grey50")
axis(side=2, line=1, cex.axis=1.1, at=axTicks(2, axp=c(-37,25,20)), tck=-0.02, col="grey50")

mtext(text="Longitude", side=1, line=3.5, cex=cex.pt)
mtext(text="Latitude", side=2, line=3.5, cex=cex.pt)

dev.off()















## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 
## LEGACY CODE FROM HERE ON. NOT FUNCTIONAL. IGNORE.
## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## ## 

## spatial stuff

library(rgeos)
library(sp)
load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/FBIsiteClusters_LatLon.RData") # False Bay inshore receivers and clusters
names(FBIsiteClusters_LatLon)
FBIlocs <- FBIsiteClusters_LatLon[,c(11,12)]
coordinates(FBIlocs) <- c("Lat_clust", "Lon_clust")


dd <- SpatialPointsDataFrame(coords = FBIlocs, 
                            data = FBIsiteClusters_LatLon, 
                            proj4string = CRS("+init=epsg:2048")) # WGS84
# dd_mrc <- spTransform(dd, CRS("+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +no_defs"))
# dd_utm <- spTransform(dd, CRS("+proj=utm +zone=34 +ellps=WGS84 +units=m +no_defs"))


##

# IAN's code for getting a transition matrix

# drop if visit only consists of 1 data-point (entry == exit)
### NOTE: THIS IS NB -- what to do with sharks detected at >1 receiver simultaneously?
#y = y[(y$exitind*y$enterind)!=1,]

# this creates the table of transitions by counting the number of transitions between sites 
res = y %>% group_by(sitecode,prevsite) %>% dplyr::summarise(count = n())#, 
                                                #wt = mean(abs(timediff)),
                                                #nprobs = sum(timediff==0))
                                                
# ttt = y %>%   %>% dplyr::summarise(count = n())#
# ttt
# # 1        26    17
# # 2        27    88
# # 3        28    18
# # 4        29    57
# y29 <- subset(y, shark_id==29)
# plot.ts(as.factor(y29$sitecode))

#write.csv(res,"zero-time-diffs.csv")

# create transition matrix

# not all sites appear in both prevsite and sitecode, so to get transition matrix to be
# square need to make sure all sites appear in both - this is done in extrabit below

re2 = res
re2$prevsite = factor(re2$prevsite)
re2$sitecode = factor(re2$sitecode)

l1 = levels(re2$prevsite)
l2 = levels(re2$sitecode)

oddonesout = c(setdiff(l1,l2),setdiff(l2,l1))

extrabit = data.frame(prevsite = oddonesout, sitecode = oddonesout, 
                      count = 0)#, wt = 0, nprobs = 0)
re2 = rbind.data.frame(extrabit,re2)

transmat = dcast(re2, prevsite ~ sitecode, 
                 value.var = "count", drop=F, fill=0)

write.csv(transmat,"receiver_transmat.csv")

# Transition matrix reshaped into long format, to exclude all the zeros 

# remove self-transitions
receiver_edgelist = re2
receiver_edgelist = receiver_edgelist[receiver_edgelist$prevsite != receiver_edgelist$sitecode,]

write.csv(receiver_edgelist,"receiver_edgelist_summer.csv")
