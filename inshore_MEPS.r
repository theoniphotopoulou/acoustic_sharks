# Inshore distribution of white sharks
# Data from Alison Kock
# Ian Durbach and Theoni Photopoulou 20161210

setwd("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore")
#setwd("/Users/iandurbach/Documents/Research/161208_AlisonMEPS")

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

options(dplyr.print_max = 1e9)

# input Stata file
library(foreign)
mydata <- read.dta("data_in_use.dta")
dim(mydata)
str(mydata)
head(mydata)
names(mydata)

head(mydata$datetime)
head(mydata$first_daily_time)
head(mydata$last_daily_time)

# select a few meaningful columns
mdf <- mydata[,-c(3,4,6:8,16,19:21,23:25,27,29:33,37:38,41,43,45:49,53,55)]
head(mdf)
dim(mdf)

# remove 439 observations at KBO between 10/2006 and 11/2007 (Alison deployment typo)
mdf = filter(mdf,!(sitecode=="KBO" & ((year==2006 & month>=10)|(year==2007 & month<=11))))

##### Notes on the raw data 20161212
# 1. datetime is not a time object at this stage and it gives time in GMT, by the looks of it. it gives the time of each detection
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
x$date_tagged <- dttg

# reformat date_last
mydtl <- ymd(as.character(x$date_last), tz = "UTC"); head(mydtl)
dtl <- format(mydtl, format="%Y-%m-%d", tz="Africa/Johannesburg", usetz=T); head(dtl)
x$date_last <- dtl

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

x = x[order(x$shark_id, x$datetime, x$sitecode),]
n = nrow(x)
head(x)

x$prevtime = c(now(), x$datetime[-n])
x$timediff = x$datetime - x$prevtime
x$newvisit = ifelse(abs(x$timediff) > 3600, 1, 0) # 60min between visits

head(x)

y <- x
y <- y[,c("shark_id","datetime","sitecode","season","month","year","hr","prevtime","timediff","newvisit")]
head(y)

# seasonal filters - CHOOSE WHICH ONE YOU WANT
ss <- filter(y, season=="Spring" | season=="Summer")
#ss <- filter(y, season=="Autumn" | season=="Winter")
# ss <- filter(y, season=="Summer")
# ss <- filter(y, season=="Winter")
ss <- droplevels(ss)
table(ss$shark_id)
y <- ss

# table(ss$inshore_island)
# ssi <- filter(ss, inshore_island=="inshore") # INSHORE DATA ONLY
# table(ssi$sitecode)

ny = nrow(y)
y$prevshark = c(-999,y$shark_id[-ny])
y$newshark =  ifelse(y$shark_id != y$prevshark, 1, 0)

y$prevsite = c(-999,y$sitecode[-ny])
y$newsite =  ifelse(y$sitecode != y$prevsite, 1, 0)

y$visitID = cumsum(y$newshark) + cumsum(y$newsite) + cumsum(y$newvisit)
y$exitind = ifelse(y$visitID != c(y$visitID[-1],-999), 1, 0)
y$enterind = ifelse(y$visitID != c(-999,y$visitID[-ny]), 1, 0)
head(y)

# only look at record where shark is first detected at a receiver (only one showing the transition)
y = y[y$enterind==1,]

# throw out very first record for every shark (since won't know previous location here)
y = y[y$newshark!=1,]

# throw out transitions that are separated by more than Tm minutes
Tm = 720
y = y[y$timediff < (60 * Tm) , ]

###########
save(y, file="/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_data.RData")
###########

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
