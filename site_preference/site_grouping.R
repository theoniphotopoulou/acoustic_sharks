# Inshore distribution of white sharks grouped by SITECODE
# Data from Alison Kock
# Ian Durbach and Theoni Photopoulou 20161210

library(ggplot2)
library(ggmap)
library(plyr)
library(dplyr)
library(gmt)
library(reshape2)
library(forcats)
library(lubridate)
library(igraph)
library(spnet)
library(sp)
library(data.table)
library(stringr)

Sys.setenv(TZ='Africa/Johannesburg')

setwd("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore")
load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_data.RData")

ls()

# SITECODE GROUPING

n <- nrow(x)
x$prevtime = c(now(), x$datetime[-n])
x$timediff = x$datetime - x$prevtime
x$newvisit = ifelse(abs(x$timediff) > 3600, 1, 0) # 60min between visits

head(x)

y <- x
y <- y[,c("shark_id","datetime","sitecode","season","month","year","hr","tod","prevtime","timediff","newvisit","inshore_island")]
head(y)

# seasonal filters - CHOOSE WHICH ONE YOU WANT
ss <- y # no filter, all seasons in
# ss <- filter(y, season=="Spring" | season=="Summer")
# ss <- filter(y, season=="Autumn" | season=="Winter")
# ss <- filter(y, season=="Summer")
# ss <- filter(y, season=="Winter")
ss <- droplevels(ss)
table(ss$shark_id)
y <- ss

table(ss$inshore_island)
ssi <- filter(ss, inshore_island=="inshore") # INSHORE DATA ONLY
table(ssi$sitecode)
y <- droplevels(ssi)

ny = nrow(y)
y <- ddply(y, "shark_id", transform, prevshark=c(lag(shark_id))) # group by shark_id and add a new variable that's the lag of shark_id
y$newshark = ifelse(is.na(y$prevshark), 1, 0)

y <- ddply(y, "shark_id", transform, prevsite=as.factor(c(lag(as.character(sitecode))))) # group by shark_id and add a new variable that's the lag of sitecode
y$newsite = ifelse(is.na(y$prevsite) | (y$prevsite!=y$sitecode), 1, 0)

y$visitID = cumsum(y$newshark) + cumsum(y$newsite) + cumsum(y$newvisit)
y$exitind = ifelse(y$visitID != c(y$visitID[-1],-999), 1, 0)
y$enterind = ifelse(y$visitID != c(-999,y$visitID[-ny]), 1, 0)

# only look at record where shark is first detected at a receiver (only one showing the transition)
y = y[y$enterind==1,]
head(y)
dim(y)

y_site <- y

###########
save(y_site, file="/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_site_data.RData")
###########
