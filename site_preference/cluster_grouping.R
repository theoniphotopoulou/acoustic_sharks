# Processed acoustic telemetry data groupedby CLUSTCODE
# Data from Alison Kock
# Ian Durbach and Theoni Photopoulou 20161210

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use this script after you have prepared the data using
# functions in the 'inshore_data_prep.R' script
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

# CLUSTER GROUPING

head(x)

# add cluster code to the detections dataframe
x$clustcode = x$sitecode %>% fct_collapse(FH = c("FHNI","FHNO","FHSI","FHSO"), # North shore
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

sitetab <- ifelse(table(x$sitecode, x$clustcode)>0, 1, 0)
nrec_cl <- apply(sitetab, 2, sum)
nrec_clust <- data.frame(clustcode=names(nrec_cl), n_rec= nrec_cl)
# FH cluster has 4 receivers but they are all less than a km apart, this suggests that the effective number is small, so I'm replacing 4 with 2.
nrec_clust[nrec_clust$clustcode=="FH",2] <- 2
nrec_clust[nrec_clust$clustcode=="FH",]
nrec_clust

nnn <- join(x, nrec_clust, by="clustcode")
head(nnn)
x <- nnn

n <- nrow(x)
x$prevtime = c(now(), x$datetime[-n])
x$timediff = x$datetime - x$prevtime
x$newvisit = ifelse(abs(x$timediff) > 3600, 1, 0) # 60min between visits

head(x)

# work out the average receiver range for each cluster of receivers 
# site_rangerec = x %>% group_by(sitecode) %>% dplyr::summarise(site_rangerec = mean(rangerec))
tt <- x %>% group_by(sitecode) %>% dplyr::summarise(site_rangerec = mean(rangerec), clustcode = names(which.max(table(clustcode))))
clust_rangerec <- tt %>% group_by(clustcode) %>% dplyr::summarise(clust_rangerec = mean(site_rangerec)) 				
																	
test <- join(x, clust_rangerec, by="clustcode")
head(test)

y <- test
y <- y[,c("shark_id","datetime","date","sitecode","clustcode","clust_rangerec","season","month","year","hr","mytod","prevtime","timediff","newvisit","inshore_island","active.tags","n_rec","size_cat")]
head(y)

# # work out the number of receivers active for all combinations of month and tod 
# kk <- y %>% group_by(month, tod) %>% dplyr::summarise(active.tags = sum(active.tags))

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
table(ssi$clustcode)
y <- droplevels(ssi)

ny = nrow(y)
y <- ddply(y, "shark_id", transform, prevshark=c(lag(shark_id))) # group by shark_id and add a new variable that's the lag of shark_id. wherever there is a mismatch, you get an NA. you can use that to work out changes.
y$newshark = ifelse(is.na(y$prevshark), 1, 0)

y <- ddply(y, "shark_id", transform, prevclust=as.factor(c(lag(as.character(clustcode))))) # group by shark_id and add a new variable that's the lag of clustcode
y$newclust = ifelse(is.na(y$prevclust) | (y$prevclust!=y$clustcode), 1, 0)

y$visitID = cumsum(y$newshark) + cumsum(y$newclust) + cumsum(y$newvisit)
y$exitind = ifelse(y$visitID != c(y$visitID[-1],-999), 1, 0)
y$enterind = ifelse(y$visitID != c(-999,y$visitID[-ny]), 1, 0)

# only look at record where shark is first detected at a receiver (only one showing the transition)
y = y[y$enterind==1,]
head(y)
dim(y)

y_cluster <- y

# # throw out very first record for every shark (since won't know previous location here)
# y = y[y$newshark!=1,]

# # throw out transitions that are separated by more than Tm minutes
# Tm = 720
# y = y[y$timediff < (60 * Tm) , ]

###########
save(y_cluster, file="/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_cluster_data.RData")
###########
