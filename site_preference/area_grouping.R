# Processed acoustic telemetry data grouped by AREACODE
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

# AREA GROUPING

head(x)

# add area code to the detections dataframe
x$areacode = x$sitecode %>% fct_collapse(
			  FBN = c(c("FHNI","FHNO","FHSI","FHSO"), # False Bay North shore
					  c("KLBI","KLBO","MBC"), 
					  c("MO", "MI","MIA","MIB"),
					  c("MSB","MZV","MJB"),
					  c("SF3","SFI4")),
                                                   	
              FBM = c(c("SF2","SFO1"), # Middle of the Bay
                      c("SIE","SIN","SINA","SINB","SINE","SIS","SISA","SISB",
                                                          "SISE","SIW"), 
                      c("WR","WRA","WRB")),
                                                          
			  FBE = c(c("HKI", "HKO"), # East shore
				      c("GBI","GBO"),
                      c("KBI","KBO"),
                      c("PBI","PBO")), 

              WC = c("NHI","NHO","NHDI","NHDO"), # West Coast
                                                   	
              FBW = c(c("STI","STO"), # West shore
                   c("PPN","PPS"), 
                   c("RKI","RKO")), 
                                                    
             NOTFB = c("RECIFE","MOSSEL","ALGOA","EELSKIN","RBGB","GSGB","GEELBEK","CAMERA","GANS"))
					# not False Bay
					
# y$areacode = y$clustcode %>% fct_collapse(FBN = c("FH","KLB","MI","MSZ", "SFB"), # North shore
                                                   	
                                                   	# FBM = c("SFA","SI","WR"), # Middle of the Bay
                                                   	                                                          
												   	# FBE = c("HK","GB","KB","PB"), # East shore

                                                   	# WC = c("NH"), # West Coast
                                                   	
                                                   	# FBW = c("ST","PP","RK"), # West shore
                                                
                                                    # NOTFB = c("NOTFB"))

n <- nrow(x)
x$prevtime = c(now(), x$datetime[-n])
x$timediff = x$datetime - x$prevtime
x$newvisit = ifelse(abs(x$timediff) > 3600, 1, 0) # 60min between visits

head(x)

y <- x
y <- y[,c("shark_id","datetime","sitecode","areacode","season","month","year","hr","tod","prevtime","timediff","newvisit","inshore_island")]
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
table(ssi$areacode)
y <- droplevels(ssi)
table(y$areacode)

ny = nrow(y)
y <- ddply(y, "shark_id", transform, prevshark=c(lag(shark_id))) # group by shark_id and add a new variable that's the lag of shark_id
y$newshark = ifelse(is.na(y$prevshark), 1, 0)

y <- ddply(y, "shark_id", transform, prevarea=as.factor(c(lag(as.character(areacode))))) # group by shark_id and add a new variable that's the lag of areacode
y$newarea = ifelse(is.na(y$prevarea) | (y$prevarea!=y$areacode), 1, 0)

y$visitID = cumsum(y$newshark) + cumsum(y$newarea) + cumsum(y$newvisit)
y$exitind = ifelse(y$visitID != c(y$visitID[-1],-999), 1, 0)
y$enterind = ifelse(y$visitID != c(-999,y$visitID[-ny]), 1, 0)

# only look at record where shark is first detected at a receiver (only one showing the transition)
y = y[y$enterind==1,]
head(y)
dim(y)

y_area <- y

head(y)
# # throw out very first record for every shark (since won't know previous location here)
# y = y[y$newshark!=1,]

# # throw out transitions that are separated by more than Tm minutes
# Tm = 720
# y = y[y$timediff < (60 * Tm) , ]

###########
save(y_area, file="/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_area_data.RData")
###########
