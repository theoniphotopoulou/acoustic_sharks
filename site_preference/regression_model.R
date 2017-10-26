# Regression modelling of processed acoustic telemetry data
# Data from Alison Kock
# Theoni Photopoulou & Ian Durbach 20161210

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Use this script after you have processed the data using
# one of the "grouping" scripts, either 'site_grouping.R',
# 'cluster_grouping.R' or 'area_grouping.R'.
# The code below is written for data processed using the 
# 'cluster_grouping.R' script, for the inshore manuscript 
# by Kock et al. 2017 
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Sys.setenv(TZ='Africa/Johannesburg')

setwd("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore")
# setwd("/Users/iandurbach/Documents/Research/161208_AlisonMEPS")
# load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/siteClusters_LatLon.RData")

# DECIDE WHAT GROUPING YOU WANT FOR THE DATA: site level, cluster level, area level

# sitecode
# load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_site_data.RData")

# clustercode
load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_cluster_data.RData")

# areacode
# load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/20161206_Inshore/processed_area_data.RData")

######### assuming there is only one object in the workspace
ls()

#### STOP! # STOP! # STOP! # STOP! ####
####    NOW CHOSE ONE and run it   ####

# chose this one if you saved the processed data object and have just loaded it
# z <- eval(parse(text=ls()[1])) 

# chose this one if you have just done the processing and have a named object in your workspace already, replace "y_cluster" with your own object's name
# z <- y_cluster  

#### carry on :) # carry on :) # carry on :) ####

#list.files()
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
library(MuMIn)
library(piecewiseSEM)

options(dplyr.print_max = 1e9)

head(z)
names(z)
#z <- z[,c(1:18)]
#head(z)

## seasonal filters - in case you need to fit separate models
z_ss <- filter(z, season=="Spring" | season=="Summer") # keeps only spring and summer
# z_aw <- filter(z, season=="Autumn" | season=="Winter") # keeps only autumn and winter
# z_s <- filter(z, season=="Summer") # keeps only summer
# z_w <- filter(z, season=="Winter") # keeps only winter

z <- z_ss
rm(z_ss)

########################
# 20170502: THEONI NOTE TO SELF - NOT GROUPING BY MONTHS (BELOW) MAKES A HUGE DIFFERENCE TO HOW MUCH SENSE THE MODEL RESULTS MAKE!! 
########################

# this counts up the number of detections for each shark in each year, each month and each time of day category 
z_counts = z %>% group_by(shark_id,year,size_cat,clustcode,mytod,n_rec) %>% dplyr::summarise(count = n(), clust_rangerec = mean(clust_rangerec)) 
z_test = z %>% group_by(year, month) %>% dplyr:: summarise(yearmonth = n()) %>% dplyr:: summarise(yearmonth = n())
# %>% transform(totyear = totyear/max(totyear))
z_t <- join(as.data.frame(z_counts), as.data.frame(z_test), by="year")
head(z_t)
dim(z_t)

z_counts <- z_t
# z_counts = z %>% group_by(shark_id,year,size_cat,clustcode,mytod,active.tags,n_rec) %>% dplyr::summarise(count = n(), clust_rangerec = mean(clust_rangerec))

#### This serves to get observed counts by hour
z_counts1 = z %>% group_by(shark_id,year,size_cat,hr,clustcode,mytod,active.tags,n_rec) %>% dplyr::summarise(count = n(), clust_rangerec = mean(clust_rangerec))
zt <- filter(z_counts1, year<2008, clustcode!="NH")
table(zt$year)
table(zt$clustcode)
ztt <- droplevels(zt)
z_counts1 <- ztt
dim(z_counts1)
# OBSERVED COUNTS BY HOUR AT EACH CLUSTER
ggplot(data = z_counts1, aes(x=hr, y=count)) + 
geom_bar(stat="identity", aes(x=hr, y=count)) + 
# scale_colour_discrete(name="Season") + 
xlab("Time") + ylab("Observed visit count") + 
facet_wrap(~ clustcode) + theme_bw() 
ggsave(paste("Time_obs.png",sep=""),width=8, height=6)
dev.off()
####

head(z_counts, n=40)
dim(z_counts)
table(z_counts$year, z_counts$mytod)
length(unique(z_counts$shark_id))
nlevels(z_counts$clustcode)
levels(z_counts$clustcode)

##############
## MODELLING
##############

# Question: what influences the presence of sharks and/or number of shark detections on the inshore in summer?
# Covariates:	
#				Time of day (dawn, day, dusk, night)
#				Receiver range (<500, or 500<)
#				Year (2004 - 2007)
#				Month ??

head(z_counts)
table(z_counts$year)
# I'm removing data from 2008 because there were such few days of data and also from clustcode Noordhoek because it's not in False Bay :)
ttt <- filter(z_counts, year<2008, clustcode!="NH")
table(ttt$year)
table(ttt$clustcode)
ttt <- droplevels(ttt)
z_counts <- ttt
dim(z_counts)

# check columns are of the right class and have the right levels and that levels are in the right order

## clustcode
# make sure receiver levels are ordered clockwise west through north to east
levels(z_counts$clustcode)
test <- factor(z_counts$clustcode, levels=c("RK", "PP", "ST", "FH", "KLB", "MSZ", "SFB", "MI", "GB", "KB", "PB", "HK"))
levels(test)
z_counts$clustcode <- test
rm(test)

# only 4 visits to HK which is why nothing shows up on the map
# there are no data from 2004 because there weren't any inshore receivers!

# OBSERVED COUNTS BY TIME OF DAY AT EACH CLUSTER
ggplot(data = z_counts, aes(x=mytod, y=count)) + 
geom_violin(aes(x=mytod, y=count)) + 
# scale_colour_discrete(name="Season") + 
xlab("Time of day") + ylab("Observed visit count") + 
facet_wrap(~ clustcode) + theme_bw() 
ggsave(paste("Tod_obs.png",sep=""),width=8, height=6)
dev.off()

## tod (time of day)
levels(z_counts$mytod)
test <- factor(z_counts$mytod, levels=levels(z_counts$mytod)[c(3,1,2,4)])
levels(test)
z_counts$mytod <- test
levels(z_counts$mytod)
rm(test)

## season
# levels(z_counts$season)
# z_counts$season <- as.factor(z_counts$season)
# levels(z_counts$season)
# test <- factor(z_counts$season, levels=levels(z_counts$season)[c(1,4,2,3)])
# levels(test)
# z_counts$season <- test
# rm(test)
## month
# head(z_counts$month)
# typeof(z_counts$month) # numeric at the moment
# table(z_counts$month)
# boxplot(count ~ month, data=z_counts)

## size
head(z_counts$size_cat)
typeof(z_counts$size_cat) # numeric at the moment
table(z_counts$size_cat)
# OBSERVED COUNTS BY SIZE AT EACH CLUSTER
ggplot(data = z_counts, aes(x=size_cat, y=count)) + 
geom_violin(aes(x=size_cat, y=count)) + 
# scale_colour_discrete(name="Season") + 
xlab("Size category") + ylab("Observed visit count") + 
facet_wrap(~ clustcode) + theme_bw() 
ggsave(paste("Sizecat_obs.png",sep=""),width=8, height=6)
dev.off()

zdf <- as.data.frame(z_counts)

## Models ##

#require(glmmADMB)
require(lme4)
require(MASS)
library(lsmeans)
library(car)
library(multcomp)

# check the distribution of the data

summary(zdf$count)
var(zdf$count)
hist(zdf$count) 
# some big counts, mean not equal to variance, straight Poisson won't fit well

pois <- fitdistr(zdf$count, "Poisson")
qqp(zdf$count, "pois", pois$estimate)

qqp(zdf$count, "lnorm")

nbinom <- fitdistr(zdf$count, "Negative Binomial")
qqp(zdf$count, "nbinom", size = nbinom$estimate[[1]], mu = nbinom$estimate[[2]])

# the data are overdispersed - surprise! :)

library(lattice)
xyplot(count ~ clustcode | year, data=zdf) # more of a seasonal effect than a tod effect
xyplot(count ~ clustcode | mytod*year, data=zdf)
xyplot(count ~ clustcode | year*size_cat, data=zdf)
xyplot(count ~ clustcode, data=zdf) 
# xyplot(count/(n_rec*clust_rangerec) ~ clustcode, data=zdf) 

# model for the number of visits by individual sharks during different times of day (4 levels: dawn day dusk night). other covariates include cluster code, year, season

# The following fits a GLMM with individual-level variability (accounting for overdispersion: higher than expected counts of sharks). For this data set the model is the same as one allowing for an interaction between shark_id and the fixed effects

dim(zdf) # [1] 774   9

# set up row id for model
zdf$ind = 1:length(zdf[,1])

m0glm <- glm(count ~ clustcode + mytod + size_cat + as.factor(year), data = zdf, family="poisson", offset=log(n_rec+clust_rangerec+yearmonth)) # without a random effect

m0 <- glmer(count ~ clustcode + mytod + size_cat + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson",control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec+yearmonth))
#, offset=log(active.tags+clust_rangerec))

m0a <- glmer(count ~ clustcode + mytod + size_cat + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa")) # no offset

m0b <- glmer(count ~ clustcode + mytod + size_cat + as.factor(year)  + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec)) # only n_rec offset

m0c <- glmer(count ~ clustcode + mytod + size_cat + as.factor(year) + clust_rangerec + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec)) # clust_rangerec as a fixed effect - gets dropped during fitting!

anova(m0, m0a, m0b, m0c, m0glm)

# m1a <- glmer(count ~ clustcode + mytod + size_cat + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa")) # no offset

# m1b <- glmer(count ~ clustcode + mytod + size_cat + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec)) 

m1 <- glmer(count ~ clustcode + mytod + size_cat + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec+yearmonth)) 
            
m2 <- glmer(count ~ clustcode + mytod + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec+yearmonth)) 

m3 <- glmer(count ~ clustcode + mytod + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec+yearmonth)) 
                   
m4 <- glmer(count ~ clustcode + size_cat + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec+yearmonth)) 
   
m5 <- glmer(count ~ clustcode + size_cat + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec+yearmonth))
            
m6 <- glmer(count ~ clustcode + mytod*size_cat + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec+yearmonth))            

m7 <- glmer(count ~ clustcode + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec+yearmonth))            
m7a <- glmer(count ~ clustcode + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec))            

## no convergence
# m7 <- glmer(count ~ clustcode*size_cat + mytod + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec))

## no convergence
# m8 <- glmer(count ~ clustcode*mytod + size_cat + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec))

## no convergence
# m10 <- glmer(count ~ clustcode*mytod*size_cat + as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson", control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec))

## no convergence
# m11 <- glmer(count ~ clustcode*as.factor(year) + (1|shark_id) + (1|ind), data = zdf, family="poisson",control=glmerControl(optimizer="bobyqa"), offset=log(n_rec+clust_rangerec))

anova(m0, m0a, m0b, m0c, m0glm, m1, m2, m3, m4, m5, m6, m7)

options(na.action = "na.fail")
# m0dredge <- dredge(m0, beta="none", rank=c("adjR^2"), extra = alist(AIC, BIC, Cp))
m0dredge <- dredge(m0, beta="none", fixed=c("clustcode"), extra = alist(BIC))
m0dredge
# model.avg(m0dredge)
m0glmdredge <- dredge(m0glm, beta="none", fixed=c("clustcode"), extra = alist(BIC))

m0dredge$BIC-min(m0dredge$BIC)
m0dredge$AICc-min(m0dredge$AICc)

#########################
# compare m1 with and without an offset
# par(mfrow=c(1,2))
m7summ <- summary(m7) # m1 and m1a
m7coeff <- m7summ$coefficients
plot(m7coeff[c(2:12),1], ylim=c(-1,3), xaxt="n")
abline(h=m7coeff[1,1], col=2)
for (i in c(2:12)){
	segments(x0=i-1, y0= m7coeff[i,1]+m7coeff[i,2], x1=i-1, y1=m7coeff[i,1]-m7coeff[i,2])
}
axis(1, at=c(1:11), labels=levels(zdf$clustcode)[2:12])
#########################

summary(m7)
anova(m7); Anova(m7)
m7pred <- predict(m7, type="response")
m7resid <- residuals(m7, type="response"); plot(m7resid)
plot(m7pred, m7resid)
plot(m7pred, m7@frame$count); abline(0,1, col=2)

# check that the variance of the observation-level random effects (OLRE) is not very big. that indicates problems  (from https://rpubs.com/INBOstats/OLRE)

# R-SQAURED FOR MIXED MODELS (from Harrison 2014)
## marginal r2 means the variance ONLY explained by fixed effects: 
## var_fixed/(var_fixed + var_random + var_resid)
## conditional r2 means the variance explained by BOTH fixed and random effects: 
## var_fixed/(var_fixed + var_random + var_resid)
MuMIn::r.squaredGLMM(m7)
      # R2m       R2c 
# 0.3005656 0.4201895 
piecewiseSEM::rsquared(m7)
     # Class  Family Link   n  Marginal Conditional
# 1 glmerMod poisson  log 774 0.3026487   0.4231017

# ## means and standard errors from package lsmeans (not sure what this does)
# m0.grid <- ref.grid(m0)
# lsms_m0_cl <- summary(lsmeans(m0, "clustcode"), type="response")  
# lsms_m0_tod <- summary(lsmeans(m0, "mytod"), type="response") 
# lsms_m0_s <- summary(lsmeans(m0, "size_cat"), type="response") 

# # colnames(lsms_m1_trt)[1] <- colnames(lsms_m1_col)[1] <- "Var" 

# lsms_m1 <- summary(lsmeans(m1, "clustcode", by = c("season","tod")), type="response")
# summary(glht(m1,linfct=mcp(season="Tukey")))
# summary(glht(m1,linfct=mcp(tod="Tukey")))
# bb <- summary(glht(m1,linfct=mcp(clustcode="Tukey"))) # not sure whether to trust this or not

# # what does this do?
# lsm.m1 <- lsmeans(m1, ~ clustcode + season + tod, adjust="tukey")
# str(lsm.m1)

m7pred <- predict(m7, type="response", allow.new.levels=F, re.form= ~(1|shark_id)+(1|ind))
summary(m7pred)
hist(m7pred)
# m7SEMpred <- sem.predict(object=m7, newdata=zdf, sefit = TRUE)

zdf$pred <- m7pred
head(zdf)
kk <- zdf %>% group_by(clustcode, year) %>% dplyr::summarise(obscount = sum(count), pred= sum(pred))
ff <- zdf %>% group_by(year) %>% dplyr::summarise(obscount = sum(count), pred= sum(pred))
# kk <- zdf %>% group_by(clustcode, season, mytod) %>% dplyr::summarise(obscount = sum(count), pred= sum(pred))
head(kk)
plot(kk$obscount, kk$pred)

# Making tables of results
m7summ <- summary(m7) 
m7coeff <- m7summ$coefficients

# Incidence ratios for CLUSTCODE
predclust.ir <- as.numeric(exp(m7coeff[c(2:12),1]))
predclust.up <- predclust.lo <- vector("numeric", length=11)
indexclust <- c(2:12)
for (i in 1:length(indexclust)){
	j <- indexclust[i]
	predclust.up[i] <- exp(m7coeff[j,1]+m7coeff[j,2])
	predclust.lo[i] <- exp(m7coeff[j,1]-m7coeff[j,2])
}
cbind(predclust.ir, predclust.lo, predclust.up)

# Incidence ratios for YEAR
predy.ir <- as.numeric(exp(m7coeff[c(13:14),1]))
predy.up <- predy.lo <- vector("numeric", length=2)
indexy <- c(13,14)
for (i in 1:length(indexy)){
	j <- indexy[i]
	predy.up[i] <- exp(m7coeff[j,1]+m7coeff[j,2])
	predy.lo[i] <- exp(m7coeff[j,1]-m7coeff[j,2])
}
cbind(predy.ir, predy.lo, predy.up)


# PLOTS RESULTS
load("/Users/theoniphotopoulou/Dropbox/RESEARCH/20160722_Sharks/mean_cluster_LatLon.RData")
preds <- join(data.frame(kk), sc_LatLon, by="clustcode")
preds <- droplevels(preds)

# make sure the factor levels are ordered in the right way
unique(preds$year)
preds$year <- as.factor(preds$year)
levels(preds$clustcode)
names(preds)
names(preds)[4] <- "Visits"
# test <- factor(preds$season, levels=levels(preds$season)[c(1,4,2,3)])
# levels(test)
# preds$season <- test
# rm(test)

## PLOT PREDICTIONS ON A MAP: Predicted number of visits to each cluster in each year
cscale = scale_colour_gradientn(colors=terrain.colors(10)[10:1], 
                                breaks = c(1,10,100,1000), 
                                limits = c(1,1000),
                                trans="log10",
                                name = "Total visits")

qmplot(Longitude, Latitude, data = preds, geom="blank") +
  geom_point(data = preds,aes(x = Longitude, y = Latitude, size = Visits), # PREDS
             colour = I("red"), shape = 1) +
  # geom_point(data = preds,aes(x = Longitude, y = Latitude, size = obscount), # OBS
             # colour = I("blue"), shape = 1) +
  facet_grid(~ year) #facet_grid(season ~ tod) + 
  scale_size_area(name="Total visits") + 
  cscale

ggsave(paste("FalseBay_preds.png",sep=""),width=8, height=6)
dev.off()

# # plot preds as a points
# ggplot(data = preds, aes(x=tod, y=pred)) + 
# geom_point(data = preds, aes(x=tod, y=pred, colour=season)) + # shape=season)) + 
# scale_colour_discrete(name="Season") + 
# xlab("Time of day") + ylab("Predicted count") + 
# facet_wrap(~ clustcode) + theme_bw()

## PLOT PREDICTIONS AS POINTS: Predicted number of visits to each cluster in each year
ggplot(data = preds, aes(x=year, y=Visits)) + 
geom_point(data = preds, aes(x=year, y=Visits), colour = I("red")) +  
# geom_point(data = preds, aes(x=year, y=obscount), colour = I("blue")) +  
# scale_colour_discrete(name="Season") + 
xlab("Year") + ylab("Predicted vists per year") + 
facet_wrap(~ clustcode) + theme_bw() #+ theme(legend.title=element_text("Visits"))

ggsave(paste("Clustfacet_preds.png",sep=""), width=8, height=6)
dev.off()

#########################


######################### LEGACY. NOT FUNCTIONAL. IGNORE.
# plot estimates 
par(mfrow=c(1,2))
m7summ <- summary(m7) 
m7coeff <- m7summ$coefficients
# CLUSTCODE
plot(exp(m7coeff[c(2:12),1]), ylim=c(0,30), xaxt="n")
abline(h=1, col=2)
for (i in c(2:12)){
	segments(x0=i-1, y0=exp(m7coeff[i,1]+m7coeff[i,2]), x1=i-1, y1=exp(m7coeff[i,1]-m7coeff[i,2]))
}
axis(1, at=c(1:11), labels=levels(zdf$clustcode)[2:12])
# year
plot(exp(m7coeff[c(13:14),1]), ylim=c(0,2), xaxt="n")
abline(h=0, col=2)
for (i in c(13:14)){
	segments(x0=i-12, y0=exp(m7coeff[i,1]+m7coeff[i,2]), x1=i-12, y1=exp(m7coeff[i,1]-m7coeff[i,2]))
}
axis(1, at=c(1:2), labels=unique(zdf$year)[2:3])

# TOD
plot(exp(m1coeff[c(13:15),1]), ylim=c(0,2), xaxt="n")
abline(h=0, col=2)
for (i in c(13:15)){
	segments(x0=i-12, y0=exp(m1coeff[i,1]+m1coeff[i,2]), x1=i-12, y1=exp(m1coeff[i,1]-m1coeff[i,2]))
}
axis(1, at=c(1:3), labels=levels(zdf$tod)[2:4])
# SEASON
plot(exp(m1coeff[c(16:18),1]), ylim=c(0,2), xaxt="n")
abline(h=0, col=2)
for (i in c(16:18)){
	segments(x0=i-15, y0=exp(m1coeff[i,1]+m1coeff[i,2]), x1=i-15, y1=exp(m1coeff[i,1]-m1coeff[i,2]))
}
axis(1, at=c(1:3), labels=levels(zdf$season)[2:4])

#########################






