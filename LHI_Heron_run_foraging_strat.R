rm(list=ls())
library(readxl)
library(ggplot2)
library(lubridate)

setwd("~/grive/phd/analyses/foraging_strategy")


source("~/grive/phd/scripts/WTSH_foraging_strategy/foragingStrat.R")


## !!!! ADD in bit to take first date of each nest and check that there isnt a D there,
## this is not needed on lord howe as I think i started at not the first dat? bit should check each
## could use the aggregate function to see how many known birds are back at the start 
## of each dataset
#data.frame(aggregate(LW~NestID, data=nest_comp, FUN=function(x){sort(unique(x))}),
#aggregate(LW~NestID, data=nest_comp, FUN=function(x){table(x)}))


###############################################################################################
##### THIS SCRIPT READS IN THE LHI AND HERON DATA, REMOVES ANY FINAL ERRONEOUS NESTS AND  #####
##### RUNS THE foragingStrat FUNCTION TO CREATE HISTOGRAMS                                #####
###############################################################################################

## LORD HOWE

# !!!2016!!! NEST COMP RECEIVED SOME MANUAL CLEANING IN CALC NAs in ck weight section changed to 0s
# Also NA lines were removed (mainly for nests not monitored early on)
nest_comp_lhi16<-read.csv("R_analyses_data/LHI_2016_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp_lhi16$Date<-ymd(nest_comp_lhi16$Date)


nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=10,]
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=19,]
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=23,]
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=36,]
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=40,]
# first stab at removing bad nests, could remove more
# applying the <=25% unknown rule
nest_comp_lhi16<-nest_comp_lhi16[nest_comp_lhi16$NestID!=16,]

lhi_16<-foragingStrat(nest_comp=nest_comp_lhi16, D1="2016-02-05"
                      ,D2="2016-03-12" , longallow=TRUE, Nruns=100)

# !!!2015!!! NEST COMP RECEIVED SOME MANUAL CLEANING IN CALC NAs in ck weight section changed to 0s
nest_comp_lhi15<-read.csv("R_analyses_data/LHI_2015_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp_lhi15$Date<-ymd(nest_comp_lhi15$Date)

nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=48,]
nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=51,]
nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=38,]
nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=14,]
#nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=2,] #extra nests that appear one adult appears to abandon
#nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=6,]
#nest_comp_lhi15<-nest_comp_lhi15[nest_comp_lhi15$NestID!=33,]

lhi_15<-foragingStrat(nest_comp=nest_comp_lhi15, D1="2015-02-18"
                      ,D2="2015-04-11" , longallow=TRUE, Nruns=100)

# LHI !!!2004!!!
nest_comp_lhi04<-read.csv("R_analyses_data/LHI_2004_nest_attendance_cleaned.csv", h=T)
nest_comp_lhi04$Date<-nest_comp_lhi04$Date_hack
nest_comp_lhi04$Date<-ymd(nest_comp_lhi04$Date)

nest_comp_lhi04<-nest_comp_lhi04[nest_comp_lhi04$NestID!="5a",] # remove 25% unknown nest

lhi_04<-foragingStrat(nest_comp=nest_comp_lhi04, D1="2004-01-31"
                     ,D2="2004-02-28" , longallow=TRUE, Nruns=100)

# plotting

bs_smry<-rbind(data.frame(tLength=seq(1:25),
mean_prop=apply(lhi_04$bs1000_tl[,2:101],1,mean, na.rm=T),
sd_prop=apply(lhi_04$bs1000_tl[,2:101],1,sd, na.rm=T),
mean_raw=apply(lhi_04$bs1000_Rtl[,2:101],1,mean, na.rm=T),
sd_raw=apply(lhi_04$bs1000_Rtl[,2:101],1,sd, na.rm=T),
col_yr="LHI04"), 

data.frame(tLength=seq(1:25),
mean_prop=apply(lhi_15$bs1000_tl[,2:101],1,mean, na.rm=T),
sd_prop=apply(lhi_15$bs1000_tl[,2:101],1,sd, na.rm=T),
mean_raw=apply(lhi_15$bs1000_Rtl[,2:101],1,mean, na.rm=T),
sd_raw=apply(lhi_15$bs1000_Rtl[,2:101],1,sd, na.rm=T),
col_yr="LHI15"),

data.frame(tLength=seq(1:25),
mean_prop=apply(lhi_16$bs1000_tl[,2:101],1,mean, na.rm=T),
sd_prop=apply(lhi_16$bs1000_tl[,2:101],1,sd, na.rm=T),
mean_raw=apply(lhi_16$bs1000_Rtl[,2:101],1,mean, na.rm=T),
sd_raw=apply(lhi_16$bs1000_Rtl[,2:101],1,sd, na.rm=T),
col_yr="LHI16"))

for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Mean time spent foraging (prop)")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)

#ggsave(paste("LHI16_foraging",D1, D2, "longallow", longallow, "feed_fun_SINGLE.png", sep="_")) #change feed_fun manually!

limits <- aes(ymax = mean_raw + sd_raw, ymin=mean_raw - sd_raw)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_raw))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Number of trips")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)

## Heron 2001 ##
nest_comp_heron01<-read.csv("R_analyses_data/Heron_2001_nest_attendance_cleaned.csv", h=T)
nest_comp_heron01$Date<-nest_comp_heron01$Date_hack
nest_comp_heron01$Date<-ymd(nest_comp_heron01$Date)

nest_comp_heron01<-nest_comp_heron01[nest_comp_heron01$NestID!=15,] # remove 25% unknown nest
nest_comp_heron01<-nest_comp_heron01[nest_comp_heron01$NestID!=18,]

heron_01<-foragingStrat(nest_comp=nest_comp_heron01, D1="2001-02-07"
                      ,D2="2001-02-27" , longallow=TRUE, Nruns=100)


## Heron 2002 ##
nest_comp_heron02<-read.csv("R_analyses_data/Heron_2002_nest_attendance_cleaned.csv", h=T)
nest_comp_heron02$Date<-nest_comp_heron02$Date_hack
nest_comp_heron02$Date<-ymd(nest_comp_heron02$Date)

heron_02<-foragingStrat(nest_comp=nest_comp_heron02, D1="2002-03-02"
                        ,D2="2002-03-30" , longallow=TRUE, Nruns=100)

## Heron 2003 ##
nest_comp_heron03<-read.csv("R_analyses_data/Heron_2003_nest_attendance_cleaned.csv", h=T)
nest_comp_heron03$Date<-nest_comp_heron03$Date_hack
nest_comp_heron03$Date<-ymd(nest_comp_heron03$Date)
summary(nest_comp_heron03$Date)

heron_03<-foragingStrat(nest_comp=nest_comp_heron03, D1="2003-02-08"
                        ,D2="2003-03-08" , longallow=TRUE, Nruns=100)
## Heron 2006 ##
nest_comp_heron06<-read.csv("R_analyses_data/Heron_2006_nest_attendance_cleaned.csv", h=T)
nest_comp_heron06$Date<-nest_comp_heron06$Date_hack
nest_comp_heron06$Date<-ymd(nest_comp_heron06$Date)
summary(nest_comp_heron06$Date)

nest_comp_heron06<-nest_comp_heron06[nest_comp_heron06$NestID!=5,] #remove 25% unknown

heron_06<-foragingStrat(nest_comp=nest_comp_heron06, D1="2006-02-02"
                        ,D2="2006-02-28" , longallow=TRUE, Nruns=100)

## Heron 2010 ##
nest_comp_heron10<-read.csv("R_analyses_data/Heron_2010_nest_attendance_cleaned.csv", h=T)
nest_comp_heron10$Date<-nest_comp_heron10$Date_hack
nest_comp_heron10$Date<-ymd(nest_comp_heron10$Date)
summary(nest_comp_heron10$Date)

heron_10<-foragingStrat(nest_comp=nest_comp_heron10, D1="2010-02-08"
                        ,D2="2010-03-12" , longallow=TRUE, Nruns=100)

## Heron 2012 ##
nest_comp_heron12<-read.csv("R_analyses_data/Heron_2012_nest_attendance_cleaned.csv", h=T)
nest_comp_heron12$Date<-nest_comp_heron12$Date_hack
nest_comp_heron12$Date<-ymd(nest_comp_heron12$Date)
summary(nest_comp_heron12$Date)

good_unk_25_nests<-c(13, "13A", 15, 16, 31,32) #below 25% unkown

nest_comp_heron12<-nest_comp_heron12[nest_comp_heron12$NestID %in% good_unk_25_nests,]

heron_12<-foragingStrat(nest_comp=nest_comp_heron12, D1="2012-02-19"
                        ,D2="2012-04-09" , longallow=TRUE, Nruns=100)

## Heron 2013 ##
nest_comp_heron13<-read.csv("R_analyses_data/Heron_2013_nest_attendance_cleaned.csv", h=T)
nest_comp_heron13$Date<-nest_comp_heron13$Date_hack
nest_comp_heron13$Date<-ymd(nest_comp_heron13$Date)
summary(nest_comp_heron13$Date)

good_2013_unk_25_nests<-c(5   ,6 ,  21,  22  ,32 ,
                          47,  54,  55,  57,  62,  63, 
                         64 , 65,  67,  68,  71,  72,  82,  "13a") #below 25% unkown

nest_comp_heron13<-nest_comp_heron13[nest_comp_heron13$NestID %in% good_2013_unk_25_nests,]


heron_13<-foragingStrat(nest_comp=nest_comp_heron13, D1="2013-03-01"
                        ,D2="2013-04-02" , longallow=TRUE, Nruns=100)


# plotting

bs_smry<-rbind(data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_01$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_01$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_01$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_01$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron01"), 
               
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_02$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_02$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_02$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_02$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron02"),
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_03$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_03$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_03$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_03$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron03"),
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_06$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_06$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_06$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_06$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron06"),
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_10$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_10$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_10$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_10$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron10"),
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_12$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_12$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_12$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_12$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron12"),
               data.frame(tLength=seq(1:25),
                         mean_prop=apply(heron_13$bs1000_tl[,2:101],1,mean, na.rm=T),
                         sd_prop=apply(heron_13$bs1000_tl[,2:101],1,sd, na.rm=T),
                         mean_raw=apply(heron_13$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                         sd_raw=apply(heron_13$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                         col_yr="Heron13"))
               

for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Mean time spent foraging (prop)")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)

#ggsave(paste("LHI16_foraging",D1, D2, "longallow", longallow, "feed_fun_SINGLE.png", sep="_")) #change feed_fun manually!

limits <- aes(ymax = mean_raw + sd_raw, ymin=mean_raw - sd_raw)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_raw))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Number of trips")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)


bs_smry<-data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_01$bs1000_tl[,2:1001],1,mean, na.rm=T),
                          sd_prop=apply(heron_01$bs1000_tl[,2:1001],1,sd, na.rm=T),
                          mean_raw=apply(heron_01$bs1000_Rtl[,2:1001],1,mean, na.rm=T),
                          sd_raw=apply(heron_01$bs1000_Rtl[,2:1001],1,sd, na.rm=T),
                          col_yr="Heron01") 
               
              
for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Mean time spent foraging (prop)")+
  xlab("Duration of foraging trip (days)")

#ggsave(paste("LHI16_foraging",D1, D2, "longallow", longallow, "feed_fun_SINGLE.png", sep="_")) #change feed_fun manually!

limits <- aes(ymax = mean_raw + sd_raw, ymin=mean_raw - sd_raw)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_raw))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Number of trips")+
  xlab("Duration of foraging trip (days)")

#############################################
### testing code and validating approach ####
#############################################

## Heron 2003 ##
nest_comp_heron03<-read.csv("R_analyses_data/Heron_2003_nest_attendance_cleaned.csv", h=T)
nest_comp_heron03$Date<-nest_comp_heron03$Date_hack
nest_comp_heron03$Date<-ymd(nest_comp_heron03$Date)

heron_03<-foragingStrat(nest_comp=nest_comp_heron03, D1="2003-02-08"
                        ,D2="2003-03-08" , longallow=TRUE, Nruns=100)

#lets try with only nests that have 3 or fewer unknwons per 30 days ~<10%
unknown_compar<-cbind(aggregate(LW_corr~NestID, data=nest_comp_heron03, FUN=function(x){length(which(x=="D"))}),
      nrow=aggregate(RW_corr~NestID, data=nest_comp_heron03, FUN=function(x){x=length(x) })[,2])


heron_03_3unkn<-foragingStrat(nest_comp=nest_comp_heron03[nest_comp_heron03$NestID %in%
                              unknown_compar[unknown_compar$LW_corr<4,]$NestID,], D1="2003-02-08"
                        ,D2="2003-03-08" , longallow=TRUE, Nruns=100)

bs_smry<-rbind(data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_03$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_03$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_03$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_03$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron03"), 
               
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_03_3unkn$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_03_3unkn$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_03_3unkn$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_03_3unkn$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="heron_03_3unkn"))

for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Mean time spent foraging (prop)")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)

#okay, now what about upping it

nrow(nest_comp_heron03)
samp<-sample( 1:nrow(nest_comp_heron03), 25)
nc25<-nest_comp_heron03
nc25[samp,]$LW_corr<-"D"
nc25[samp,]$RW_corr<-"D"

nc25out<-foragingStrat(nest_comp=nc25, D1="2003-02-08"
                        ,D2="2003-03-08" , longallow=TRUE, Nruns=100)

samp<-sample( 1:nrow(nest_comp_heron03), 50)
nc50<-nest_comp_heron03
nc50[samp,]$LW_corr<-"D"
nc50[samp,]$RW_corr<-"D"

nc50out<-foragingStrat(nest_comp=nc50, D1="2003-02-08"
                       ,D2="2003-03-08" , longallow=TRUE, Nruns=100)

samp<-sample( 1:nrow(nest_comp_heron03), 75)
nc75<-nest_comp_heron03
nc75[samp,]$LW_corr<-"D"
nc75[samp,]$RW_corr<-"D"

nc75out<-foragingStrat(nest_comp=nc75, D1="2003-02-08"
                       ,D2="2003-03-08" , longallow=TRUE, Nruns=100)

bs_smry<-rbind(data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_03$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_03$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_03$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_03$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron03"), 
               
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(nc25out$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(nc25out$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(nc25out$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(nc25out$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="nc25out"),
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(nc50out$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(nc50out$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(nc50out$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(nc50out$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="nc50out"),
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(nc75out$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(nc75out$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(nc75out$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(nc75out$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="nc75out"))


for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Mean time spent foraging (prop)")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)

#testing removing unknowns in output

nest_comp_heron03<-read.csv("R_analyses_data/Heron_2003_nest_attendance_cleaned.csv", h=T)
nest_comp_heron03$Date<-nest_comp_heron03$Date_hack
nest_comp_heron03$Date<-ymd(nest_comp_heron03$Date)

heron_03<-foragingStrat(nest_comp=nest_comp_heron03, D1="2003-02-08"
                        ,D2="2003-03-08" , longallow=TRUE, Nruns=100)


heron_03_remUn<-foragingStrat(nest_comp=nest_comp_heron03, D1="2003-02-08"
                        ,D2="2003-03-08" , longallow=TRUE, Nruns=100, remUnknowns=TRUE)

bs_smry<-rbind(data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_03$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_03$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_03$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_03$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="Heron03"), 
               
               data.frame(tLength=seq(1:25),
                          mean_prop=apply(heron_03_remUn$bs1000_tl[,2:101],1,mean, na.rm=T),
                          sd_prop=apply(heron_03_remUn$bs1000_tl[,2:101],1,sd, na.rm=T),
                          mean_raw=apply(heron_03_remUn$bs1000_Rtl[,2:101],1,mean, na.rm=T),
                          sd_raw=apply(heron_03_remUn$bs1000_Rtl[,2:101],1,sd, na.rm=T),
                          col_yr="heron_03_remUn"))

for(i in 2:5){
  bs_smry[which(bs_smry[,i]==0),i]<-NA} #change 0 to NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Mean time spent foraging (prop)")+
  xlab("Duration of foraging trip (days)")+
  facet_grid(col_yr~.)

# need to decide on minimum number of unknowns across entire dataset

nest_comp_lhi16<-read.csv("R_analyses_data/LHI_2016_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp_lhi15<-read.csv("R_analyses_data/LHI_2015_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp_lhi04<-read.csv("R_analyses_data/LHI_2004_nest_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp_heron01<-read.csv("R_analyses_data/Heron_2001_nest_attendance_cleaned.csv", h=T)
nest_comp_heron02<-read.csv("R_analyses_data/Heron_2002_nest_attendance_cleaned.csv", h=T)
nest_comp_heron03<-read.csv("R_analyses_data/Heron_2003_nest_attendance_cleaned.csv", h=T)
nest_comp_heron06<-read.csv("R_analyses_data/Heron_2006_nest_attendance_cleaned.csv", h=T)
nest_comp_heron10<-read.csv("R_analyses_data/Heron_2010_nest_attendance_cleaned.csv", h=T)
nest_comp_heron12<-read.csv("R_analyses_data/Heron_2012_nest_attendance_cleaned.csv", h=T)
nest_comp_heron13<-read.csv("R_analyses_data/Heron_2013_nest_attendance_cleaned.csv", h=T)


out<-rbind(data.frame(aggregate(LW_corr~NestID, data=nest_comp_lhi16, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_lhi16, FUN=function(x){x=length(x) })[,2],
           DatasetID="lhi16"), 
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_lhi15, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_lhi15, FUN=function(x){x=length(x) })[,2],
                      DatasetID="lhi15"),
           
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_lhi04, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_lhi04, FUN=function(x){x=length(x) })[,2],
                      DatasetID="lhi04"),
           
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_heron01, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_heron01, FUN=function(x){x=length(x) })[,2],
                      DatasetID="heron01"),
           
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_heron02, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_heron02, FUN=function(x){x=length(x) })[,2],
                      DatasetID="heron02"),
           
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_heron03, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_heron03, FUN=function(x){x=length(x) })[,2],
                      DatasetID="heron03"),
           
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_heron06, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_heron06, FUN=function(x){x=length(x) })[,2],
                      DatasetID="heron06"),
           
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_heron10, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_heron10, FUN=function(x){x=length(x) })[,2],
                      DatasetID="heron10"),
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_heron12, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_heron12, FUN=function(x){x=length(x) })[,2],
                      DatasetID="heron12"),
           
           data.frame(aggregate(LW_corr~NestID, data=nest_comp_heron13, FUN=function(x){length(which(x=="D"))}),
                      nrow=aggregate(RW_corr~NestID, data=nest_comp_heron13, FUN=function(x){x=length(x) })[,2],
                      DatasetID="heron13"))

write.csv(out, "R_analyses_data/foraging_strat_nests_unknowns.csv", quote=F, row.names=F)



