rm(list=ls())
library(readxl)
library(ggplot2)
library(lubridate)

#setwd("~/grive/phd/analyses/foraging_strategy")

dat_2004<-read.csv("~/grive/phd/sourced_data/IH_DP_moonphase/IH_DP_ck_weight_feb_may_04.csv", h=T)

dat_2004<-dat_2004[dat_2004$AM_PM=="PM",] # select only PM weights, comparable with other yrs

weight_2004<-NULL

for (i in 3:19)
{
  d1<-data.frame(NestID=names(dat_2004)[i], Year="2004",
                 Date=as.Date(dat_2004[,1], format="%d/%m/%Y"),
                 Weight=dat_2004[,i])
  weight_2004<-rbind(weight_2004, d1)
}


weight_2015<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2015/data/Chick_data_2015.xlsx", 1 ,
                        skip=2, col_names=T)

weight_2016<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2016/data/Chick_data_2016.xlsx", 1 ,
                   skip=2, col_names=T)

weight_2014<-read_excel("~/grive/phd/fieldwork/LHI_Mar_2014/data/Chick_data_2014.xlsx", 1 ,
                        skip=2, col_names=T)

w2014<-NULL
for(i in 2:length(weight_2014))
{
  df1<-data.frame(NestID=weight_2014$NestID, Year=2014, 
             Date=names(weight_2014[i]), Weight=weight_2014[,i])
w2014<-rbind(w2014,df1)   
}

w2015<-NULL
for(i in 2:length(weight_2015))
{
  df1<-data.frame(NestID=weight_2015$NestID, Year=2015, 
                  Date=names(weight_2015[i]), Weight=weight_2015[,i])
  w2015<-rbind(w2015,df1)   
}

w2016<-NULL
for(i in 2:length(weight_2016))
{
  df1<-data.frame(NestID=weight_2016$NestID, Year=2016, 
                  Date=names(weight_2016[i]), Weight=weight_2016[,i])
  w2016<-rbind(w2016,df1)   
}

all_weight<-rbind(w2014, w2015, w2016)
all_weight$Weight<-as.numeric(all_weight$Weight)

all_weight$Date<-as.Date(as.numeric(as.character(all_weight$Date)), origin="1899-12-30")

# Add in 2004 data
all_weight<-rbind(all_weight, weight_2004)

p<-ggplot(all_weight[all_weight$Year=="2004",], aes(x=Date, y=Weight, group=Date))
p+geom_boxplot()

#each ck - do adults feed in exponential growth and lose mass then replenish themselves?
p<-ggplot(all_weight[all_weight$Year==2015,], aes(x=as.factor(Date), y=Weight, colour=NestID))
p+geom_line()

library(dplyr)

weight_mn<-summarise(group_by(all_weight, Date), mean_weight=median(Weight, na.rm=T))
#mean or median??
weight_mn$year=substr(weight_mn$Date,1,4)

weight_mn$Month<-as.Date(paste(day(weight_mn$Date),"-", month(weight_mn$Date), sep=""), format="%d-%m")

p<-ggplot(data=weight_mn, aes(y=mean_weight, x=Month, colour=year))
p+geom_line(size=1.5)+xlab(size=2)

### below needs to be worked on and adapted for all data
# feeds

gg_feed<-NULL
for (i in 3:max(which(colSums(weight[,2:59], na.rm=T)>0))) # weight changes run from the 18th of Feb
{
  d1<-data.frame(weight=weight[,i], diff=weight[,i]-weight[,(i-1)], Date=names(weight)[i], NestID=weight$NestID)
  gg_feed<-rbind(gg_feed, d1)
}

gg_feed$Date<-dmy(substr(gg_feed$Date,1,11))

gg_feed$Chick<-as.factor(sapply(gg_feed$diff, FUN=function(x) (if(is.na(x)) {x="NA"}else{if(x>=0){"Fed"}else{"Not fed"}})))
## have set to weight diff = 0 then is 'fed' ie hasnt lost mass in 24 hr, althou for starving chicks this could be flatlining

# Moonphase
#library(moonsun)
#dat$Julian.Date <- julian(as.Date(dat$Date, format="%d/%m/%Y"))
# "Percentage of bright area visible from Earth"
#dat$Phase <- sapply(dat$Julian.Date, function(date) { options(latitude=-31.55, longitude=159.083333); moon(date)$phase })

#more bind up and feed analyses, could be killed??

######
## compare multiple years of ck weight

library(reshape2)

weight_2004<-read.csv("~/grive/phd/sourced_data/IH_DP_moonphase/IH_DP_ck_weight_feb_may_04.csv", h=T)

weight_2004<-weight_2004[weight_2004$AM_PM=="PM",] # select only PM weights, comparable with other yrs
weight_2004$AM_PM<-NULL

weight_2004<-melt(weight_2004, value.name="weight", id=1)
names(weight_2004)[2]<-"NestID"

weight_2014<-read_excel("~/grive/phd/fieldwork/LHI_Mar_2014/data/Chick_data_2014.xlsx", 1 ,
                        skip=2, col_names=T, col_types=rep("numeric", 21)) #"DEAD" is converted NA

weight_2014<-melt(weight_2014, value.name="weight", id=1)
names(weight_2014)[2]<-"Date"

weight_2014$Date<-as.Date(as.numeric(as.character(weight_2014$Date)), origin = "1899-12-30") # origin dumb on MS Excel

weight_2015<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2015/data/Chick_data_2015.xlsx", 1 ,
                        skip=2, col_names=T, col_types=c("character", rep("numeric", 59)))

weight_2015<-melt(weight_2015, value.name="weight", id=1)
names(weight_2015)[2]<-"Date"

#standardize and bind together

weight_2004<-weight_2004[, c(2,1,3)] #reorder columns

weight_2004$NestID<-as.character(weight_2004$NestID)
weight_2014$NestID<-as.character(weight_2014$NestID)
weight_2015$NestID<-as.character(weight_2015$NestID)

weight_2004$Date<-dmy(weight_2004$Date)
weight_2014$Date<-ymd(as.Date(as.numeric(as.character(weight_2014$Date)), origin = "1899-12-30"))
weight_2015$Date<-dmy(weight_2015$Date)

weight_yrs<-rbind(weight_2004, weight_2014, weight_2015)
weight_yrs$Year<-year(weight_yrs$Date)
weight_yrs<-weight_yrs[order(weight_yrs$Date),]

weight_yrs$diff<-NA
weight_yrs$temp_id<-paste(weight_yrs$Year, weight_yrs$NestID, sep="_")

for (i in unique(weight_yrs$temp_id))
{
  for(j in 2:nrow(weight_yrs[weight_yrs$temp_id==i,]))
  {
    weight_yrs[weight_yrs$temp_id==i,]$diff[j]<-
      weight_yrs[weight_yrs$temp_id==i,]$weight[j]-weight_yrs[weight_yrs$temp_id==i,]$weight[(j-1)]
  }   
}

weight_yrs$temp_id<-NULL

weight_yrs$Feed<-as.factor(sapply(weight_yrs$diff, FUN=function(x) (if(is.na(x)) {x="NA"}else{if(x>=0){"Fed"}else{"Not fed"}})))

#


p<-ggplot(data=weight_yrs[weight_yrs$year!="2014",], aes(y=weight, x=as.Date(Date, format="%d/%m"),
                                                         group=Date, colour=year))
p+geom_boxplot()

library(dplyr)

weight_mn<-summarise(group_by(weight_yrs, Date), mean_weight=mean(weight, na.rm=T))

weight_mn$year=substr(weight_mn$Date,7,10)

p<-ggplot(data=weight_mn, aes(y=mean_weight, x=as.Date(Date, format="%d/%m"), colour=year))
p+geom_line()

