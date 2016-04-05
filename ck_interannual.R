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

#library(moonsun)
#dat$Julian.Date <- julian(as.Date(dat$Date, format="%d/%m/%Y"))
# "Percentage of bright area visible from Earth"
#dat$Phase <- sapply(dat$Julian.Date, function(date) { options(latitude=-31.55, longitude=159.083333); moon(date)$phase })

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



gg_feed<-NULL
for (i in 3:max(which(colSums(weight[,2:59], na.rm=T)>0))) # weight changes run from the 18th of Feb
{
  d1<-data.frame(weight=weight[,i], diff=weight[,i]-weight[,(i-1)], Date=names(weight)[i], NestID=weight$NestID)
  gg_feed<-rbind(gg_feed, d1)
}

gg_feed$Date<-dmy(substr(gg_feed$Date,1,11))

gg_feed$Chick<-as.factor(sapply(gg_feed$diff, FUN=function(x) (if(is.na(x)) {x="NA"}else{if(x>=0){"Fed"}else{"Not fed"}})))
## have set to weight diff = 0 then is 'fed' ie hasnt lost mass in 24 hr, althou for starving chicks this could be flatlining

