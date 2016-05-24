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


## bind up ALL! chick data run on 24/05/16

library(readxl)
library(ggplot2)
library(lubridate)
library(reshape2)

setwd("~/grive/phd/analyses/foraging_strategy")


lhi_04<-read_excel("~/grive/phd/sourced_data/IH_DP_moonphase/WTSH 2004 Lord Howe_DP.xls", 1 ,
                   skip=0, col_names=T)

lhi_04<-data.frame(Col="LHI", Year=2004,
                   NestID=lhi_04$Nest, DateTime=as.POSIXlt((lhi_04$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                   ck_weight=lhi_04$'chick wt (g)', 
                   ck_cul=lhi_04$'ch cul (mm)', 
                   ck_tar=lhi_04$'ch tarsus')
                   

lhi_04<-lhi_04[-which(is.na(lhi_04$DateTime)),] #remove NAs


weight_2014<-read_excel("~/grive/phd/fieldwork/LHI_Mar_2014/data/Chick_data_2014.xlsx", 1 ,
                        skip=2, col_names=T) #"DEAD" is converted NA

tar_2014<-read_excel("~/grive/phd/fieldwork/LHI_Mar_2014/data/Chick_data_2014.xlsx", 2 ,
                        skip=2, col_names=T)

weight_2014<-melt(weight_2014, value.name="weight", id=1)
weight_2014$variable<-as.numeric(as.character(weight_2014$variable))
tar_2014<-melt(tar_2014, value.name="tar", id=1)

lhi_14<-data.frame(Col="LHI", Year=2014,
                   NestID=weight_2014$Nest, 
                   DateTime=as.POSIXlt((weight_2014$variable* 86400)+(16*3600), origin="1899-12-30", tz="UTC"),
                   ck_weight=weight_2014$weight, 
                   ck_cul=NA, 
                   ck_tar=tar_2014$tar)

weight_2015<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2015/data/Chick_data_2015.xlsx", 1 ,
                        skip=2, col_names=T) #"DEAD" is converted NA

tar_2015<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2015/data/Chick_data_2015.xlsx", 2 ,
                     skip=2, col_names=T)

weight_2015<-melt(weight_2015, value.name="weight", id=1)
weight_2015$variable<-as.numeric(as.character(weight_2015$variable))
tar_2015<-melt(tar_2015, value.name="tar", id=1)

lhi_15<-data.frame(Col="LHI", Year=2015,
                   NestID=weight_2015$Nest, 
                   DateTime=as.POSIXlt((weight_2015$variable* 86400)+(16*3600), origin="1899-12-30", tz="UTC"),
                   ck_weight=weight_2015$weight, 
                   ck_cul=NA, 
                   ck_tar=tar_2015$tar)

weight_2016<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2016/data/Chick_data_2016.xlsx", 1 ,
                        skip=2, col_names=T) #"DEAD" is converted NA

tar_2016<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2016/data/Chick_data_2016.xlsx", 2 ,
                     skip=2, col_names=T)

weight_2016<-melt(weight_2016, value.name="weight", id=1)
weight_2016$variable<-as.numeric(as.character(weight_2016$variable))
tar_2016<-melt(tar_2016, value.name="tar", id=1)

lhi_16<-data.frame(Col="LHI", Year=2016,
                   NestID=weight_2016$Nest, 
                   DateTime=as.POSIXlt((weight_2016$variable* 86400)+(16*3600), origin="1899-12-30", tz="UTC"),
                   ck_weight=weight_2016$weight, 
                   ck_cul=NA, 
                   ck_tar=tar_2016$tar)


heron_01<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2001-Heron nesting-karlina.xlsx", 1 ,
                     skip=0, col_names=T)

heron_01<-heron_01[-1,] #removes dummy row

heron_01<-data.frame( Col="Heron", Year=2001,
                      NestID=heron_01$'Nest #',
                      DateTime=as.POSIXlt((heron_01$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_01$'chick wt (g)', ck_cul=heron_01$'ch cul (mm)', ck_tar=heron_01$'ch tarsus')
                    
heron_01<-heron_01[-which(is.na(heron_01$DateTime)),] #remove NAs


heron_02<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2002-Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_02<-heron_02[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_02<-data.frame(Col="Heron", Year=2002,
                     NestID=heron_02$Nest,
                     DateTime=as.POSIXlt((heron_02$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_02$'chick wt (g)',
                     ck_cul=heron_02$'ch cul (mm)', ck_tar=heron_02$'ch tarsus')
                 
heron_02<-heron_02[-which(is.na(heron_02$DateTime)),] #remove NAs

heron_03<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2003 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_03<-heron_03[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_03<-data.frame(Col="Heron", Year=2003,
                     NestID=heron_03$Nest, DateTime=as.POSIXlt((heron_03$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                    ck_weight=heron_03$'chick wt (g)', ck_cul=heron_03$'ch cul (mm)', 
                    ck_tar=heron_03$'ch tarsus') 

heron_03<-heron_03[-which(is.na(heron_03$DateTime)),] #remove NAs

heron_06<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2006 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_06<-heron_06[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_06<-data.frame(Col="Heron", Year=2006,
                     NestID=heron_06$Nest, 
                     DateTime=as.POSIXlt((heron_06$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                      ck_weight=heron_06$'chick wt (g)', ck_cul=heron_06$'ch cul (mm)', 
                     ck_tar=heron_06$'ch tarsus')

heron_06<-heron_06[-which(is.na(heron_06$DateTime)),] #remove NAs

heron_10<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2010 Heron nesting-karlina.xlsx", 1 ,
                     skip=0, col_names=T)

heron_10<-heron_10[-1,] #removes dummy row

# In 2010 there are some differences in col names and data available
heron_10<-data.frame(Col="Heron", Year=2010,
                     NestID=heron_10$Nest, DateTime=as.POSIXlt((heron_10$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                           ck_weight=heron_10$'chick wt (g)', ck_cul=NA, ck_tar=heron_10$'chick tarsus (mm)')

heron_10<-heron_10[-which(is.na(heron_10$DateTime)),] #remove NAs


heron_12<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2012 nest records.xlsx", 1 ,
                     skip=0, col_names=T)

heron_12<-heron_12[-1,] #removes dummy row

# change time NA to 0.7 ~ 16:00
heron_12[is.na(heron_12$TIME),]$TIME<-0.7

# In 2012 there are some differences in col names and data available
heron_12<-data.frame(Col="Heron", Year=2012,
                     NestID=heron_12$Nest, DateTime=as.POSIXlt((heron_12$DATE+heron_12$TIME)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_12$'CHICK WT (g)', ck_cul=NA, ck_tar=heron_12$'CHICK TAR (mm)')

heron_13<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2013 nesting data.xlsx", 1 ,
                     skip=0, col_names=T)

heron_13<-heron_13[-1,] #removes dummy row

# change time NA to 0.7 ~ 16:00
heron_13[is.na(heron_13$TIME),]$TIME<-0.7

# In 2012 there are some differences in col names and data available
heron_13<-data.frame(Col="Heron", Year=2013,
                     NestID=heron_13$Nest, DateTime=as.POSIXlt((heron_13$DATE+heron_13$TIME)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_13$'Net chick wt (g)', ck_cul=NA, ck_tar=heron_13$'CHICK TARSUS(mm)')

# now some of the extra datasets that I added

heron_11<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2011 Heron nesting-karlina.xlsx", 1 ,
                     skip=0, col_names=T)

# In 2012 there are some differences in col names and data available
heron_11<-data.frame(Col="Heron", Year=2011,
                     NestID=heron_11$Nest, 
                     DateTime=as.POSIXlt((heron_11$Date+heron_11$Time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_11$'chick wt (g)', ck_cul=NA, ck_tar=heron_11$'chick tarsus (mm)')

heron_14<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2014 Heron nesting.xlsx", 1 ,
                     skip=0, col_names=T)

heron_14<-heron_14[-1,] 


heron_14<-data.frame(Col="Heron", Year=2014,
                     NestID=heron_14$Nest, 
                     DateTime=as.POSIXlt((heron_14$Date+heron_14$Time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_14$'chick wt (g)', ck_cul=NA, ck_tar=heron_14$'chick tarsus (mm)')

heron_14<-heron_14[-which(is.na(heron_14$DateTime)),] 

#final 2015 from heron

heron_15<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2015 Heron nesting.xlsx", 1 ,
                     skip=0, col_names=T)

heron_15<-heron_15[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_15<-data.frame(Col="Heron", Year=2015,
                     NestID=heron_15$Nest, DateTime=as.POSIXlt((heron_15$Date+heron_15$Time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_15$'chick wt (g)', ck_cul=NA, 
                     ck_tar=heron_15$'ch tarsus') 

heron_15<-heron_15[-which(is.na(heron_15$DateTime)),] #remove NAs




