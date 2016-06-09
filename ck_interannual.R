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
                   NestID=as.character(lhi_04$Nest), DateTime=as.POSIXlt((lhi_04$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                   ck_weight=as.numeric(as.character(lhi_04$'chick wt (g)')), 
                   ck_cul=as.numeric(as.character(lhi_04$'ch cul (mm)')), 
                   ck_tar=as.numeric(as.character(lhi_04$'ch tarsus')))
                   

lhi_04<-lhi_04[-which(is.na(lhi_04$DateTime)),] #remove NAs


weight_2014<-read_excel("~/grive/phd/fieldwork/LHI_Mar_2014/data/Chick_data_2014.xlsx", 1 ,
                        skip=2, col_names=T) #"DEAD" is converted NA

tar_2014<-read_excel("~/grive/phd/fieldwork/LHI_Mar_2014/data/Chick_data_2014.xlsx", 2 ,
                        skip=2, col_names=T)

weight_2014<-melt(weight_2014, value.name="weight", id=1)
weight_2014$variable<-as.numeric(as.character(weight_2014$variable))
tar_2014<-melt(tar_2014, value.name="tar", id=1)

lhi_14<-data.frame(Col="LHI", Year=2014,
                   NestID=as.character(weight_2014$Nest), 
                   DateTime=as.POSIXlt((weight_2014$variable* 86400)+(16*3600), origin="1899-12-30", tz="UTC"),
                   ck_weight=as.numeric(as.character(weight_2014$weight)), 
                   ck_cul=as.numeric(as.character(NA)), 
                   ck_tar=as.numeric(as.character(tar_2014$tar)))

weight_2015<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2015/data/Chick_data_2015.xlsx", 1 ,
                        skip=2, col_names=T) #"DEAD" is converted NA

tar_2015<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2015/data/Chick_data_2015.xlsx", 2 ,
                     skip=2, col_names=T)

weight_2015<-melt(weight_2015, value.name="weight", id=1)
weight_2015$variable<-as.numeric(as.character(weight_2015$variable))
tar_2015<-melt(tar_2015, value.name="tar", id=1)

lhi_15<-data.frame(Col="LHI", Year=2015,
                   NestID=as.character(weight_2015$Nest), 
                   DateTime=as.POSIXlt((weight_2015$variable* 86400)+(16*3600), origin="1899-12-30", tz="UTC"),
                   ck_weight=as.numeric(as.character(weight_2015$weight)), 
                   ck_cul=as.numeric(as.character(NA)), 
                   ck_tar=as.numeric(as.character(tar_2015$tar)))

weight_2016<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2016/data/Chick_data_2016.xlsx", 1 ,
                        skip=2, col_names=T) #"DEAD" is converted NA

tar_2016<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2016/data/Chick_data_2016.xlsx", 2 ,
                     skip=2, col_names=T)

weight_2016<-melt(weight_2016, value.name="weight", id=1)
weight_2016$variable<-as.numeric(as.character(weight_2016$variable))
tar_2016<-melt(tar_2016, value.name="tar", id=1)

lhi_16<-data.frame(Col="LHI", Year=2016,
                   NestID=as.character(weight_2016$Nest), 
                   DateTime=as.POSIXlt((weight_2016$variable* 86400)+(16*3600), origin="1899-12-30", tz="UTC"),
                   ck_weight=as.numeric(as.character(weight_2016$weight)), 
                   ck_cul=NA, 
                   ck_tar=as.numeric(as.character(tar_2016$tar)))


heron_01<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2001-Heron nesting-karlina.xlsx", 1 ,
                     skip=0, col_names=T)

heron_01<-heron_01[-1,] #removes dummy row

heron_01<-data.frame( Col="Heron", Year=2001,
                      NestID=as.character(heron_01$'Nest #'),
                      DateTime=as.POSIXlt((heron_01$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                      ck_weight=as.numeric(as.character(heron_01$'chick wt (g)')), 
                      ck_cul=as.numeric(as.character(heron_01$'ch cul (mm)')), 
                      ck_tar=as.numeric(as.character(heron_01$'ch tarsus')))
                    
heron_01<-heron_01[-which(is.na(heron_01$DateTime)),] #remove NAs


heron_02<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2002-Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_02<-heron_02[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_02<-data.frame(Col="Heron", Year=2002,
                     NestID=as.character(heron_02$Nest),
                     DateTime=as.POSIXlt((heron_02$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_02$'chick wt (g)')),
                     ck_cul=as.numeric(as.character(heron_02$'ch cul (mm)')), 
                     ck_tar=as.numeric(as.character(heron_02$'ch tarsus')))
                 
heron_02<-heron_02[-which(is.na(heron_02$DateTime)),] #remove NAs

heron_03<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2003 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_03<-heron_03[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_03<-data.frame(Col="Heron", Year=2003,
                     NestID=as.character(heron_03$Nest), DateTime=as.POSIXlt((heron_03$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_03$'chick wt (g)')),
                     ck_cul=as.numeric(as.character(heron_03$'ch cul (mm)')), 
                    ck_tar=as.numeric(as.character(heron_03$'ch tarsus'))) 

heron_03<-heron_03[-which(is.na(heron_03$DateTime)),] #remove NAs

heron_06<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2006 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_06<-heron_06[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_06<-data.frame(Col="Heron", Year=2006,
                     NestID=as.character(heron_06$Nest), 
                     DateTime=as.POSIXlt((heron_06$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_06$'chick wt (g)')),
                     ck_cul=as.numeric(as.character(heron_06$'ch cul (mm)')), 
                     ck_tar=as.numeric(as.character(heron_06$'ch tarsus')))

heron_06<-heron_06[-which(is.na(heron_06$DateTime)),] #remove NAs

heron_10<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2010 Heron nesting-karlina.xlsx", 1 ,
                     skip=0, col_names=T)

heron_10<-heron_10[-1,] #removes dummy row

# In 2010 there are some differences in col names and data available
heron_10<-data.frame(Col="Heron", Year=2010,
                     NestID=as.character(heron_10$Nest),
                     DateTime=as.POSIXlt((heron_10$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_10$'chick wt (g)')),
                     ck_cul=NA,
                     ck_tar=as.numeric(as.character(heron_10$'chick tarsus (mm)')))

heron_10<-heron_10[-which(is.na(heron_10$DateTime)),] #remove NAs


heron_12<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2012 nest records.xlsx", 1 ,
                     skip=0, col_names=T)

heron_12<-heron_12[-1,] #removes dummy row

# change time NA to 0.7 ~ 16:00
heron_12[is.na(heron_12$TIME),]$TIME<-0.7

# In 2012 there are some differences in col names and data available
heron_12<-data.frame(Col="Heron", Year=2012,
                     NestID=as.character(heron_12$Nest),
                     DateTime=as.POSIXlt((heron_12$DATE+heron_12$TIME)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_12$'CHICK WT (g)')), 
                     ck_cul=NA, 
                     ck_tar=as.numeric(as.character(heron_12$'CHICK TAR (mm)')))

heron_13<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2013 nesting data.xlsx", 1 ,
                     skip=0, col_names=T)

heron_13<-heron_13[-1,] #removes dummy row

# change time NA to 0.7 ~ 16:00
heron_13[is.na(heron_13$TIME),]$TIME<-0.7

# In 2012 there are some differences in col names and data available
heron_13<-data.frame(Col="Heron", Year=2013,
                     NestID=as.character(heron_13$Nest),
                     DateTime=as.POSIXlt((heron_13$DATE+heron_13$TIME)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_13$'Net chick wt (g)')),
                     ck_cul=NA,
                     ck_tar=as.numeric(as.character(heron_13$'CHICK TARSUS(mm)')))

# now some of the extra datasets that I added

heron_11<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2011 Heron nesting-karlina.xlsx", 1 ,
                     skip=0, col_names=T)

# In 2012 there are some differences in col names and data available
heron_11<-data.frame(Col="Heron", Year=2011,
                     NestID=as.character(heron_11$Nest), 
                     DateTime=as.POSIXlt((heron_11$Date+heron_11$Time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_11$'chick wt (g)')),
                                ck_cul=NA,
                                ck_tar=as.numeric(as.character(heron_11$'chick tarsus (mm)')))

heron_14<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2014 Heron nesting.xlsx", 1 ,
                     skip=0, col_names=T)

heron_14<-heron_14[-1,] 


heron_14<-data.frame(Col="Heron", Year=2014,
                     NestID=as.character(heron_14$Nest), 
                     DateTime=as.POSIXlt((heron_14$Date+heron_14$Time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_14$'chick wt (g)')),
                     ck_cul=NA, 
                     ck_tar=as.numeric(as.character(heron_14$'chick tarsus (mm)')))

heron_14<-heron_14[-which(is.na(heron_14$DateTime)),] 

#final 2015 from heron

heron_15<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2015 Heron nesting.xlsx", 1 ,
                     skip=0, col_names=T)

heron_15<-heron_15[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_15<-data.frame(Col="Heron", Year=2015,
                     NestID=as.character(heron_15$Nest), DateTime=as.POSIXlt((heron_15$Date+heron_15$Time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_15$'chick wt (g)')),
                     ck_cul=NA, 
                     ck_tar=as.numeric(as.character(heron_15$'ch tarsus'))) 

heron_15<-heron_15[-which(is.na(heron_15$DateTime)),] #remove NAs


# extra years, I overlooked

heron_09<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2009 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

# I have modified and saved the xlsx in libreoffice so use
heron_09<-data.frame(Col="Heron", Year=2009,
                     NestID=as.character(heron_09$'Nest #'), 
                     DateTime=as.POSIXlt((heron_09$Date + heron_09$Time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_09$'chick wt (g)')),
                     ck_cul=as.numeric(as.character(heron_09$'ch cul (mm)')), 
                     ck_tar=as.numeric(as.character(heron_09$'ch tarsus'))) 

heron_09<-heron_09[-which(is.na(heron_09$DateTime)),] #

heron_05<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2005 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_05<-heron_05[-1,] #

heron_05<-data.frame(Col="Heron", Year=2005,
                     NestID=as.character(heron_05$'Nest #'), 
                     DateTime=as.POSIXlt((heron_05$Date + heron_05$Time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_05$'chick wt (g)')),
                     ck_cul=as.numeric(as.character(heron_05$'ch cul (mm)')), 
                     ck_tar=as.numeric(as.character(heron_05$'ch tarsus'))) 

heron_05<-heron_05[-which(is.na(heron_05$DateTime)),] #

heron_08<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2008 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_08<-data.frame(Col="Heron", Year=2008,
                     NestID=as.character(heron_08$'nest'), 
                     DateTime=as.POSIXlt((heron_08$date + heron_08$time)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=as.numeric(as.character(heron_08$'chick wt')),
                     ck_cul=NA, 
                     ck_tar=NA) 

heron_08<-heron_08[-which(is.na(heron_08$DateTime)),] #


full_ck<-rbind(lhi_04, lhi_14, lhi_15, lhi_16,
               heron_01, heron_02, heron_03, heron_05, heron_06, heron_08, 
               heron_09, heron_10, heron_11, heron_12, heron_13, heron_14, heron_15)

write.csv(full_ck, "R_analyses_data/ck_wgt_tar_LHI_Heron_ALL.csv", quote=F, row.names=F)

setwd("~/grive/phd/analyses/foraging_strategy")
full_ck<-read.csv("R_analyses_data/ck_wgt_tar_LHI_Heron_ALL.csv", h=T, strip.white=T) # read sukka back in, hopefully sorts lots os NA rows

# bit of cleaning

full_ck<-full_ck[full_ck$ck_weight>1,]

full_ck<-full_ck[full_ck$ck_weight<800,]

full_ck<-full_ck[full_ck$ck_tar<55,]

full_ck[full_ck$ck_tar<10 & !is.na(full_ck$ck_tar),]$ck_tar<-NA

full_ck<-full_ck[-which(is.na(full_ck$ck_weight) & is.na(full_ck$ck_cul) & is.na(full_ck$ck_tar)),]


library(ggplot2)

p<-ggplot(full_ck ,aes( x=Year, y=ck_weight/ck_tar, colour=paste(Col, Year)))
p+geom_boxplot() # could be bullshit

hist(full_ck$ck_weight) #looks normal

m1<-lm(ck_weight~ck_tar, data=full_ck[full_ck$Col=="Heron",])
summary(m1)

growth_reg<-data.frame(
  pred_wgt=predict(m1, newdata=data.frame(ck_tar=seq(16, 55, 0.5))), 
  tar=seq(16, 55, 0.5))

p<-ggplot(data=full_ck[full_ck$Col=="Heron",], aes(x=ck_tar, y=ck_weight))
p+geom_point(aes(colour=as.factor(Year)), shape=5)+
  geom_line(data=growth_reg, aes(x=tar, y=pred_wgt), size=2)


library(lme4)

full_ck$tempID<-paste(full_ck$Col, full_ck$Year, full_ck$NestID) 

m2<-lmer(ck_weight~ck_tar+(ck_tar|Year), data=full_ck[full_ck$Col=="Heron",])
summary(m2) # random intercept and slope

growth_reg_lmer<-data.frame(
  pred_wgt=predict(m2, newdata=data.frame(ck_tar=seq(16, 55, 0.5)), re.form=~0), 
  tar=seq(16, 55, 0.5))

growth_reg_lmer_years<-expand.grid(ck_tar=seq(16, 55, 0.5),
              Year=unique(full_ck[full_ck$Col=="Heron",]$Year))

growth_reg_lmer_years$pred_wgt<-predict(m2, newdata=growth_reg_lmer_years,
                                      re.form=~(ck_tar|Year))

p<-ggplot(data=full_ck[full_ck$Col=="Heron",], aes(x=ck_tar, y=ck_weight))
p+geom_point(aes(colour=as.factor(Year)), shape=5)+
  geom_line(data=growth_reg_lmer_years, aes(x=ck_tar, y=pred_wgt, colour=as.factor(Year)))+
  geom_line(data=growth_reg_lmer, aes(x=tar, y=pred_wgt), size=2, colour="dark grey")+
  geom_line(data=growth_reg, aes(x=tar, y=pred_wgt), size=2, linetype=2)+theme_bw()

# now calc residuals of each chick (in each year) from the regression line

hires_pred<-data.frame(ck_tar=seq(16, 55, 0.01))
hires_pred$lmer_pred<-predict(m2, newdata=hires_pred, re.form=~0)

#now we just have to match up any ck tar with regression
#tar values and then calc the difference between the 
# observed ck weight and predicted. Average first by chick,
# then by year.



