rm(list=ls())

full_ck<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/ck_wgt_tar_LHI_Heron_ALL.csv", h=T, strip.white=T) # read sukka back in, hopefully sorts lots os NA rows

full_ck<-full_ck[-which(is.na(full_ck$ck_weight) & is.na(full_ck$ck_cul) & is.na(full_ck$ck_tar)),]

# bit of cleaning

full_ck<-full_ck[full_ck$ck_weight>1,]

full_ck<-full_ck[full_ck$ck_weight<800,]

full_ck[full_ck$ck_tar<10 & !is.na(full_ck$ck_tar),]$ck_tar<-NA

ck2013<-full_ck[full_ck$Year==2013,]

ck2013<-ck2013[-which(is.na(ck2013$ck_weight) & is.na(ck2013$ck_cul) & is.na(ck2013$ck_tar)),]

# SST dat
library(lubridate)

sst11<-read.csv("~/grive/phd/sourced_data/Heron/GBROTE-1209-SBE39-11.csv", h=T, skip=85)
sst50<-read.csv("~/grive/phd/sourced_data/Heron/GBROTE-1209-SBE39-50.csv", h=T, skip=89)

sst11$DateTimeUTC=as.POSIXlt(paste(substr(sst11$TIME, 1, 10), 
                                   substr(sst11$TIME, 12, 19)), origin="1899-12-30", tz="UTC")

sst11$DateTimeAEST<-sst11$DateTimeUTC+ 10*3600

sst11$DateAEST<-date(sst11$DateTimeAEST)

ag_sst11<-aggregate(TEMP~DateAEST, data=sst11, median)

sst50$DateTimeUTC=as.POSIXlt(paste(substr(sst50$TIME, 1, 10), 
                                   substr(sst50$TIME, 12, 19)), origin="1899-12-30", tz="UTC")

sst50$DateTimeAEST<-sst50$DateTimeUTC+ 10*3600

sst50$DateAEST<-date(sst50$DateTimeAEST)

ag_sst50<-aggregate(TEMP~DateAEST, data=sst50, median)

ag_sst11$Depth="11"

ag_sst50$Depth="50"

sst_comb<-rbind(ag_sst11, ag_sst50)

ck2013$DateAEST<-date(ck2013$DateTime)

library(ggplot2)

p<-ggplot(sst_comb, aes(x=DateAEST))
p+geom_line(aes(y=TEMP, colour=Depth))

p+geom_line(aes(y=TEMP, colour=Depth))+
  geom_boxplot(data=ck2013, aes(y=ck_weight/10, group=DateAEST))+
scale_x_date(date_breaks = "1 day", 
             limits = as.Date(c('2013-02-08','2013-03-24')))

p+geom_boxplot(data=ck2013, aes(y=ck_weight, group=DateAEST))+
  geom_line(aes(y=TEMP*10, colour=Depth, size=2))+
  scale_x_date(date_breaks = "1 day", 
               limits = as.Date(c('2013-02-08','2013-03-24')))

ag_ck<-aggregate(ck_weight~DateAEST, data=ck2013, median)
l1<-as.Date('2013-02-08')
l2<-as.Date('2013-03-24')

ag1_ck<-ag_ck[ag_ck$DateAEST>l1,]

ag2_ck<-ag1_ck[ag1_ck$DateAEST<l2,]


ag_sst501<-ag_sst50[ag_sst50$DateAEST>l1,]

ag_sst502<-ag_sst501[ag_sst501$DateAEST<l2,]

test<-cbind(ag_sst502[2:nrow(ag_sst502),], ag2_ck)

m1<-lm(ck_weight~TEMP, test)

qplot(data=test, x=TEMP, y=ck_weight, geom="point")

m125<-lm(ck_weight~TEMP, test[test$TEMP>25.8,])
summary(m125)

qplot(data=test[test$TEMP>25.8,], x=TEMP, y=ck_weight, geom="point")


test<-cbind(ag_sst502[1:42,], ag2_ck)

m1<-lm(ck_weight~TEMP, test)

qplot(data=test, x=TEMP, y=ck_weight, geom="point")




'2013-03-24')))






































