rm(list=ls())
library(readxl)
library(ggplot2)
library(lubridate)

setwd("~/grive/phd/analyses/foraging_strategy")

###############################################################################################
##### THIS SCRIPT COMPILES ATTENDENCE DATA AND CLEANS IT BY PULLING CONCURRENT CHICK DATA #####
##### AND COMBINING THE TWO TO RESOLVE WHETHER CHICKS WERE FED OR NOT, AND IF SO WHICH    #####
##### ADULT BIRD DID THE FEEDING                                                          #####
###############################################################################################

# FIRST SECTION OF SCRIPT IS FOR 2016 DATA

atten<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2016/data/Adult_attendence_2016.xlsx", 1 ,
                  skip=0, col_names=T)

m1<-as.matrix(atten)
dimnames(m1)[[1]]<-m1[,1]
m1<-m1[,-1]

weight<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2016/data/Chick_data_2016.xlsx", 1 ,
                   skip=2, col_names=T)
weight<-data.matrix(weight) # data.matrix converts into numeric matrix

gg_feed<-NULL
for (i in 3:max(which(colSums(weight[,2:40], na.rm=T)>0))) # weight changes run from the 18th of Feb
{
  d1<-data.frame(weight=weight[,i], diff=weight[,i]-weight[,(i-1)], Date=dimnames(weight)[[2]][i], NestID=weight[,1])
  gg_feed<-rbind(gg_feed, d1)
}

gg_feed$Date<-as.Date(as.numeric(as.character(gg_feed$Date)), origin="1899-12-30")

gg_feed$Chick<-as.factor(sapply(gg_feed$diff, FUN=function(x) (if(is.na(x)) {x="NA"}else{if(x>=0){"Fed"}else{"Not fed"}})))
## have set to weight diff = 0 then is 'fed' ie hasnt lost mass in 24 hr, althou for starving chicks this could be flatlining


### initially just for ck1


ck1<-cbind(gg_feed[gg_feed$NestID==1,],
           LW=m1[which(dimnames(m1)[[1]]=="1 LW"), which(dimnames(m1)[[2]]=="Feb_05") : which(dimnames(m1)[[2]]=="Mar_12")],
           RW=m1[which(dimnames(m1)[[1]]=="1 RW"), which(dimnames(m1)[[2]]=="Feb_05") : which(dimnames(m1)[[2]]=="Mar_12")])

## ck weights run from 05 Feb - 13 Mar, we select attendance data from 05 Feb - 12 Mar as feed would occur night before



ggplot(data=ck1, aes(y=weight, x=Date))+
  geom_line()+geom_point(aes(x=Date, y=diff)) + 
  geom_line(aes(x=Date, y=as.numeric(LW)*100), colour="green")+
  geom_line(aes(x=Date, y=as.numeric(RW)*100), colour="red")

## Reasons to remove nest/pairs from analyses: never figured what was going on i.e. 3 parents?
## Some chicks died early and parents abandoned

atten_pairs<-dimnames(m1)[[1]] # get attendence data nests
atten_pairs<-atten_pairs[-grep("12", atten_pairs)] #get rid nest 12
atten_pairs<-atten_pairs[-grep("54", atten_pairs)] #get rid nest 54

atten_nests<-1
for(j in 3:66){atten_nests<-c(atten_nests, strsplit(atten_pairs, " ")[[j]][1])} # discustingly inelegant
atten_nests<-as.numeric(unique(atten_nests))

## Now compile ck weights and adult attendence for these nests.

nest_comp<-NULL
for(j in atten_nests)
{
  nestX<-cbind(gg_feed[gg_feed$NestID==j,],
               LW=m1[which(dimnames(m1)[[1]]==paste(j, "LW")), which(dimnames(m1)[[2]]=="Feb_17") : which(dimnames(m1)[[2]]=="Apr_10")],
               RW=m1[which(dimnames(m1)[[1]]==paste(j, "RW")), which(dimnames(m1)[[2]]=="Feb_17") : which(dimnames(m1)[[2]]=="Apr_10")])
  
  nest_comp<-rbind(nest_comp, nestX)
}

# remember attendence data is shifted forward 1 day to match the corresponding chick weight.

write.csv(nest_comp, "LHI_2015_nest_weights_attendance.csv", row.names=F, quote=F)

## does appear like we are overly harrassing the tracked/attendenc birds
gg_feed$harrass<-"no"
gg_feed[gg_feed$NestID %in% nest_comp$NestID,]$harrass<-"yes"
ggplot(data=gg_feed, aes(y=weight, x=Date, group=Date)) + geom_boxplot() +facet_wrap(~harrass)

###### next stage of data cleaning ######

nest_comp$LW_corr<-nest_comp$LW
nest_comp$RW_corr<-nest_comp$RW

nest_comp[nest_comp$LW=="D",]

nest_comp[nest_comp$LW=="D" & nest_comp$Chick=="Not fed",]$LW_corr<-"A"
nest_comp[nest_comp$LW=="D" & nest_comp$Chick=="Not fed",]$RW_corr<-"A"

nest_comp[nest_comp$LW_corr=="D" & nest_comp$diff==0,]

weight[weight$NestID==24,] ## correct for nest 24

nest_comp[nest_comp$LW_corr=="D" &  nest_comp$diff==0 & nest_comp$NestID==24,]$LW_corr<-"A"
nest_comp[nest_comp$LW_corr=="D" &  nest_comp$diff==0 & nest_comp$NestID==24,]$RW_corr<-"A"

nest_comp[nest_comp$Date==nest_comp$Date[39],] #for the date we went to the golfy and didnt sample

nest_comp[nest_comp$Date==nest_comp$Date[39],]$LW_corr<-"D"
nest_comp[nest_comp$Date==nest_comp$Date[39],]$RW_corr<-"D"

nest_comp[nest_comp$Date==nest_comp$Date[39]& nest_comp$Chick=="Not fed",]$LW_corr<-"A"
nest_comp[nest_comp$Date==nest_comp$Date[39]& nest_comp$Chick=="Not fed",]$RW_corr<-"A"

# writing out nest_comp
write.csv(nest_comp, "LHI_2015_nest_weights_attendance_cleaned.csv", row.names=F, quote=F)

###### 2015 ATTENDENCE CLEANING BELOW, JUST A CLONE SCRIPT OF THE 2016 ABOVE ######

atten<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2015/data/Adult_attendence_2015.xlsx", 1 ,
                  skip=0, col_names=T)

m1<-as.matrix(atten)
dimnames(m1)[[1]]<-m1[,1]
m1<-m1[,-1]

weight<-read_excel("~/grive/phd/fieldwork/LHI_Feb_2015/data/Chick_data_2015.xlsx", 1 ,
                   skip=2, col_names=T, col_types=c("character", 
                                                    rep("numeric", 59)))


gg_feed<-NULL
for (i in 3:max(which(colSums(weight[,2:59], na.rm=T)>0))) # weight changes run from the 18th of Feb
{
  d1<-data.frame(weight=weight[,i], diff=weight[,i]-weight[,(i-1)], Date=names(weight)[i], NestID=weight$NestID)
  gg_feed<-rbind(gg_feed, d1)
}

gg_feed$Date<-dmy(substr(gg_feed$Date,1,11))

gg_feed$Chick<-as.factor(sapply(gg_feed$diff, FUN=function(x) (if(is.na(x)) {x="NA"}else{if(x>=0){"Fed"}else{"Not fed"}})))
## have set to weight diff = 0 then is 'fed' ie hasnt lost mass in 24 hr, althou for starving chicks this could be flatlining

# Did chicks stack it at a certainl point?
ck_death<-data.frame(NestID=weight$NestID, Date=names(weight)[apply(weight, 1, FUN=function(x) max(which(is.na(x)!=TRUE)))])
ck_death$Date<-dmy(substr(ck_death$Date,1,11))
ggplot(data=gg_feed[gg_feed$Chick=="Fed",], aes(y=diff, x=Date, group=Date)) + geom_boxplot() + geom_point(data=ck_death, aes(y=NestID, x=Date, colour="red"))

### initially just for ck1


ck1<-cbind(gg_feed[gg_feed$NestID==1,],
           LW=m1[which(dimnames(m1)[[1]]=="1 LW"), which(dimnames(m1)[[2]]=="Feb_17") : which(dimnames(m1)[[2]]=="Apr_10")],
           RW=m1[which(dimnames(m1)[[1]]=="1 RW"), which(dimnames(m1)[[2]]=="Feb_17") : which(dimnames(m1)[[2]]=="Apr_10")])

## ck weights run from 18 Feb - 11 Apr, we select attendance data from 17 Feb - 10 Apr as feed would occur night before


ggplot(data=ck1, aes(y=weight, x=Date))+
  geom_line()+geom_point(aes(x=Date, y=diff)) + 
  geom_line(aes(x=Date, y=as.numeric(LW)*100), colour="green")+
  geom_line(aes(x=Date, y=as.numeric(RW)*100), colour="red")

## Reasons to remove nest/pairs from analyses: never figured what was going on i.e. 3 parents?
## Some chicks died early and parents abandoned

atten_pairs<-dimnames(m1)[[1]] # get attendence data nests
atten_pairs<-atten_pairs[-grep("12", atten_pairs)] #get rid nest 12
atten_pairs<-atten_pairs[-grep("54", atten_pairs)] #get rid nest 54

atten_nests<-1
for(j in 3:66){atten_nests<-c(atten_nests, strsplit(atten_pairs, " ")[[j]][1])} # discustingly inelegant
atten_nests<-as.numeric(unique(atten_nests))

## Now compile ck weights and adult attendence for these nests.

nest_comp<-NULL
for(j in atten_nests)
    {
    nestX<-cbind(gg_feed[gg_feed$NestID==j,],
               LW=m1[which(dimnames(m1)[[1]]==paste(j, "LW")), which(dimnames(m1)[[2]]=="Feb_17") : which(dimnames(m1)[[2]]=="Apr_10")],
               RW=m1[which(dimnames(m1)[[1]]==paste(j, "RW")), which(dimnames(m1)[[2]]=="Feb_17") : which(dimnames(m1)[[2]]=="Apr_10")])

    nest_comp<-rbind(nest_comp, nestX)
    }

# remember attendence data is shifted forward 1 day to match the corresponding chick weight.

write.csv(nest_comp, "LHI_2015_nest_weights_attendance.csv", row.names=F, quote=F)

## does appear like we are overly harrassing the tracked/attendenc birds
gg_feed$harrass<-"no"
gg_feed[gg_feed$NestID %in% nest_comp$NestID,]$harrass<-"yes"
ggplot(data=gg_feed, aes(y=weight, x=Date, group=Date)) + geom_boxplot() +facet_wrap(~harrass)

###### next stage of data cleaning ######

nest_comp$LW_corr<-nest_comp$LW
nest_comp$RW_corr<-nest_comp$RW

nest_comp[nest_comp$LW=="D",]

nest_comp[nest_comp$LW=="D" & nest_comp$Chick=="Not fed",]$LW_corr<-"A"
nest_comp[nest_comp$LW=="D" & nest_comp$Chick=="Not fed",]$RW_corr<-"A"

nest_comp[nest_comp$LW_corr=="D" & nest_comp$diff==0,]

weight[weight$NestID==24,] ## correct for nest 24

nest_comp[nest_comp$LW_corr=="D" &  nest_comp$diff==0 & nest_comp$NestID==24,]$LW_corr<-"A"
nest_comp[nest_comp$LW_corr=="D" &  nest_comp$diff==0 & nest_comp$NestID==24,]$RW_corr<-"A"

nest_comp[nest_comp$Date==nest_comp$Date[39],] #for the date we went to the golfy and didnt sample

nest_comp[nest_comp$Date==nest_comp$Date[39],]$LW_corr<-"D"
nest_comp[nest_comp$Date==nest_comp$Date[39],]$RW_corr<-"D"

nest_comp[nest_comp$Date==nest_comp$Date[39]& nest_comp$Chick=="Not fed",]$LW_corr<-"A"
nest_comp[nest_comp$Date==nest_comp$Date[39]& nest_comp$Chick=="Not fed",]$RW_corr<-"A"

# writing out nest_comp
write.csv(nest_comp, "LHI_2015_nest_weights_attendance_cleaned.csv", row.names=F, quote=F)

## Now can start from here for analyses using nest_comp

nest_comp<-read.csv("LHI_2015_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)

nest_comp$Date<-ymd(nest_comp$Date)

## descriptive stuff. Size of meals from individual and both parent visits

mean(nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="B" & nest_comp$Chick=="Fed",]$diff, na.rm=T)
# 50.59375 nrow =80
mean(nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="A" & nest_comp$Chick=="Fed",]$diff, na.rm=T)
# 25.37844 nrow =218
mean(nest_comp[nest_comp$RW_corr=="B" & nest_comp$LW_corr=="A"  & nest_comp$Chick=="Fed",]$diff, na.rm=T)
# 27.54255 nrow =282

meal_hist_dat<-rbind(data.frame(Feeder="both", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="B" & nest_comp$Chick=="Fed",]$diff),
                     data.frame(Feeder="LW", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="A" & nest_comp$Chick=="Fed",]$diff),
                     data.frame(Feeder="RW", Meal_size =nest_comp[nest_comp$RW_corr=="B" & nest_comp$LW_corr=="A" & nest_comp$Chick=="Fed",]$diff),
                     data.frame(Feeder="Unknown", Meal_size =nest_comp[nest_comp$LW_corr=="D" & nest_comp$Chick=="Fed",]$diff))

meal_hist<-ggplot(meal_hist_dat, aes(x=Meal_size, fill=Feeder))
meal_hist+geom_bar(position="dodge")

meal_hist<-ggplot(meal_hist_dat, aes(x=Meal_size, ..density.., colour=Feeder))
meal_hist+geom_freqpoly(binwidth = 10)

meal_hist<-ggplot(meal_hist_dat, aes(x=Meal_size, fill=Feeder))
meal_hist+geom_density(alpha=0.4)

# bit of pred modelling

meal_mod_dat<-rbind(data.frame(Feeder="both", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="B" & nest_comp$Chick=="Fed",]$diff),
                     data.frame(Feeder="single", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="A" & nest_comp$Chick=="Fed",]$diff),
                     data.frame(Feeder="single", Meal_size =nest_comp[nest_comp$RW_corr=="B" & nest_comp$LW_corr=="A" & nest_comp$Chick=="Fed",]$diff))
                     
                     
test_data<- data.frame(Feeder="Unknown", Meal_size =nest_comp[nest_comp$LW_corr=="D" & nest_comp$Chick=="Fed",]$diff)

library(tree)
t1<-tree(Feeder~Meal_size, data=meal_mod_dat)

meal_mod_dat$tree_pred<-predict(t1, meal_mod_dat)

m1<-glm(Feeder~Meal_size, data=meal_mod_dat, family=binomial)



