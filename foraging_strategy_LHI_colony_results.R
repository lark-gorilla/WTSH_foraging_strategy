rm(list=ls())
library(ggplot2)
library(lubridate)

setwd("~/grive/phd/analyses/foraging_strategy")

# !!!2016!!! NEST COMP RECEIVED SOME MANUAL CLEANING IN CALC NAs in ck weight section changed to 0s
nest_comp<-read.csv("LHI_2016_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp$Date<-ymd(nest_comp$Date)


nest_comp<-nest_comp[nest_comp$NestID!=10,]
nest_comp<-nest_comp[nest_comp$NestID!=19,]
nest_comp<-nest_comp[nest_comp$NestID!=23,]
nest_comp<-nest_comp[nest_comp$NestID!=36,]
nest_comp<-nest_comp[nest_comp$NestID!=40,]
# first stab at removing bad nests, could remove more

# !!!2015!!! NEST COMP RECEIVED SOME MANUAL CLEANING IN CALC NAs in ck weight section changed to 0s
nest_comp<-read.csv("LHI_2015_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp$Date<-ymd(nest_comp$Date)

nest_comp<-nest_comp[nest_comp$NestID!=48,]
nest_comp<-nest_comp[nest_comp$NestID!=51,]
nest_comp<-nest_comp[nest_comp$NestID!=38,]
nest_comp<-nest_comp[nest_comp$NestID!=14,]
#nest_comp<-nest_comp[nest_comp$NestID!=2,] #extra nests that appear one adult appears to abandon
#nest_comp<-nest_comp[nest_comp$NestID!=6,]
#nest_comp<-nest_comp[nest_comp$NestID!=33,]

## create simple predictive model to predict when feeding by both adults occurs
meal_mod_dat<-rbind(data.frame(Feeder="both", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="B" & nest_comp$Chick=="Fed",]$diff),
                    data.frame(Feeder="single", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="A" & nest_comp$Chick=="Fed",]$diff),
                    data.frame(Feeder="single", Meal_size =nest_comp[nest_comp$RW_corr=="B" & nest_comp$LW_corr=="A" & nest_comp$Chick=="Fed",]$diff))


m1<-glm(Feeder~Meal_size, data=meal_mod_dat, family=binomial)


# function for single and potential both adult feeds
fun_both<-function(x) 
{ 
  
  if(is.na(x)){LWO<-"NA"; RWO<-"NA"}else{
    
    Z1<-sample(c("single", "both"),1, prob=
                 c(predict(m1, newdata=data.frame(Meal_size=x), type="response"),1-predict(m1, newdata=data.frame(Meal_size=x), type="response"))) 
    if(Z1=="single")
    {LWO<-sample(c("B", "A"),1,)
     if(LWO=="B"){RWO<-"A"}else{RWO<-"B"}}
    
    if(Z1=="both")
    {LWO<-"B"; RWO<-"B"}}
  
  return(c(LWO, RWO))}

# function for single adult feeds
fun_single<-function(x) 
{ 
  LWO<-sample(c("B", "A"),1,)
  if(LWO=="B"){RWO<-"A"}else{RWO<-"B"}
  
  return(c(LWO, RWO))}

#### !!! Always run function from here !!! to reset all data.frames !!!

### Parameters

D1="2016-02-05"
D2="2016-03-12" 
longallow=FALSE
feed_fun=fun_single

##### EXTRA NEST PARAMETER TO RUN ANALYSES ON SELECTED NESTS. REREAD IN
##### ALL ABOVE AFTER RUN !!!!!!!

tar_comp<-read.csv("D:/research/phd/analyses/foraging_strategy/tarsus_comp_04_15.csv", h=T)

summary(tar_comp[tar_comp$Date=="16/02/2004",]$Tar)
summary(tar_comp[tar_comp$Date=="20/02/2004",]$Tar)

summary(tar_comp[tar_comp$Date=="17/02/2015" & tar_comp$Tar<=37,]$Tar) # try with upper limit of 37mm for tarsus

comparable_nests<-unique(nest_comp$NestID)[which(unique(nest_comp$NestID) %in%
                                 tar_comp[tar_comp$Date=="17/02/2015" & tar_comp$Tar<=37,]$NestID)]

summary(tar_comp[tar_comp$Date=="17/02/2015" & tar_comp$NestID %in% comparable_nests,]$Tar)

## !! DANGER !! ##
#nest_comp<-nest_comp[nest_comp$NestID %in% comparable_nests,]
## !! DANGER !! ##

##### !!!!!!!!!!!!!

## bootstrapping
bs1000_tl<-data.frame(tLength=seq(1:25))
bs1000_Rtl<-data.frame(tLength=seq(1:25))
bs1000_Ntl<-expand.grid(tLength=seq(1:25), NestID=unique(nest_comp$NestID))
mean_tl<-data.frame(NestID=unique(nest_comp$NestID))

for(h in 1:1000)
  
{
  out<-lapply(nest_comp[nest_comp$LW_corr=="D",]$NestID, FUN=feed_fun)  
  
  nest_comp[nest_comp$LW_corr=="D",]$LW_assn<-unlist(out)[seq(1,(length(unlist(out))-1),2)]
  nest_comp[nest_comp$LW_corr=="D",]$RW_assn<-unlist(out)[seq(2,length(unlist(out)),2)]
  
  all_trips<-NULL
  for(i in unique(nest_comp$NestID))
  {
    for(j in c("LW", "RW"))
      {  
      if(longallow==TRUE)
        {
          backs<-NULL
          if(j=="LW")
          {
            backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_assn=="B")
           if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]))
             {
              backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1,]$LW_assn=="B")[length(backs)+1])
              backs<-na.omit(backs)
             }
          }
          
          if(j=="RW")
          {
            backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_assn=="B")
           if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]))
             {
              backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1,]$RW_assn=="B")[length(backs)+1])
              backs<-na.omit(backs)
             }
          }
        }
        
      if(longallow==FALSE)
        {  
        backs<-NULL
        if(j=="LW")
          {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_assn=="B")}
        if(j=="RW")
          {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_assn=="B")}
        }
        
        trip_lz<-NULL  
        for(k in 2:length(backs))
          {
          tl1<-backs[k]-backs[k-1]
          trip_lz<-c(trip_lz, tl1) 
          }
        df_internal<-data.frame(NestID=i, BirdID=j, tLength=trip_lz)
        all_trips<-rbind(all_trips, df_internal)
      }
  }
  #
  agg1<-aggregate(tLength~NestID, all_trips,  FUN=mean)
  
  
  # to calc hists per individual nests (pairs) %%$$
  nest_propz<-NULL
  tripPropN<-expand.grid(tLength=seq(1:25), NestID=unique(nest_comp$NestID), tProp=0)
  for(i in unique(all_trips$NestID))
  {
    if(sum(all_trips[all_trips$NestID==i,]$tLength, na.rm=T)==0){next} # skips nest on rare occasion that both birds are NA
    df1<-data.frame(NestID=i,table(na.omit(all_trips[all_trips$NestID==i,]$tLength))) # removes NA values for birds in small time selections
    df1$Var1<-as.numeric(as.character(df1$Var1))
    df1$TotTrip<-df1$Var1 * df1$Freq 
    df1$TripProp<-df1$TotTrip/sum(df1$TotTrip)
    nest_propz<-rbind(nest_propz, df1)
    
    for(j in unique(df1$Var1))
      {
      tripPropN[tripPropN$NestID==i & tripPropN$tLength==j,]$tProp<-df1[df1$Var1==j,]$TripProp
      
      }
    
  }
  # %%$$
  
  # to calc hists per individual birds ~~**
  all_trips$BirdID2<-paste(all_trips$NestID, all_trips$BirdID,sep="_")
  trip_propz<-NULL
  for(i in unique(all_trips$BirdID2))
  {
    if(is.na(sum(all_trips[all_trips$BirdID2==i,]$tLength))){next} # this next catches any NA birds for whatever reason and removes. corrects warning created in max(backs) above
    df1<-data.frame(BirdID2=i,table(all_trips[all_trips$BirdID2==i,]$tLength))
    df1$Var1<-as.numeric(as.character(df1$Var1))
    df1$TotTrip<-df1$Var1 * df1$Freq 
    df1$TripProp<-df1$TotTrip/sum(df1$TotTrip)
    #print(i);print(sum(df1$TotTrip))
    trip_propz<-rbind(trip_propz, df1)
  }
  
  #length(unique(trip_propz$BirdID2)) #shows how many birds are monitored
  tripPropB<-NULL
  for(i in 1:25)
  { 
    df1<-data.frame(tLength=i,
                    tFreq=sum(trip_propz[trip_propz$Var1==i,]$Freq),
                    tProp=sum(trip_propz[trip_propz$Var1==i,]$TripProp)/length(unique(trip_propz$BirdID2)))
    tripPropB<-rbind(tripPropB, df1)
  }
  # ~~**
  
  
  # compile birds bootstrap proportion
  bs1000_tl<-cbind(bs1000_tl, tripPropB[,2] )
  names(bs1000_tl)[names(bs1000_tl)=="tripPropB[, 2]"]<-paste("run", h, sep="_")
  
  # compile birds bootstrap raw
  bs1000_Rtl<-cbind(bs1000_Rtl, tripPropB[,3] )
  names(bs1000_Rtl)[names(bs1000_Rtl)=="tripPropB[, 3]"]<-paste("run", h, sep="_")
  
  # compile nests bootstrap
  bs1000_Ntl<-cbind(bs1000_Ntl, tripPropN[,3] )
  names(bs1000_Ntl)[names(bs1000_Ntl)=="tripPropN[, 3]"]<-paste("run", h, sep="_")
 
  #compile mean trips per nest
  mean_tl<-cbind(mean_tl,agg1[,2])
  names(mean_tl)[names(mean_tl)=="agg1[, 2]"]<-paste("run", h, sep="_")
  print(h)
}


#plot all birds

bs_smry<-data.frame(tLength=seq(1:25))
bs_smry$mean_prop<-apply(bs1000_tl[,2:1001],1,mean, na.rm=T)
bs_smry$sd_prop<-apply(bs1000_tl[,2:1001],1,sd, na.rm=T)

bs_smry[bs_smry$mean_prop==0,]$sd_prop<-NA #makes graph neater 
bs_smry[bs_smry$mean_prop==0,]$mean_prop<-NA

limmy<-max(na.omit(bs_smry)$tLength)+1

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0, limmy), breaks=seq(1,limmy))+
  theme_classic()+ylab("Mean time spent foraging (prop)")+xlab("Duration of foraging trip (days)")

ggsave(paste("LHI16_foraging",D1, D2, "longallow", longallow, "feed_fun_SINGLE.png", sep="_")) #change feed_fun manually!

#plot individual nests

bs_Nsmry<-bs1000_Ntl[,1:2]
bs_Nsmry$mean_prop<-apply(bs1000_Ntl[,3:1002],1,mean, na.rm=T)
bs_Nsmry$sd_prop<-apply(bs1000_Ntl[,3:1002],1,sd, na.rm=T)

bs_Nsmry[bs_Nsmry$mean_prop==0,]$sd_prop<-NA #makes graph neater 
bs_Nsmry[bs_Nsmry$mean_prop==0,]$mean_prop<-NA#makes graph neater 

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_Nsmry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(limits=c(0,limmy), breaks=seq(1,limmy))+
  theme_bw()+ylab("Mean time spent foraging (prop)")+xlab("Duration of foraging trip (days)")+facet_wrap(~NestID)

ggsave(paste("LHI15_Nest_foraging",D1, D2, "longallow", longallow, "feed_fun_SINGLE_BIGTAR.png", sep="_"), scale=2) #change feed_fun manually!

###for wsc2 presentation

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="gray75", colour="black", width=0.6)+
  geom_errorbar(limits, width=0.3)+
  scale_x_continuous(expand=c(0,0),limits=c(0, 12.5), breaks=seq(1,13))+
  scale_y_continuous(expand=c(0,0))+
  theme_classic()+ylab("")+xlab("")+theme(
  axis.text.y=element_text(size = rel(1)),axis.text.x=element_blank())

ggsave(paste("WSCpres_LHI15_Nest_foraging",D1, D2, "longallow", longallow, "feed_fun_SINGLE.png", sep="_"), scale=2) #change feed_fun manually!



# Test whether chick size (tarsus) effects foraging strategy

tarsus<-read_excel("D:/research/phd/fieldwork/LHI_Feb_2015/data/Chick_data_2015.xlsx", 2 ,
                   skip=2, col_names=T, col_types=c("character", 
                                                    rep("numeric", 59)))

tar_tl<-data.frame(NestID=mean_tl[,1])
tar_tl$bs_mean_tl<-apply(mean_tl[,2:length(mean_tl)],1,mean, na.rm=T)
tar_tl$bs_sd_tl<-apply(mean_tl[,2:length(mean_tl)],1,sd, na.rm=T)
temp_tar<-tarsus[tarsus$NestID %in% unique(tar_tl$NestID),]
tar_tl$mean_tar<-rowMeans(temp_tar[,2:length(temp_tar)], na.rm=T)

limits <- aes(ymax = bs_mean_tl + bs_sd_tl, ymin=bs_mean_tl - bs_sd_tl)
pp<-ggplot(tar_tl, aes(x=mean_tar, y=bs_mean_tl))
pp+geom_point()+geom_errorbar(limits, width=0.5)

## Compare Peck 2004 and my 2015 tarsus data (for end of Feb overlap)

peck_tar<-read.csv("D:/research/phd/sourced_data/IH_DP_moonphase/peck2004_ck_tar_wgt.csv", h=T)

peck_tar<-peck_tar[with(peck_tar, Date=="16/02/2004" | Date=="20/02/2004" | Date=="24/02/2004" |Date=="28/02/2004"),]

tarsus<-tarsus[,which(colSums(tarsus, na.rm=TRUE)>0)]
library(reshape2)
miller_tar<-melt(tarsus, id.vars="NestID") #nice: melt sorts by NestID so tarsus measures now in chrono order :)

tar_comp_peck<-data.frame(NestID=as.character(peck_tar$NestID), Date=peck_tar$Date, Tar=peck_tar$tarsus, Year="2004")

tar_comp_miller<-data.frame(NestID=as.character(miller_tar[1:264,]$NestID), Date=miller_tar[1:264,]$variable, Tar=miller_tar[1:264,]$value, Year="2015")

tar_comp<-rbind(tar_comp_peck, tar_comp_miller)

tar_comp$DateGroup<-substr(tar_comp$Date, 2,2)
tar_comp[tar_comp$Year=="2015",]$DateGroup<-as.character(
  as.numeric(tar_comp[tar_comp$Year=="2015",]$DateGroup)-1)

tar_comp[tar_comp$Date=="01/03/2015",]$DateGroup<-as.character(8)

tar_comp[tar_comp$DateGroup=="6",]$DateGroup<-"16"
tar_comp[tar_comp$DateGroup=="0",]$DateGroup<-"20"
tar_comp[tar_comp$DateGroup=="4",]$DateGroup<-"24"
tar_comp[tar_comp$DateGroup=="8",]$DateGroup<-"28"

library(ggplot2)

p<-ggplot(tar_comp, aes(x=DateGroup, y=Tar, colour=Year))
p+geom_boxplot()+xlab("Day in February")+ylab("Tarsus length (mm)")+theme_classic()

ggsave("D:/research/phd/analyses/foraging_strategy/tarsus_comp1_04_15.png") #change feed_fun manually!


p<-ggplot(tar_comp, aes(x=Year, y=Tar, colour=DateGroup))
p+geom_boxplot()+ylab("Tarsus length (mm)")+labs(colour="Day in February")+theme_classic()

ggsave("D:/research/phd/analyses/foraging_strategy/tarsus_comp2_04_15.png") #change feed_fun manually!
write.csv(tar_comp, "D:/research/phd/analyses/foraging_strategy/tarsus_comp_04_15.csv", quote=F, row.names=F)

### dicking around
library("dplyr")
library("reshape2")

m1<-melt(bs1000_tl, id.var=c('tLength'))

ggplot(m1, aes(y=value, x=tLength))+
  geom_line(,alpha=0.01, show_guide=F)+
  geom_line(data=m1, aes(y=Median), alpha=1) + 
  theme_classic()+theme(legend.position=c(1,1), legend.justification=c(1,1))

