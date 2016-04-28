rm(list=ls())
library(readxl)
library(ggplot2)
library(lubridate)

setwd("~/grive/phd/analyses/foraging_strategy")

###############################################################################################
##### THIS SCRIPT COMPILES ATTENDENCE DATA CHICK DATA from the long-term Heron Island and #####
##### Lord Howe datasets                                                                  #####
###############################################################################################

# get darrens Lord Howe data first
lhi_04<-read_excel("~/grive/phd/sourced_data/IH_DP_moonphase/WTSH 2004 Lord Howe_DP.xls", 1 ,
                  skip=0, col_names=T)


lhi_04<-data.frame(NestID=lhi_04$Nest, DateTime=as.POSIXlt((lhi_04$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                   DateTime_hack=as.POSIXlt((lhi_04$'date/time'+0.5)* 86400, origin="1899-12-30", tz="UTC"),
                   ck_weight=lhi_04$'chick wt (g)', ck_cul=lhi_04$'ch cul (mm)', ck_tar=lhi_04$'ch tarsus',
                   AdultID=lhi_04$'ad1 band #', Adult_weight=lhi_04$'ad1 wt (g)')

#The datetime hack shifts time forward by 12 hours effectively changing the date at 12 midday 
#rather than middnight, this is more useful for assiging feeds per date,

lhi_04<-lhi_04[-which(is.na(lhi_04$DateTime)),] #remove NAs
lhi_04<-lhi_04[lhi_04$NestID!="e2",] #remove egg nest

nest_comp<-data.frame(NestID=lhi_04$NestID, DateTime=lhi_04$DateTime, 
                      DateTime_hack=lhi_04$DateTime_hack, AdultID=lhi_04$AdultID, LW=lhi_04$AdultID, RW=lhi_04$AdultID) 

nest_comp$LW<-as.character(lhi_04$AdultID)
nest_comp$RW<-as.character(lhi_04$AdultID)

nest_comp[which(is.na(nest_comp$LW)),]$LW<-"A"
nest_comp[which(is.na(nest_comp$RW)),]$RW<-"A"

nest_comp[nest_comp$LW=="?",]$LW<-"D"
nest_comp[nest_comp$RW=="?",]$RW<-"D"

nest_clean<-NULL
for (i in na.omit(unique(nest_comp$NestID)))
    {
    t1<-nest_comp[nest_comp$NestID==i,]
    
    if(sum(as.integer(t1$AdultID), na.rm=T)==0){
      print(paste(i, " dropped as no atten data")); next}
    
     LWRW<-unique(t1$AdultID)[which(unique(t1$AdultID) !='?')]
     t1[which(t1$LW==LWRW[1]),]$LW<-"B"
     t1[which(t1$LW==LWRW[2]),]$LW<-"A"
     t1[which(t1$RW==LWRW[2]),]$RW<-"B"
     t1[which(t1$RW==LWRW[1]),]$RW<-"A"
     
     nest_clean<-rbind(nest_clean, t1)
     
     print(i)
    }

nest_clean$AdultID<-NULL
nest_clean<-na.omit(nest_clean)
nest_clean$Date_hack<-as.Date(nest_clean$DateTime_hack)

date_comp<-NULL
for (i in unique(nest_clean$NestID))
{
  d1<-nest_clean[nest_clean$NestID==i,]
  
  d1_comp<-NULL
  for(j in unique(d1$Date_hack))
      {
      d1_out<- d1[d1$Date_hack==j,][1,]
      d1_out$LW_corr<-"A"
      d1_out$RW_corr<-"A"
      
      if("D" %in% d1[d1$Date_hack==j,]$LW){d1_out$LW_corr<-"D"}
      if("B" %in% d1[d1$Date_hack==j,]$LW){d1_out$LW_corr<-"B"}
      
      if("D" %in% d1[d1$Date_hack==j,]$RW){d1_out$RW_corr<-"D"}
      if("B" %in% d1[d1$Date_hack==j,]$RW){d1_out$RW_corr<-"B"}
      
      d1_comp<-rbind(d1_comp, d1_out)
      }

  date_comp<-rbind(date_comp, d1_comp)
  
  print(i)
}


date_comp[with(date_comp, LW_corr=="D" & RW_corr=="B"),]$LW_corr<-"A"
date_comp[with(date_comp, LW_corr=="B" & RW_corr=="D"),]$RW_corr<-"A"
#this sets any nights where 1 bird was seen feeding and another or maybe the same bird fed again
# to keep the same as the 15 & 16 data we just say only the bird that was seen was there and 
#that the other bird did not return, bit harsh but their sampling was more intense than ours

date_comp$LW_assn<-date_comp$LW_corr
date_comp$RW_assn<-date_comp$RW_corr

fun_single<-function(x) 
{ 
  LWO<-sample(c("B", "A"),1,)
  if(LWO=="B"){RWO<-"A"}else{RWO<-"B"}
  
  return(c(LWO, RWO))}

out<-lapply(date_comp[date_comp$LW_corr=="D",]$LW, FUN=fun_single)  

date_comp[date_comp$LW_corr=="D",]$LW_assn<-unlist(out)[seq(1,(length(unlist(out))-1),2)]
date_comp[date_comp$LW_corr=="D",]$RW_assn<-unlist(out)[seq(2,length(unlist(out)),2)]

# writing out nest_comp
write.csv(date_comp, "R_analyses_data/LHI_2014_nest_attendance_cleaned.csv", row.names=F, quote=F)


### Parameters

D1="2014-02-05"
D2="2014-02-25" 
longallow=FALSE
feed_fun=fun_single


##### !!!!!!!!!!!!!

## bootstrapping
bs1000_tl<-data.frame(tLength=seq(1:25))
bs1000_Ntl<-expand.grid(tLength=seq(1:25), NestID=unique(nest_comp$NestID))
mean_tl<-data.frame(NestID=unique(nest_comp$NestID))

for(h in 1:1000)
  
{
  out<-lapply(nest_comp[nest_comp$LW_corr=="D",]$diff, FUN=feed_fun)  
  
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
    df1<-data.frame(tLength=i, tProp=sum(trip_propz[trip_propz$Var1==i,]$TripProp)/length(unique(trip_propz$BirdID2)))
    tripPropB<-rbind(tripPropB, df1)
  }
  # ~~**
  
  # compile birds bootstrap
  bs1000_tl<-cbind(bs1000_tl, tripPropB[,2] )
  names(bs1000_tl)[names(bs1000_tl)=="tripPropB[, 2]"]<-paste("run", h, sep="_")
  
  
  # compile nests bootstrap
  bs1000_Ntl<-cbind(bs1000_Ntl, tripPropN[,3] )
  names(bs1000_Ntl)[names(bs1000_Ntl)=="tripPropN[, 3]"]<-paste("run", h, sep="_")
  
  #compile mean trips per nest
  mean_tl<-cbind(mean_tl,agg1[,2])
  names(mean_tl)[names(mean_tl)=="agg1[, 2]"]<-paste("run", h, sep="_")
  print(h)
}


