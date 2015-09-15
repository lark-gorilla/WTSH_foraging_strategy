library(ggplot2)
library(lubridate)

setwd("~/grive/phd/analyses/foraging_strategy")

# NEST COMP RECEIVED SOME MANUAL CLEANING IN CALC NAs in ck weight section changed to 0s
nest_comp<-read.csv("LHI_2015_nest_weights_attendance_cleaned.csv", h=T, strip.white=T)
nest_comp$Date<-ymd(nest_comp$Date)

nest_comp<-nest_comp[nest_comp$NestID!=48,]
nest_comp<-nest_comp[nest_comp$NestID!=51,]
nest_comp<-nest_comp[nest_comp$NestID!=38,]
nest_comp<-nest_comp[nest_comp$NestID!=14,]

## create simple predictive model to predict when feeding by both adults occurs
meal_mod_dat<-rbind(data.frame(Feeder="both", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="B" & nest_comp$Chick=="Fed",]$diff),
                    data.frame(Feeder="single", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="A" & nest_comp$Chick=="Fed",]$diff),
                    data.frame(Feeder="single", Meal_size =nest_comp[nest_comp$RW_corr=="B" & nest_comp$LW_corr=="A" & nest_comp$Chick=="Fed",]$diff))


m1<-glm(Feeder~Meal_size, data=meal_hist_dat, family=binomial)


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

### Use this to set specific dates to test temporal foraging strat changes

D1="2015-02-18"
D2="2015-03-01"  

#nest_comp<-nest_comp[nest_comp$Date<"2015-03-02",] #just 2 weeks in Feb
#nest_comp<-nest_comp[nest_comp$Date<"2015-03-19",] # allows 1 month from mid feb start (for long trips to get back)
#nest_comp<-nest_comp[nest_comp$Date>"2015-03-10",] # allows 1 month from mid march to apr

## bootstrapping
bs1000_tl<-data.frame(tLength=seq(1:25))
for(h in 1:1000)
  
{
  out<-lapply(nest_comp[nest_comp$LW_corr=="D",]$diff, FUN=fun_single)  
  
  nest_comp[nest_comp$LW_corr=="D",]$LW_assn<-unlist(out)[seq(1,(length(unlist(out))-1),2)]
  nest_comp[nest_comp$LW_corr=="D",]$RW_assn<-unlist(out)[seq(2,length(unlist(out)),2)]
  
  all_trips<-NULL
  for(i in unique(nest_comp$NestID))
  {
    for(j in c("LW", "RW")) # the below calc of backs allows trips that start within the time period but not end to be included
    {
      backs<-NULL
      if(j=="LW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_assn=="B")
       
       if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]))
       {backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1,]$LW_assn=="B")[length(backs)+1])
        backs<-na.omit(backs)}
      }
      
      if(j=="RW")
      {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_assn=="B")
       
       if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]))
       {backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1,]$RW_assn=="B")[length(backs)+1])
        backs<-na.omit(backs)}
      }
      
      #if(j=="LW")
      #{backs<-which(nest_comp[nest_comp$NestID==i,]$LW_assn=="B")}
      #if(j=="RW")
      #{backs<-which(nest_comp[nest_comp$NestID==i,]$RW_assn=="B")}
      
      
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
  
  length(unique(trip_propz$BirdID2))
  #shows how many birds are monitored
  
  tripPropB<-NULL
  for(i in 1:25)
  { 
    df1<-data.frame(tLength=i, tProp=sum(trip_propz[trip_propz$Var1==i,]$TripProp)/length(unique(trip_propz$BirdID2)))
    tripPropB<-rbind(tripPropB, df1)
  }
  
  bs1000_tl<-cbind(bs1000_tl, tripPropB[,2] )
  
  names(bs1000_tl)[names(bs1000_tl)=="tripPropB[, 2]"]<-paste("run", h, sep="_")
  print(h)
}

bs1000_tl[bs1000_tl==0]<-NA

bs_smry<-data.frame(tLength=seq(1:25))
bs_smry$mean_prop<-apply(bs1000_tl[,2:1001],1,mean, na.rm=T)
bs_smry$sd_prop<-apply(bs1000_tl[,2:1001],1,sd, na.rm=T)

bs_smry<-na.omit(bs_smry)

limits <- aes(ymax = mean_prop + sd_prop, ymin=mean_prop - sd_prop)
pp<-ggplot(bs_smry, aes(x=tLength, y=mean_prop))
pp+geom_bar(stat="identity", fill="dark grey", colour="black")+
  geom_errorbar(limits, width=0.5)+
  scale_x_continuous(breaks=seq(1:25))+
  theme_classic()+ylab("Mean time spent forgaing (prop)")+xlab("Duration of foraging trip (days)")

ggsave("LHI_foraging_mar_apr_15_single_feed_assn.png")
