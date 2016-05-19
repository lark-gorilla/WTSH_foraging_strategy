# foragingStrat function
# Mark Miller 04/05/2016
# Function creates dataframe for plotting the foraging strategy of a sampled seabird colony. The function
# uses the date when each individual of a breeding pair returned to the chick to create a histogram of trip length 
# for the whole colony. Where it is unknown which indivudual returned to feed the chick, the function randomly
# ONE individual of the pair, for each nest this process is bootstrapped 1000 times (default can be changed). The funtion outputs the
# raw histogram data for all birds (binned by trip length/day) and also the proportion of time that each bird 
# spent undertaking those trips (a longer trip carries more weight proportionally)

foragingStrat<-function(nest_comp=mynestdata, D1="2016-02-05"
                        ,D2="2016-03-12", longallow=FALSE, Nruns=1000, remUnkowns=FALSE)
{

## create simple predictive model to predict when feeding by both adults occurs
#meal_mod_dat<-rbind(data.frame(Feeder="both", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="B" & nest_comp$Chick=="Fed",]$diff),
#                    data.frame(Feeder="single", Meal_size =nest_comp[nest_comp$LW_corr=="B" & nest_comp$RW_corr=="A" & nest_comp$Chick=="Fed",]$diff),
#                    data.frame(Feeder="single", Meal_size =nest_comp[nest_comp$RW_corr=="B" & nest_comp$LW_corr=="A" & nest_comp$Chick=="Fed",]$diff))
#m1<-glm(Feeder~Meal_size, data=meal_mod_dat, family=binomial)

# function for single and potential both adult feeds
#fun_both<-function(x) 
#{ 
#  
#  if(is.na(x)){LWO<-"NA"; RWO<-"NA"}else{
#    
#    Z1<-sample(c("single", "both"),1, prob=
#                 c(predict(m1, newdata=data.frame(Meal_size=x), type="response"),1-predict(m1, newdata=data.frame(Meal_size=x), type="response"))) 
#    if(Z1=="single")
#    {LWO<-sample(c("B", "A"),1,)
#    if(LWO=="B"){RWO<-"A"}else{RWO<-"B"}}
#    
#    if(Z1=="both")
#    {LWO<-"B"; RWO<-"B"}}
#  
#  return(c(LWO, RWO))}

# function for single adult feeds
fun_single<-function(x) 
{ 
  LWO<-sample(c("B", "A"),1,)
  if(LWO=="B"){RWO<-"A"}else{RWO<-"B"}
  
  return(c(LWO, RWO))}

#### !!! Always run function from here !!! to reset all data.frames !!!

### Parameters

feed_fun=fun_single


##### !!!!!!!!!!!!!

## bootstrapping
bs1000_tl<-data.frame(tLength=seq(1:25))
bs1000_Rtl<-data.frame(tLength=seq(1:25))
bs1000_Ntl<-expand.grid(tLength=seq(1:25), NestID=unique(nest_comp$NestID))
mean_tl<-data.frame(NestID=unique(nest_comp$NestID))

for(h in 1:Nruns)
  
{
  out<-lapply(nest_comp[nest_comp$LW_corr=="D",]$NestID, FUN=feed_fun)  
  
  nest_comp[nest_comp$LW_corr=="D",]$LW_assn<-unlist(out)[seq(1,(length(unlist(out))-1),2)]
  nest_comp[nest_comp$LW_corr=="D",]$RW_assn<-unlist(out)[seq(2,length(unlist(out)),2)]
  
  all_trips<-NULL
  for(i in unique(nest_comp$NestID))
  {
    D1_mod<-nest_comp[nest_comp$NestID==i,]$Date[min( min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$LW_corr=="B")), 
                 min(which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_corr=="B")))]
    
    for(j in c("LW", "RW"))
    {  
      if(longallow==TRUE)
      {
        backs<-NULL
        if(j=="LW")
        {
          backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")
          if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
          {
            backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$LW_assn=="B")[length(backs)+1])
            backs<-na.omit(backs)
          }
        }
        
        if(j=="RW")
        {
          backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")
          if(max(backs)<nrow(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]))
          {
            backs<-c(backs, which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod,]$RW_assn=="B")[length(backs)+1])
            backs<-na.omit(backs)
          }
        }
      }
      
      if(longallow==FALSE)
      {  
        backs<-NULL
        if(j=="LW")
        {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$LW_assn=="B")}
        if(j=="RW")
        {backs<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1_mod & nest_comp$Date<=D2,]$RW_assn=="B")}
      }
      
      if(remUnkowns=FALSE)
        {
        trip_lz<-diff(backs)
        }
      
      if(remUnkowns=TRUE)
      {
        unknownz<-which(nest_comp[nest_comp$NestID==i& nest_comp$Date>=D1 & nest_comp$Date<=D2,]$RW_corr=="D")
        trip_lz<-NULL  
        for(k in 2:length(backs))
        {
          if(TRUE %in% (unknownz %in% backs[k-1]:backs[k])){next}
          tl1<-backs[k]-backs[k-1]
          trip_lz<-c(trip_lz, tl1) 
        }
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
  bs1000_tl<-cbind(bs1000_tl, tripPropB[,3] )
  names(bs1000_tl)[names(bs1000_tl)=="tripPropB[, 3]"]<-paste("run", h, sep="_")
  
  # compile birds bootstrap raw
  bs1000_Rtl<-cbind(bs1000_Rtl, tripPropB[,2] )
  names(bs1000_Rtl)[names(bs1000_Rtl)=="tripPropB[, 2]"]<-paste("run", h, sep="_")
  
  # compile nests bootstrap
  bs1000_Ntl<-cbind(bs1000_Ntl, tripPropN[,3] )
  names(bs1000_Ntl)[names(bs1000_Ntl)=="tripPropN[, 3]"]<-paste("run", h, sep="_")
  
  #compile mean trips per nest
  mean_tl<-cbind(mean_tl,agg1[,2])
  names(mean_tl)[names(mean_tl)=="agg1[, 2]"]<-paste("run", h, sep="_")
  print(h)
}

return(list(bs1000_tl=bs1000_tl, bs1000_Rtl=bs1000_Rtl, bs1000_Ntl=bs1000_Ntl, mean_tl=mean_tl))
}

