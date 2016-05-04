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
lhi_04<-lhi_04[lhi_04$NestID!="13.000000",] # remove ck dead nest

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
write.csv(date_comp, "R_analyses_data/LHI_2004_nest_attendance_cleaned.csv", row.names=F, quote=F)

## Heron 2001
# I have inserted a dummy row at top to force Nest as a factor, otherwise any chacter
# Nests e.g. C1 are set to NA 
heron_01<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2001-Heron nesting-karlina.xlsx", 1 ,
                   skip=0, col_names=T)

heron_01<-heron_01[-1,] #removes dummy row


# If i don't save the heron xlsx then these work for the format
#DateTime=as.POSIXlt((heron_01$'date/time'), origin="1899-12-30", tz="UTC"),
#DateTime_hack=as.POSIXlt((heron_01$'date/time'+((60*60)*12))

# I have modified and saved the xlsx in libreoffice so use
heron_01<-data.frame(NestID=heron_01$Nest, DateTime=as.POSIXlt((heron_01$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                   DateTime_hack=as.POSIXlt((heron_01$'date/time'+0.5)* 86400, origin="1899-12-30", tz="UTC"),
                   ck_weight=heron_01$'chick wt (g)', ck_cul=heron_01$'ch cul (mm)', ck_tar=heron_01$'ch tarsus',
                   AdultID=heron_01$'ad1 band #', Adult_weight=heron_01$'ad1 wt (g)')

#The datetime hack shifts time forward by 12 hours effectively changing the date at 12 midday 
#rather than middnight, this is more useful for assiging feeds per date,

heron_01<-heron_01[-which(is.na(heron_01$DateTime)),] #remove NAs

unique(heron_01$NestID)

heron_01<-heron_01[heron_01$NestID!="17",] #remove bad nests
heron_01<-heron_01[heron_01$NestID!="19-23",] 
heron_01<-heron_01[heron_01$NestID!="19",] 
heron_01<-heron_01[heron_01$NestID!="21",] 
heron_01<-heron_01[heron_01$NestID!="23",] 
heron_01<-heron_01[heron_01$NestID!="24",] 
heron_01<-heron_01[heron_01$NestID!="A",] 
heron_01<-heron_01[heron_01$NestID!="c1",] 
heron_01<-heron_01[heron_01$NestID!="c2",] 
heron_01<-heron_01[heron_01$NestID!="c3",] 
heron_01<-heron_01[heron_01$NestID!="c4",] 
heron_01<-heron_01[heron_01$NestID!="c5",] 

nest_comp<-data.frame(NestID=heron_01$NestID, DateTime=heron_01$DateTime, 
                      DateTime_hack=heron_01$DateTime_hack, AdultID=heron_01$AdultID, LW=heron_01$AdultID, RW=heron_01$AdultID) 

nest_comp$LW<-as.character(heron_01$AdultID)
nest_comp$RW<-as.character(heron_01$AdultID)

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



