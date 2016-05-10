rm(list=ls())
library(readxl)
library(ggplot2)
library(lubridate)

setwd("~/grive/phd/analyses/foraging_strategy")

###############################################################################################
##### THIS SCRIPT COMPILES ATTENDENCE DATA CHICK DATA from the long-term Heron Island and #####
##### Lord Howe datasets                                                                  #####
##### Nests are removed from compilation if the chick dies before the endof the sampling  #####
##### period or if there is too many unkown feeds or seemingly > 2 adults feeding a chick #####
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
heron_01<-heron_01[heron_01$NestID!="11",] 
heron_01<-heron_01[heron_01$NestID!="19-23",] 
heron_01<-heron_01[heron_01$NestID!="23-19",] 
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

# At this point see if there are some pairs that have more than
# 2 adults working, modify original data accordingly in excel
aggregate(LW~NestID, data=nest_comp, FUN=function(x){unique(x)})

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

write.csv(date_comp, "R_analyses_data/Heron_2001_nest_attendance_cleaned.csv", row.names=F, quote=F)

## Heron 2001
# I have inserted a dummy row at top to force Nest as a factor, otherwise any chacter
# Nests e.g. C1 are set to NA 
heron_02<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2002-Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_02<-heron_02[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_02<-data.frame(NestID=heron_02$Nest, DateTime=as.POSIXlt((heron_02$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     DateTime_hack=as.POSIXlt((heron_02$'date/time'+0.5)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_02$'chick wt (g)', ck_cul=heron_02$'ch cul (mm)', ck_tar=heron_02$'ch tarsus',
                     AdultID=heron_02$'ad1 band #', Adult_weight=heron_02$'ad1 wt (g)')

#The datetime hack shifts time forward by 12 hours effectively changing the date at 12 midday 
#rather than middnight, this is more useful for assiging feeds per date,

heron_02<-heron_02[-which(is.na(heron_02$DateTime)),] #remove NAs

unique(heron_02$NestID)

# OUT NestIDs

bad<-c("C1", "C2", "C3", "C4", "C5", "C6", "C7", "unknown",
       "Vag", "Vag 1", "Vag 3", "Vag 8", "Vag 9", "Vag 11",
       "Vag 12", "Vag 13", "Vag 14", "9", "4", "2") # 2002 such a 
  # crap year better to see which nests are good!

# few good nests, here were only keeping nests that raised a ck for ~30days
good<-c("1.000000", "3.000000", "5.000000", "6.000000", "7.000000", "12.000000")
#heron_02<-heron_02[!heron_02$NestID %in% bad,] #remove bad nests
heron_02<-heron_02[heron_02$NestID %in% good,] #keep only good nests


nest_comp<-data.frame(NestID=heron_02$NestID, DateTime=heron_02$DateTime, 
                      DateTime_hack=heron_02$DateTime_hack, AdultID=heron_02$AdultID, LW=heron_02$AdultID, RW=heron_02$AdultID) 

nest_comp$LW<-as.character(heron_02$AdultID)
nest_comp$RW<-as.character(heron_02$AdultID)

nest_comp[which(is.na(nest_comp$LW)),]$LW<-"A"
nest_comp[which(is.na(nest_comp$RW)),]$RW<-"A"

nest_comp[nest_comp$LW=="?",]$LW<-"D"
nest_comp[nest_comp$RW=="?",]$RW<-"D"

# At this point see if there are some pairs that have more than
# 2 adults working, modify original data accordingly in excel
aggregate(LW~NestID, data=nest_comp, FUN=function(x){unique(x)})

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

write.csv(date_comp, "R_analyses_data/Heron_2002_nest_attendance_cleaned.csv", row.names=F, quote=F)


## Heron 2003
# I have inserted a dummy row at top to force Nest as a factor, otherwise any chacter
# Nests e.g. C1 are set to NA 
heron_03<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2003 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_03<-heron_03[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_03<-data.frame(NestID=heron_03$Nest, DateTime=as.POSIXlt((heron_03$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     DateTime_hack=as.POSIXlt((heron_03$'date/time'+0.5)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_03$'chick wt (g)', ck_cul=heron_03$'ch cul (mm)', ck_tar=heron_03$'ch tarsus',
                     AdultID=heron_03$'ad1 band #', Adult_weight=heron_03$'ad1 wt (g)')

#The datetime hack shifts time forward by 12 hours effectively changing the date at 12 midday 
#rather than middnight, this is more useful for assiging feeds per date,

heron_03<-heron_03[-which(is.na(heron_03$DateTime)),] #remove NAs

unique(heron_03$NestID)

# OUT NestIDs

bad<-c("c1", "c2", "c4", "c5", "c6", "24/E4",
       "21.000000", "10.000000") # these nests chick died before end of study

heron_03<-heron_03[!heron_03$NestID %in% bad,] #remove bad nests



nest_comp<-data.frame(NestID=heron_03$NestID, DateTime=heron_03$DateTime, 
                      DateTime_hack=heron_03$DateTime_hack, AdultID=heron_03$AdultID, LW=heron_03$AdultID, RW=heron_03$AdultID) 

nest_comp$LW<-as.character(heron_03$AdultID)
nest_comp$RW<-as.character(heron_03$AdultID)

nest_comp[which(is.na(nest_comp$LW)),]$LW<-"A"
nest_comp[which(is.na(nest_comp$RW)),]$RW<-"A"

nest_comp[nest_comp$LW=="?",]$LW<-"D"
nest_comp[nest_comp$RW=="?",]$RW<-"D"

# At this point see if there are some pairs that have more than
# 2 adults working, modify original data accordingly in excel
aggregate(LW~NestID, data=nest_comp, FUN=function(x){unique(x)})

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

write.csv(date_comp, "R_analyses_data/Heron_2003_nest_attendance_cleaned.csv", row.names=F, quote=F)

## Heron 2006
# I have inserted a dummy row at top to force Nest as a factor, otherwise any chacter
# Nests e.g. C1 are set to NA 
heron_06<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2006 Heron nesting-karlina.xls", 1 ,
                     skip=0, col_names=T)

heron_06<-heron_06[-1,] #removes dummy row

# I have modified and saved the xlsx in libreoffice so use
heron_06<-data.frame(NestID=heron_06$Nest, DateTime=as.POSIXlt((heron_06$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     DateTime_hack=as.POSIXlt((heron_06$'date/time'+0.5)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_06$'chick wt (g)', ck_cul=heron_06$'ch cul (mm)', ck_tar=heron_06$'ch tarsus',
                     AdultID=heron_06$'ad1 band #', Adult_weight=heron_06$'ad1 wt (g)')

#The datetime hack shifts time forward by 12 hours effectively changing the date at 12 midday 
#rather than middnight, this is more useful for assiging feeds per date,

heron_06<-heron_06[-which(is.na(heron_06$DateTime)),] #remove NAs

unique(heron_06$NestID)

# OUT NestIDs


# for some reason most nests seem to have no atten data for last half of month, here we only select the 
# good ones and keep for further analyses
good<-c( "5.000000", "26.000000", "46.000000", "49.000000", "55.000000", "60.000000")
#heron_06<-heron_06[!heron_06$NestID %in% bad,] #remove bad nests
heron_06<-heron_06[heron_06$NestID %in% good,] #keep only good nests


nest_comp<-data.frame(NestID=heron_06$NestID, DateTime=heron_06$DateTime, 
                      DateTime_hack=heron_06$DateTime_hack, AdultID=heron_06$AdultID, LW=heron_06$AdultID, RW=heron_06$AdultID) 

nest_comp$LW<-as.character(heron_06$AdultID)
nest_comp$RW<-as.character(heron_06$AdultID)

nest_comp[which(is.na(nest_comp$LW)),]$LW<-"A"
nest_comp[which(is.na(nest_comp$RW)),]$RW<-"A"

nest_comp[nest_comp$LW=="?",]$LW<-"D"
nest_comp[nest_comp$RW=="?",]$RW<-"D"

# At this point see if there are some pairs that have more than
# 2 adults working, modify original data accordingly in excel
aggregate(LW~NestID, data=nest_comp, FUN=function(x){unique(x)})

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

write.csv(date_comp, "R_analyses_data/Heron_2006_nest_attendance_cleaned.csv", row.names=F, quote=F)

## Heron 2010
# I have inserted a dummy row at top to force Nest as a factor, otherwise any chacter
# Nests e.g. C1 are set to NA 
heron_10<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2010 Heron nesting-karlina.xlsx", 1 ,
                     skip=0, col_names=T)

heron_10<-heron_10[-1,] #removes dummy row

# In 2010 there are some differences in col names and data available
heron_10<-data.frame(NestID=heron_10$Nest, DateTime=as.POSIXlt((heron_10$'date/time')* 86400, origin="1899-12-30", tz="UTC"),
                     DateTime_hack=as.POSIXlt((heron_10$'date/time'+0.5)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_10$'chick wt (g)', ck_cul=NA, ck_tar=heron_10$'chick tarsus (mm)',
                     AdultID=heron_10$'visiting adult ID.', Adult_weight=heron_10$'adult weight')

#The datetime hack shifts time forward by 12 hours effectively changing the date at 12 midday 
#rather than middnight, this is more useful for assiging feeds per date,

heron_10<-heron_10[-which(is.na(heron_10$DateTime)),] #remove NAs

unique(heron_10$NestID)


# A few nests had chicks die, in total there werent many nests that had good atten data
good<-c(12, 15, 21, 22, 26, 27, 28)
#heron_10<-heron_10[!heron_10$NestID %in% bad,] #remove bad nests
heron_10<-heron_10[heron_10$NestID %in% good,] #keep only good nests


nest_comp<-data.frame(NestID=heron_10$NestID, DateTime=heron_10$DateTime, 
                      DateTime_hack=heron_10$DateTime_hack, AdultID=heron_10$AdultID, LW=heron_10$AdultID, RW=heron_10$AdultID) 

nest_comp$LW<-as.character(heron_10$AdultID)
nest_comp$RW<-as.character(heron_10$AdultID)

nest_comp[which(is.na(nest_comp$LW)),]$LW<-"A"
nest_comp[which(is.na(nest_comp$RW)),]$RW<-"A"

nest_comp[nest_comp$LW=="?",]$LW<-"D"
nest_comp[nest_comp$RW=="?",]$RW<-"D"

# At this point see if there are some pairs that have more than
# 2 adults working, modify original data accordingly in excel
aggregate(LW~NestID, data=nest_comp, FUN=function(x){unique(x)})

# kill dates > 13 march as theyre not sampled
nest_comp<-nest_comp[nest_comp$DateTime<"2010-03-13 00:00:00",]

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

write.csv(date_comp, "R_analyses_data/Heron_2010_nest_attendance_cleaned.csv", row.names=F, quote=F)

#2012!!
# I created 2 columns in the spreadsheet to calc the unkown feeds. This assigns the feed
# to the date that is d-1 the observed chick increase
heron_12<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2012 nest records.xlsx", 1 ,
                     skip=0, col_names=T)

heron_12<-heron_12[-1,] #removes dummy row

# change time NA to 0.7 ~ 16:00
heron_12[is.na(heron_12$TIME),]$TIME<-0.7

# In 2012 there are some differences in col names and data available
heron_12<-data.frame(NestID=heron_12$Nest, DateTime=as.POSIXlt((heron_12$DATE+heron_12$TIME)* 86400, origin="1899-12-30", tz="UTC"),
                     DateTime_hack=as.POSIXlt((heron_12$DATE+heron_12$TIME+0.5)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_12$'CHICK WT (g)', ck_cul=NA, ck_tar=heron_12$'CHICK TAR (mm)',
                     AdultID=heron_12$'band and unknown', Adult_weight=heron_12$'Net adult weight (g)')

#The datetime hack shifts time forward by 12 hours effectively changing the date at 12 midday 
#rather than middnight, this is more useful for assiging feeds per date,

#heron_12<-heron_12[-which(is.na(heron_12$DateTime)),] #remove NAs

unique(heron_12$NestID)


# Many nests have way too many unknowns, try and select those which have decent recording
good<-c("13", "13A", "15", "16", "18", "19", "31", "32", "37", "48")
#heron_12<-heron_12[!heron_12$NestID %in% bad,] #remove bad nests
heron_12<-heron_12[heron_12$NestID %in% good,] #keep only good nests


nest_comp<-data.frame(NestID=heron_12$NestID, DateTime=heron_12$DateTime, 
                      DateTime_hack=heron_12$DateTime_hack, AdultID=heron_12$AdultID, LW=heron_12$AdultID, RW=heron_12$AdultID) 

nest_comp$LW<-as.character(heron_12$AdultID)
nest_comp$RW<-as.character(heron_12$AdultID)

nest_comp[which(is.na(nest_comp$LW)),]$LW<-"A"
nest_comp[which(is.na(nest_comp$RW)),]$RW<-"A"

nest_comp[nest_comp$LW=="?",]$LW<-"D"
nest_comp[nest_comp$RW=="?",]$RW<-"D"

# At this point see if there are some pairs that have more than
# 2 adults working, modify original data accordingly in excel
data.frame(aggregate(LW~NestID, data=nest_comp, FUN=function(x){sort(unique(x))}),
aggregate(LW~NestID, data=nest_comp, FUN=function(x){table(x)}))



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

write.csv(date_comp, "R_analyses_data/Heron_2012_nest_attendance_cleaned.csv", row.names=F, quote=F)

#2013!!
#this dataset is similar to 2012... with similar 'issues'
heron_13<-read_excel("~/grive/phd/sourced_data/Heron/Heron_nesting_data_summary/2013 nesting data.xlsx", 1 ,
                     skip=0, col_names=T)

heron_13<-heron_13[-1,] #removes dummy row

# change time NA to 0.7 ~ 16:00
heron_13[is.na(heron_13$TIME),]$TIME<-0.7

# In 2012 there are some differences in col names and data available
heron_13<-data.frame(NestID=heron_13$Nest, DateTime=as.POSIXlt((heron_13$DATE+heron_13$TIME)* 86400, origin="1899-12-30", tz="UTC"),
                     DateTime_hack=as.POSIXlt((heron_13$DATE+heron_13$TIME+0.5)* 86400, origin="1899-12-30", tz="UTC"),
                     ck_weight=heron_13$'Net chick wt (g)', ck_cul=NA, ck_tar=heron_13$'CHICK TARSUS(mm)',
                     AdultID=heron_13$'atten cleaned', Adult_weight=heron_13$'Net adult weight (g)')

#The datetime hack shifts time forward by 12 hours effectively changing the date at 12 midday 
#rather than middnight, this is more useful for assiging feeds per date,

#heron_13<-heron_13[-which(is.na(heron_13$DateTime)),] #remove NAs

unique(heron_13$NestID)


# Many nests have way too many unknowns, try and select those which have decent recording
bad<-c("1", "2", "4", "7", "8", "9", "10", "12", "14", "16", "17", "25","26", "29", "31",
       "48", "50", "52", "53", "58", "59", "60", "61", "66", "69", "70", "74", "75", 
       "76", "78", "81", "80", "83", "84", "85", "86", "87", "91", "92","93", "95", "97",
       "99", "2a", "47a", "9a")
heron_13<-heron_13[!heron_13$NestID %in% bad,] #remove bad nests



nest_comp<-data.frame(NestID=heron_13$NestID, DateTime=heron_13$DateTime, 
                      DateTime_hack=heron_13$DateTime_hack, AdultID=heron_13$AdultID, LW=heron_13$AdultID, RW=heron_13$AdultID) 

nest_comp$LW<-as.character(heron_13$AdultID)
nest_comp$RW<-as.character(heron_13$AdultID)

nest_comp[which(is.na(nest_comp$LW)),]$LW<-"A"
nest_comp[which(is.na(nest_comp$RW)),]$RW<-"A"

nest_comp[nest_comp$LW=="?",]$LW<-"D"
nest_comp[nest_comp$RW=="?",]$RW<-"D"

# At this point see if there are some pairs that have more than
# 2 adults working, modify original data accordingly in excel
data.frame(aggregate(LW~NestID, data=nest_comp, FUN=function(x){sort(unique(x))}),
           aggregate(LW~NestID, data=nest_comp, FUN=function(x){table(x)}))



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

write.csv(date_comp, "R_analyses_data/Heron_2013_nest_attendance_cleaned.csv", row.names=F, quote=F)
