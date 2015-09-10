library(xlsx)
library(ggplot2)
library(lubridate)

setwd("~/grive/phd/analyses/foraging_strategy")


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
