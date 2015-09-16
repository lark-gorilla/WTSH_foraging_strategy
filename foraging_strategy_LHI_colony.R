library(readxl)
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

ck_death<-data.frame(NestId=weight$NestID, Survival=names(weight)[apply(weight, 1, FUN=function(x) max(which(is.na(x)!=TRUE)))])

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

m1<-glm(Feeder~Meal_size, data=meal_hist_dat, family=binomial)

## need equal numbers of each class for tree to work well.. hmm maybe the higher numbers in the

d1<-data.frame(Feeder="both", Meal_size= meal_mod_dat[meal_mod_dat$Feeder=="both",]$Meal_size)
d1<-rbind(d1,data.frame(Feeder="single", Meal_size=sample(meal_mod_dat[meal_mod_dat$Feeder=="single",]$Meal_size, 80)))

t2<-tree(Feeder~Meal_size, data=d1)
d1$tree_pred<-predict(t2, d1)

m2<-glm(Feeder~Meal_size, data=d1, family=binomial)
d1$glm_pred<-predict(m2, d1, type="response")

sum(resid(m2, type="pearson")^2)/158

## t2 and m2 are simple models, to make more robust bootstrap with more randomly sampled 80 'single' datapoints
## however is fine for what we want (helping decide if both or only one adult fed the ck)





