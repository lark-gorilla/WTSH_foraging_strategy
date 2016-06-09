# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #
# Attempt to apply HMM modelling to shearwater nest attendance data #
# 09/06/16                                                          #
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ #

rm(list=ls())

dat<-read.csv("~/grive/phd/analyses/foraging_strategy/R_analyses_data/Heron_2003_nest_attendance_cleaned.csv", h=T)
head(dat)
dat$Date_hack<-as.Date(dat$Date_hack, "%Y-%m-%d")
str(dat)

library(ggplot2)

# The below plot hopefully shows the issue with the unkowns for this nest (1)
# The timeseries starts with 'left wing' bird in green back at the nest each night
# while 'right wing' bird is on a forgaing trip away. On the 6th night there is
# an feed made by an unknown individual from the pair, if it was 'left-wing' then
# then the bird continues its earlier pattern of feeding each night but if it
# was 'right-wing' then this bird has returned from a 5 day foraging trip, we
# subsequently observe that the bird returns for sure on the 7th night.

p<-ggplot(data=dat[dat$NestID==1,], aes(x=Date_hack, y=LW_corr, group=1))
p+geom_point(colour="green")+geom_line(colour="green")+
  geom_point(data=dat[dat$NestID==1,], aes(x=Date_hack, y=RW_corr, group=1), colour="red")+
  geom_line(data=dat[dat$NestID==1,], aes(x=Date_hack, y=RW_corr, group=1), colour="red", linetype=2)+
  scale_y_discrete("Attendance class", labels=c("A"="Away", "B"= "Back", "D"="Unknown"))+
  labs(x="Date")

