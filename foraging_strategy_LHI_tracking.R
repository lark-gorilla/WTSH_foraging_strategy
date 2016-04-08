rm(list=ls())

library(maps)
library(mapdata)
library(lubridate)

setwd("~/grive/phd/analyses/foraging_strategy")

gps_2016<-read.csv("~/grive/phd/fieldwork/LHI_Feb_2016/data/tracking_data/compiled_tracking_data.csv",
                   h=T, strip.white=T)

gps_2015<-read.csv("~/grive/phd/fieldwork/LHI_Feb_2015/data/tracking_results/compiled_tracking_data.csv",
                   h=T, strip.white=T)

gps_2014<-read.csv("~/grive/phd/fieldwork/LHI_Mar_2014/data/tracking_results/compiled_tracking_data.csv",
                   h=T, strip.white=T)

gps_2016$datetime<-NULL
gps_dat<-rbind(gps_2014, gps_2015, gps_2016)

head(gps_dat)
 
gps_dat$DateTime<-as.POSIXlt(paste(gps_dat$Date, gps_dat$Time, tz="GMT"), format = "%Y/%m/%d %H:%M:%S")
#!!!! FROM NOW ALL TZ WILL BE SET TO GMT (the R default as my system has Sys.timezone==NA), even though its not actually it makes life so much easier!!!
gps_dat$TrackTime <- as.double(gps_dat$DateTime)

plot(Latitude~Longitude, gps_dat, pch=16, cex=0.5, col=factor(year))
map("worldHires", add=T,  col=5)

plot(Latitude~Longitude, gps_dat, pch=16, cex=0.5, col=factor(Type))
map("worldHires", add=T,  col=3)

#speed filter and resample

source("~/research/seabird_analyses/Heron_Island/heron_analyses_r_GPS/Velocity_Functions.r")

datout <- NULL
for(i in unique(gps_dat$TrackID))
{
  Temp <-  gps_dat[gps_dat$TrackID == i,]
  plot(Latitude~Longitude, data=Temp, asp=1, cex=0.5, col=4, main=Temp$TrackID[1])
  
  Temp$Vel <- 0
  for(j in 1:nrow(Temp))
  {
    Temp[j,]$Vel <- backforVel(point=j, trip=Temp, n=4, alt=FALSE, filter=FALSE)
  
  }
  points(Latitude~Longitude, data=Temp[Temp$Vel > 75,], asp=1, cex=0.5, col=2)
  datout <- rbind(datout, Temp)
  #readline("OK")
}

#removing points going with speeds over 75 km/h 
datout<-datout[datout$Vel<75,]
datout<-na.omit(datout)


####@@@@ RESAMPLE @@@@####

  source("~/research/seabird_analyses/Heron_Island/heron_analyses_r_GPS/Resample.r")
## set up to not interpolate between points > 1 hr apart

  trackz<-unique(datout$TrackID)
  
  resamp<-NULL
  for(k in trackz)
  {
  Track<-datout[datout$TrackID ==k,]
  
  resample_output<-resample(Track, timeStep=(0.1666667))  ## timeStep set for 10 min intervals
  
  resample_output$Date_resamp <- format(as.Date(as.POSIXlt(resample_output$TrackTime, origin="1970-01-01")), format="%Y/%m/%d")
  resample_output$Time_resamp <- format((as.POSIXlt(resample_output$TrackTime, origin="1970-01-01")), "%H:%M:%S")
  
  resamp<-rbind(resamp, resample_output)
  }

#write.csv(resamp, "LHI_gps_14_15_16_interp.csv", quote=F, row.names=F)  
#resamp<-read.csv("LHI_gps_14_15_16_interp.csv", h=T)  

#correct Date, Time and DateTime fields to reflect interpolation changes    
resamp$Date<- format(as.Date(as.POSIXlt(resamp$TrackTime, origin="1970-01-01")), format="%Y/%m/%d")
resamp$Time <- format((as.POSIXlt(resamp$TrackTime, origin="1970-01-01")), "%H:%M:%S")  
resamp$DateTime<-as.POSIXlt(paste(resamp$Date, resamp$Time), format = "%Y/%m/%d %H:%M:%S")

library(lubridate)

resamp$day<-day(resamp$DateTime)
resamp$month<-month(resamp$DateTime)
resamp$year<-year(resamp$DateTime)


#kill unneccessary fields, as resample has made non-sensical duplicates
resamp$Date_resamp<-NULL
resamp$Time_resamp<-NULL
resamp$Altitude<-NULL
resamp$Speed<-NULL
resamp$Course<-NULL
resamp$Type<-NULL
resamp$Distance<-NULL
resamp$Essential<-NULL
resamp$Vel<-NULL

require(sp)
require(rgdal)
require(maps)
require(mapdata)
require(maptools)

##Tripsplit

signal_point<-data.frame(Latitude=-31.524732, Longitude=159.059787)

#### Trip split ####
source("~/grive/phd/scripts/MIBA_scripts_revised/TripSplit_revised_MMLINUX.r")

birds <-unique(resamp$TrackID)
resamp$ID<-resamp$TrackID


for(i in birds)
{
  Temp <- resamp[resamp$TrackID==i,]
  
  Trip <- tripSplit(Track=Temp, Colony=signal_point, InnerBuff=1, ReturnBuff=5, Duration=0.5, plotit=T)
  
  if(which(birds==i) == 1) {Trips <- Trip} else
    Trips <- spRbind(Trips,Trip)
  
  #readline("")
}

write.csv(Trips@data, "LHI_gps_14_15_16_interp_tripsplit.csv", quote=F, row.names=F)

plot(Trips)
plot(Trips[Trips$trip_id==-1,], col=2, add=T)

## Trip lengths from GPS data

trip_lens<-NULL
for(j in unique(Trips$trip_id))
{
  trippy<-Trips[Trips$trip_id==j,]
  diff<-((trippy[nrow(trippy),]$TrackTime-trippy[1,]$TrackTime)/3600)/24
  out<-data.frame(trip_id=j, trip_len=diff, st_dt=trippy[1,]$DateTime, 
                  nd_dt=trippy[nrow(trippy),]$DateTime, st_dist=trippy[1,]$ColDist, 
                  nd_dist=trippy[nrow(trippy),]$ColDist, return=unique(trippy$Returns))
  trip_lens<-rbind(trip_lens, out)
}

write.csv(trip_lens, "trip_lengths_from_gps.csv", quote=F, row.names=F)
# Add in the actual return dates of missing trips manually, then read back in
t_len_corr<-read.csv("trip_lengths_from_gps_act_return.csv", h=T) 

t_len_corr$DateTime<-as.POSIXlt(t_len_corr$st_dt, format = "%Y-%m-%d %H:%M:%S")
t_len_corr$st_tt <- as.double(t_len_corr$DateTime)

t_len_corr$DateTime_nd<-as.POSIXlt(t_len_corr$Actual_return, format = "%d/%m/%Y %H:%M")
t_len_corr$nd_tt <- as.double(t_len_corr$DateTime_nd)

t_len_corr$trip_len2<-((t_len_corr$nd_tt-t_len_corr$st_tt)/3600)/24

t_len_corr[which(is.na(t_len_corr$trip_len2)),]$trip_len2<-
  t_len_corr[which(is.na(t_len_corr$trip_len2)),]$trip_len

t_len_corr$day<-day(t_len_corr$DateTime)
t_len_corr$month<-month(t_len_corr$DateTime)
t_len_corr$year<-year(t_len_corr$DateTime)

ggplot(data=t_len_corr, aes(x=trip_len2))+geom_histogram()+facet_grid(~year)

#these plots could be cleaned up more: start time doesnt always reflect when bird left colony (some 
#loggers starting while bird is at sea) and there is defo an overestimate on the length of some
# return=N trips as birds evaded before retrieving the logger.

## plots for when birds are on colony/foraging to improve attendence data




#plots

library(ggplot2)

Trips@data$trip_id_seq<-substr(Trips@data$trip_id, nchar(Trips@data$trip_id),
                               nchar(Trips@data$trip_id))


region<-map_data("world")

#to appease ggplot
names(region)[names(region)=="long"]<-"Longitude"
names(region)[names(region)=="lat"]<-"Latitude"



ggplot(Trips@data, aes(y=Latitude, x=Longitude, colour=trip_id_seq))+geom_point()+
  geom_polygon(data=region, aes(group=group))+facet_wrap(~TrackID, scale="free")

p<-ggplot(Trips@data, aes(y=Latitude, x=Longitude))+
  geom_point(aes(colour=trip_id_seq))+geom_polygon(data=region, aes(group=group))+
  coord_cartesian(xlim=c(152, 162), ylim=c(-35.5, -20))+
  facet_wrap(~TrackID)

p

ggsave(filename="LHI_gps_trips1.png", scale=2)

graphics.off()

p2<-ggplot(Trips@data, aes(y=Latitude, x=Longitude))+
  geom_point(aes(colour=trip_id_seq))+
  facet_wrap(~TrackID, scale="free")

p2

ggsave(filename="LHI_gps_trips2.png", scale=2)

graphics.off()



