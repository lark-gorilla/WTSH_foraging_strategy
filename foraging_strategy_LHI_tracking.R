
library(maps)
library(mapdata)
library(lubridate)

setwd("C:/research/phd/analyses/foraging_strategy")

gps_2015<-read.csv("C:/research/phd/fieldwork/LHI_Feb_2015/data/tracking_results/compiled_tracking_data.csv",
                   h=T, strip.white=T)

gps_2014<-read.csv("C:/research/phd/fieldwork/LHI_Mar_2014/data/tracking_results/compiled_tracking_data.csv",
                   h=T, strip.white=T)

gps_dat<-rbind(gps_2014, gps_2015)

head(gps_dat)
 
gps_dat$DateTime<-as.POSIXlt(paste(gps_dat$Date, gps_dat$Time), format = "%Y/%m/%d %H:%M:%S")
gps_dat$TrackTime <- as.double(gps_dat$DateTime)

gps_dat$day<-day(gps_dat$DateTime)
gps_dat$month<-month(gps_dat$DateTime)
gps_dat$year<-year(gps_dat$DateTime)

plot(Latitude~Longitude, gps_dat, pch=16, cex=0.5, col=factor(year))
map("worldHires", add=T,  col=4)

plot(Latitude~Longitude, gps_dat, pch=16, cex=0.5, col=factor(Type))
map("worldHires", add=T,  col=3)

#speed filter and resample

source("D:/research/seabird_analyses/Heron_Island/heron_analyses_r_GPS/Velocity_Functions.r")

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

###@@@ removing points going with speeds over 75 km/h @@@###
#datout<-datout[datout$Vel<75,]
#datout<-na.omit(datout)

#NOT removing high speeds, max vel= 97 km/h. Could be realistic, had 35 knot winds smashing the area for the duration of biologging

####@@@@ RESAMPLE @@@@####

  source("D:/research/seabird_analyses/Heron_Island/heron_analyses_r_GPS/Resample.r")
  
  trackz<-unique(datout$TrackID)
  
  resamp<-NULL
  for(k in trackz)
  {
  Track<-datout[datout$TrackID ==k,]
  
  resample_output<-resample(Track, timeStep=(0.1666667))  ## timeStep set for 10 min intervals
  
  resample_output$Date <- format(as.Date(as.POSIXlt(resample_output$TrackTime, origin="1970-01-01")), format="%Y/%m/%d")
  resample_output$Time <- format((as.POSIXlt(resample_output$TrackTime, origin="1970-01-01")), "%H:%M:%S")
  
  resamp<-rbind(resamp, resample_output)
  }

datout<-resamp

plot(Latitude~Longitude, datout, pch=16, cex=0.4, col=factor(TrackID))
map("worldHires", add=T, col=3)

require(geosphere)
require(sp)
require(rgdal)
require(rgeos)
require(maptools)

##Tripsplit

signal_point<-data.frame(Latitude=-31.524732, Longitude=159.059787)


#### Trip split ####
source("D:/research/seabird_analyses/Complete March/TripSplit.r")

birds <-unique(datout$TrackID)
datout$ID<-datout$TrackID


for(i in birds)
{
  Temp <- datout[datout$TrackID==i,]
  
  Trip <- tripSplit(Track=Temp, Colony=signal_point, InnerBuff=1, ReturnBuff=5, Duration=0.5, plotit=T)
  
  if(which(birds==i) == 1) {Trips <- Trip} else
    Trips <- spRbind(Trips,Trip)
  
  readline("")
}

write.csv(Trips@data, "LHI_14_15_gps_interp_tripsplit.csv", quote=F, row.names=F)

plot(Trips)
plot(Trips[Trips$trip_id==-1,], col=2, add=T)

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

trip_lens<-NULL
for(j in unique(Trips$trip_id))
{
  trippy<-Trips[Trips$trip_id==j,]
  diff<-((trippy[nrow(trippy),]$TrackTime-trippy[1,]$TrackTime)/3600)/24
  out<-data.frame(trip_id=j, trip_len=diff, st_dt=trippy[1,]$DateTime, 
                  nd_dt=trippy[nrow(trippy),]$DateTime, st_dist=trippy[1,]$Distance, 
                  nd_dist=trippy[nrow(trippy),]$Distance, return=unique(trippy$Returns))
  trip_lens<-rbind(trip_lens, out)
}

write.csv(trip_lens, "trip_lengths_from_gps.csv", quote=F, row.names=F)

t_len_corr<-read.csv("trip_lengths_from_gps.csv", h=T)

t_len_corr$DateTime<-as.POSIXlt(t_len_corr$st_dt, format = "%d/%m/%Y %H:%M")
t_len_corr$st_tt <- as.double(t_len_corr$DateTime)


t_len_corr$DateTime_nd<-as.POSIXlt(t_len_corr$Actual_return, format = "%d/%m/%Y %H:%M")
t_len_corr$nd_tt <- as.double(t_len_corr$DateTime_nd)

t_len_corr$trip_len2<-((t_len_corr$nd_tt-t_len_corr$st_tt)/3600)/24

t_len_corr[which(is.na(t_len_corr$trip_len2)),]$trip_len2<-
  t_len_corr[which(is.na(t_len_corr$trip_len2)),]$trip_len

qplot(data=t_len_corr, x=trip_len2, geom="histogram")



