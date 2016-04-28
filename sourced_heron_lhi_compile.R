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

lhi_04<-data.frame(NestID=lhi_04$Nest, Date=as.Date(lhi_04$Date, origin="1899-12-30"), 
                   Time=format(as.POSIXct((lhi_04$Time) * 86400, origin = "1899-12-30", tz="UTC"), "%H:%M:%S"),
                   ck_weight=lhi_04$'chick wt (g)', ck_cul=lhi_04$'ch cul (mm)', ck_tar=lhi_04$'ch tarsus',
                   AdultID=lhi_04$'ad1 band #', Adult_weight=lhi_04$'ad1 wt (g)')

lhi_04<-lhi_04[-which(is.na(lhi_04$Date)),] #remove NAs

