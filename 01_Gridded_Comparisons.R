library(naniar)
library(plyr)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(ggpubr)
library(reshape2)
library(gridExtra)
setwd("C:/Users/malbarn/Documents/Datasets/")
#Use na.strings argument in read.csv to change -9999 to NA
US_Dk1 <- read.csv("Ameriflux_9_11_2018/AMF_US-Dk1_BASE-BADM_4-5/AMF_US-Dk1_BASE_HH_4-5.csv", na.strings=-9999, skip=2)
US_Dk2 <- read.csv("Ameriflux_9_11_2018/AMF_US-Dk2_BASE-BADM_4-5/AMF_US-Dk2_BASE_HH_4-5.csv", na.strings=-9999, skip=2)
US_Mms <- read.csv("Ameriflux_9_11_2018/AMF_US-MMS_BASE-BADM_10-5/AMF_US-MMS_BASE_HR_10-5.csv", na.strings=-9999, skip=2) 
US_Hva <- read.csv("Ameriflux_9_11_2018/AMF_US-HVa_BASE-BADM_2-1/AMF_US-HVa_BASE_HH_2-1.csv", na.strings=-9999, skip=2)

#When modifying function for non-mmf: 
#Sensor may not have 1_1_1 designation

Format_Ameriflux <- function(x){
  #1) Parse tmestamp
  x$date <- as.Date(paste(eval(substr(x$TIMESTAMP_START, 1,4)) ,eval(substr(x$TIMESTAMP_START, 5,6)), eval(substr(x$TIMESTAMP_START, 7,8)), sep="_"), format="%Y_%m_%d")
  x$time <-as.numeric(substr(x$TIMESTAMP_START, 9,12)) 
  x$month <-substr(x$TIMESTAMP_START, 5,6)
  x$year <-substr(x$TIMESTAMP_START, 1,4)
  x$daynight <- ifelse(x$time>800 & x$time<1700, "day","night")
  #Calculate TS from albedo and LW_OUT using Stefan Boltzman
  sigma = 5.67 * 10^-8
  #For MMS: 
  x$TA <- x$TA_1_1_1
  x$albedo <- (x$SW_OUT_1_1_1/x$SW_IN_1_1_1)
  #Filter albedo
  #If albedo is less or equal to zero, means a negative or zero SW_IN value which is either incorrect or nighttime (replace with NA)
  x$albedo[x$albedo <= 0] <- NA
  #If albedo is over one, that is also impossible (replace with NA)
  x$albedo[x$albedo > 1] <- NA
  #Relate emissivity to albedo according to Juang et al. 2007 in GRL
  #E = -0.16*albedo + 0.99
  day <- subset(x, x$daynight== "day")
  daytime_albedo <- ddply(day, .(date), summarize, daytime_albedo=mean(albedo, na.rm=TRUE))
  x <- merge(x, daytime_albedo, all.x = TRUE)
  x$albedo <- ifelse(x$time>800 & x$time<1700, x$albedo, x$daytime_albedo)
  x$emiss <- (-0.16*x$albedo + 0.99)
  #Calculate TS 
  x$TS <- (x$LW_OUT_1_1_1/(sigma *(x$emiss)))^(0.25)
  return(x)}

x <- Format_Ameriflux(US_Mms)

#Get plots of average winter vs. summer diurnal signals
winter <- subset(x, year=="2010" & month=="01")
win <- ddply(winter, .(time), summarize, TA = mean(TA, na.rm=TRUE),TS= mean(TS, na.rm=TRUE))
summer <- subset(x, year=="2010" & month=="07")
sum <- ddply(summer, .(time), summarize, TA = mean(TA, na.rm=TRUE),TS= mean(TS, na.rm=TRUE))
splot <- ggplot(sum, aes(x=time, group=1))+geom_line(aes(y=TA), colour="blue", size=1) + geom_line(aes(y=TS-273.15), colour="red", size=1)+labs(title="Daily summertime temp", y="Temperature (c)",x="Time of Day") +theme_minimal()
wplot <- ggplot(win, aes(x=time, group=1))+geom_line(aes(y=TA), colour="blue", size=1) + geom_line(aes(y=TS-273.15), colour="red", size=1)+labs(title="Daily wintertime temp", y="Temperature (c)",x="Time of Day") +theme_minimal()
(grid.arrange(wplot, splot))

#Get daily data
temp <- ddply(x, .(date), summarize, Tower_TAavg = mean(TA, na.rm=TRUE), Tower_TSavg= mean(TS, na.rm=TRUE), 
                Tower_TAmax= max(TA, na.rm=TRUE), Tower_TSmax = max(TS, na.rm=TRUE), 
                Tower_TAmin = min(TA, na.rm=TRUE), Tower_TSmin = min(TS, na.rm=TRUE), albedo= mean(albedo, trim=0.2, na.rm=TRUE),
                emiss=mean(emiss, na.rm=TRUE), LW_OUT=mean(LW_OUT_1_1_1, na.rm=TRUE))
temp$Tower_TScor <-(temp$LW_OUT/(sigma *(temp$emiss)))^(0.25) - 273.15
temp$Tower_TSavg <- temp$Tower_TSavg - 273.15
temp$Tower_TSmax <- temp$Tower_TSmax - 273.15
temp$Tower_TSmin <- temp$Tower_TSmin - 273.15
temp <- temp[Reduce(`&`, lapply(temp, is.finite)),]

#Plot 1: 
ggplot(temp, aes(x=date)) + 
  geom_point(aes(y=Tower_TAavg), colour="blue", size=1) +
  geom_point(aes(y=Tower_TScor), colour="red", size=1)+
  annotate("text", label=paste("r=", as.character(round(eval(cor(temp$Tower_TAavg, temp$Tower_TScor, use="complete.obs")),digits=3))), x=as.Date("2010-10-05"), y=40, fontface="bold")+
  labs(title="Time Series MMF", y="Temperature (c)",x="Date") +theme_minimal()

temp$year <- substr(as.character(temp$date),1,4)
yr_2010 <- subset(temp, year==2010)

ggplot(yr_2010, aes(x=date)) + 
  geom_line(aes(y=Tower_TAavg), colour="blue", size=1) +
  geom_line(aes(y=Tower_TScor), colour="red", size=1)+
  annotate("text", label=paste("r=", as.character(round(eval(cor(yr_2010$Tower_TAavg, yr_2010$Tower_TScor, use="complete.obs")),digits=3))), x=as.Date("2010-10-05"), y=40, fontface="bold")+
  labs(title="Time Series MMF", y="Temperature (c)",x="Date") +theme_minimal()

ggplot(yr_2010, aes(x=date)) + 
  geom_line(aes(y=Tower_TAmin), colour="blue", size=1) +
  geom_line(aes(y=Tower_TSmin), colour="red", size=1)+
  annotate("text", label=paste("r=", as.character(round(eval(cor(yr_2010$Tower_TAmin, yr_2010$Tower_TSmin, use="complete.obs")),digits=3))), x=as.Date("2010-10-05"), y=40, fontface="bold")+
  labs(title="Time Series MMF", y="Temperature (c)",x="Date") +theme_minimal()

ggplot(yr_2010, aes(x=date)) + 
  geom_line(aes(y=Tower_TAmax), colour="blue", size=1) +
  geom_line(aes(y=Tower_TSmax), colour="red", size=1)+
  annotate("text", label=paste("r=", as.character(round(eval(cor(yr_2010$Tower_TAmax, yr_2010$Tower_TSmax, use="complete.obs")),digits=3))), x=as.Date("2010-10-05"), y=40, fontface="bold")+
  labs(title="Time Series MMF", y="Temperature (c)",x="Date") +theme_minimal()


#2) Pull in anciallary datasets: Daymet, Modis LST, and Gridmet. Gridmet will be trickiest. 
#2a) Daymet
Dk1Daymet <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/11211_lat_35.9712_lon_-79.09338_2018-09-17_133048.csv", skip=7)
Dk1Daymet$date <- as.Date(paste(Dk1Daymet$year, Dk1Daymet$yday, sep="-"), format="%Y-%j")
head(Dk1Daymet)
Dk1Daymet <- plyr::rename(Dk1Daymet, replace=c("tmin..deg.c." = "Daymet_tmax", "tmin..deg.c."="Daymet_tmin"))
Dk1Daymet <- Dk1Daymet[,c("date","Daymet_Tmax","Daymet_Tmin")]
Dk1Daymet$Daymet_Tavg <- rowMeans(Dk1Daymet[c('Daymet_Tmax', 'Daymet_Tmin')], na.rm=TRUE)


Dk2Daymet <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/11211_lat_35.97358_lon_-79.10043_2018-09-17_133101.csv", skip=7)
Dk2Daymet$date <- as.Date(paste(Dk2Daymet$year, Dk2Daymet$yday, sep="-"), format="%Y-%j")
head(Dk2Daymet)
Dk2Daymet <- plyr::rename(Dk2Daymet, replace=c("tmax..deg.c." = "Daymet_Tmax", "tmin..deg.c."="Daymet_Tmin"))
Dk2Daymet <- Dk2Daymet[,c("date","Daymet_Tmax","Daymet_Tmin")]
Dk2Daymet$Daymet_Tavg <- rowMeans(Dk2Daymet[c('Daymet_Tmax', 'Daymet_Tmin')], na.rm=TRUE)

MmsDaymet <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/11567_lat_39.3232_lon_-86.4131_2018-09-17_132853.csv", skip=7)
MmsDaymet$date <- as.Date(paste(MmsDaymet$year, MmsDaymet$yday, sep="-"), format="%Y-%j")
head(MmsDaymet)
tail(MmsDaymet)
MmsDaymet <- plyr::rename(MmsDaymet, replace=c("tmax..deg.c." = "Daymet_Tmax", "tmin..deg.c."="Daymet_Tmin"))
MmsDaymet <- MmsDaymet[,c("date","Daymet_Tmax","Daymet_Tmin")]
MmsDaymet$Daymet_Tavg <- rowMeans(MmsDaymet[c('Daymet_Tmax', 'Daymet_Tmin')], na.rm=TRUE)

Mms_daymet_flux <- merge(temp,MmsDaymet)
cor(Mms_daymet_flux$Tower_TScor, Mms_daymet_flux$Daymet_Tavg)
cor(Mms_daymet_flux$Tower_TAavg, Mms_daymet_flux$Daymet_Tavg)
ggplot(Mms_daymet_flux, aes(x=date)) + 
  geom_point(aes(y=Tower_TScor), colour="blue", size=1) +
  geom_point(aes(y=Tower_TAavg), colour="red", size=1)+
  geom_point(aes(y=Daymet_Tavg), colour="green", size=1)+
  annotate("text", label=paste("r=", as.character(round(eval(cor(Mms_daymet_flux$Tower_TAavg, Mms_daymet_flux$Daymet_Tavg, use="complete.obs")),digits=3))), x=as.Date("2010-10-05"), y=40, fontface="bold")+
  labs(title="Time Series MMF", y="Temperature (c)",x="Date") +theme_minimal()

cor(Mms_daymet_flux$Tower_TSmax, Mms_daymet_flux$Daymet_Tmax)
cor(Mms_daymet_flux$Tower_TAmax, Mms_daymet_flux$Daymet_Tmax)

cor(Mms_daymet_flux$Tower_TSmin, Mms_daymet_flux$Daymet_Tmin)
cor(Mms_daymet_flux$Tower_TAmin, Mms_daymet_flux$Daymet_Tmin)


#2b) MODIS LST
MODISDk1 <- read.csv("C:/Users/malbarn/Documents/LST_Project/MODIS_LST/MODIS_LST_dk1.csv")
MODISDk1$date <- as.Date(MODISDk1$system.time_start, format="%b %d, %Y")
MODISDk1$Modis_Temp <- 0.02*(as.numeric(gsub(",", "", MODISDk1$LST_Day_1km))) - 273.15
head(MODISDk1)
MODISDk2 <- read.csv("C:/Users/malbarn/Documents/LST_Project/MODIS_LST/MODIS_LST_dk2.csv")
MODISDk2$date <- as.Date(MODISDk2$system.time_start, format="%b %d, %Y")
MODISDk2$Modis_Temp <- 0.02*(as.numeric(gsub(",", "", MODISDk2$LST_Day_1km))) - 273.15
head(MODISDk2)
MODISMms <- read.csv("C:/Users/malbarn/Documents/LST_Project/MODIS_LST/MODIS_LST_mms.csv")
MODISMms$date <- as.Date(MODISMms$system.time_start, format="%b %d, %Y")
MODISMms$Modis_Temp <- 0.02*(as.numeric(gsub(",", "", MODISMms$LST_Day_1km))) - 273.15
head(MODISMms)
MODISMms <- MODISMms[c("date", "Modis_Temp")]

#2c) Gridmet
Gridmet_Tmax_mms <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmax_mms_gridmet.csv")
head(Gridmet_Tmax_mms)
Gridmet_Tmax_mms$date <- as.Date(Gridmet_Tmax_mms$system.time_start, format="%b %d, %Y")
Gridmet_Tmax_mms$Gridmet_Tmax <-as.numeric(Gridmet_Tmax_mms$tmmx) - 273.15

tmingridmet_mms<- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmin_mms_gridmet.csv")
head(tmingridmet_mms)
tmingridmet_mms$date <- as.Date(tmingridmet_mms$system.time_start, format="%b %d, %Y")
tmingridmet_mms$Gridmet_Tmin <-as.numeric(tmingridmet_mms$tmmn) - 273.15


Gridmetmms <- merge(tmingridmet_mms, Gridmet_Tmax_mms, by="date")
Gridmetmms <- subset(Gridmetmms, select=-c(system.time_start.x, system.time_start.y, tmmn, tmmx))


Gridmet_Tmax_dk1 <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmax_dk1_gridmet.csv")
Gridmet_Tmax_dk1$date <- as.Date(Gridmet_Tmax_dk1$system.time_start, format="%b %d, %Y")
Gridmet_Tmax_dk1$Gridmet_Tmax <-as.numeric(Gridmet_Tmax_dk1$tmmx) - 273.15
Gridmet_Tmax_dk2 <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmax_dk2_gridmet.csv")
Gridmet_Tmax_dk2$date <- as.Date(Gridmet_Tmax_dk2$system.time_start, format="%b %d, %Y")
Gridmet_Tmax_dk2$Gridmet_Tmax <-as.numeric(Gridmet_Tmax_dk2$tmmx) - 273.15


tmingridmet_dk1 <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmin_dk1_gridmet.csv")
tmingridmet_dk1$date <- as.Date(tmingridmet_dk1$system.time_start, format="%b %d, %Y")
tmingridmet_dk1$Gridmet_Tmin <-as.numeric(tmingridmet_dk1$tmmn) - 273.15

tmingridmet_dk2 <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmin_dk2_gridmet.csv")
tmingridmet_dk2$date <- as.Date(tmingridmet_dk2$system.time_start, format="%b %d, %Y")
tmingridmet_dk2$Gridmet_Tmin <-as.numeric(tmingridmet_dk2$tmmn) - 273.15

Gridmetdk1 <- merge(tmingridmet_dk1, Gridmet_Tmax_dk1)
Gridmetdk2 <- merge(tmingridmet_dk2, Gridmet_Tmax_dk2)



