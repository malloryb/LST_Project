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
US_Ha1 <- read.csv("Ameriflux_9_11_2018/AMF_US-Ha1_BASE-BADM_10-1/AMF_US-Ha1_BASE_HR_10-1.csv", na.strings=-9999, skip=2)
US_Ha1 <- read.csv("Fluxnet2015_US_sites/FLX_US-Ha1_FLUXNET2015_FULLSET_1991-2012_1-3/FLX_US-Ha1_FLUXNET2015_FULLSET_HR_1991-2012_1-3.csv")
str(US_Ha1)
colSums(is.na(US_Ha1))
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
  x$TA <- x$TA
  x$albedo <- (x$SW_OUT/x$SW_IN)
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
  x$TS <- (x$LW_OUT/(sigma *(x$emiss)))^(0.25)
  return(x)}

Mms_hourly <- Format_Ameriflux(US_Mms)
Ha1_hourly <- Format_Ameriflux(US_Hva)
Dk1_30min <- Format_Ameriflux(US_Dk1)
Dk2_30min <- Format_Ameriflux(US_Dk2)

write.csv(Mms_hourly, "Mms_hourly_flux.csv")
write.csv(Ha1_hourly, "Ha1_hourly_flux.csv")
write.csv(Dk1_30min, "Ha1_hourly_flux.csv")
write.csv(Dk2_30min, "Ha1_hourly_flux.csv")

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


