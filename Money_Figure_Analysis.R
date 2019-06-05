library(naniar)
library(plyr)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(ggpubr)
library(reshape2)
library(gridExtra)
setwd("/Users/mallory/Documents/Datasets/")
#Use na.strings argument in read.csv to change -9999 to NA
US_Akn <- read.csv("LST_Flux/AMF_US-Akn_BASE-BADM_4-1/AMF_US-Akn_BASE_HH_4-1.csv", na.strings=-9999, skip=2)
US_Bo1 <- read.csv("LST_Flux/AMF_US-Bo1_BASE-BADM_2-1/AMF_US-Bo1_BASE_HH_2-1.csv", na.strings=-9999, skip=2)
US_Cav <- read.csv("LST_Flux/AMF_US-CaV_BASE-BADM_2-1/AMF_US-CaV_BASE_HH_2-1.csv", na.strings=-9999, skip=2)
US_Goo <- read.csv("LST_Flux/AMF_US-Goo_BASE-BADM_2-1/AMF_US-Goo_BASE_HH_2-1.csv", na.strings=-9999, skip=2)
US_Dk1 <- read.csv("LST_Flux/AMF_US-Dk1_BASE-BADM_4-5/AMF_US-Dk1_BASE_HH_4-5.csv", na.strings=-9999, skip=2)
US_Dk2 <- read.csv("LST_Flux//AMF_US-Dk2_BASE-BADM_4-5/AMF_US-Dk2_BASE_HH_4-5.csv", na.strings=-9999, skip=2)
US_Mms <- read.csv("LST_Flux/AMF_US-MMS_BASE-BADM_10-5/AMF_US-MMS_BASE_HR_10-5.csv", na.strings=-9999, skip=2) 
US_Nc2 <- read.csv("LST_Flux/AMF_US-NC2_BASE-BADM_3-1/AMF_US-NC2_BASE_HH_3-1.csv", na.strings=-9999, skip=2)
US_Orv <- read.csv("LST_Flux/AMF_US-ORv_BASE-BADM_1-1/AMF_US-ORv_BASE_HH_1-1.csv", na.strings=-9999, skip=2)
US_Sp1 <- read.csv("LST_Flux/AMF_US-SP1_BASE-BADM_4-1/AMF_US-SP1_BASE_HH_4-1.csv", na.strings=-9999, skip=2)

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
  #x$TA <- x$TA_1_1_1
  x$TA <- x$TA
  #x$albedo <- (x$SW_OUT_1_1_1/x$SW_IN_1_1_1)
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
  #x$TS <- (x$LW_OUT_1_1_1/(sigma *(x$emiss)))^(0.25)
  return(x)}
Mms_hourly <- Format_Ameriflux(US_Mms)
Bo1_30min <- Format_Ameriflux(US_Bo1)
Cav_30min <- Format_Ameriflux(US_Cav)
Goo_30min <- Format_Ameriflux(US_Goo)
Dk1_30min <- Format_Ameriflux(US_Dk1)
Dk2_30min <- Format_Ameriflux(US_Dk2)
Nc2_30min <- Format_Ameriflux(US_Nc2)
Orv_30min <- Format_Ameriflux(US_Orv)
Sp1_30min <- Format_Ameriflux(US_Sp1)
                          
#Do this over again with proper file read in as "x"...maybe not the best idea but it's what is 
#going on for now 

Daily_Temps <- function (x){
#TOB <- subset(x, x$time>=1000 & x$time<=1100)
#TOB2 <- ddply(TOB, .(date), summarize, TOB = mean(TA, na.rm=TRUE))
#Get daily data
temp <- ddply(x, .(date), summarize, Tower_TAavg = mean(TA, na.rm=TRUE), Tower_TSavg= mean(TS, na.rm=TRUE), 
              Tower_TAmax= max(TA, na.rm=TRUE), Tower_TSmax = max(TS, na.rm=TRUE), 
              Tower_TAmin = min(TA, na.rm=TRUE), Tower_TSmin = min(TS, na.rm=TRUE), albedo= mean(albedo, trim=0.2, na.rm=TRUE),
              emiss=mean(emiss, na.rm=TRUE), LW_OUT=mean(LW_OUT, na.rm=TRUE))
#temp <- merge(temp, TOB2, by="date", all.x=TRUE)
#sigma = 5.67 * 10^-8
#temp$Tower_TScor <-(temp$LW_OUT/(sigma *(temp$emiss)))^(0.25) - 273.15
temp$Tower_TSavg <- temp$Tower_TSavg - 273.15
temp$Tower_TSmax <- temp$Tower_TSmax - 273.15
temp$Tower_TSmin <- temp$Tower_TSmin - 273.15
temp <- temp[Reduce(`&`, lapply(temp, is.finite)),]
return(temp)
}
Bo1_Temp <- Daily_Temps(Bo1_30min)
Cav_Temp <- Daily_Temps(Cav_30min)
Dk1_Temp <- Daily_Temps(Dk1_30min)
Dk2_Temp <- Daily_Temps(Dk2_30min)
Mms_Temp <- Daily_Temps(Mms_hourly)
Nc2_Temp <- Daily_Temps(Nc2_30min)
Orv_Temp <- Daily_Temps(Orv_30min)
Sp1_Temp <- Daily_Temps(Sp1_30min)

#Load met data
mms_met <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/mms_met_data.csv")
mms_met$date <- as.Date(mms_met$date)
dk1_met <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/dk1_met_data.csv")
dk1_met$date <- as.Date(dk1_met$date)
dk2_met <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/dk2_met_data.csv")
dk2_met$date <- as.Date(dk2_met$date)
ha1_met <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/ha1_met_data.csv")
ha1_met$date <- as.Date(ha1_met$date)

mms_temp_comp <- merge(temp, mms_met, all.x=TRUE)
dk1_temp_comp <- merge(temp, dk1_met, all.x=TRUE)
dk2_temp_comp <- merge(temp, dk2_met, all.x=TRUE)

write.csv(mms_temp_comp, "mms_temp_comp.csv")
write.csv(dk1_temp_comp, "dk1_temp_comp.csv")
write.csv(dk2_temp_comp, "dk2_temp_comp.csv")

x <- read.csv("mms_temp_comp.csv")
x$date <- as.Date(x$date)
str(x)

#Adding TOB to exisitng files
dk1 <- read.csv("C:/Users/malbarn/Documents/Datasets/dk1_temp_comp_10_8.csv")
#Get TOB from flux data 
dk1_flux <- read.csv("DK130min_flux.csv")
TOB <- subset(dk1_flux, dk1_flux$time>=1000 & dk1_flux$time<=1100)
TOB2 <- ddply(TOB, .(date), summarize, TOB_A = mean(TA, na.rm=TRUE), TOB_S = mean(TS, na.rm=TRUE))
#merge
dk1_fix <- merge(dk1, TOB2, by="date", all.x=TRUE)
#should be same number of obvservations
str(dk1)
str(dk1_fix)
#fixed! 
write.csv(dk1_fix, "C:/Users/malbarn/Documents/Datasets/dk1_temp_comp_10_8.csv")

#dk2
dk2 <- read.csv("C:/Users/malbarn/Documents/Datasets/dk2_temp_comp_10_8.csv")
dk2_flux <- read.csv("dk230min_flux.csv")
TOB <- subset(dk2_flux, dk2_flux$time>=1000 & dk2_flux$time<=1100)
TOB2 <- ddply(TOB, .(date), summarize, TOB_A = mean(TA, na.rm=TRUE), TOB_S = mean(TS, na.rm=TRUE))
#merge
dk2_fix <- merge(dk2, TOB2, by="date", all.x=TRUE)
#should be same number of obvservations
str(dk2)
str(dk2_fix)
dk2_fix <- subset(dk2_fix, select=-c(TOB, X.1, X))
#fixed! 
write.csv(dk2_fix, "C:/Users/malbarn/Documents/Datasets/dk2_temp_comp_10_8.csv")

#mms
mms <- read.csv("C:/Users/malbarn/Documents/Datasets/mms_temp_comp_10_8.csv")
mms_flux <- read.csv("Mms_hourly_flux.csv")
TOB <- subset(mms_flux, mms_flux$time>=1000 & mms_flux$time<=1100)
TOB2 <- ddply(TOB, .(date), summarize, TOB_A = mean(TA_1_1_1, na.rm=TRUE), TOB_S = mean(TS, na.rm=TRUE))
#merge
mms_fix <- merge(mms, TOB2, by="date", all.x=TRUE)
#should be same number of obvservations
str(mms)
str(mms_fix)
mms_fix <- subset(mms_fix, select=-c(X.1, X.2, X))
#fixed! 
write.csv(mms_fix, "C:/Users/malbarn/Documents/Datasets/mms_temp_comp_10_8.csv")

#ha1
ha1 <- read.csv("C:/Users/malbarn/Documents/Datasets/ha1_temp_comp_10_8.csv")
ha1_flux <- read.csv("ha1_hourly_flux.csv")
TOB <- subset(ha1_flux, ha1_flux$time>=1000 & ha1$time<=1100)
TOB2 <- ddply(TOB, .(date), summarize, TOB_A = mean(TA, na.rm=TRUE), TOB_S = mean(TS, na.rm=TRUE))
#merge
ha1_fix <- merge(ha1, TOB2, by="date", all.x=TRUE)
#should be same number of obvservations
str(ha1)
str(ha1_fix)
#fixed! 
write.csv(ha1_fix, "C:/Users/malbarn/Documents/Datasets/ha1_temp_comp_10_8.csv")


#Don't need this stuff____________________________
#Get plots of average winter vs. summer diurnal signals
winter <- subset(x, year=="2008" & month=="01")
win <- ddply(winter, .(time), summarize, TA = mean(TA, na.rm=TRUE),TS= mean(TS, na.rm=TRUE))
summer <- subset(x, year=="2008" & month=="07")
sum <- ddply(summer, .(time), summarize, TA = mean(TA, na.rm=TRUE),TS= mean(TS, na.rm=TRUE))
splot <- ggplot(sum, aes(x=time, group=1))+geom_line(aes(y=TA), colour="blue", size=1) + geom_line(aes(y=TS-273.15), colour="red", size=1)+labs(title="Daily summertime temp", y="Temperature (c)",x="Time of Day") +theme_minimal()
wplot <- ggplot(win, aes(x=time, group=1))+geom_line(aes(y=TA), colour="blue", size=1) + geom_line(aes(y=TS-273.15), colour="red", size=1)+labs(title="Daily wintertime temp", y="Temperature (c)",x="Time of Day") +theme_minimal()
grid.arrange(wplot, splot, top = "Diurnal Temperature Signal - US-Dk2")

#Plot 1: 
ggplot(x, aes(x=date)) + 
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


