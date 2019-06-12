library(naniar)
library(plyr)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(ggpubr)
library(reshape2)
library(gridExtra)
library(raster)
setwd("/Users/mallory/Documents/Datasets/")
#Use na.strings argument in read.csv to change -9999 to NA
US_Akn <- read.csv("LST_Flux/AMF_US-Akn_BASE-BADM_4-1/AMF_US-Akn_BASE_HH_4-1.csv", na.strings=-9999, skip=2)
US_Bo1 <- read.csv("LST_Flux/AMF_US-Bo1_BASE-BADM_2-1/AMF_US-Bo1_BASE_HH_2-1.csv", na.strings=-9999, skip=2)
US_Cav <- read.csv("LST_Flux/AMF_US-CaV_BASE-BADM_2-1/AMF_US-CaV_BASE_HH_2-1.csv", na.strings=-9999, skip=2)
US_Chr <- read.csv("LST_Flux/AMF_US-ChR_BASE-BADM_2-1/AMF_US-ChR_BASE_HH_2-1.csv", na.strings=-9999, skip=2)
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
  x$TS <- x$TS-273.15
  return(x)}
Format_Ameriflux_MMF <- function(x){
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
  x$TS <- x$TS-273.15
  return(x)}

Mms_hourly <- Format_Ameriflux_MMF(US_Mms)
Akn_30min <- Format_Ameriflux(US_Akn)
Bo1_30min <- Format_Ameriflux(US_Bo1)
Chr_30min <- Format_Ameriflux(US_Chr)
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
temp <- ddply(x, .(date), summarize, Tower_TAavg = mean(TA, na.rm=TRUE), TsMax= mean(TS, na.rm=TRUE), 
              Tower_TAmax= max(TA, na.rm=TRUE), Tower_TSmax = max(TS, na.rm=TRUE), 
              Tower_TAmin = min(TA, na.rm=TRUE), Tower_TSmin = min(TS, na.rm=TRUE), albedo= mean(albedo, trim=0.2, na.rm=TRUE),
              emiss=mean(emiss, na.rm=TRUE), LW_OUT=mean(LW_OUT, na.rm=TRUE))
#temp <- merge(temp, TOB2, by="date", all.x=TRUE)
#sigma = 5.67 * 10^-8
#temp$Tower_TScor <-(temp$LW_OUT/(sigma *(temp$emiss)))^(0.25) - 273.15
temp <- temp[Reduce(`&`, lapply(temp, is.finite)),]
return(temp)
}
Daily_Temps_Mms <- function (x){
  #TOB <- subset(x, x$time>=1000 & x$time<=1100)
  #TOB2 <- ddply(TOB, .(date), summarize, TOB = mean(TA, na.rm=TRUE))
  #Get daily data
  temp <- ddply(x, .(date), summarize, Tower_TAavg = mean(TA, na.rm=TRUE), TsMax= mean(TS, na.rm=TRUE), 
                Tower_TAmax= max(TA, na.rm=TRUE), Tower_TSmax = max(TS, na.rm=TRUE), 
                Tower_TAmin = min(TA, na.rm=TRUE), Tower_TSmin = min(TS, na.rm=TRUE), albedo= mean(albedo, trim=0.2, na.rm=TRUE),
                emiss=mean(emiss, na.rm=TRUE), LW_OUT=mean(LW_OUT_1_1_1, na.rm=TRUE))
  #temp <- merge(temp, TOB2, by="date", all.x=TRUE)
  #sigma = 5.67 * 10^-8
  #temp$Tower_TScor <-(temp$LW_OUT/(sigma *(temp$emiss)))^(0.25) - 273.15
  temp <- temp[Reduce(`&`, lapply(temp, is.finite)),]
  return(temp)
}

Akn_Temp <- Daily_Temps(Akn_30min)
Bo1_Temp <- Daily_Temps(Bo1_30min)
Cav_Temp <- Daily_Temps(Cav_30min)
Chr_Temp <- Daily_Temps(Chr_30min)
Dk1_Temp <- Daily_Temps(Dk1_30min)
Dk2_Temp <- Daily_Temps(Dk2_30min)
Goo_Temp <- Daily_Temps(Goo_30min)
Mms_Temp <- Daily_Temps_Mms(Mms_hourly)
Nc2_Temp <- Daily_Temps(Nc2_30min)
Orv_Temp <- Daily_Temps(Orv_30min)
Sp1_Temp <- Daily_Temps(Sp1_30min)

qplot(Akn_Temp$date, Akn_Temp$Tower_TSmax)
qplot(Bo1_Temp$date, Bo1_Temp$Tower_TSmax)
qplot(Cav_Temp$date, Cav_Temp$Tower_TSmax)
qplot(Chr_Temp$date, Chr_Temp$Tower_TSmax)
qplot(Dk1_Temp$date, Dk1_Temp$Tower_TSmax)
qplot(Dk2_Temp$date, Dk2_Temp$Tower_TSmax)
qplot(Goo_Temp$date, Goo_Temp$Tower_TSmax)
qplot(Mms_Temp$date, Mms_Temp$Tower_TSmax)
qplot(Nc2_Temp$date, Nc2_Temp$Tower_TSmax)
qplot(Orv_Temp$date, Orv_Temp$Tower_TSmax)
qplot(Sp1_Temp$date, Sp1_Temp$Tower_TSmax)


format_daymet<- function(x){
x$date <- as.Date(paste(x$year, x$yday, sep="-"), format="%Y-%j")
x <- plyr::rename(x, replace=c("tmin..deg.c." = "Daymet_Tmin", "tmax..deg.c."="Daymet_Tmax"))
x <- x[,c("date","Daymet_Tmax","Daymet_Tmin")]
x$Daymet_Tavg <- rowMeans(x[c('Daymet_Tmax', 'Daymet_Tmin')], na.rm=TRUE)
x$MAT <- mean(x$Daymet_Tavg)
return(x)
}

Bo1_daymet <- format_daymet(read.csv("Daymet_Points/Bo1_11750_lat_40.0062_lon_-81.5656_2019-06-05_130116.csv", skip=7))
Cav_daymet <- format_daymet(read.csv("Daymet_Points/Cav_11566_lat_39.0633_lon_-88.2904_2019-06-05_130136.csv", skip=7))
Chr_daymet <- format_daymet(read.csv("Daymet_Points/Chr_11208_lat_35.9311_lon_-84.3324_2019-06-05_130434.csv", skip=7))
Dk1_daymet <- format_daymet(read.csv("Daymet_Points/Dk1_11211_lat_35.9712_lon_-79.0934_2019-06-05_130448.csv", skip=7))
Dk2_daymet <- format_daymet(read.csv("Daymet_Points/Dk2_11211_lat_35.9736_lon_-79.1004_2019-06-05_130510.csv", skip=7))
Goo_daymet <- format_daymet(read.csv("Daymet_Points/Goo_11206_lat_34.2647_lon_-89.8735_2019-06-05_130528.csv", skip=7))
Mms_daymet <- format_daymet(read.csv("Daymet_Points/Mms_11567_lat_39.3232_lon_-86.4131_2019-06-05_130538.csv", skip=7))
Nc2_daymet <- format_daymet(read.csv("Daymet_Points/Nc2_11212_lat_35.803_lon_-76.6685_2019-06-05_130600.csv", skip=7))
Orv_daymet <- format_daymet(read.csv("Daymet_Points/Orv_11749_lat_40.0201_lon_-83.0183_2019-06-05_130616.csv", skip=7))

Bo1_Temps <- merge(Bo1_Temp, Bo1_daymet, by="date")
Cav_Temps <- merge(Cav_Temp, Cav_daymet, by="date")
Chr_Temps <- merge(Chr_Temp, Chr_daymet, by="date")
Dk1_Temps <- merge(Dk1_Temp, Dk1_daymet, by="date")
Dk2_Temps <- merge(Dk2_Temp, Dk2_daymet, by="date")
Goo_Temps <- merge(Goo_Temp, Goo_daymet, by="date")
Mms_Temps <- merge(Mms_Temp, Mms_daymet, by="date")
Nc2_Temps <- merge(Nc2_Temp, Nc2_daymet, by="date")
Orv_Temps <- merge(Orv_Temp, Orv_daymet, by="date")

#Get the land cover types right------
Landcover_Rast <- raster("/Users/mallory/Documents/Temp_Project/landcvi020l_nt00016/landcover_proj.tif")
plot(Landcover_Rast)
dataType(Landcover_Rast)="INT4S"
barplot(Landcover_Rast)
#Need to re-code raster. Non-forest = "0" and forest = "1". I think taking the mean of the buffer this way should result 
#in the propeor % mature forest category I want. 
#Forest values: 11, 12, 13, 14, and 15. Everything else is not forest (or mature forest)
Landcover_Rast[Landcover_Rast>0 & Landcover_Rast <11] <- 0
Landcover_Rast[Landcover_Rast>10 & Landcover_Rast <16] <- 1
Landcover_Rast[Landcover_Rast>15 & Landcover_Rast <Inf] <- 0
#plot(Landcover_Rast)
#barplot(Landcover_Rast)

Bo1 <- cbind(-88.2904, 40.0062)
Cav <- cbind(-79.4208, 39.0633)
Chr <- cbind(-84.3324,	35.9311)
Dk1 <- cbind(-79.0934,	35.9712)
Dk2 <- cbind(-79.1004,	35.9736)
Goo <- cbind(-89.8735,	34.2647)
Mms <- cbind(-86.4131,	39.3232)
Nc2 <- cbind(-76.6685,	35.803)
Orv <- cbind(-83.0183,	40.0201)
Bo1_Temps$forest <- raster::extract(Landcover_Rast, Bo1, buffer=3000, fun=mean)
Cav_Temps$forest <- raster::extract(Landcover_Rast, Cav, buffer=3000, fun=mean)
Chr_Temps$forest <- raster::extract(Landcover_Rast, Chr, buffer=3000, fun=mean)
Dk1_Temps$forest <- raster::extract(Landcover_Rast, Dk1, buffer=3000, fun=mean)
Dk2_Temps$forest <- raster::extract(Landcover_Rast, Dk2, buffer=3000, fun=mean)
Goo_Temps$forest <- raster::extract(Landcover_Rast, Goo, buffer=3000, fun=mean)
Mms_Temps$forest <- raster::extract(Landcover_Rast, Mms, buffer=3000, fun=mean)
Nc2_Temps$forest <- raster::extract(Landcover_Rast, Nc2, buffer=3000, fun=mean)
Orv_Temps$forest <- raster::extract(Landcover_Rast, Orv, buffer=3000, fun=mean)

Format_plot_temps <- function(x){
  x$month <- month(x$date)
  x$season <- ifelse(x$month==6 | x$month==7 | x$month==8, "growing", 
                     ifelse(x$month==12 | x$month==1 | x$month==2, "dormant","neither"))
  y <- ddply(x, .(season), summarize, Daymet_Mean=mean(Daymet_Tmax, na.rm=TRUE), TsMax=mean(Tower_TSmax, na.rm=TRUE), TaMax=mean(Tower_TAmax, na.rm=TRUE), forest=mean(forest), MAT=mean(MAT))
  return(y)
}
sitelist <- list(Bo1_Temps, Cav_Temps, Chr_Temps, Dk1_Temps, 
              Dk2_Temps, Goo_Temps, Mms_Temps, Nc2_Temps, Orv_Temps)
To_Plot <- do.call("rbind", lapply(sitelist, Format_plot_temps))
To_Plot$Ts_Air <- To_Plot$TsMax - To_Plot$Daymet_Mean
To_Plot$Ta_Air <- To_Plot$TaMax - To_Plot$Daymet_Mean
growing_plot <- subset(To_Plot, season=="growing")
dormant_plot <- subset(To_Plot, season=="dormant")
growing_toplot <- melt(growing_plot, id.vars="forest", measure.vars=c("Ts_Air", "Ta_Air"))
dormant_toplot <- melt(dormant_plot, id.vars="forest", measure.vars=c("Ts_Air", "Ta_Air"))

ggplot(growing_toplot, aes(forest, value, colour=variable))+
  geom_point(size=3)+
  scale_color_manual(values=c("red", "black"))+
  ylab("Delta T")+
  xlab("Forest Cover (%)")+
  scale_y_reverse(lim=c(4,-4))+
  theme_bw()

ggplot(dormant_toplot, aes(forest, value, colour=variable))+
  geom_point(size=3)+
  scale_color_manual(values=c("red", "black"))+
  ylab("Delta T")+
  xlab("Forest Cover (%)")+
  scale_y_reverse(lim=c(4,-4))+
  theme_bw()



