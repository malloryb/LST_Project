library(plyr)
#Clean up and merge: Gridmet, LST, and Daymet. 
#2) Pull in anciallary datasets: Daymet, Modis LST, and Gridmet. Gridmet will be trickiest. 
#2a) Daymet------------------
format_daymet<- function(x){
  x$date <- as.Date(paste(x$year, x$yday, sep="-"), format="%Y-%j")
  x <- plyr::rename(x, replace=c("tmin..deg.c." = "Daymet_Tmin", "tmax..deg.c."="Daymet_Tmax"))
  x <- x[,c("date","Daymet_Tmax","Daymet_Tmin")]
  x$Daymet_Tavg <- rowMeans(x[c('Daymet_Tmax', 'Daymet_Tmin')], na.rm=TRUE)
  return(x)
}

Dk1Daymetraw <- read.csv("Daymet_Dk1_lat_35.9712_lon_-79.093388_2018-09-26_143425.csv", skip=7)
Dk1Daymet <- format_daymet(Dk1Daymetraw)

Dk2Daymetraw <- read.csv("DaymetDk2_35.97358_lon_-79.10043_2018-09-26_143501.csv", skip=7)
Dk2Daymet <- format_daymet(Dk2Daymetraw)

MmsDaymetraw <- read.csv("DaymetMms_lat_39.3232_lon_-86.4143_2018-09-26_143337.csv", skip=7)
MmsDaymet <- format_daymet(MmsDaymetraw)

Ha1Daymetraw <- read.csv("DaymetHa1_lat_35.9621_lon_-84.2916_2018-09-26_143242.csv", skip=7)
Ha1Daymet <- format_daymet(MmsDaymetraw)

#2b) MODIS LST
setwd("C:/Users/malbarn/Documents/LST_Project/MODIS_LST/")

format_MODIS <- function(x){
  x$date <- as.Date(x$system.time_start, format="%b %d, %Y")
  x <- subset(x, select=-c(system.time_start))
  return(x)
}

modismms_day <- rbind(read.csv("us_mmf_daily_00_10.csv"),read.csv("us_mmf_daily_10_17.csv"))
modismms_day <- format_MODIS(modismms_day)
modismms_8day <- format_MODIS(read.csv("us_mms_8day.csv"))
modismms <- merge(modismms_day, modismms_8day, by="date", all.x=TRUE)
modismms <- rename(modismms, replace=c("LST_Day_1km_1.x" = "Modis_Temp", "LST_Day_1km_1.y"="Modis_8day_Temp"))

modisdk1_day <- rbind(read.csv("us_dk1_daily_00_10.csv"),read.csv("us_dk1_daily_10_17.csv"))
modisdk1_day <- format_MODIS(modisdk1_day)
modisdk1_8day <- format_MODIS(read.csv("us_dk1_8day.csv"))
modisdk1 <- merge(modisdk1_day, modisdk1_8day, by="date", all.x=TRUE)
modisdk1 <- rename(modisdk1, replace=c("LST_Day_1km_1.x" = "Modis_Temp", "LST_Day_1km_1.y"="Modis_8day_Temp"))

modisdk2_day <- rbind(read.csv("us_dk2_daily_00_10.csv"),read.csv("us_dk2_daily_10_17.csv"))
modisdk2_day <- format_MODIS(modisdk2_day)
modisdk2_8day <- format_MODIS(read.csv("us_dk2_8day.csv"))
modisdk2 <- merge(modisdk2_day, modisdk2_8day, by="date", all.x=TRUE)
modisdk2 <- rename(modisdk2, replace=c("LST_Day_1km_1.x" = "Modis_Temp", "LST_Day_1km_1.y"="Modis_8day_Temp"))

modisha1_day <- rbind(read.csv("us_ha1_daily_00_10.csv"),read.csv("us_ha1_daily_10_17.csv"))
modisha1_day <- format_MODIS(modisha1_day)
modisha1_8day <- format_MODIS(read.csv("us_ha1_8day.csv"))
modisha1 <- merge(modisha1_day, modisha1_8day, by="date", all.x=TRUE)
modisha1 <- rename(modisha1, replace=c("LST_Day_1km_1.x" = "Modis_Temp", "LST_Day_1km_1.y"="Modis_8day_Temp"))

#2c) Gridmet
Gridmet_Tmax_mms <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmax_mms_gridmet.csv")

format_gridmet <- function(x,y){
  x$date <- as.Date(x$system.time_start, format="%b %d, %Y")
  x$Gridmet_Tmax <-as.numeric(x$tmmx) - 273.15
  y$date <- as.Date(y$system.time_start, format="%b %d, %Y")
  y$Gridmet_Tmin <-as.numeric(y$tmmn) - 273.15
  z <- merge(x,y, by="date")
  z$Gridmet_Tavg <- ((z$Gridmet_Tmax+z$Gridmet_Tmin)/2)
  z <- subset(z, select=-c(system.time_start.x, tmmx, system.time_start.y, tmmn))
  return(z)
  }

gridmetmmstmax_raw <- rbind(read.csv("Gridmet_mms_max_00_10.csv"),read.csv("Gridmet_mms_max_10_17.csv"))
gridmetmmstmin_raw <- rbind(read.csv("Gridmet_mms_min_00_10.csv"),read.csv("Gridmet_mms_min_10_17.csv"))

gridmet_mms <- format_gridmet(gridmetmmstmax_raw, gridmetmmstmin_raw)

gridmetdk1tmax_raw <- rbind(read.csv("Gridmet_dk1_max_00_10.csv"),read.csv("Gridmet_dk1_max_10_17.csv"))
gridmetdk1tmin_raw <- rbind(read.csv("Gridmet_dk1_min_00_10.csv"),read.csv("Gridmet_dk1_min_10_17.csv"))

gridmet_dk1 <- format_gridmet(gridmetdk1tmax_raw, gridmetdk1tmin_raw)

gridmetdk2tmax_raw <- rbind(read.csv("Gridmet_dk2_max_00_10.csv"),read.csv("Gridmet_dk2_max_10_17.csv"))
gridmetdk2tmin_raw <- rbind(read.csv("Gridmet_dk2_min_00_10.csv"),read.csv("Gridmet_dk2_min_10_17.csv"))

gridmet_dk2 <- format_gridmet(gridmetdk2tmax_raw, gridmetdk2tmin_raw)

gridmetha1tmax_raw <- rbind(read.csv("Gridmet_ha1_max_00_10.csv"),read.csv("Gridmet_ha1_max_10_17.csv"))
gridmetha1tmin_raw <- rbind(read.csv("Gridmet_ha1_min_00_10.csv"),read.csv("Gridmet_ha1_min_10_17.csv"))

gridmet_ha1 <- format_gridmet(gridmetha1tmax_raw, gridmetha1tmin_raw)

#Merging this stuff together
#two stages per site
#Merge daymet and gridmet, then modis (has lots of NAs)

str(MmsDaymet)
str(gridmet_mms)
str(modismms)

Mmstemp<- merge(MmsDaymet, gridmet_mms, by="date")
Mms_met <- merge(Mmstemp, modismms, by="date", all.x=TRUE)
write.csv(Mms_met, "mms_met_data.csv")

str(Dk1Daymet)
str(gridmet_dk1)
str(modisdk1)

dk1temp<- merge(Dk1Daymet, gridmet_dk1, by="date")
dk1_met <- merge(dk1temp, modisdk1, by="date", all.x=TRUE)
write.csv(dk1_met, "dk1_met_data.csv")


str(Dk2Daymet)
str(gridmet_dk2)
str(modisdk2)

dk2temp<- merge(Dk2Daymet, gridmet_dk2, by="date")
dk2_met <- merge(dk2temp, modisdk2, by="date", all.x=TRUE)
write.csv(dk2_met, "dk2_met_data.csv")


str(Ha1Daymet)
str(gridmet_ha1)
str(modisha1)

ha1temp<- merge(Ha1Daymet, gridmet_ha1, by="date")
ha1_met <- merge(ha1temp, modisha1, by="date", all.x=TRUE)
write.csv(ha1_met, "ha1_met_data.csv")


#have to re-merge and re-save .csv files post 10/8/2018 so I can get MODIS LST (daily and 8day) and proper coordinates
setwd("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/")

mms_met <- read.csv("mms_met_data.csv")
mms_met$date <- as.Date(mms_met$date)
mms_met <- subset(mms_met, select=-c(Modis_Temp))
mergedmms <- merge(mms_met, modismms, by="date", all.x=TRUE)
str(mergedmms)
mergedmms[100:140,]
write.csv(mergedmms, "mms_met_data.csv")

dk1_met <- read.csv("dk1_met_data.csv")
dk1_met$date <- as.Date(dk1_met$date)
dk1_met <- subset(dk1_met, select=-c(Modis_Temp))
mergeddk1 <- merge(dk1_met, modisdk1, by="date", all.x=TRUE)
str(mergeddk1)
mergeddk1[100:140,]
write.csv(mergeddk1,"dk1_met_data.csv")


dk2_met <- read.csv("dk2_met_data.csv")
dk2_met$date <- as.Date(dk2_met$date)
dk2_met <- subset(dk2_met, select=-c(Modis_Temp))
mergeddk2 <- merge(dk2_met, modisdk2, by="date", all.x=TRUE)
str(mergeddk2)
mergeddk2[100:140,]
write.csv(mergeddk2,"dk2_met_data.csv")


ha1_met <- read.csv("ha1_met_data.csv")
ha1_met$date <- as.Date(ha1_met$date)
ha1_met <- subset(ha1_met, select=-c(Modis_Temp))
ha1merged <- merge(ha1_met, modisha1, by="date", all.x=TRUE)
str(ha1merged)
ha1merged[100:140,]
write.csv(ha1merged, "ha1_met_data.csv")
