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
format_MODIS <- function(x){
  x$date <- as.Date(x$system.time_start, format="%b %d, %Y")
  x$Modis_Temp <- 0.02*(as.numeric(gsub(",", "", x$LST_Day_1km)))
  x$Modis_Temp <- x$Modis_Temp - 273.15
  x <- subset(x, select=-c(system.time_start, LST_Day_1km))
  return(x)
}

modisdk1_raw <- rbind(read.csv("MODIS_dk1_00_10.csv"),read.csv("MODIS_dk1_10_17.csv"))
modisdk1 <- format_MODIS(modisdk1_raw)

modismms_raw <- rbind(read.csv("MODIS_mms_00_10.csv"),read.csv("MODIS_mms_10_17.csv"))
modismms <- format_MODIS(modismms_raw)

modisdk2_raw <- rbind(read.csv("MODIS_dk2_00_10.csv"),read.csv("MODIS_dk2_10_17.csv"))
modisdk2 <- format_MODIS(modisdk2_raw)

modisha1_raw <- rbind(read.csv("MODIS_ha1_00_10.csv"),read.csv("MODIS_ha1_10_17.csv"))
modisha1 <- format_MODIS(modisha1_raw)

#2c) Gridmet
Gridmet_Tmax_mms <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmax_mms_gridmet.csv")
format_gridmet(x,y){
  
}
head(Gridmet_Tmax_mms)
Gridmet_Tmax_mms$date <- as.Date(Gridmet_Tmax_mms$system.time_start, format="%b %d, %Y")
Gridmet_Tmax_mms$gridmettmax <-as.numeric(Gridmet_Tmax_mms$tmmx) - 273.15
Gridmet_Tmax_dk1 <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmax_dk1_gridmet.csv")
Gridmet_Tmax_dk1$date <- as.Date(Gridmet_Tmax_dk1$system.time_start, format="%b %d, %Y")
Gridmet_Tmax_dk1$Gridmet_Tmax <-as.numeric(Gridmet_Tmax_dk1$tmmx) - 273.15
Gridmet_Tmax_dk2 <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmax_dk2_gridmet.csv")
Gridmet_Tmax_dk2$date <- as.Date(Gridmet_Tmax_dk2$system.time_start, format="%b %d, %Y")
Gridmet_Tmax_dk2$Gridmet_Tmax <-as.numeric(Gridmet_Tmax_dk2$tmmx) - 273.15

tmingridmet_mms<- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmin_mms_gridmet.csv")
head(tmingridmet_mms)
tmingridmet_mms$date <- as.Date(tmingridmet_mms$system.time_start, format="%b %d, %Y")
tmingridmet_mms$Gridmet_Tmin <-as.numeric(tmingridmet_mms$tmmn) - 273.15
tmingridmet_dk1 <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmin_dk1_gridmet.csv")
tmingridmet_dk1$date <- as.Date(tmingridmet_dk1$system.time_start, format="%b %d, %Y")
tmingridmet_dk1$Gridmet_Tmin <-as.numeric(tmingridmet_dk1$tmmn) - 273.15
tmingridmet_dk2 <- read.csv("C:/Users/malbarn/Documents/LST_Project/Initial_Met_Comparisons/tmin_dk2_gridmet.csv")
tmingridmet_dk2$date <- as.Date(tmingridmet_dk2$system.time_start, format="%b %d, %Y")
tmingridmet_dk2$Gridmet_Tmin <-as.numeric(tmingridmet_dk2$tmmn) - 273.15

Gridmetmms <- merge(tmingridmet_mms, Gridmet_Tmax_mms, by="date")
Gridmetdk1 <- merge(tmingridmet_dk1, Gridmet_Tmax_dk1)
Gridmetdk2 <- merge(tmingridmet_dk2, Gridmet_Tmax_dk2)
