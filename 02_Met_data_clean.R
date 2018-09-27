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

