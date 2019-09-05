library(naniar)
library(plyr)
library(dplyr)
library(ggplot2)
library(ncdf4)
library(ggpubr)
library(reshape2)
library(gridExtra)
library(raster)
library(data.table)
#Making some changes, need to test: Daily Daytime Temperature, NOT Tavg. Going to use tower data to then modify 
#the daily MODIS data. 
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

#We also need those NEON flux sites: 
#Need to get daily in 1 file by site
#Need to read all files and get daily (?) values of: SW_OUT, SW_IN
setwd("/Users/mallory/Documents/Datasets/")
Get_Daily_NEON <- function(site){
site <- deparse(substitute(site))
files <- list.files(path=".", pattern=paste0("\\W*(", site, ")\\W(.*)\\W*(SLRNR_30min)\\W*"), recursive=TRUE)
files2 <- list.files(path=".", pattern=paste0("\\W*(", site, ")\\W(.*)\\W*(SAAT_30min)\\W*"), recursive=TRUE)
tables <- lapply(files, read.csv, header = TRUE)
tables2 <- lapply(files2, read.csv, header = TRUE)
combined.df <- do.call(rbind , tables)
combined.df2 <- do.call(rbind, tables2)
x <- merge(combined.df, combined.df2, by="startDateTime", all=TRUE)
#All Concatenated 
x$date <- as.Date(paste(eval(substr(x$startDateTime, 1,4)) ,eval(substr(x$startDateTime, 6,7)), eval(substr(x$startDateTime, 9,10)), sep="_"), format="%Y_%m_%d")
x$time <-as.numeric(paste(eval(substr(x$startDateTime, 12,13)), eval(substr(x$startDateTime, 15,16)), sep=""))
x$month <-substr(x$startDateTime, 6,7)
x$year <-substr(x$startDateTime, 1,4)
x$daynight <- ifelse(x$time>800 & x$time<1700, "day","night")
#Calculate TS from albedo and LW_OUT using Stefan Boltzman
sigma = 5.67 * 10^-8
x$TA <- x$tempSingleMean
x$albedo <- (x$outSWMean/x$inSWMean)
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
x$TS <- (x$outLWMean/(sigma *(x$emiss)))^(0.25)
x$TS <- x$TS-273.15
temp <- ddply(x, .(date), summarize, Tower_TAavg = mean(TA, na.rm=TRUE), Tower_TSavg= mean(TS, na.rm=TRUE), 
              Tower_TAmax= max(TA, na.rm=TRUE), Tower_TSmax = max(TS, na.rm=TRUE), 
              Tower_TAmin = min(TA, na.rm=TRUE), Tower_TSmin = min(TS, na.rm=TRUE), albedo= mean(albedo, trim=0.2, na.rm=TRUE),
              emiss=mean(emiss, na.rm=TRUE), LW_OUT=mean(outLWMean, na.rm=TRUE))

#temp <- merge(temp, TOB2, by="date", all.x=TRUE)
#sigma = 5.67 * 10^-8
#temp$Tower_TScor <-(temp$LW_OUT/(sigma *(temp$emiss)))^(0.25) - 273.15
temp <- temp[Reduce(`&`, lapply(temp, is.finite)),]
return(temp)
}
#WORKS
#files <- list.files(path=".", pattern='\\W*(BARC)\\W(.*)\\W*(30min)\\W*', recursive=TRUE)

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
temp <- temp[Reduce(`&`, lapply(temp, is.finite)),]
return(temp)
}
Daily_Temps_Mms <- function (x){
  #TOB <- subset(x, x$time>=1000 & x$time<=1100)
  #TOB2 <- ddply(TOB, .(date), summarize, TOB = mean(TA, na.rm=TRUE))
  #Get daily data
  temp <- ddply(x, .(date), summarize, Tower_TAavg = mean(TA, na.rm=TRUE), Tower_TSavg= mean(TS, na.rm=TRUE), 
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
Barc_Temp <- Get_Daily_NEON(site=BARC)
Dsny_Temp <- Get_Daily_NEON(site=DSNY)
Flnt_Temp <- Get_Daily_NEON(site=BARC)
Jerc_Temp<- Get_Daily_NEON(site=JERC)
Osbs_Temp<- Get_Daily_NEON(site=OSBS)
Sugg_Temp<- Get_Daily_NEON(site=SUGG)
Dela_Temp<- Get_Daily_NEON(site=DELA)
Leno_Temp<- Get_Daily_NEON(site=LENO)
Mayf_Temp<- Get_Daily_NEON(site=MAYF)
Tall_Temp<- Get_Daily_NEON(site=TALL)
Blue_Temp<- Get_Daily_NEON(site=BLUE)
Clbj_Temp<- Get_Daily_NEON(site=CLBJ)
Oaes_Temp<- Get_Daily_NEON(site=OAES)
Prin_Temp<- Get_Daily_NEON(site=PRIN)




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

Barc_daymet <- format_daymet(read.csv("Daymet_Points/Barc_10669_lat_29.675982_lon_-82.008414_2019-08-14_130111.csv", skip=7))
Dsny_daymet <- format_daymet(read.csv("Daymet_Points/Dsny_10670_lat_28.12505_lon_-81.43619_2019-08-14_130219.csv", skip=7))
Flnt_daymet <- format_daymet(read.csv("Daymet_Points/Flnt_10848_lat_31.18542_lon_-84.437403_2019-08-14_130233.csv", skip=7))
Jerc_daymet <- format_daymet(read.csv("Daymet_Points/Jerc_10848_lat_31.194839_lon_-84.468623_2019-08-14_130244.csv", skip=7))
Osbs_daymet <- format_daymet(read.csv("Daymet_Points/Osbs_10670_lat_29.689282_lon_-81.993431_2019-08-14_130300.csv", skip=7))
Sugg_daymet <- format_daymet(read.csv("Daymet_Points/Sugg_10669_lat_29.68778_lon_-82.017745_2019-08-14_130310.csv", skip=7))
Dela_daymet <- format_daymet(read.csv("Daymet_Points/Dela_11027_lat_32.541727_lon_-87.803877_2019-08-14_134103.csv", skip=7))
Leno_daymet <- format_daymet(read.csv("Daymet_Points/Leno_10846_lat_31.853861_lon_-88.161181_2019-08-14_134119.csv", skip=7))
Mayf_daymet <- format_daymet(read.csv("Daymet_Points/Mayf_11027_lat_32.960365_lon_-87.407688_2019-08-14_134132.csv", skip=7))
Tall_daymet <- format_daymet(read.csv("Daymet_Points/Tall_11027_lat_32.95047_lon_-87.393259_2019-08-14_134141.csv", skip=7))
Blue_daymet <- format_daymet(read.csv("Daymet_Points/Blue_11202_lat_34.444218_lon_-96.624201_2019-08-14_134152.csv", skip=7))
Clbj_daymet <- format_daymet(read.csv("Daymet_Points/Clbj_11022_lat_33.40123_lon_-97.57_2019-08-14_134204.csv", skip=7))
Oaes_daymet <- format_daymet(read.csv("Daymet_Points/Oaes_11201_lat_35.410599_lon_-99.058779_2019-08-14_134211.csv", skip=7))
Prin_daymet <- format_daymet(read.csv("Daymet_Points/Prin_11022_lat_33.378517_lon_-97.782312_2019-08-14_134218.csv", skip=7))


Bo1_Temps <- merge(Bo1_Temp, Bo1_daymet, by="date")
Cav_Temps <- merge(Cav_Temp, Cav_daymet, by="date")
Chr_Temps <- merge(Chr_Temp, Chr_daymet, by="date")
Dk1_Temps <- merge(Dk1_Temp, Dk1_daymet, by="date")
Dk2_Temps <- merge(Dk2_Temp, Dk2_daymet, by="date")
Goo_Temps <- merge(Goo_Temp, Goo_daymet, by="date")
Mms_Temps <- merge(Mms_Temp, Mms_daymet, by="date")
Nc2_Temps <- merge(Nc2_Temp, Nc2_daymet, by="date")
Orv_Temps <- merge(Orv_Temp, Orv_daymet, by="date")

Barc_Temps <- merge(Barc_Temp, Barc_daymet, by="date")
Dsny_Temps <- merge(Dsny_Temp, Dsny_daymet, by="date")
Flnt_Temps <- merge(Flnt_Temp, Flnt_daymet, by="date")
Jerc_Temps <- merge(Jerc_Temp, Jerc_daymet, by="date")
Osbs_Temps <- merge(Osbs_Temp, Osbs_daymet, by="date")
Sugg_Temps <- merge(Sugg_Temp, Sugg_daymet, by="date")
Dela_Temps <- merge(Dela_Temp, Dela_daymet, by="date")
Leno_Temps <- merge(Leno_Temp, Leno_daymet, by="date")
Mayf_Temps <- merge(Mayf_Temp, Mayf_daymet, by="date")
Tall_Temps <- merge(Tall_Temp, Tall_daymet, by="date")
Blue_Temps <- merge(Blue_Temp, Blue_daymet, by="date")
Clbj_Temps <- merge(Clbj_Temp, Clbj_daymet, by="date")
Oaes_Temps <- merge(Oaes_Temp, Oaes_daymet, by="date")
Prin_Temps <- merge(Prin_Temp, Prin_daymet, by="date")

#Test of Tavg-----------------
library(data.table)
List_Ameriflux <- list(Bo1_Temps, Cav_Temps, Dk1_Temps, Dk2_Temps, Goo_Temps, Mms_Temps, Nc2_Temps, 
                   Orv_Temps)
List_NEON <- list(Barc_Temps, Dsny_Temps, Flnt_Temps, Jerc_Temps, Osbs_Temps, Sugg_Temps,
                  Dela_Temps, Leno_Temps, Mayf_Temps, Tall_Temps, Blue_Temps, Clbj_Temps, Oaes_Temps, Prin_Temps)

List_All <- list(Bo1_Temps, Cav_Temps, Dk1_Temps, Dk2_Temps, Goo_Temps, Mms_Temps, Nc2_Temps, 
                Orv_Temps, Barc_Temps, Dsny_Temps, Flnt_Temps, Jerc_Temps, Osbs_Temps, Sugg_Temps,
                Dela_Temps, Leno_Temps, Mayf_Temps, Tall_Temps, Blue_Temps, Clbj_Temps, Oaes_Temps, Prin_Temps)

Test_Temps <- rbindlist(List_All)
head(Test_Temps)
summary(lm(formula= Tower_TAavg~ Tower_TAmax+Tower_TAmin, data=Test_Temps))
summary(lm(forumal=TA))

#Get the land cover types right------
#Landcover_Rast <- raster("/Users/mallory/Documents/Temp_Project/landcvi020l_nt00016/landcover_proj.tif")
rasterOptions(tmpdir="C:\\",tmptime = 24,progress="text",timer=TRUE,overwrite = T,chunksize=2e+08,maxmemory=1e+8)
Landcover_Rast <- raster("/Users/mallory/Documents/Temp_Project/NLCD_LandCover/NLCD_Land_Cover_L48_2019424_full_zip/NLCD_2008_Land_Cover_L48_20190424.img")
plot(Landcover_Rast)
dataType(Landcover_Rast)="INT4S"
extent(Landcover_Rast)
extent(e2)
e2 <- extent(500000, 2300000, 177285, 2900000)
cropped <- crop(Landcover_Rast, e2)
plot(cropped)
#faster.Agg fun from: 
#Josh O'Brien - the point is to make it coarser so things go faster. 
#https://stackoverflow.com/questions/37229450/a-faster-function-to-lower-the-resolution-of-a-raster-r
# This isn't working right now
fasterAgg.Fun <- function(x,...) {
  myRle.Alt <- function (x1) {
    n1 <- length(x1)
    y1 <- x1[-1L] != x1[-n1]
    i <- c(which(y1), n1)
    x1[i][which.max(diff(c(0L, i)))]
  }
  
  if (sum(x)==0) {
    return(NA)
  } else {
    myRle.Alt(sort(x, method="quick"))
  }
}
#cropped2<- aggregate(cropped, fact=10, fun=fasterAgg.Fun)
cropped2<-aggregate(cropped, fact=10, fun=modal)
str(cropped2)
plot(cropped2)
#Trying to project 
Landcover <- projectRaster(cropped2, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
ext <- extent(-88.775, -74.85, 29.25, 41.41667)
plot(Landcover)
writeRaster(Landcover, "/Users/mallory/Documents/Temp_Project/NLCD_LandCover/NCLD_2008_processed.tif")
#Need to re-code raster. Non-forest = "0" and forest = "1". I think taking the mean of the buffer this way should result 
#in the propeor % mature forest category I want. 
#Forest values: 11, 12, 13, 14, and 15. Everything else is not forest (or mature forest)
#Landcover_Rast[Landcover_Rast>0 & Landcover_Rast <11] <- 0
#Landcover_Rast[Landcover_Rast>10 & Landcover_Rast <16] <- 1
#Landcover_Rast[Landcover_Rast>15 & Landcover_Rast <Inf] <- 0

# Recode NCLD raster according to data types here: https://www.mrlc.gov/data/legends/national-land-cover-database-2016-nlcd2016-legend
Landcover_Rast[Landcover_Rast>0 & Landcover_Rast <41] <- 0
Landcover_Rast[Landcover_Rast>40 & Landcover_Rast <44] <- 1
Landcover_Rast[Landcover_Rast>43 & Landcover_Rast <Inf] <- 0
barplot(Landcover_Rast)
Bo1 <- cbind(-88.2904, 40.0062)
Cav <- cbind(-79.4208, 39.0633)
Chr <- cbind(-84.3324,	35.9311)
Dk1 <- cbind(-79.0934,	35.9712)
Dk2 <- cbind(-79.1004,	35.9736)
Goo <- cbind(-89.8735,	34.2647)
Mms <- cbind(-86.4131,	39.3232)
Nc2 <- cbind(-76.6685,	35.803)
Orv <- cbind(-83.0183,	40.0201)
Barc <- cbind(-82.008414, 29.675982)
Dsny <- cbind(-81.43619,  28.12505)
Flnt <- cbind(-84.437403, 31.18542)
Jerc <- cbind(-84.468623, 31.194839)
Osbs <- cbind(-81.993431, 29.689282)
Sugg <- cbind(-82.017745, 29.68778)
Dela <- cbind(-87.803877, 32.541727)
Leno <- cbind(-88.161181, 32.541727)
Mayf<- cbind(-87.407688, 32.960365)
Tall<- cbind(-87.393259, 32.95047)
Blue<- cbind(-96.624201, 34.444218)
Clbj<- cbind(-97.57, 33.40123)
Oaes<- cbind(-99.058779, 35.410599) 
Prin<- cbind(-97.782312, 33.378517)

Bo1_Temps$forest <- raster::extract(Landcover_Rast, Bo1, buffer=3000, fun=mean)
Cav_Temps$forest <- raster::extract(Landcover_Rast, Cav, buffer=3000, fun=mean)
Chr_Temps$forest <- raster::extract(Landcover_Rast, Chr, buffer=3000, fun=mean)
Dk1_Temps$forest <- raster::extract(Landcover_Rast, Dk1, buffer=3000, fun=mean)
Dk2_Temps$forest <- raster::extract(Landcover_Rast, Dk2, buffer=3000, fun=mean)
Goo_Temps$forest <- raster::extract(Landcover_Rast, Goo, buffer=3000, fun=mean)
Mms_Temps$forest <- raster::extract(Landcover_Rast, Mms, buffer=3000, fun=mean)
Nc2_Temps$forest <- raster::extract(Landcover_Rast, Nc2, buffer=3000, fun=mean)
Orv_Temps$forest <- raster::extract(Landcover_Rast, Orv, buffer=3000, fun=mean)

Barc_Temps$forest <- raster::extract(Landcover_Rast, Barc, buffer=3000, fun=mean)
Dsny_Temps$forest <- raster::extract(Landcover_Rast, Dsny, buffer=3000, fun=mean)
Flnt_Temps$forest <- raster::extract(Landcover_Rast, Flnt, buffer=3000, fun=mean)
Jerc_Temps$forest <- raster::extract(Landcover_Rast, Jerc, buffer=3000, fun=mean)
Osbs_Temps$forest <- raster::extract(Landcover_Rast, Osbs, buffer=3000, fun=mean)
Sugg_Temps$forest <- raster::extract(Landcover_Rast, Sugg, buffer=3000, fun=mean)
Dela_Temps$forest <- raster::extract(Landcover_Rast, Dela, buffer=3000, fun=mean)
Leno_Temps$forest <- raster::extract(Landcover_Rast, Leno, buffer=3000, fun=mean)
Mayf_Temps$forest <- raster::extract(Landcover_Rast, Mayf, buffer=3000, fun=mean)
Tall_Temps$forest <- raster::extract(Landcover_Rast, Tall, buffer=3000, fun=mean)
Blue_Temps$forest <- raster::extract(Landcover_Rast, Blue, buffer=3000, fun=mean)
Clbj_Temps$forest <- raster::extract(Landcover_Rast, Clbj, buffer=3000, fun=mean)
Oaes_Temps$forest <- raster::extract(Landcover_Rast, Oaes, buffer=3000, fun=mean)
Prin_Temps$forest <- raster::extract(Landcover_Rast, Prin, buffer=3000, fun=mean)


Format_plot_temps <- function(x){
  x$month <- month(x$date)
  x$season <- ifelse(x$month==6 | x$month==7 | x$month==8, "growing", 
                     ifelse(x$month==12 | x$month==1 | x$month==2, "dormant","neither"))
  x$Ts_Air <- x$Tower_TSmax - x$Daymet_Tmax
  x$Ta_Air <- x$Daymet_Tmax - x$Tower_TAmax
  print(head(x))
  y <- ddply(x, .(season), summarize, Ts_Air_min=min(Ts_Air, na.rm = TRUE), Ta_Air_max =max(Ta_Air, na.rm=TRUE), Ts_Air = mean(Ts_Air, na.rm=TRUE),  
             Ta_Air= mean(Ta_Air, na.rm=TRUE), forest=mean(forest), MAT=mean(MAT))
  return(y)
}

 
sitelist <- list(Bo1_Temps, Cav_Temps, Chr_Temps, Dk1_Temps, 
              Dk2_Temps, Goo_Temps, Mms_Temps, Nc2_Temps, Orv_Temps, Barc_Temps, Dsny_Temps, 
              Flnt_Temps, Jerc_Temps, Osbs_Temps, Sugg_Temps, Leno_Temps, Mayf_Temps, Tall_Temps, Blue_Temps, Clbj_Temps, 
              Oaes_Temps, Prin_Temps)
To_Plot <- do.call("rbind", lapply(sitelist, Format_plot_temps))
growing_plot <- subset(To_Plot, season=="growing")
dormant_plot <- subset(To_Plot, season=="dormant")
growing_toplot <- melt(growing_plot, id.vars="forest", measure.vars=c("Ts_Air", "Ta_Air"))
dormant_toplot <- melt(dormant_plot, id.vars="forest", measure.vars=c("Ts_Air", "Ta_Air"))
growing_toplot
x1 <- ggplot(growing_toplot, aes(forest, value, colour=variable))+
  geom_point(size=3)+
  scale_color_manual(values=c("red", "black"))+
  ylab("Delta T")+
  xlab("Forest Cover (%)")+
  scale_y_reverse(lim=c(5,-5))+
  theme_bw()


x2 <- ggplot(growingcloudplot, aes(x=forest, y=MODIS) ) +
  geom_hex(binwidth = c(.05, 0.2)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10)))+
  ylab("Delta T")+
  xlab("Forest Cover (%)")+
  scale_y_reverse(lim=c(4,-4))+
  theme_bw()

x3 <- ggplot(dormant_toplot, aes(forest, value, colour=variable))+
  geom_point(size=3)+
  scale_color_manual(values=c("red", "black"))+
  ylab("Delta T")+
  xlab("Forest Cover (%)")+
  scale_y_reverse(lim=c(4,-4))+
  theme_bw()

x4 <- ggplot(dormantcloudplot, aes(x=forest, y=MODIS) ) +
  geom_hex(binwidth = c(.05, 0.2)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10)))+
  ylab("Delta T")+
  xlab("Forest Cover (%)")+
  scale_y_reverse(lim=c(4,-4))+
  theme_bw()

grid.arrange(x1,x2, ncol=1)
grid.arrange(x3,x4, ncol=1)

hh <- ggplot(df, aes(x=Month, group=1)) + 
  geom_line(aes(y=meanUrban), color="red") + 
  geom_errorbar(aes(x=Month, ymin=meanUrban-seUrban, ymax=meanUrban+seUrban), width=0.2, size=0.5,color="red")+
  geom_line(aes(y=meanCrop), color="yellowgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanCrop-seCrop, ymax=meanCrop+seCrop), width=0.2, size=0.5, color="yellowgreen")+
  geom_line(aes(y=meanFo), color="darkgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanFo-seFo, ymax=meanFo+seFo),width=0.2, size=0.5, color="darkgreen")+
  labs(title="Ta-Ts by Land Cover Type", 
       y="Ta-Ts (degrees C)", 
       x="Month")+
  scale_x_continuous(breaks=seq(1,12,3))+
  theme_bw()+
  scale_y_reverse(lim=c(5,-4.4))

#Get the rasters
Diffs <- brick("/Users/mallory/Documents/Temp_Project/Ta_Ts_All.tif")
Growing_Diffs <- mean(Diffs[[6:8]])
Dormant_Diffs <- mean(Diffs[[1:2& 12]])

#Have to multiply everything by 1000 because it does by integers 
pairOne = ((-88.775*1000):(-74.85*1000))
pairTwo = ((29.25*1000):(41.41667*1000))
nSamples = 20000

dt = data.table(expand.grid(pairOne, pairTwo))
dt2 = dt[sample(1:dim(dt)[1], size = nSamples), ]
dt2 = dt2/1000
str(dt2)

cloud1 <- raster::extract(Landcover_Rast, SpatialPoints(dt2), buffer=3000, fun=mean, sp = T)  
cloud2 <- raster::extract(Growing_Diffs, SpatialPoints(dt2), sp=T)
cloud3 <- raster::extract(Dormant_Diffs, SpatialPoints(dt2), sp=T)

growingcloudplot <- cbind(as.data.frame(cloud1$landcover_proj), as.data.frame(cloud2$layer))
names(growingcloudplot) <- c("forest", "MODIS")  
dormantcloudplot <- cbind(as.data.frame(cloud1$landcover_proj), as.data.frame(cloud3$layer))
names(dormantcloudplot) <- c("forest", "MODIS")  
# Need to take a look at geom_density by latitude
x2 <- ggplot(growingcloudplot, aes(x=forest, y=MODIS) ) +
  geom_hex(binwidth = c(.05, 0.2)) +
  scale_fill_gradientn(colours = rev(terrain.colors(10)))+
  ylab("Delta T")+
  xlab("Forest Cover (%)")+
  scale_y_reverse(lim=c(10,-6))+
  theme_bw()

growingcloudtoplot <- as.data.frame(growingcloudplot)

ggplot(growingcloudtoplot, aes(x=forest, y=MODIS))+
  geom_density2d()+
  ylab("Delta T")+
  xlab("Forest Cover (%)")+
  scale_y_reverse(lim=c(4,-4))+
  theme_bw()
  

library(matrixStats)
delta <- 
d_q1 <- rowQuantiles(delta, probs = c(0.25, 0.75))

delta2 <- as.data.frame(cbind(delta,d_q1))
dim(delta2) # 12579    23

library(dplyr)
delta2 <- filter(delta2, delta2[,1:21] <= `25%` & delta2[,1:21] >= delta2$`75%`)
  