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

#Format_Ameriflux function: 
#1) Parse timestamp
#2) Calculate TS from TA using Stefan Boltzman and emissivity modeled from Juang et al. GRL (2007)
#3) calculate daily TAmax, TAmin, TAavg, TSmax, TSmin, TSavg
x  <- US_Mms
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
y <- merge(x, daytime_albedo, all.x = TRUE)
y$albedo <- ifelse(x$time>800 & x$time<1700, y$albedo, y$daytime_albedo)
y$emiss <- (-0.16*x$albedo + 0.99)
length(y)
length(x)

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
  #Write daily plots to plot 
  winter <- subset(x, year=="2010" & month=="01")
  win <- ddply(winter, .(time), summarize, TA = mean(TA, na.rm=TRUE),TS= mean(TS, na.rm=TRUE))
  summer <- subset(x, year=="2010" & month=="07")
  sum <- ddply(summer, .(time), summarize, TA = mean(TA, na.rm=TRUE),TS= mean(TS, na.rm=TRUE))
  print(head(sum))
  splot <- ggplot(sum, aes(x=time, group=1))+geom_line(aes(y=TA), colour="blue", size=1) + geom_line(aes(y=TS-273.15), colour="red", size=1)+labs(title="Daily summertime temp", y="Temperature (c)",x="Date") +theme_minimal()
  wplot <- ggplot(win, aes(x=time, group=1))+geom_line(aes(y=TA), colour="blue", size=1) + geom_line(aes(y=TS-273.15), colour="red", size=1)+labs(title="Daily wintertime temp", y="Temperature (c)",x="Date") +theme_minimal()
  print(grid.arrange(wplot, splot))
  #Get daily data
  temp <- ddply(x, .(date), summarize, Tower_TAavg = mean(TA, na.rm=TRUE), Tower_TSavg= mean(TS, na.rm=TRUE), 
                Tower_TAmax= max(TA, na.rm=TRUE), Tower_TSmax = max(TS, na.rm=TRUE), 
                Tower_TAmin = min(TA, na.rm=TRUE), Tower_TSmin = min(TS, na.rm=TRUE), albedo= mean(albedo, trim=0.2, na.rm=TRUE),
                emiss=mean(emiss, na.rm=TRUE), LW_OUT=mean(LW_OUT_1_1_1, na.rm=TRUE))
  temp$Tower_TScor <-(temp$LW_OUT/(sigma *(temp$emiss)))^(0.25) - 273.15
  temp$Tower_TSavg <- temp$Tower_TSavg - 273.15
  temp$Tower_TSmax <- temp$Tower_TSmax - 273.15
  temp$Tower_TSmin <- temp$Tower_TSmin - 273.15
  return(temp)
}


Mms_test <- Format_Ameriflux(US_Mms)
Mms_test <- Mms_test[Reduce(`&`, lapply(Mms_test, is.finite)),]
#Plot 1: 
ggplot(Mms_test, aes(x=date)) + 
  geom_point(aes(y=Tower_TAavg), colour="blue", size=1.5) +
  geom_point(aes(y=Tower_TScor), colour="red", size=1)+
  annotate("text", label=paste("r=", as.character(round(eval(cor(Mms_test$Tower_TAavg, Mms_test$Tower_TScor, use="complete.obs")),digits=3))), x=as.Date("2010-10-05"), y=40, fontface="bold")+
  labs(title="Time Series MMF", y="Temperature (c)",x="Date") +theme_minimal()
  






#1) Parse timestamp
US_Dk1$date <- as.Date(paste(eval(substr(US_Dk1$TIMESTAMP_START, 1,4)) ,eval(substr(US_Dk1$TIMESTAMP_START, 5,6)), eval(substr(US_Dk1$TIMESTAMP_START, 7,8)), sep="_"), format="%Y_%m_%d")
US_Dk2$date <- as.Date(paste(eval(substr(US_Dk2$TIMESTAMP_START, 1,4)) ,eval(substr(US_Dk2$TIMESTAMP_START, 5,6)), eval(substr(US_Dk2$TIMESTAMP_START, 7,8)), sep="_"), format="%Y_%m_%d")
US_Mms$date <- as.Date(paste(eval(substr(US_Mms$TIMESTAMP_START, 1,4)) ,eval(substr(US_Dk2$TIMESTAMP_START, 5,6)), eval(substr(US_Mms$TIMESTAMP_START, 7,8)), sep="_"), format="%Y_%m_%d")

US_Dk1temp <- ddply(US_Dk1, .(date), summarize, Tower_Tavg = mean(TA, na.rm=TRUE), Tower_Tmax = max(TA, na.rm=TRUE), Tower_Tmin = min(TA, na.rm=TRUE))
US_Dk2temp <- ddply(US_Dk2, .(date), summarize, Tower_Tavg = mean(TA, na.rm=TRUE), Tower_Tmax = max(TA, na.rm=TRUE), Tower_Tmin = min(TA, na.rm=TRUE))
US_Mmstemp <- ddply(US_Mms, .(date), summarize, Tower_Tavg = mean(TA_1_1_1, na.rm=TRUE), Tower_Tmax = max(TA_1_1_1, na.rm=TRUE), Tower_Tmin=min(TA_1_1_1, na.rm=TRUE))
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
MmsDaymet <- plyr::rename(MmsDaymet, replace=c("tmax..deg.c." = "Daymet_Tmax", "tmin..deg.c."="Daymet_Tmin"))
MmsDaymet <- MmsDaymet[,c("date","Daymet_Tmax","Daymet_Tmin")]
MmsDaymet$Daymet_Tavg <- rowMeans(MmsDaymet[c('Daymet_Tmax', 'Daymet_Tmin')], na.rm=TRUE)

MMS1 <-merge(US_Mmstemp,MmsDaymet)
MMS2 <- merge(MMS1, MODISMms)
MMS3 <- 

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



#For later----------------------------
#Steps:
#1) stack up all (relevant) netcdf
#2) Extract time series 

#1) stack Netcdf
setwd("C:/Users/malbarn/Documents/Datasets/Gridmet/")
#retrieve a list of nc files in folder
flist <- list.files(path=, pattern= "*tmmx.*nc$")
tlist <- flist[1:2]
nc <- nc_open(paste0(flist[4]))

#ncname <-  "2017315214914EnsembleGPP_MR"
#ncfname <- paste(ncname, ".nc", sep="")
dname <- "gpp"
# Get a list of the NetCDF's R attributes:
attributes(nc)$names
# Get a list of the nc variable names.
attributes(nc$var)$names
# Take a look at the GPP variable's nc attributes (units etc).
ncatt_get(nc, attributes(nc$var)$names[4])

print(nc)

#setwd("C:/Users/rsstudent/odrive/UA_Google_Drive/FluxCom_Monthly/GPP_Reichstein/")

#User-defined function to process all .ncdf files (thesese files are monthly) in a given list

process_nc1 <- function(f){
  nc_tmp <- nc_open(f)
  #store values from variables and attributes
  temp_array <- ncvar_get(nc_tmp, "air_temperature")
  print(temp_array)
  dlname <- ncatt_get(nc_tmp, "air_temperature", "long_name")
  fillvalue <- ncatt_get(nc_tmp, "air_temperature", "_FillValue")
  nc_lat <- ncvar_get(nc_tmp, "lat", verbose=F)
  nc_lon <- ncvar_get (nc_tmp, "lon")
  nlon <- dim(nc_lon)
  nlat <- dim((nc_lat))
  t <- ncvar_get(nc_tmp, "day")
  nt <- dim(t)
  #Units of time: dats since 1582_2011_14
  tunits <- ncatt_get(nc_tmp, "day", "units")
  #Get input variable (temp) and its attributes and verify the size of the array
  #Convert Netcdf into dataframes so I can actually understand what's going on here
  #replace netCDF varaible's fill values with R NA's
  temp_array[temp_array==fillvalue$value] <-NA
  #This is the number of non_missing grid cells (i.e. land cells)
  #Convert the whole array to a dataframe
  temp_vec_long <- as.vector(temp_array)
  length(temp_vec_long)
  #reshape vector into 2592000x360 (months) matrix using matrix() function and verify dimensions
  temp_mat <- matrix(temp_vec_long, nrow=nlon*nlat, ncol=nt)
  dim(temp_mat)
  lonlat <- as.matrix(expand.grid(nc_lon,nc_lat))
  print(temp_mat)
  temp_df <- data.frame(cbind(lonlat, temp_mat))
  return(temp_df)
}
#This step takes awhile - just looks like a ton of NAs going by....
tmp <- lapply(tlist, process_nc1) %>% bind_cols()
tmp <- lapply(flist, process_nc1) %>% bind_cols()
str(tmp)
#This step got rid of 26 columns
data <- tmp[!duplicated(as.list(tmp))]

str(data)

#If you want to change the number of years, change it here and move the .nc files into the Flucxcom_2017 folder (from UA google drive)
colnames(data) <- c("lon", "lat",
                    #"Jan_1980", "Feb_1980","Mar_1980","Apr_1980","May_1980","Jun_1980","Jul_1980","Aug_1980","Sep_1980","Oct_1980","Nov_1980","Dec_1980","Jan_1981","Feb_1981","Mar_1981","Apr_1981","May_1981","Jun_1981","Jul_1981","Aug_1981","Sep_1981","Oct_1981","Nov_1981","Dec_1981","Jan_1982","Feb_1982","Mar_1982","Apr_1982","May_1982","Jun_1982","Jul_1982","Aug_1982","Sep_1982","Oct_1982","Nov_1982","Dec_1982","Jan_1983","Feb_1983","Mar_1983","Apr_1983","May_1983","Jun_1983","Jul_1983","Aug_1983","Sep_1983","Oct_1983","Nov_1983","Dec_1983","Jan_1984","Feb_1984","Mar_1984","Apr_1984","May_1984","Jun_1984","Jul_1984","Aug_1984","Sep_1984","Oct_1984","Nov_1984","Dec_1984","Jan_1985","Feb_1985","Mar_1985","Apr_1985","May_1985","Jun_1985","Jul_1985","Aug_1985","Sep_1985","Oct_1985","Nov_1985","Dec_1985","Jan_1986","Feb_1986","Mar_1986","Apr_1986","May_1986","Jun_1986","Jul_1986","Aug_1986","Sep_1986","Oct_1986","Nov_1986","Dec_1986","Jan_1987","Feb_1987","Mar_1987","Apr_1987","May_1987","Jun_1987","Jul_1987","Aug_1987","Sep_1987","Oct_1987","Nov_1987","Dec_1987","Jan_1988","Feb_1988","Mar_1988","Apr_1988","May_1988","Jun_1988","Jul_1988","Aug_1988","Sep_1988","Oct_1988","Nov_1988","Dec_1988","Jan_1989","Feb_1989","Mar_1989","Apr_1989","May_1989","Jun_1989","Jul_1989","Aug_1989","Sep_1989","Oct_1989","Nov_1989","Dec_1989","Jan_1990","Feb_1990","Mar_1990","Apr_1990","May_1990","Jun_1990","Jul_1990","Aug_1990","Sep_1990","Oct_1990","Nov_1990","Dec_1990","Jan_1991","Feb_1991","Mar_1991","Apr_1991","May_1991","Jun_1991","Jul_1991","Aug_1991","Sep_1991","Oct_1991","Nov_1991","Dec_1991","Jan_1992","Feb_1992","Mar_1992","Apr_1992","May_1992","Jun_1992","Jul_1992","Aug_1992","Sep_1992","Oct_1992","Nov_1992","Dec_1992","Jan_1993","Feb_1993","Mar_1993","Apr_1993","May_1993","Jun_1993","Jul_1993","Aug_1993","Sep_1993","Oct_1993","Nov_1993","Dec_1993","Jan_1994","Feb_1994","Mar_1994","Apr_1994","May_1994","Jun_1994","Jul_1994","Aug_1994","Sep_1994","Oct_1994","Nov_1994","Dec_1994","Jan_1995","Feb_1995","Mar_1995","Apr_1995","May_1995","Jun_1995","Jul_1995","Aug_1995","Sep_1995","Oct_1995","Nov_1995","Dec_1995","Jan_1996","Feb_1996","Mar_1996","Apr_1996","May_1996","Jun_1996","Jul_1996","Aug_1996","Sep_1996","Oct_1996","Nov_1996","Dec_1996","Jan_1997","Feb_1997","Mar_1997","Apr_1997","May_1997","Jun_1997","Jul_1997","Aug_1997","Sep_1997","Oct_1997","Nov_1997","Dec_1997","Jan_1998","Feb_1998","Mar_1998","Apr_1998","May_1998","Jun_1998","Jul_1998","Aug_1998","Sep_1998","Oct_1998","Nov_1998","Dec_1998",
                    "Jan_1999","Feb_1999","Mar_1999","Apr_1999","May_1999","Jun_1999","Jul_1999","Aug_1999","Sep_1999","Oct_1999","Nov_1999", "Dec_1999",
                    "Jan_2000","Feb_2000","Mar_2000","Apr_2000","May_2000","Jun_2000","Jul_2000","Aug_2000","Sep_2000","Oct_2000","Nov_2000","Dec_2000","Jan_2001","Feb_2001","Mar_2001","Apr_2001","May_2001","Jun_2001","Jul_2001","Aug_2001","Sep_2001","Oct_2001","Nov_2001","Dec_2001","Jan_2002","Feb_2002","Mar_2002","Apr_2002","May_2002","Jun_2002","Jul_2002","Aug_2002","Sep_2002","Oct_2002","Nov_2002","Dec_2002","Jan_2003","Feb_2003","Mar_2003","Apr_2003","May_2003","Jun_2003","Jul_2003","Aug_2003","Sep_2003","Oct_2003","Nov_2003","Dec_2003","Jan_2004","Feb_2004","Mar_2004","Apr_2004","May_2004","Jun_2004","Jul_2004","Aug_2004","Sep_2004","Oct_2004","Nov_2004","Dec_2004","Jan_2005","Feb_2005","Mar_2005","Apr_2005","May_2005","Jun_2005","Jul_2005","Aug_2005","Sep_2005","Oct_2005","Nov_2005","Dec_2005","Jan_2006","Feb_2006","Mar_2006","Apr_2006","May_2006","Jun_2006","Jul_2006","Aug_2006","Sep_2006","Oct_2006","Nov_2006","Dec_2006","Jan_2007","Feb_2007","Mar_2007","Apr_2007","May_2007","Jun_2007","Jul_2007","Aug_2007","Sep_2007","Oct_2007","Nov_2007","Dec_2007","Jan_2008","Feb_2008","Mar_2008","Apr_2008","May_2008","Jun_2008","Jul_2008","Aug_2008","Sep_2008","Oct_2008","Nov_2008","Dec_2008","Jan_2009","Feb_2009","Mar_2009","Apr_2009","May_2009","Jun_2009","Jul_2009","Aug_2009","Sep_2009","Oct_2009","Nov_2009","Dec_2009","Jan_2010", "Feb_2010", "Mar_2010", "Apr_2010", "May_2010", "Jun_2010", "Jul_2010", 
                    "Aug_2010", "Sep_2010","Oct_2010","Nov_2010","Dec_2010","Jan_2011","Feb_2011","Mar_2011","Apr_2011","May_2011","Jun_2011","Jul_2011","Aug_2011","Sep_2011","Oct_2011","Nov_2011","Dec_2011","Jan_2012","Feb_2012","Mar_2012","Apr_2012","May_2012","Jun_2012","Jul_2012","Aug_2012","Sep_2012","Oct_2012","Nov_2012","Dec_2012","Jan_2013","Feb_2013","Mar_2013","Apr_2013","May_2013","Jun_2013","Jul_2013","Aug_2013","Sep_2013","Oct_2013","Nov_2013","Dec_2013")
head(na.omit(data, 20))
dim(na.omit(data))
csvfile <- "RF_GPP_1999_to_2013.csv"
write.table(na.omit(data), csvfile,row.names=FALSE, sep=",")


#If you want to extract a slice____________________
#Get single slice of the data, create an R data frame, and write a .csv file
m <- 1
gpp_slice <- gpp_array[,,m]
#dimensions should be 720 by 360 rows
#verify dimensions
dim(gpp_slice)
gpp_slice
lat
lon

#image(lon,lat, gpp_slice, col=rev(brewer.pal(10, "RdBu")))

grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(0,0.00000001,0.00000002, 0.00000004, 0.00000006, 0.0000001)
levelplot(gpp_slice ~ lon*lat, data=grid, at=cutpts, cuts=6, pretty=T, col.regions=(rev(brewer.pal(10,"RdBu"))))
