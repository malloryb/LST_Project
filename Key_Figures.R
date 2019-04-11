#Making figures for LST Manuscript
#Figure 1: Changing air temperature (re-create 'warming hole')
#Figure 2: Average difference between surface temperature and air temperature from Remote sensing (by pixel)
#Figure 3: Difference in surface and air temperature by forest stand age (correlation map)
#Figure 3: Lily's figure
#Figure 4: Flux synthesis figure

#Before starting: raster options
rasterOptions(tmpdir="C:\\",tmptime = 24,progress="text",timer=TRUE,overwrite = T,chunksize=2e+08,maxmemory=1e+8)
#Load packages
library(ncdf4)
library(raster)
library(proj4)
library(rgdal)
library(gdalUtils)
library(spatialEco)
library(greenbrown)
library(RColorBrewer)
library(MODIS)
library(rasterVis)


#Figure 1---------------------
#Using University of Delaware Air Temperature & Precipitation
# Data product accessed from: https://www.esrl.noaa.gov/psd/data/gridded/data.UDel_AirT_Precip.html
#Open air temp file (0.5 degree)
fname <- file.choose("air.mon.mean.v501.nc")
nc<-nc_open(fname)
# Get a list of the NetCDF's R attributes:
attributes(nc)$names
# Get a list of the nc variable names.
attributes(nc$var)$names
### My variable is 2-m air temperature "t2m" ###
TAS <- brick(fname, varname="air")
#Crop to US extent
ext <- extent(233, 290, 23, 51)
TAS_test <- crop(TAS, ext)
#Do the kendall test
kendall_raster <- raster.kendall(TAS_test, tau=TRUE, p.value=TRUE)
writeRaster(kendall_raster, "Temp_Kendall_US.tif")
#Trying to center color ramp so white is at zero:
pal <- colorRampPalette(c("blue","cadetblue1", "lightblue", "white","red", "red3"))
plot(kendall_raster, col=pal(6))
#Trying with greenbrown
#Figure 1a: Change over time
#greenbrown_test <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=1)
greenbrown_test <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=0)
plot(greenbrown_test[[2]], col=pal(10), main="Slope of temperature trend: 1900-present (Degrees C per year)")
#Change over time in terms of degrees C per 50 years
plot((greenbrown_test[[2]]*50), col=pal(20), main="Slope of temperature trend: 1900-present (Degrees C per 50 years)")
#Trying with 1 break point
greenbrown_test1break <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=1)
plot(greenbrown_test1break)
plot(greenbrown_test1break, col=(brewer.pal(n=6, name='Spectral')))
plot(greenbrown_test1break[[3]], (brewer.pal(n=10, name='Spectral')), main="Break point in temperature trend (1900-present)")
#Seeing slope differences between two sections
plot(greenbrown_test1break[[4]], col=pal(10))
plot(greenbrown_test1break[[5]], col=pal(10))
slope_diff <- greenbrown_test1break[[5]]-greenbrown_test1break[[4]]
plot(slope_diff, col=pal(10))
#Trying with 2 break point
greenbrown_test2break <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=2)
plot(greenbrown_test2break)
plot(greenbrown_test1break, col=(brewer.pal(n=6, name='Spectral')))
plot(greenbrown_test1break[[3]], (brewer.pal(n=10, name='Spectral')), main="Break point in temperature trend (1900-present)")
#Trying with 3 break points
greenbrown_test3break <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=3)
plot(greenbrown_test3break)
plot(greenbrown_test3break, col=(brewer.pal(n=6, name='Spectral')))
plot(greenbrown_test3break[[3]], (brewer.pal(n=10, name='Spectral')), main="Break point in temperature trend (1900-present)")



#Create Ta-Ts map for the US ---------
Diffs <- brick("/Users/mallory/Documents/Temp_Project/Ta_Ts_All.tif")
cols <- colorRampPalette(brewer.pal(9,"RdBu"))
my.at <- seq(-6,6,1)
# create a level plot
levelplot(Diffs, at=my.at, main="Difference between Air Temperature and Surface Temperature (Ta-Ts)",
          col.regions=(cols))
#Fancy Plots: 
#Density plot
densityplot(Diffs)
#Bow plot
bwplot(Diffs)

#Looking at 1km land cover data from USGS: https://nationalmap.gov/small_scale/mld/landcvi.html
Land_Cover <- raster("/Users/mallory/Documents/Temp_Project/landcvi020l_nt00016/landcvi020l.tif")
#Reproject and crop land cover data 
e2 <- extent(-40000, 2300000, -1800000, 400000)
LC <- crop(Land_Cover, e2)
plot(LC)
LC_proj <-projectRaster(LC, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
ext <- extent(Diffs)
LC_crop <- crop(LC_proj, ext)
#Create 3 masks: 
#1: Urban (1)
Urbanmask <- LC_crop
Urbanmask[Urbanmask >1] <- NA
plot(Urbanmask)
Urbanmask <- resample(Urbanmask, Diffs, method="bilinear")
plot(Urbanmask)
#2: Croplands (2,6)
Cropmask <- LC_crop
Cropmask[Cropmask <2 | Cropmask>6 | Cropmask==3 | Cropmask==4 | Cropmask==5] <- NA
plot(Cropmask)
Cropmask <- resample(Cropmask, Diffs, method="bilinear")
plot(Cropmask)
#2b: Testing Croplands again (2,4,5,6)
Cropmask2 <- LC_crop
Cropmask2[Cropmask2 <2 | Cropmask2>6 | Cropmask2==3] <- NA
plot(Cropmask2)
Cropmask2 <- resample(Cropmask2, Diffs, method="bilinear")
plot(Cropmask2)
#3: Deciduous forests (11,12)
Decfomask <- LC_crop
Decfomask[Decfomask <11 |  Decfomask>12] <- NA
plot(Decfomask)
Decfomask <- resample(Decfomask, Diffs, method="bilinear")
plot(Decfomask)
#4: Evergreen forests (13,14)
Evmask <- LC_crop
Evmask[Evmask <13 |  Evmask>14] <- NA
plot(Evmask)
Evmask <- resample(Evmask, Diffs, method="bilinear")
plot(Evmask)
#5: All forests (11-15)
Fomask <- LC_crop
Fomask[Fomask <11 |  Fomask>15] <- NA
plot(Fomask)
Fomask <- resample(Fomask, Diffs, method="bilinear")
plot(Fomask)

Urban_Diff <- mask(Diffs, Urbanmask)
Crop_Diff <- mask(Diffs,Cropmask)
Crop2_Diff <- mask(Diffs,Cropmask2)
Dec_Diff <- mask(Diffs, Decfomask)
Ev_Diff <- mask(Diffs, Evmask)
Fo_Diff <- mask(Diffs, Fomask)

plot(Urban_Diff)
plot(Crop_Diff)
plot(Crop2_Diff)
plot(Dec_Diff)
plot(Ev_Diff)
plot(Fo_Diff)

my.at <- seq(-6,6,1)
levelplot(Fo_Diff, at=my.at, main="Ta_Ts in forests", col.regions=(cols))
levelplot(Crop_Diff, at=my.at, main="Ta-Ts in croplands",col.regions=(cols))
levelplot(Crop2_Diff, at=my.at, main="Ta-Ts in croplands",col.regions=(cols))


densityplot(Urban_Diff, main="Ta-Ts in Urban Environments")
bwplot(Urban_Diff, main="Ta-Ts in Urban Environments")

densityplot(Crop_Diff, main="Ta_Ts in Agricultural Environments")
bwplot(Crop_Diff, main="Ta-Ts in Agricultural Environments")

densityplot(Dec_Diff, main="Ta_Ts in Deciduous Forests")
bwplot(Dec_Diff, main="Ta_Ts in Deciduous Forest")


densityplot(Ev_Diff, main="Ta-Ts in Evergreen Forests")
bwplot(Ev_Diff, main="Ta_Ts in Evergreen Forests")

densityplot(Fo_Diff, main="Ta-Ts in Forest Environments")
bwplot(Fo_Diff, main="Ta_Ts in Forest Environments")

#Create data frame for plotting comparisons 
#df <- data.frame(Month=c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
df <- data.frame(Month=c(1:12))
df$Month <- as.numeric(df$Month)
df$meanUrban <- as.numeric(cellStats(Urban_Diff, stat='mean', na.rm=TRUE))
df$sdUrban <- as.numeric(cellStats(Urban_Diff, stat='sd', na.rm=TRUE))
df$seUrban <- (df$sdUrban)/(sqrt(ncell(Urban_Diff)))
df$meanCrop <- as.numeric(cellStats(Crop_Diff, stat='mean', na.rm=TRUE))
df$sdCrop <- as.numeric(cellStats(Crop_Diff, stat='sd', na.rm=TRUE))
df$seCrop <- (df$sdCrop)/(sqrt(ncell(Crop_Diff)))
df$meanCrop2 <- as.numeric(cellStats(Crop2_Diff, stat='mean', na.rm=TRUE))
df$sdCrop2 <- as.numeric(cellStats(Crop2_Diff, stat='sd', na.rm=TRUE))
df$seCrop2 <- (df$sdCrop)/(sqrt(ncell(Crop2_Diff)))
df$meanFo <- as.numeric(cellStats(Fo_Diff, stat='mean', na.rm=TRUE))
df$sdFo <- as.numeric(cellStats(Fo_Diff, stat='sd', na.rm=TRUE))
df$seFo <- (df$sdFo)/(sqrt(ncell(Fo_Diff)))
df$meanEv <- as.numeric(cellStats(Ev_Diff, stat='mean', na.rm=TRUE))
df$sdEv <- as.numeric(cellStats(Ev_Diff, stat='sd', na.rm=TRUE))
df$seEv <- (df$sdEv)/(sqrt(ncell(Ev_Diff)))
df$meanDec <- as.numeric(cellStats(Dec_Diff, stat='mean', na.rm=TRUE))
df$sdDec <- as.numeric(cellStats(Dec_Diff, stat='sd', na.rm=TRUE))

dft <- df[,c("Month", "meanUrban", "meanCrop", "meanFo")]
dfm <- melt(dft, id="Month")
dfm$LandCoverdfm$variable 
ggplot(data = dfm, aes(x = Month, y = value, color = variable)) + 
  geom_line(size=5) +
  scale_color_manual(labels = c("Forest", "Cropland", "Urban"), values = c("darkgreen", "yellowgreen", "red"))+
  labs(color = "Land Cover\n") 
  

ff <- ggplot(df, aes(x=Month, group=1)) + 
  geom_line(aes(y=meanUrban), color="red") + 
  geom_errorbar(aes(x=Month, ymin=meanUrban-sdUrban, ymax=meanUrban+sdUrban), width=0.2, size=0.5,color="red")+
  geom_line(aes(y=meanCrop2), color="yellowgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanCrop-sdCrop2, ymax=meanCrop2+sdCrop2), width=0.2, size=0.5, color="yellowgreen")+
  geom_line(aes(y=meanFo), color="darkgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanFo-sdFo, ymax=meanFo+sdFo),width=0.2, size=0.5, color="darkgreen")+
  labs(title="Ta-Ts by Land Cover Type", 
       y="Ta-Ts (degrees C)", 
       x="Month")+
  scale_x_continuous(breaks=seq(1,12,3))+
  theme_bw()

gg <- ggplot(df, aes(x=Month, group=1)) + 
  geom_line(aes(y=meanUrban), color="red") + 
  geom_errorbar(aes(x=Month, ymin=meanUrban-sdUrban, ymax=meanUrban+sdUrban), width=0.2, size=0.5,color="red")+
  geom_line(aes(y=meanCrop), color="yellowgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanCrop-sdCrop, ymax=meanCrop+sdCrop), width=0.2, size=0.5, color="yellowgreen")+
  geom_line(aes(y=meanFo), color="darkgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanFo-sdFo, ymax=meanFo+sdFo),width=0.2, size=0.5, color="darkgreen")+
  labs(title="Ta-Ts by Land Cover Type", 
       y="Ta-Ts (degrees C)", 
       x="Month")+
  scale_x_continuous(breaks=seq(1,12,3))+
theme_bw()


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
  ylim(-4.4,5)


ff <- ggplot(df, aes(x=Month, group=1)) + 
  geom_line(aes(y=meanDec), color="orange") + 
  geom_errorbar(aes(x=Month, ymin=meanDec-sdDec, ymax=meanDec+sdDec), width=0.2, size=0.5,color="orange")+
  geom_line(aes(y=meanEv), color="green") + 
  geom_errorbar(aes(x=Month, ymin=meanEv-sdEv, ymax=meanEv+sdEv), width=0.2, size=0.5, color="green")+
  geom_line(aes(y=meanFo), color="darkgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanFo-sdFo, ymax=meanFo+sdFo),width=0.2, size=0.5, color="darkgreen")+
  labs(title="Ta-Ts by Forest Type", y="Ta-Ts (degrees C)",  x="Month")+
  scale_x_continuous(breaks=seq(1,12,3))+
  theme_bw()

#Just TA (how does it vary by land cover type)
Monthly_Ta <- function(year){
  filenamedaymet1 <- paste0("/Users/mallory/Documents/Temp_Project/Daymet/daymet_v3_tmax_monavg_", year, "_na.tif")
  daymet <- stack(filenamedaymet1)
  #Crop
  e2 <- extent(-40000, 2300000, -1600000, 400000)
  print("initial crop")
  cropped <- crop(daymet, e2)
  print("projecting raster")
  Ta <- projectRaster(cropped, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  ext <- extent(-88.775, -74.85, 29.25, 41.41667)
  print("second crop")
  Ta <- crop(Ta, ext)
  return(Ta)
}

Ta_2001 <- Monthly_Ta(2001)
Ta_2002 <- Monthly_Ta(2002)
Ta_2003 <- Monthly_Ta(2003)
Ta_2004 <- Monthly_Ta(2004)
Ta_2005 <- Monthly_Ta(2005)
Ta_2006 <- Monthly_Ta(2006)
Ta_2007 <- Monthly_Ta(2007)
Ta_2008 <- Monthly_Ta(2008)
Ta_2009 <- Monthly_Ta(2009)
Ta_2010 <- Monthly_Ta(2010)
Ta_2011 <- Monthly_Ta(2011)
Ta_2012 <- Monthly_Ta(2012)
Ta_2013 <- Monthly_Ta(2013)
Ta_2014 <- Monthly_Ta(2014)
Ta_2015 <- Monthly_Ta(2015)
Ta_2016 <- Monthly_Ta(2016)
Ta_2017 <- Monthly_Ta(2017)

Jan_Ta <- stack(Ta_2001[[1]], Ta_2002[[1]], Ta_2003[[1]], Ta_2004[[1]],
                   Ta_2005[[1]], Ta_2006[[1]], Ta_2007[[1]], Ta_2008[[1]],
                   Ta_2009[[1]], Ta_2010[[1]], Ta_2011[[1]], Ta_2012[[1]],
                   Ta_2013[[1]], Ta_2014[[1]], Ta_2015[[1]], Ta_2016[[1]],
                   Ta_2017[[1]])
Jan_Ta_Mean <- calc(Jan_Ta, mean_na)
plot(Jan_Ta_Mean)
Feb_Ta <- stack(Ta_2001[[2]], Ta_2002[[2]], Ta_2003[[2]], Ta_2004[[2]],
                   Ta_2005[[2]], Ta_2006[[2]], Ta_2007[[2]], Ta_2008[[2]],
                   Ta_2009[[2]], Ta_2010[[2]], Ta_2011[[2]], Ta_2012[[2]],
                   Ta_2013[[2]], Ta_2014[[2]], Ta_2015[[2]], Ta_2016[[2]],
                   Ta_2017[[2]])
Feb_Ta_Mean <- calc(Feb_Ta, mean_na)
Mar_Ta <- stack(Ta_2001[[3]], Ta_2002[[3]], Ta_2003[[3]], Ta_2004[[3]],
                   Ta_2005[[3]], Ta_2006[[3]], Ta_2007[[3]], Ta_2008[[3]],
                   Ta_2009[[3]], Ta_2010[[3]], Ta_2011[[3]], Ta_2012[[3]],
                   Ta_2013[[3]], Ta_2014[[3]], Ta_2015[[3]], Ta_2016[[3]],
                   Ta_2017[[3]])
Mar_Ta_Mean <- calc(Mar_Ta, mean_na)
Apr_Ta <- stack(Ta_2001[[4]], Ta_2002[[4]], Ta_2003[[4]], Ta_2004[[4]],
                   Ta_2005[[4]], Ta_2006[[4]], Ta_2007[[4]], Ta_2008[[4]],
                   Ta_2009[[4]], Ta_2010[[4]], Ta_2011[[4]], Ta_2012[[4]],
                   Ta_2013[[4]], Ta_2014[[4]], Ta_2015[[4]], Ta_2016[[4]],
                   Ta_2017[[4]])
Apr_Ta_Mean <- calc(Apr_Ta, mean_na)
May_Ta <- stack(Ta_2001[[5]], Ta_2002[[5]], Ta_2003[[5]], Ta_2004[[5]],
                   Ta_2005[[5]], Ta_2006[[5]], Ta_2007[[5]], Ta_2008[[5]],
                   Ta_2009[[5]], Ta_2010[[5]], Ta_2011[[5]], Ta_2012[[5]],
                   Ta_2013[[5]], Ta_2014[[5]], Ta_2015[[5]], Ta_2016[[5]],
                   Ta_2017[[5]])
May_Ta_Mean <- calc(May_Ta, mean_na)
Jun_Ta <- stack(Ta_2001[[6]], Ta_2002[[6]], Ta_2003[[6]], Ta_2004[[6]],
                   Ta_2005[[6]], Ta_2006[[6]], Ta_2007[[6]], Ta_2008[[6]],
                   Ta_2009[[6]], Ta_2010[[6]], Ta_2011[[6]], Ta_2012[[6]],
                   Ta_2013[[6]], Ta_2014[[6]], Ta_2015[[6]], Ta_2016[[6]],
                   Ta_2017[[6]])
Jun_Ta_Mean <- calc(Jun_Ta, mean_na)

Jul_Ta <- stack(Ta_2001[[7]], Ta_2002[[7]], Ta_2003[[7]], Ta_2004[[7]],
                   Ta_2005[[7]], Ta_2006[[7]], Ta_2007[[7]], Ta_2008[[7]],
                   Ta_2009[[7]], Ta_2010[[7]], Ta_2011[[7]], Ta_2012[[7]],
                   Ta_2013[[7]], Ta_2014[[7]], Ta_2015[[7]], Ta_2016[[7]],
                   Ta_2017[[7]])
Jul_Ta_Mean <- calc(Jul_Ta, mean_na)

Aug_Ta <- stack(Ta_2001[[8]], Ta_2002[[8]], Ta_2003[[8]], Ta_2004[[8]],
                   Ta_2005[[8]], Ta_2006[[8]], Ta_2007[[8]], Ta_2008[[8]],
                   Ta_2009[[8]], Ta_2010[[8]], Ta_2011[[8]], Ta_2012[[8]],
                   Ta_2013[[8]], Ta_2014[[8]], Ta_2015[[8]], Ta_2016[[8]],
                   Ta_2017[[8]])
Aug_Ta_Mean <- calc(Aug_Ta, mean_na)
Sep_Ta <- stack(Ta_2001[[9]], Ta_2002[[9]], Ta_2003[[9]], Ta_2004[[9]],
                   Ta_2005[[9]], Ta_2006[[9]], Ta_2007[[9]], Ta_2008[[9]],
                   Ta_2009[[9]], Ta_2010[[9]], Ta_2011[[9]], Ta_2012[[9]],
                   Ta_2013[[9]], Ta_2014[[9]], Ta_2015[[9]], Ta_2016[[9]],
                   Ta_2017[[9]])
Sep_Ta_Mean <- calc(Sep_Ta, mean_na)
Oct_Ta <- stack(Ta_2001[[10]], Ta_2002[[10]], Ta_2003[[10]], Ta_2004[[10]],
                   Ta_2005[[10]], Ta_2006[[10]], Ta_2007[[10]], Ta_2008[[10]],
                   Ta_2009[[10]], Ta_2010[[10]], Ta_2011[[10]], Ta_2012[[10]],
                   Ta_2013[[10]], Ta_2014[[10]], Ta_2015[[10]], Ta_2016[[10]],
                   Ta_2017[[10]])
Oct_Ta_Mean <- calc(Oct_Ta, mean_na)
Nov_Ta <- stack(Ta_2001[[11]], Ta_2002[[11]], Ta_2003[[11]], Ta_2004[[11]],
                   Ta_2005[[11]], Ta_2006[[11]], Ta_2007[[11]], Ta_2008[[11]],
                   Ta_2009[[11]], Ta_2010[[11]], Ta_2011[[11]], Ta_2012[[11]],
                   Ta_2013[[11]], Ta_2014[[11]], Ta_2015[[11]], Ta_2016[[11]],
                   Ta_2017[[11]])
Nov_Ta_Mean <- calc(Nov_Ta, mean_na)
Dec_Ta <- stack(Ta_2001[[12]], Ta_2002[[12]], Ta_2003[[12]], Ta_2004[[12]],
                   Ta_2005[[12]], Ta_2006[[12]], Ta_2007[[12]], Ta_2008[[12]],
                   Ta_2009[[12]], Ta_2010[[12]], Ta_2011[[12]], Ta_2012[[12]],
                   Ta_2013[[12]], Ta_2014[[12]], Ta_2015[[12]], Ta_2016[[12]],
                   Ta_2017[[12]])
Dec_Ta_Mean <- calc(Dec_Ta, mean_na)

Ta<- stack(Jan_Ta_Mean, Feb_Ta_Mean, Mar_Ta_Mean, Apr_Ta_Mean, May_Ta_Mean,
              Jun_Ta_Mean, Jul_Ta_Mean, Aug_Ta_Mean, Sep_Ta_Mean, Oct_Ta_Mean, 
              Nov_Ta_Mean, Dec_Ta_Mean)
names(Ta) <- c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec")
plot(Ta)
writeRaster(Ta, "/Users/mallory/Documents/Temp_Project/Ta_All.tif")
# create a level plot - plot
cols <- colorRampPalette(rev(brewer.pal(15,"RdBu")))
my.at <- seq(0,35,2)
levelplot(Ta, at=my.at, main="Air Temperature (Ta)",
          col.regions=(cols))
#Recreating figure 4 (by land cover type) but for air temperature 
FomaskTa <- resample(Fomask, Ta, method="bilinear")
CropmaskTa <- resample(Cropmask2, Ta, method="bilinear")
UrbanmaskTa <- resample(Urbanmask, Ta, method="bilinear")

Urban_Diff <- mask(Ta, UrbanmaskTa)
Crop_Diff <- mask(Ta,CropmaskTa)
Fo_Diff <- mask(Ta, FomaskTa)

#By Lat instead of by land cover type
DiffsBrick <- brick(Diffs)
extent(DiffsBrick)
#High_Lat_Ext <- extent(-88.775, -74.85, 37.75, 41.41667)
#MidHigh_Lat_Ext <- extent(-88.775, -74.85, 34.25, 37.75)
#MidLow_Lat_Ext <- extent(-88.775, -74.85, 32.75, 34.25)
#Low_Lat_Ext <- extent(-88.775, -74.85, 29.25, 32.75)

High_Lat_Ext <- extent(-88.775, -74.85, 37.75, 41.41667)
MidHigh_Lat_Ext <- extent(-88.775, -74.85, 33.25, 37.25)
MidLow_Lat_Ext <- extent(-88.775, -74.85, 29.25, 33.25)

plot(DiffsBrick)
High_Lat <- crop(Diffs, High_Lat_Ext)
MidHigh_Lat <- crop(Diffs,MidHigh_Lat_Ext)
MidLow_Lat <- crop(Diffs, MidLow_Lat_Ext)
Low_Lat <- crop(Diffs, Low_Lat_Ext)

fg <- data.frame(Month=c(1:12))

fg$Month <- as.numeric(fg$Month)
fg$meanHigh <- as.numeric(cellStats(High_Lat, stat='mean', na.rm=TRUE))
fg$sdHigh <- as.numeric(cellStats(High_Lat, stat='sd', na.rm=TRUE))
fg$seHigh <- (fg$sdHigh)/(sqrt(ncell(High_Lat)))
fg$meanMidHigh <- as.numeric(cellStats(MidHigh_Lat, stat='mean', na.rm=TRUE))
fg$sdMidHigh <- as.numeric(cellStats(MidHigh_Lat, stat='sd', na.rm=TRUE))
fg$seMidHigh <- (fg$sdMidHigh)/(sqrt(ncell(MidHigh_Lat)))
fg$meanMidLow <- as.numeric(cellStats(MidLow_Lat, stat='mean', na.rm=TRUE))
fg$sdMidLow <- as.numeric(cellStats(MidLow_Lat, stat='sd', na.rm=TRUE))
fg$seMidLow <- (fg$sdMidLow)/(sqrt(ncell(MidLow_Lat)))
fg$meanLow <- as.numeric(cellStats(Low_Lat, stat='mean', na.rm=TRUE))
fg$sdLow <- as.numeric(cellStats(Low_Lat, stat='sd', na.rm=TRUE))
fg$seLow <- (fg$sdLow)/(sqrt(ncell(Low_Lat)))




jj <- ggplot(fg, aes(x=Month, group=1)) + 
  geom_line(aes(y=meanHigh), color="purple") + 
  geom_errorbar(aes(x=Month, ymin=meanHigh-seHigh, ymax=meanHigh+seHigh), width=0.2, size=0.5,color="purple")+
  geom_line(aes(y=meanMidHigh), color="blue") + 
  geom_errorbar(aes(x=Month, ymin=meanMidHigh-seMidHigh, ymax=meanMidHigh+seMidHigh), width=0.2, size=0.5, color="blue")+
  geom_line(aes(y=meanMidLow), color="red") + 
  geom_errorbar(aes(x=Month, ymin=meanMidLow-seMidLow, ymax=meanMidLow+seMidLow),width=0.2, size=0.5, color="red")+
  #geom_line(aes(y=meanLow), color="orange") + 
  #geom_errorbar(aes(x=Month, ymin=meanLow-seLow, ymax=meanLow+seLow),width=0.2, size=0.5, color="orange")+
  labs(title="Ta-Ts by Latitude", y="Ta-Ts (degrees C)",  x="Month")+
  scale_x_continuous(breaks=seq(1,12,3))+
  theme_bw()+
  ylim(-4.4,5)

jj
hh

#
#----------now looking at forest data
F1 <- raster("/Users/mallory/Documents/Temp_Project/NA_TREEAGE_1096/data/sc_age06_1km.tif")
F2 <- raster("/Users/mallory/Documents/Temp_Project/NA_TREEAGE_1096/data/se_age06_1km.tif")
F3 <- raster("/Users/mallory/Documents/Temp_Project/NA_TREEAGE_1096/data/nl_age06_1km.tif")
F4 <- raster("/Users/mallory/Documents/Temp_Project/NA_TREEAGE_1096/data/ne_age06_1km.tif")
F5 <- raster("/Users/mallory/Documents/Temp_Project/NA_TREEAGE_1096/data/np_age06_1km.tif")

F_m1 <- merge(F1, F2,tolerance = 0.2)
F_m2 <- merge(F_m1, F3, tolerance=0.2)
F_m3 <- merge(F_m2, F4, tolerance=0.2)
F4 <- merge(F_m3, F5,tolerance=0.2)
Forest_Proj <- projectRaster(F4, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
plot(Forest_Proj)
ext <- extent(Diffs)
Forest_Proj_crop <- crop(Forest_Proj, ext)
Forest_Proj
Forest_Age<- resample(Forest_Proj_crop,LST,method='bilinear')
plot(Forest_Age)
Diffs
Test <- Diffs[[7]]
Check <- spatialEco::rasterCorrelation(Test, Forest_Age, s = 5, type = "pearson", file.name = NULL)

Check2 <- spatialEco::rasterCorrelation(Test, Forest_Age, s=5, type="covariance",file.name=NULL)
cor(values(Diffs[[8]]), values(Forest_Age), use = "na.or.complete")

cuts=c(-1,0,1) #set breaks
pal <- colorRampPalette(c("Blue","Yellow"))
plot(Check, breaks=cuts, col = pal(25))
r.cor <- rasterCorrelation(Test, Forest_Age, s = 5, type = "pearson") 
plot(r.cor)
r.cor2 <- rasterCorrelation(Diffs[[6]], Forest_Age, s=5, type="pearson")
plot(r.cor2)
plot(r.cor)


