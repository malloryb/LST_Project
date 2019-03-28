#Making figures for LST Manuscript
#Figure 1a: Changing air temperature (re-create 'warming hole')
#Figure 1b: Average difference between surface temperature and air temperature from Remote sensing (by pixel)
#Figure 2: Difference in surface and air temperature by forest stand age (correlation map)
#Figure 3: Lily's figure
#Figure 4: Flux synthesis figure


#Figure 1a---------------------
#Using University of Delaware Air Temperature & Precipitation
# Data product accessed from: https://www.esrl.noaa.gov/psd/data/gridded/data.UDel_AirT_Precip.html
library(ncdf4)
library(raster)
library(proj4)
library(rgdal)
library(gdalUtils)
library(spatialEco)
library(greenbrown)
library(RColorBrewer)
library(MODIS)
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
#install.packages("greenbrown", repos="http://R-Forge.R-project.org")
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


#Loading up modis surface temperature data
#Want to create Ta-Ts map for the US 
#Checking out 1 file
#Creating monthly Ts Files
testmodis <- raster("/Users/mallory/Documents/APPEARS_LST/MOD11A2.006_LST_Day_1km_doy2000065_aid0001.tif")
plot(testmodis)
plot((testmodis*0.02 - 273.15))
#Daymet end:
#Get Tmax stacks, crop to extent, and reproject as necessary! Already made monthly average Tmax 
#rasters for years 1980-2017! 
brick("/Users/mallory/Documents/Temp_Project/Daymet/daymet_v3_tmax_monavg_1980_na.tif")
#LST:
#Rescale, create monthly composites, and stack them all up, do it for each year
#To-dos: 
setwd("/Users/mallory/Documents/APPEARS_LST/")
mean_na <- function(x) {
  mean(x,na.rm=T)
}

Monthly_LST <- function(year){
flist <- list.files(pattern= paste0("(LST_Day).*doy", year, sep=""))
flist
str(flist)
year <- "2001"
(paste0("(LST_Day).*doy", year, sep=""))
Process_LST <- function(x){
    filename <- paste(x)
    print(filename)
    rast <- raster(filename)
    year <- substr(filename, 28,31)
    print(year)
    day <- substr(filename, 32,34)
    print(day)
    rast_date <- as.Date(paste(day, year, sep="-"), format="%j-%Y")
    print(rast_date)
    date <- (as.character(rast_date))
    rast <- setMinMax(rast)
    rast <- (rast*0.02 -273.15)
    rast <- setNames(rast, date)
    return(rast)
}

LST <- stack(lapply(flist, Process_LST))

LST_Jan <- stack(LST[[1]], LST[[2]], LST[[3]], LST[[4]])
Jan_mean <- calc(LST_Jan, mean_na)
print("Jan_mean")
LST_Feb <- stack(LST[[5]], LST[[6]], LST[[7]], LST[[8]])
Feb_mean <- calc(LST_Feb, mean_na)
print("Feb_mean")
LST_Mar <- stack(LST[[9]], LST[[10]], LST[[11]], LST[[12]])
Mar_mean <- calc(LST_Mar, mean_na)
print("Mar_mean")
LST_Apr <- stack(LST[[13]], LST[[14]], LST[[15]])
Apr_mean <- calc(LST_Apr, mean_na)
print("Apr_mean")
LST_May <- stack(LST[[16]], LST[[17]], LST[[18]], LST[[19]])
May_mean <- calc(LST_May, mean_na)
print("May_mean")
LST_Jun <- stack(LST[[20]], LST[[21]], LST[[22]], LST[[23]])
Jun_mean <- calc(LST_Jun, mean_na)
print("Jun_mean")
LST_Jul <- stack(LST[[24]], LST[[25]], LST[[26]], LST[[27]])
Jul_mean <- calc(LST_Jul, mean_na)
print("Jul_mean")
LST_Aug <- stack(LST[[28]], LST[[29]], LST[[30]])
Aug_mean <- calc(LST_Aug, mean_na)
print("Aug_mean")
LST_Sep <- stack(LST[[31]], LST[[32]], LST[[33]], LST[[34]])
Sep_mean <- calc(LST_Sep, mean_na)
print("Sep_mean")
LST_Oct <- stack(LST[[35]], LST[[36]], LST[[37]])
Oct_mean <- calc(LST_Oct, mean_na)
print("Oct_mean")
LST_Nov <- stack(LST[[38]], LST[[39]], LST[[40]], LST[[41]])
Nov_mean <- calc(LST_Nov, mean_na)
print("Nov_mean")
LST_Dec <- stack(LST[[42]], LST[[43]], LST[[44]], LST[[45]])
Dec_mean <- calc(LST_Dec, mean_na)
print("Dec_mean")
yearstack <- stack(Jan_mean, Feb_mean, Mar_mean, Apr_mean, May_mean, Jun_mean, Jul_mean, Aug_mean, Sep_mean, Oct_mean, Nov_mean, Dec_mean)
return(yearstack)

}

LST_2001 <- Monthly_LST(2001)
writeRaster(LST_2001, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2001.tif")

LST_2002 <- Monthly_LST(2002)
writeRaster(LST_2002, "MODIS_LST_2002.tif")

LST_2003 <- Monthly_LST(2003)
writeRaster(LST_2003, "MODIS_LST_2003.tif")

LST_2004 <- Monthly_LST(2004)
writeRaster(LST_2004, "MODIS_LST_2004.tif")


LST_2005 <- Monthly_LST(2005)
writeRaster(LST_2005, "MODIS_LST_2005.tif")

LST_2006 <- Monthly_LST(2006)
writeRaster(LST_2006, "MODIS_LST_2006.tif")

LST_2007 <- Monthly_LST(2007)
writeRaster(LST_2007, "MODIS_LST_2007.tif")

LST_2008 <- Monthly_LST(2008)
writeRaster(LST_2008, "MODIS_LST_2008.tif")

LST_2009 <- Monthly_LST(2009)
writeRaster(LST_2009, "MODIS_LST_2009.tif")

LST_2010 <- Monthly_LST(2010)
writeRaster(LST_2010, "MODIS_LST_2010.tif")

LST_2011 <- Monthly_LST(2011)
writeRaster(LST_2011, "MODIS_LST_2011.tif")

LST_2012 <- Monthly_LST(2012)
writeRaster(LST_2012, "MODIS_LST_2012.tif")
