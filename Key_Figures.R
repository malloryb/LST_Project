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


#Loading up 
