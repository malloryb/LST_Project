#Moving steps to create intermediate files for "Key_Figures.R" over into this file

#Creating Monthly LST files for years 2000 - 2017 from MODIS LST Data -------------
setwd("/Users/mallory/Documents/APPEARS_LST/")
mean_na <- function(x) {
  mean(x,na.rm=T)
}

#Creating monthly Ts Files
#Rescale, create monthly composites, and stack them all up, do it for each year
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
  LST_Feb <- stack(LST[[5]], LST[[6]], LST[[8]], LST[[8]])
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
#plot(brick("/Users/mallory/Documents/Temp_Project/MODIS_LST_2001.tif"))

LST_2002 <- Monthly_LST(2002)
writeRaster(LST_2002, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2002.tif")

LST_2003 <- Monthly_LST(2003)
writeRaster(LST_2003, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2003.tif")

LST_2004 <- Monthly_LST(2004)
writeRaster(LST_2004, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2004.tif")

LST_2005 <- Monthly_LST(2005)
writeRaster(LST_2005, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2005.tif")

LST_2006 <- Monthly_LST(2006)
writeRaster(LST_2006, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2006.tif")

LST_2007 <- Monthly_LST(2007)
writeRaster(LST_2007, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2007.tif")

LST_2008 <- Monthly_LST(2008)
writeRaster(LST_2008, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2008.tif")

LST_2009 <- Monthly_LST(2009)
writeRaster(LST_2009, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2009.tif")

LST_2010 <- Monthly_LST(2010)
writeRaster(LST_2010, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2010.tif")

LST_2011 <- Monthly_LST(2011)
writeRaster(LST_2011, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2011.tif")

LST_2012 <- Monthly_LST(2012)
writeRaster(LST_2012, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2012.tif")

LST_2013 <- Monthly_LST(2013)
writeRaster(LST_2013, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2013.tif")

LST_2014 <- Monthly_LST(2014)
writeRaster(LST_2014, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2014.tif")

LST_2015 <- Monthly_LST(2015)
writeRaster(LST_2015, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2015.tif")

LST_2016 <- Monthly_LST(2016)
writeRaster(LST_2016, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2016.tif")

LST_2017 <- Monthly_LST(2017)
writeRaster(LST_2017, "/Users/mallory/Documents/Temp_Project/MODIS_LST_2017.tif")

#Creating Ta-Ts maps from MODIS and Daymet Data --------------------------
#Join up LST and daymet: crop daymet and reproject to proper extent
#Increase raster chunk size for this 
rasterOptions(tmpdir="C:\\",tmptime = 24,progress="text",timer=TRUE,overwrite = T,chunksize=2e+08,maxmemory=1e+8)

#Function per year that returns: Ta-Ts (for all 12 months)
Monthly_Ta_Ts <- function(year){
  filenamelst <- paste0("/Users/mallory/Documents/Temp_Project/MODIS_LST_", year, ".tif", sep="")
  filenamedaymet1 <- paste0("/Users/mallory/Documents/Temp_Project/Daymet/daymet_v3_tmax_monavg_", year, "_na.tif")
  #filenamedaymet2 <- paste0("/Users/mallory/Documents/Temp_Project/Daymet/daymet_v3_tmin_monavg_", year, "_na.tif")
  LST <- stack(filenamelst)
  daymet <- stack(filenamedaymet1)
  #Tmin <- stack(filenamedaymet2)
  #Ta <- overlay(Tmax, Tmin, fun=mean_na)
  #Crop
  e2 <- extent(-40000, 2300000, -1600000, 400000)
  print("initial crop")
  cropped <- crop(daymet, e2)
  #cropped2 <- calc(cropped, fun = mean)
  print("projecting raster")
  #Project raster to lat/long coordinates
  projected_raster <- projectRaster(cropped, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
  ext <- extent(LST)
  Ta_res <- crop(projected_raster, ext)
  print("resampling")
  Ta<- resample(Ta_res,LST,method='bilinear')
  #Crop to study extent
  stack(Ta, LST )
  plot(LST)
  plot(Ta)
  print("Subtracting")
  Ta_Ts <- Ta-LST
  plot(Ta_Ts)
  return(Ta_Ts)
}

Diff_2001 <- Monthly_Ta_Ts(2001)
Diff_2002 <- Monthly_Ta_Ts(2002)
Diff_2003 <- Monthly_Ta_Ts(2003)
Diff_2004 <- Monthly_Ta_Ts(2004)
Diff_2005 <- Monthly_Ta_Ts(2005)
Diff_2006 <- Monthly_Ta_Ts(2006)
Diff_2007 <- Monthly_Ta_Ts(2007)
Diff_2008 <- Monthly_Ta_Ts(2008)
Diff_2009 <- Monthly_Ta_Ts(2009)
Diff_2010 <- Monthly_Ta_Ts(2010)
Diff_2011 <- Monthly_Ta_Ts(2011)
Diff_2012 <- Monthly_Ta_Ts(2012)
Diff_2013 <- Monthly_Ta_Ts(2013)
Diff_2014 <- Monthly_Ta_Ts(2014)
Diff_2015 <- Monthly_Ta_Ts(2015)
Diff_2016 <- Monthly_Ta_Ts(2016)
Diff_2017 <- Monthly_Ta_Ts(2017)

Jan_Diffs <- stack(Diff_2001[[1]], Diff_2002[[1]], Diff_2003[[1]], Diff_2004[[1]],
                   Diff_2005[[1]], Diff_2006[[1]], Diff_2007[[1]], Diff_2008[[1]],
                   Diff_2009[[1]], Diff_2010[[1]], Diff_2011[[1]], Diff_2012[[1]],
                   Diff_2013[[1]], Diff_2014[[1]], Diff_2015[[1]], Diff_2016[[1]],
                   Diff_2017[[1]])
Jan_Diffs_Mean <- calc(Jan_Diffs, mean_na)
plot(Jan_Diffs_Mean)
Feb_Diffs <- stack(Diff_2001[[2]], Diff_2002[[2]], Diff_2003[[2]], Diff_2004[[2]],
                   Diff_2005[[2]], Diff_2006[[2]], Diff_2007[[2]], Diff_2008[[2]],
                   Diff_2009[[2]], Diff_2010[[2]], Diff_2011[[2]], Diff_2012[[2]],
                   Diff_2013[[2]], Diff_2014[[2]], Diff_2015[[2]], Diff_2016[[2]],
                   Diff_2017[[2]])
Feb_Diffs_Mean <- calc(Feb_Diffs, mean_na)
Mar_Diffs <- stack(Diff_2001[[3]], Diff_2002[[3]], Diff_2003[[3]], Diff_2004[[3]],
                   Diff_2005[[3]], Diff_2006[[3]], Diff_2007[[3]], Diff_2008[[3]],
                   Diff_2009[[3]], Diff_2010[[3]], Diff_2011[[3]], Diff_2012[[3]],
                   Diff_2013[[3]], Diff_2014[[3]], Diff_2015[[3]], Diff_2016[[3]],
                   Diff_2017[[3]])
Mar_Diffs_Mean <- calc(Mar_Diffs, mean_na)
Apr_Diffs <- stack(Diff_2001[[4]], Diff_2002[[4]], Diff_2003[[4]], Diff_2004[[4]],
                   Diff_2005[[4]], Diff_2006[[4]], Diff_2007[[4]], Diff_2008[[4]],
                   Diff_2009[[4]], Diff_2010[[4]], Diff_2011[[4]], Diff_2012[[4]],
                   Diff_2013[[4]], Diff_2014[[4]], Diff_2015[[4]], Diff_2016[[4]],
                   Diff_2017[[4]])
Apr_Diffs_Mean <- calc(Apr_Diffs, mean_na)
May_Diffs <- stack(Diff_2001[[5]], Diff_2002[[5]], Diff_2003[[5]], Diff_2004[[5]],
                   Diff_2005[[5]], Diff_2006[[5]], Diff_2007[[5]], Diff_2008[[5]],
                   Diff_2009[[5]], Diff_2010[[5]], Diff_2011[[5]], Diff_2012[[5]],
                   Diff_2013[[5]], Diff_2014[[5]], Diff_2015[[5]], Diff_2016[[5]],
                   Diff_2017[[5]])
May_Diffs_Mean <- calc(May_Diffs, mean_na)
Jun_Diffs <- stack(Diff_2001[[6]], Diff_2002[[6]], Diff_2003[[6]], Diff_2004[[6]],
                   Diff_2005[[6]], Diff_2006[[6]], Diff_2007[[6]], Diff_2008[[6]],
                   Diff_2009[[6]], Diff_2010[[6]], Diff_2011[[6]], Diff_2012[[6]],
                   Diff_2013[[6]], Diff_2014[[6]], Diff_2015[[6]], Diff_2016[[6]],
                   Diff_2017[[6]])
Jun_Diffs_Mean <- calc(Jun_Diffs, mean_na)

Jul_Diffs <- stack(Diff_2001[[7]], Diff_2002[[7]], Diff_2003[[7]], Diff_2004[[7]],
                   Diff_2005[[7]], Diff_2006[[7]], Diff_2007[[7]], Diff_2008[[7]],
                   Diff_2009[[7]], Diff_2010[[7]], Diff_2011[[7]], Diff_2012[[7]],
                   Diff_2013[[7]], Diff_2014[[7]], Diff_2015[[7]], Diff_2016[[7]],
                   Diff_2017[[7]])
Jul_Diffs_Mean <- calc(Jul_Diffs, mean_na)

Aug_Diffs <- stack(Diff_2001[[8]], Diff_2002[[8]], Diff_2003[[8]], Diff_2004[[8]],
                   Diff_2005[[8]], Diff_2006[[8]], Diff_2007[[8]], Diff_2008[[8]],
                   Diff_2009[[8]], Diff_2010[[8]], Diff_2011[[8]], Diff_2012[[8]],
                   Diff_2013[[8]], Diff_2014[[8]], Diff_2015[[8]], Diff_2016[[8]],
                   Diff_2017[[8]])
Aug_Diffs_Mean <- calc(Aug_Diffs, mean_na)
Sep_Diffs <- stack(Diff_2001[[9]], Diff_2002[[9]], Diff_2003[[9]], Diff_2004[[9]],
                   Diff_2005[[9]], Diff_2006[[9]], Diff_2007[[9]], Diff_2008[[9]],
                   Diff_2009[[9]], Diff_2010[[9]], Diff_2011[[9]], Diff_2012[[9]],
                   Diff_2013[[9]], Diff_2014[[9]], Diff_2015[[9]], Diff_2016[[9]],
                   Diff_2017[[9]])
Sep_Diffs_Mean <- calc(Sep_Diffs, mean_na)
Oct_Diffs <- stack(Diff_2001[[10]], Diff_2002[[10]], Diff_2003[[10]], Diff_2004[[10]],
                   Diff_2005[[10]], Diff_2006[[10]], Diff_2007[[10]], Diff_2008[[10]],
                   Diff_2009[[10]], Diff_2010[[10]], Diff_2011[[10]], Diff_2012[[10]],
                   Diff_2013[[10]], Diff_2014[[10]], Diff_2015[[10]], Diff_2016[[10]],
                   Diff_2017[[10]])
Oct_Diffs_Mean <- calc(Oct_Diffs, mean_na)
Nov_Diffs <- stack(Diff_2001[[11]], Diff_2002[[11]], Diff_2003[[11]], Diff_2004[[11]],
                   Diff_2005[[11]], Diff_2006[[11]], Diff_2007[[11]], Diff_2008[[11]],
                   Diff_2009[[11]], Diff_2010[[11]], Diff_2011[[11]], Diff_2012[[11]],
                   Diff_2013[[11]], Diff_2014[[11]], Diff_2015[[11]], Diff_2016[[11]],
                   Diff_2017[[11]])
Nov_Diffs_Mean <- calc(Nov_Diffs, mean_na)
Dec_Diffs <- stack(Diff_2001[[12]], Diff_2002[[12]], Diff_2003[[12]], Diff_2004[[12]],
                   Diff_2005[[12]], Diff_2006[[12]], Diff_2007[[12]], Diff_2008[[12]],
                   Diff_2009[[12]], Diff_2010[[12]], Diff_2011[[12]], Diff_2012[[12]],
                   Diff_2013[[12]], Diff_2014[[12]], Diff_2015[[12]], Diff_2016[[12]],
                   Diff_2017[[12]])
Dec_Diffs_Mean <- calc(Dec_Diffs, mean_na)

Diffs<- stack(Jan_Diffs_Mean, Feb_Diffs_Mean, Mar_Diffs_Mean, Apr_Diffs_Mean, May_Diffs_Mean,
              Jun_Diffs_Mean, Jul_Diffs_Mean, Aug_Diffs_Mean, Sep_Diffs_Mean, Oct_Diffs_Mean, 
              Nov_Diffs_Mean, Dec_Diffs_Mean)
names(Diffs) <- c("Jan", "Feb", "Mar","Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct","Nov", "Dec")
writeRaster(Diffs, "/Users/mallory/Documents/Temp_Project/Ta_Ts_All.tif")