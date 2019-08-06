#Making figures for LST Manuscript
#Figure 1: Air temperature trends (re-create 'warming hole')
#Figure 2: Average difference between surface temperature and air temperature from Remote sensing (by pixel)
#Figure 3: Difference in surface and air temperature by forest stand age (correlation map)
#Figure 3: Lily's figure
#Figure 4: Flux synthesis figure

#Load packages
Packages <- c("here", "ncdf4", "ggplot2", "reshape2", "raster", "proj4", "rgdal", "gdalUtils", "greenbrown", "RColorBrewer",
              "MODIS", "rasterVis", "gridExtra", "plyr", "gridBase", "devtools","spatialEco")

library(greenbrown)
library(rasterVis)
library(devtools)
library(maps)
library(mapdata)
library(maptools)
#
sessionInfo()
lapply(Packages, library, character.only = TRUE)
#Set raster options
rasterOptions(tmpdir="C:\\",tmptime = 24,progress="text",timer=TRUE,overwrite = T,chunksize=2e+08,maxmemory=1e+8)

 #Figure 1---------------------
#Create bounding box out of extent of Diffs
Diffs <- brick("/Users/mallory/Documents/Temp_Project/Ta_Ts_All.tif")
library(sp)
e <- as(raster::extent(Diffs), "SpatialPolygons")
proj4string(e) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
plot(e)
#Using University of Delaware Air Temperature & Precipitation
# Data product accessed from: https://www.esrl.noaa.gov/psd/data/gridded/data.UDel_AirT_Precip.html
#Open air temp file (0.5 degree)
fname <- ("/Users/mallory/Documents/Temp_Project/air.mon.mean.v501.nc")
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
#kendall_raster <- raster.kendall(TAS_test, tau=TRUE, p.value=TRUE)
#writeRaster(kendall_raster, "Temp_Kendall_US.tif")
kendall_raster <- raster("/Users/mallory/Documents/Temp_Project/Temp_Kendall_US.tif")
#Trying to center color ramp so white is at zero:
pal <- colorRampPalette(c("blue","cadetblue1", "lightblue", "white","red", "red3"))
#kendall_raster[kendall_raster < -0.00245] <- NA
plot(kendall_raster, col=pal(10))
#Trying with greenbrown
#Figure 1a: Change over time
#greenbrown_test <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=1)
greenbrown_test <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=0)
plot(greenbrown_test[[2]], col=pal(10), main="Slope of temperature trend: 1900-present (Degrees C per year)")
#Change over time in terms of degrees C per 50 years
temp_raster <- greenbrown_test[[2]]
#writeRaster(temp_raster, "/Users/mallory/Documents/Temp_Project/Temp_Change_Map.tif")
#Getting color ramp to diverge at zero
temp_raster <- raster("/Users/mallory/Documents/Temp_Project/Temp_Change_Map.tif")
temp_raster[temp_raster < -0.029] <-NA
devtools::source_gist('306e4b7e69c87b1826db')
pal <- colorRampPalette(rev(brewer.pal(11, 'RdBu')))
#Gotta fix this from the -360 again (with Mar package)
# Switching from a raster to a matrix of class 'bathy'
library(marmap)
temp <- as.bathy(temp_raster)
summary(temp)

# Changing the relevant longitude
names <- as.numeric(row.names(temp))
names[names > 180] <- names[names > 180] - 360

# Renaming the longitudes and switching back from a 'bathy' object to a raster
rownames(temp) <- names
temp_raster.modified <- as.raster(temp)
while (!is.null(dev.list()))  dev.off()
png("/Users/mallory/Documents/Temp_Project/Fig1a.png", width=4, height=4, units="in", res=300)
p <- levelplot(temp_raster.modified*50, margin=F, col.regions=(rev(brewer.pal(11,"RdBu"))), pretty=T, interpolate=T)+latticeExtra::layer(sp.polygons(e))+latticeExtra::layer(sp.polygons(bPols))
diverge0(p, ramp=pal)
dev.off()

#Figure 1b-----------------
#Testing break points
plot(TAS_test)
greenbrown_test1break <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=1)
greenbrown_maxbreak <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=1, funAnnual=max)
plot(greenbrown_maxbreak)
plot(greenbrown_maxbreak[[3]])
plot(greenbrown_test1break)
plot(greenbrown_test1break, col=(brewer.pal(n=6, name='Spectral')))
breaks <- greenbrown_test1break[[3]]
# Switching from a raster to a matrix of class 'bathy'
library(marmap)
temp <- as.bathy(breaks)
summary(temp)

# Changing the relevant longitude
names <- as.numeric(row.names(temp))
names[names > 180] <- names[names > 180] - 360

# Renaming the longitudes and switching back from a 'bathy' object to a raster
rownames(temp) <- names
breaks.modified <- as.raster(temp)
#Plot finally
plot(greenbrown_test1break[[3]], zlim=c(1910,2010), col=terrain(100), main="Break point in temperature trend (1900-present)")
my.at <- seq(1910, 2010, 10)
#Add boundary
ext <- as.vector(extent(breaks.modified))
#ext[1] <- 233-360
#ext[2] <- 290-360
boundaries <- map('worldHires', fill=TRUE,
                  xlim=ext[1:2], ylim=ext[3:4],
                  plot=FALSE)
IDs <- sapply(strsplit(boundaries$names, ":"), function(x) x[1])
bPols <- map2SpatialPolygons(boundaries, IDs=IDs,
                             proj4string=CRS(projection(breaks)))

png("/Users/mallory/Documents/Temp_Project/Fig1b.png", width=4, height=4, units="in", res=300)
levelplot(breaks.modified, at=my.at, margin=F,col.regions=((brewer.pal(12,"Paired"))))+latticeExtra::layer(sp.polygons(e))+latticeExtra::layer(sp.polygons(bPols))
dev.off()
plot(cars)
trendclassmap <- TrendClassification(greenbrown_test1break, min.length=8, max.pval=0.05)
plot(trendclassmap, col=pal(n=11), legend.width=2) 
#Seeing slope differences between two sections
slopes <- stack(greenbrown_test1break[[4]], greenbrown_test1break[[5]])
slopes[slopes < -0.015] <-NA
slopes[slopes > 0.015] <-NA



plot(greenbrown_test1break[[4]], col=pal(10))
plot(greenbrown_test1break[[5]], col=pal(10))
slope_diff <- greenbrown_test1break[[5]]-greenbrown_test1break[[4]]
plot(slope_diff, col=pal(10))
gradient <- TrendGradient(TAS_test, start=c(1900, 1), freq=12)
#Trying with 2 break point
greenbrown_test2break <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=2)
plot(greenbrown_test2break)
plot(greenbrown_test1break, col=(brewer.pal(n=6, name='Spectral')))
par(mfrow=c(1,2))
plot(greenbrown_test2break[[4]], zlim=c(1910,2010), col= rainbow(100), main="Break point 1")
plot(greenbrown_test2break[[5]], zlim=c(1910,2010), col= rainbow(100), main="Break point 2")
#Trying with 3 break points
greenbrown_test3break <- TrendRaster(TAS_test, start=c(1900,1), freq=12, breaks=3)
plot(greenbrown_test3break)
plot(greenbrown_test3break, col=(brewer.pal(n=6, name='Spectral')))
par(mfrow=c(1,3))
plot(greenbrown_test3break[[5]], zlim=c(1910,2010), col=  rainbow(100), main="Break point 1 in temperature trend (1900-present)")
plot(greenbrown_test3break[[6]], zlim=c(1910,2010), col=  rainbow(100), main="Break point 2 in temperature trend (1900-present)")
plot(greenbrown_test3break[[7]], zlim=c(1910,2010), col=  rainbow(100), main="Break point 3 in temperature trend (1900-present)")


#Figure 2: Create Ta-Ts map for the US ---------
Diffs <- brick("/Users/mallory/Documents/Temp_Project/Ta_Ts_All.tif")
cols <- colorRampPalette(brewer.pal(9,"RdBu"))
my.at <- seq(-6,6,1)
# create a level plot
levelplot(Diffs, at=my.at, main="Difference between Air Temperature and Surface Temperature (Ta-Ts)",
          col=(cols))
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
writeRaster(LC_proj, "/Users/mallory/Documents/Temp_Project/landcvi020l_nt00016/landcover_proj.tif")
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

#Levelplots by land cover type-------------
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
plot(Fo_Diff)
#Buffer analysis------------
Blob_analysis <- function(x, y){
  names(x) <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                "Oct", "Nov", "Dec")
  Blob <-  as.data.frame(extract(x,y, buffer=300))
  Blob2 <- as.data.frame(extract(x,y, buffer=500))
  Blob3 <- colMeans(as.data.frame(extract(x,y, buffer=1000)), na.rm=TRUE)
  Blob4 <- colMeans(as.data.frame(extract(x,y, buffer=1500)), na.rm=TRUE)
  Blob5 <- colMeans(as.data.frame(extract(x,y, buffer=2000)), na.rm=TRUE)
  Blob6 <- colMeans(as.data.frame(extract(x,y, buffer=3000)), na.rm=TRUE)
  Blob7 <- colMeans(as.data.frame(extract(x,y, buffer=4000)), na.rm=TRUE)
  Blob8 <- colMeans(as.data.frame(extract(x,y, buffer=5000)), na.rm=TRUE)
  Blob9 <- colMeans(as.data.frame(extract(x,y, buffer=7500)), na.rm=TRUE)
  Blob10 <- colMeans(as.data.frame(extract(x,y, buffer=10000)), na.rm=TRUE)
  
  
  melted300 <- as.data.frame(as.numeric(t(Blob)))
  melted300$month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                        "Oct", "Nov", "Dec")
  melted500 <- as.data.frame(as.numeric(t(Blob2)))
  melted500$month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep",
                       "Oct", "Nov", "Dec")
  melted1000 <- melt(Blob3)
  melted1000$month <- rownames(melted1000)
  melted1500 <- melt(Blob4)
  melted1500$month <- rownames(melted1500)
  melted2000 <- melt(Blob5)
  melted2000$month <- rownames(melted2000)
  melted3000 <- melt(Blob6)
  melted3000$month <- rownames(melted3000)
  melted4000 <- melt(Blob7)
  melted4000$month <- rownames(melted4000)
  melted5000 <- melt(Blob8)
  melted5000$month <- rownames(melted5000)
  melted7500 <- melt(Blob9)
  melted7500$month <- rownames(melted7500)
  melted10000 <- melt(Blob10)
  melted10000$month <- rownames(melted10000)
  
  print("done melting")
  melted300 = rename(melted300,c("as.numeric(t(Blob))"="res_300"))
  melted500 = rename(melted500,c("as.numeric(t(Blob2))"="res_500"))
  melted1000 = rename(melted1000,c("value"="res_1000"))
  melted1500 <- rename(melted1500, c("value"="res_1500"))
  melted2000 <- rename(melted2000, c("value"="res_2000"))
  melted3000 <- rename(melted3000, c("value"="res_3000"))
  melted4000 <- rename(melted4000, c("value"="res_4000"))
  melted5000 <- rename(melted5000, c("value"="res_5000"))
  melted7500 <- rename(melted7500, c("value"="res_7500"))
  melted10000 <- rename(melted10000, c("value"="res_10000"))
  print("merging")
  new <- merge(melted300, melted500, by="month")
  new2 <- merge(new, melted1000)
  new3 <- merge(new2, melted1500)
  new4 <- merge(new3, melted2000)
  new5 <- merge(new4, melted3000)
  new6 <- merge(new5, melted4000)
  new7 <- merge(new6, melted5000)
  new8 <- merge(new7, melted7500)
  new9 <- merge(new8, melted10000)
  return(new9)
}
#Transect Analysis--------------------
#Put coordinates into list, extract from raster (using lapply), rbind, then plot. 
#Read list of transect coordinates
MODIS_LST <- raster("/Users/mallory/Documents/Temp_Project/APPEARS_LST/MOD11A2.006_LST_Day_1km_doy2012201_aid0001.tif")
#MODIS_LST <- raster("/Users/mallory/Documents/APPEARS_LST/MOD11A2.006_LST_Day_1km_doy2016209_aid0001.tif")
MODIS_LST <- (MODIS_LST*0.02 -273.15)
points <- read.csv("/Users/mallory/Documents/Temp_Project/Transect_points2.csv")
subset_transect <- function(x){
  transect <- subset(points, id==x)
  transectx <- transect[c(1,2)]
  xy <- cbind(transectx$X, transectx$Y)
  transectx_plot <- as.data.frame(extract(MODIS_LST,xy))
  names(transectx_plot)[1]<-"LST"
  transectx_plot$distance <- ((transect$distance*111139)/1000)
  return(transectx_plot)
}


x1 <- subset_transect(1)
x1plot <- ggplot(x1, aes(distance, LST)) +
  geom_point() +
  geom_smooth()+
  #xlab("Forest to Cropland in km (~900 m apart)")+
  ylab("MODIS LST")+
  labs(tag = "1")+
  theme(axis.title.x = element_blank())

x2 <- subset_transect(2)
x2$distance <- -x2$distance
x2plot <- ggplot(x2, aes(distance, LST)) +
  geom_point() +
  geom_smooth()+
  #xlab("Forest to Cropland in km (~900 m apart)")+
  ylab("MODIS LST")+
  labs(tag = "2")+
  theme(axis.title.x = element_blank())

x3 <- subset_transect(3)
x3$id <- seq_len(nrow(x3))
x3plot <- ggplot(x3, aes(distance, LST)) +
  geom_point() +
  geom_smooth()+
  #xlab("Forest to Cropland in km (~900 m apart)")+
  ylab("MODIS LST")+
  labs(tag = "3")+
  theme(axis.title.x = element_blank())

x4 <- subset_transect(4)
x4$id <- seq_len(nrow(x4))
x4plot <- ggplot(x4, aes(distance, LST)) +
  geom_point() +
  geom_smooth()+
  #xlab("Forest to Cropland in km (~900 m apart)")+
  ylab("MODIS LST")+
  labs(tag = "4")+
  theme(axis.title.x = element_blank())

x5 <- subset_transect(5)
x5$id <- seq_len(nrow(x5))
x5plot <- ggplot(x5, aes(distance, LST)) +
  geom_point() +
  geom_smooth()+
  #xlab("Forest to Cropland in km (~900 m apart)")+
  ylab("MODIS LST")+
  labs(tag = "5")+
  theme(axis.title.x = element_blank())


x6 <- subset_transect(6)
x6$distance <- -x6$distance
x6plot <- ggplot(x6, aes(distance, LST)) +
  geom_point() +
  geom_smooth()+
  #xlab("Forest to Cropland along transect in km (~900 m apart)")+
  ylab("MODIS LST")+
  labs(tag = "6")+
  theme(axis.title.x = element_blank())

grid.arrange(x1plot, x2plot, x3plot, x4plot, x5plot, x6plot, nrow=3, top=grid::textGrob("8-day LST, July 21 2012",gp=grid::gpar(fontsize=16,font=3)), bottom=grid::textGrob("Forest to cropland along transect in km (~800 m apart)", gp=grid::gpar(fontsize=18)))
grid.arrange(x1plot, x2plot, x3plot, x4plot, x5plot, x6plot, nrow=3, top=grid::textGrob("8-day LST, July 29 2016",gp=grid::gpar(fontsize=16,font=3)), bottom=grid::textGrob("Forest to cropland along transect in km (~800 m apart)", gp=grid::gpar(fontsize=18)))
#Landsat Transects-----------------
Landsat_pts <- read.csv("/Users/mallory/Documents/Temp_Project/Landsat_Points.csv")
str(Landsat_pts)
Landsat_pts$id <- as.factor(Landsat_pts$id)
Landsat_pts$LandCover <- as.factor(Landsat_pts$LandCover)
Landsat_pts$LE07CU02600 <- (Landsat_pts$LE07CU02600*0.1 - 273.15)
Landsat_pts$LT05CU02101 <- (Landsat_pts$LT05CU02101*0.1 - 273.15)
Landsat_pts$LT05CU02301 <- (Landsat_pts$LT05CU02301*0.1 - 273.15)
Landsat_pts$LT05CU02501 <- (Landsat_pts$LT05CU02501*0.1 - 273.15)
LandsatPlot <- ggplot(Landsat_pts, aes(distance, LT05CU02301, group=id, color=id)) +
  geom_smooth()+
  geom_point(color=Landsat_pts$LandCover)+
  #xlab("Forest to Cropland along transect in km (~900 m apart)")+
  ylab("Landsat LST")+
  theme(axis.title.x = element_blank()) 

#Create data frame for plotting comparisons ----------
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

write.csv(df, "for_plotting.csv")
df <- read.csv("/Users/mallory/Documents/Temp_Project/APPEARS_LST/for_plotting.csv")

dft <- df[,c("Month", "meanUrban", "meanCrop", "meanFo")]
dfm <- melt(dft, id="Month")
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
  scale_y_reverse(lim=c(5,-4.4))


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

#Just TA (how does it vary by land cover type)-----
Ta <- brick("/Users/mallory/Documents/Temp_Project/Ta_All.tif")
# create a level plot - plot
cols <- colorRampPalette(rev(brewer.pal(15,"RdBu")))
my.at <- seq(0,35,2)
levelplot(Ta, at=my.at, main="Air Temperature (Ta)",
          col.regions=(cols))
#Recreating figure 4 (by land cover type) but for air temperature 
FomaskTa <- resample(Fomask, Ta, method="bilinear")
CropmaskTa <- resample(Cropmask2, Ta, method="bilinear")
UrbanmaskTa <- resample(Urbanmask, Ta, method="bilinear")

Urban_Ta <- mask(Ta, UrbanmaskTa)
Crop_Ta <- mask(Ta,CropmaskTa)
Fo_Ta <- mask(Ta, FomaskTa)

dfTa <- data.frame(Month=c(1:12))
dfTa$Month <- as.numeric(dfTa$Month)
dfTa$meanUrban <- as.numeric(cellStats(Urban_Ta, stat='mean', na.rm=TRUE))
dfTa$sdUrban <- as.numeric(cellStats(Urban_Ta, stat='sd', na.rm=TRUE))
dfTa$seUrban <- (df$sdUrban)/(sqrt(ncell(Urban_Ta)))
dfTa$meanCrop <- as.numeric(cellStats(Crop_Ta, stat='mean', na.rm=TRUE))
dfTa$sdCrop <- as.numeric(cellStats(Crop_Ta, stat='sd', na.rm=TRUE))
dfTa$seCrop <- (df$sdCrop)/(sqrt(ncell(Crop_Ta)))
dfTa$meanFo <- as.numeric(cellStats(Fo_Ta, stat='mean', na.rm=TRUE))
dfTa$sdFo <- as.numeric(cellStats(Fo_Ta, stat='sd', na.rm=TRUE))
dfTa$seFo <- (df$sdFo)/(sqrt(ncell(Fo_Ta)))

kk <- ggplot(dfTa, aes(x=Month, group=1)) + 
  geom_line(aes(y=meanUrban), color="red") + 
  geom_errorbar(aes(x=Month, ymin=meanUrban-seUrban, ymax=meanUrban+seUrban), width=0.2, size=0.5,color="red")+
  geom_line(aes(y=meanCrop), color="yellowgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanCrop-seCrop, ymax=meanCrop+seCrop), width=0.2, size=0.5, color="yellowgreen")+
  geom_line(aes(y=meanFo), color="darkgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanFo-seFo, ymax=meanFo+seFo),width=0.2, size=0.5, color="darkgreen")+
  labs(title="Ta by Land Cover Type", 
       y="Ta (degrees C)", 
       x="Month")+
  scale_x_continuous(breaks=seq(1,12,3))+
  theme_bw()+
  ylim(0,35)

grid.arrange(kk, hh, ncol=2)


#By Lat instead of by land cover type
DiffsBrick <- brick(Diffs)
extent(DiffsBrick)
#High_Lat_Ext <- extent(-88.775, -74.85, 37.75, 41.41667)
#MidHigh_Lat_Ext <- extent(-88.775, -74.85, 34.25, 37.75)
#MidLow_Lat_Ext <- extent(-88.775, -74.85, 32.75, 34.25)
#Low_Lat_Ext <- extent(-88.775, -74.85, 29.25, 32.75)

High_Lat_Ext <- extent(-88.775, -74.85, 37.75, 41.41667)
MidHigh_Lat_Ext <- extent(-88.775, -74.85, 33.25, 37.75)
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

dfl <- fg[,c("Month", "meanHigh", "meanMidHigh", "meanMidLow")]
dfk <- melt(dfl, id="Month")
ggplot(data = dfk, aes(x = Month, y = value, color = variable)) + 
  geom_line(size=5) +
  scale_color_manual(labels = c("High", "Medium", "Low"), values = c("purple", "blue", "red"))+
  labs(color = "Latitude\n") 

jj
hh
grid.arrange(jj, hh, ncol=2)

#Need to confirm that differences within land cover types still exist within a given latitude. 
plot(MidHigh_Lat)
plot(MidLow_Lat)
plot(High_Lat)

#Just checking
FomaskMid <- resample(Fomask, High_Lat, method="bilinear")
CropmaskMid <- resample(Cropmask2, High_Lat, method="bilinear")
UrbanmaskMid <- resample(Urbanmask, High_Lat, method="bilinear")
plot(FomaskMid)
plot(CropmaskMid)
plot(UrbanmaskMid)
Urban_Mid <- mask(High_Lat, UrbanmaskMid)
Crop_Mid <- mask(High_Lat,CropmaskMid)
Fo_Mid <- mask(High_Lat, FomaskMid)

dfMid <- data.frame(Month=c(1:12))
dfMid$Month <- as.numeric(dfMid$Month)
dfMid$meanUrban <- as.numeric(cellStats(Urban_Mid, stat='mean', na.rm=TRUE))
dfMid$sdUrban <- as.numeric(cellStats(Urban_Mid, stat='sd', na.rm=TRUE))
dfMid$seUrban <- (df$sdUrban)/(sqrt(ncell(Urban_Mid)))
dfMid$meanCrop <- as.numeric(cellStats(Crop_Mid, stat='mean', na.rm=TRUE))
dfMid$sdCrop <- as.numeric(cellStats(Crop_Mid, stat='sd', na.rm=TRUE))
dfMid$seCrop <- (df$sdCrop)/(sqrt(ncell(Crop_Mid)))
dfMid$meanFo <- as.numeric(cellStats(Fo_Mid, stat='mean', na.rm=TRUE))
dfMid$sdFo <- as.numeric(cellStats(Fo_Mid, stat='sd', na.rm=TRUE))
dfMid$seFo <- (df$sdFo)/(sqrt(ncell(Fo_Mid)))

xx <- ggplot(dfMid, aes(x=Month, group=1)) + 
  geom_line(aes(y=meanUrban), color="red") + 
  geom_errorbar(aes(x=Month, ymin=meanUrban-seUrban, ymax=meanUrban+seUrban), width=0.2, size=0.5,color="red")+
  geom_line(aes(y=meanCrop), color="yellowgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanCrop-seCrop, ymax=meanCrop+seCrop), width=0.2, size=0.5, color="yellowgreen")+
  geom_line(aes(y=meanFo), color="darkgreen") + 
  geom_errorbar(aes(x=Month, ymin=meanFo-seFo, ymax=meanFo+seFo),width=0.2, size=0.5, color="darkgreen")+
  labs(title="Ta by Land Cover Type", 
       y="Ta (degrees C)", 
       x="Month")+
  scale_x_continuous(breaks=seq(1,12,3))+
  theme_bw()+
  ylim(-6,6)


#----------now looking at forest data
#Level plot for 1c------
F1 <- raster("/Users/mallory/Documents/Temp_Project/NA_TREEAGE_1096/data/conus_age06_1km.tif")
Forest_Proj <- projectRaster(F1, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
#Make it so you can see the lower intervals better
Forest_age_2019 <- Forest_Proj + 13
writeRaster(Forest_age_2019, "/Users/mallory/Documents/Temp_Project/Forest_Age_Conus.tif")
my.at=c(0,15,30,45,60,80,100,125,150,200,250,300,400,500)
my.brks=seq(0, 500, by=25)
myColorkey <- list(at=my.brks, labels=list(at=my.brks, labels=my.at), space="bottom")
levelplot(Forest_age_2019, at=my.at, margin=F, col.regions=topo.colors(100), pretty=T, interpolate=T)+latticeExtra::layer(sp.polygons(e))+latticeExtra::layer(sp.polygons(bPols))


ext <- extent(Diffs)
Forest_Proj_crop <- crop(Forest_Proj, ext)
Forest_Proj
plot(Forest_Proj_crop)
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

Forest_Plot <- stack(Forest_Age, Diffs)
plot(Forest_Plot)

r1Extent <- extent(Forest_Plot)
r1Extraction<-extract(Forest_Plot, layer=1, n1=1, r1Extent, df=TRUE, cellnumbers=TRUE)


cor(r1Extraction$layer, r1Extraction$Jun, use="complete.cases")
#Plot
qplot(r1Extraction$layer, r1Extraction$Jun)

#Trying to plot but there are so many points
sp <- ggplot(r1Extraction, aes(x=layer, y=Jun)) +
  geom_point()
sp + stat_density_2d(aes(fill = ..level..), geom="polygon")+
  scale_fill_gradient(low="blue", high="red")
