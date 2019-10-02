#Figures for Lily's Undergraduate Thesis Defense: 
#Agreed-upon color scheme for the different land cover types: Color blind friendly!
#Urban "c51b7d"
#Forest "#276419"
#Grass "#7fbc41"
#Update 7/4/2019: Re-doing change point analyses!
library(ggplot2)
library(plyr)
library(dplyr)
library(reshape2)
library(stringr)
library(tidyr)
rasterOptions(tmpdir="C:\\",tmptime = 24,progress="text",timer=TRUE,overwrite = T,chunksize=2e+08,maxmemory=1e+8)

setwd("/Volumes/G-RAID Thunderbolt 3/Temp_Project")
#Load file for plotting
df <- read.csv("/Processed/for_plotting.csv")
str(df)
#Melt mean values
dft <- df[,c("Month", "meanUrban", "meanCrop2", "meanFo")]
dfm <- melt(dft, id="Month")
#Melt errors
dferror <- df[,c("Month", "seUrban", "seCrop2", "seFo")]
dfe <- melt(dferror, id="Month", value.name="error")
dfm$error <- dfe$error
#Order factors properly
fm$variable <- factor(dfm$variable,levels = c("meanFo", "meanCrop2", "meanUrban"))
#First Figure - Fig 4 from paper
ggplot(data = dfm, aes(x = Month, y = value, color = variable)) + 
  #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = 0),
  #fill = "red", alpha = 0.005)+
  #geom_rect(aes(xmin = -Inf, xmax = Inf, ymin = 0, ymax = Inf),
  # fill = "blue", alpha = 0.005)+
  #annotate("text", x = 7, y = 4, label = "Surface Cooler", color="Blue")+
  #annotate("text", x = 7, y = -4, label = "Surface Warmer", color="Red")+
  geom_line() +
  scale_color_manual(labels = c("Forest", "Cropland", "Urban"), values = c("#276419", "#7fbc41", "#c51b7d"))+
  labs(color = "Land Cover\n") +
  geom_errorbar(aes(x=Month, ymin=value-error, ymax=value + error), width=0.2, size=0.5)+
  labs(title="Ta-Ts by Land Cover Type", 
       y="Ta-Ts (degrees C)", 
       x="Month")+
  ylim(-4.5, 4.5)+
  scale_x_continuous(breaks=seq(1,12,3))+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  theme_grey(base_size = 16)+ 
  theme(plot.title = element_text(size=18))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"))

ggsave("Remote_Sensing_MLB.pdf")

#Chen06 dataset: Land cover change from 1920 to 1992 - from Christy et al. table 2
#Create dataframe
df2 <- data.frame(LandCover = c("Forest", "Cropland", "Urban"), change = c(.14, -.157, .002))
df2$LandCover <- factor(df2$LandCover,levels = c("Forest", "Cropland", "Urban"))
df2$change <- df2$change*100
#Barplot of land cover change 
ggplot(df2, aes(fill=LandCover, y=change, x=LandCover)) + 
  geom_bar(colour="black", stat="identity")+
  scale_fill_manual(values=c("#276419", "#7fbc41", "#c51b7d"))+
  labs(title="Change by Land Cover Type (1920 to 1992)", 
       y="Percent Change (%)", 
       x="Land Cover Type")+
  theme_grey(base_size = 16)+ 
  theme(plot.title = element_text(size=18))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  ylim(-25,25)+
  geom_hline(yintercept=0, linetype="dashed", 
             color = "black", size=0.5)+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), legend.title = element_blank())

ggsave("Land_Cover_Change.pdf")


#--------- Lily Data
allmeans <- read.csv("/Lily_Data/allmeans200mupdateddata.csv")
fomeans <- read.csv("/Lily_Data/allmeans200mupdateddata.csv")
agmeans <- read.csv("/Lily_Data/allmeans200mupdateddata.csv")

allmeans$category <- "all"
agmeans$category <- "ag"
fomeans$category <- "for"

alltoplot <- rbind(rbind(allmeans, agmeans), fomeans)
ggplot(alltoplot, aes(x=year, y=y, color=category))+
  geom_line()

alltoplot

#---------------Remaking Lily's figs
#Lookup table first (T_site_lookup will work for Tmax,Tmax, and Tmin)
Tavg_site <- read.csv("Raw/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", nrows=3, header=T)
colnames(Tavg_site)[1] <- "Type"
Tavg_site <- as.data.frame(t(Tavg_site))
#If rowmax is <0.5, then LC will be "NA"
Tavg_site$V1 <- as.numeric(levels(Tavg_site$V1)[Tavg_site$V1])
Tavg_site$V2 <- as.numeric(levels(Tavg_site$V2)[Tavg_site$V2])
Tavg_site$V3 <- as.numeric(levels(Tavg_site$V3)[Tavg_site$V3])
Tavg_site <- na.omit(Tavg_site)
Tavg_site$LC <- ifelse(Tavg_site$V1>=0.52, "Dev",ifelse(Tavg_site$V2 >=0.52,"Ag", ifelse(Tavg_site$V3 >=0.52, "For", NA)))
Tavg_site <- na.omit(Tavg_site)
Tavg_site
#First 3 rows decide which land cover type it is
#Then, it goes from 1900 to 2019 or whatever
head(Tavg_site)
#Create lookup table 
T_site_lookup <- cbind(names = rownames(Tavg_site), Tavg_site)
rownames(T_site_lookup) <- c()
T_site_lookup <- as.data.frame(T_site_lookup[c(1,5)])
write.csv(T_site_lookup, "/Lily_Data/Lookup.csv")
#Tavg---
headers <- read.csv("Raw/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", header = F, nrows=1, as.is=T)
Tavg <- read.csv("Raw/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
names(Tavg) <- headers
Year <- Tavg$Year

#Get z-scores (anomalies)
Tavg <- cbind(Year, as.data.frame(scale(Tavg[2:186], center = TRUE, scale = TRUE)))

cbind(Year, as.data.frame(scale(Tavg[2:40], center=TRUE, scale=TRUE)))
#scale(Tavg$Aberdeen, center=TRUE, scale=TRUE)
str(Tavg)
#Get list of forest sites 
Fo_list <- as.vector(T_site_lookup$names[T_site_lookup$LC=="For"])
Ag_list <- as.vector(T_site_lookup$names[T_site_lookup$LC=="Ag"])
Dev_list <- as.vector(T_site_lookup$names[T_site_lookup$LC=="Dev"])
Fo_subset <- Tavg[, Fo_list]
str(Fo_subset)
Ag_subset <- Tavg[, Ag_list]
str(Ag_subset)
Tavg$Allmean <- rowMeans(Tavg[2:186], na.rm=TRUE)
Tavg$Forestmean <- rowMeans(Fo_subset[,1:28], na.rm=TRUE)
Tavg$Agmean <- rowMeans(Ag_subset[,1:45], na.rm=TRUE)
head(Tavg)
str(Tavg)
#Write out files for change point analysis 
All <- Tavg[c(1,187)]
names(All) <- c("year", "y")
For_all <- Tavg[c(1,188)]
names(For_all) <- c("year", "y")
Ag_all<- Tavg[c(1,189)]

names(Ag_all) <- c("year", "y")
write.csv(All, "Raw/Weather Station/CPModel/data/Tavg_Allsite.csv")
write.csv(For_all, "Raw/Weather Station/CPModel/data/Tavg_Forsite.csv")
write.csv(Ag_all, "Raw/Weather Station/CPModel/data/Tavg_Agsite.csv")

For_all$x <- Ag_all$Agmean
names(For_all) <- c("year", "forest", "ag")
str(For_all)
toplot5 <- melt(For_all, id.vars = "year", measure.vars = c("forest", "ag"))
ggplot(toplot5, aes(x=year, y=value, colour=variable))+
  geom_line()
#Tmax---
Tmax<- read.csv("Raw/Weather Station/TMAX_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
names(Tmax) <- headers
Year <- Tmax$Year

#Get z-scores (anomalies)
Tmax <- cbind(Year, as.data.frame(scale(Tmax[2:186], center = TRUE, scale = TRUE)))


str(Tmax)
#Get list of forest sites 
Fo_subset <- Tmax[, Fo_list]
str(Fo_subset)
Ag_subset <- Tmax[, Ag_list]
str(Ag_subset)
Tmax$Allmean <- rowMeans(Tmax[2:186], na.rm=TRUE)
Tmax$Forestmean <- rowMeans(Fo_subset[,1:28], na.rm=TRUE)
Tmax$Agmean <- rowMeans(Ag_subset[,1:45], na.rm=TRUE)
head(Tmax)

#Write out files for change point analysis 
Allmax <- Tmax[c(1,187)]
names(Allmax) <- c("year", "y")
For_allmax <- Tmax[c(1,188)]
names(For_allmax) <- c("year", "y")
Ag_allmax<- Tmax[c(1,189)]
names(Ag_allmax) <- c("year", "y")
write.csv(Allmax, "Raw/Weather Station/CPModel/data/Tmax_Allsite.csv")
write.csv(For_allmax, "Raw/Weather Station/CPModel/data/Tmax_Forsite.csv")
write.csv(Ag_allmax, "Raw/Weather Station/CPModel/data/Tmax_Agsite.csv")

#Tmin--- 
Tmin <- read.csv("Raw/Weather Station/TMIN_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
names(Tmin) <- headers
Year <- Tmin$Year

#Get z-scores (anomalies)
Tmin <- cbind(Year, as.data.frame(scale(Tmin[2:186], center = TRUE, scale = TRUE)))
str(Tmin)
#Get list of forest sites 
Fo_subset <- Tmin[, Fo_list]
str(Fo_subset)
Ag_subset <- Tmin[, Ag_list]
str(Ag_subset)
Tmin$Allmean <- rowMeans(Tmin[2:186], na.rm=TRUE)
Tmin$Forestmean <- rowMeans(Fo_subset[,1:28], na.rm=TRUE)
Tmin$Agmean <- rowMeans(Ag_subset[,1:45], na.rm=TRUE)
head(Tmin)
#Write out files for change point analysis 
Allmin <- Tmin[c(1,187)]
names(Allmin) <- c("year", "y")
For_allmin <- Tmin[c(1,188)]
names(For_allmin) <- c("year", "y")
Ag_allmin<- Tmin[c(1,189)]
names(Ag_allmin) <- c("year", "y")
write.csv(Allmin, "Raw/Weather Station/CPModel/data/Tmin_Allsite.csv")
write.csv(For_allmin, "Raw/Weather Station/CPModel/data/Tmin_Forsite.csv")
write.csv(Ag_allmin, "Raw/Weather Station/CPModel/data/Tmin_Agsite.csv")

#To-do's
#1: Remake star figure
#2: Remake CP lines figure (prob easier)
library(reshape2)
library(ggplot2)
#For Average Lines
All_line <- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Avg_all_output_line.csv")
names(All_line) <-c("x", "All", "Year")
Fo_line <- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Avg_for_output_line.csv")
names(Fo_line) <-c("x", "Fo", "Year")
Ag_line<- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Avg_ag_output_line.csv")
names(Ag_line) <-c("x", "Ag", "Year")

All_line$x <- NULL
All_line$Fo <- Fo_line$Fo
All_line$Ag <- Ag_line$Ag

toplot <- melt(data = All_line, id.vars = "Year", measure.vars = c("All", "Fo", "Ag"))
toplot2 <- melt(data = All_line, id.vars = "Year", measure.vars = c("Fo", "Ag"))

ggplot(data=toplot2, aes(y=value, colour=variable, x=Year))+
  geom_line()+
  scale_color_manual(labels = c("Forest", "Cropland", "All"), values = c("darkgreen", "red"))+
  labs(color = "Land Cover\n") +
  labs(title="Changepoint Line by Land Cover Type", 
       y="Temperature Anomaly", 
       x="Year")+
  ylim(-2, 2)+
  theme_grey(base_size = 16)+ 
  theme(plot.title = element_text(size=18))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"))

#For maxline
All_line <- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Max_all_output_line.csv")
names(All_line) <-c("x", "All", "Year")
Fo_line <- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Max_for_output_line.csv")
names(Fo_line) <-c("x", "Fo", "Year")
Ag_line<- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Max_ag_output_line.csv")
names(Ag_line) <-c("x", "Ag", "Year")

All_line$x <- NULL
All_line$Fo <- Fo_line$Fo
All_line$Ag <- Ag_line$Ag

toplot <- melt(data = All_line, id.vars = "Year", measure.vars = c("All", "Fo", "Ag"))

ggplot(data=toplot2, aes(y=value, colour=variable, x=Year))+
  geom_line()+
  scale_color_manual(labels = c("Forest", "Cropland", "All"), values = c("Darkgreen", "Red"))+
  labs(color = "Land Cover\n") +
  labs(title="Changepoint Line by Land Cover Type", 
       y="Temperature Anomaly", 
       x="Year")+
  ylim(-2, 2)+
  theme_grey(base_size = 16)+ 
  theme(plot.title = element_text(size=18))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"))

#For minline
All_line <- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Min_all_output_line.csv")
names(All_line) <-c("x", "All", "Year")
Fo_line <- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Min_for_output_line.csv")
names(Fo_line) <-c("x", "Fo", "Year")
Ag_line<- read.csv("Raw/Weather Station/CPModel/Novick_Analyses/Min_ag_output_line.csv")
names(Ag_line) <-c("x", "Ag", "Year")

All_line$x <- NULL
All_line$Fo <- Fo_line$Fo
All_line$Ag <- Ag_line$Ag

toplot <- melt(data = All_line, id.vars = "Year", measure.vars = c("All", "Fo", "Ag"))

ggplot(data=toplot, aes(y=value, colour=variable, x=Year))+
  geom_line()+
  scale_color_manual(labels = c("Forest", "Cropland", "All"), values = c("#276419", "#7fbc41", "black"))+
  labs(color = "Land Cover\n") +
  labs(title="Changepoint Line by Land Cover Type", 
       y="Temperature Anomaly", 
       x="Year")+
  ylim(-2, 2)+
  theme_grey(base_size = 16)+ 
  theme(plot.title = element_text(size=18))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"))

#Change in temp
#Forest vs. Ag 
All_avg <- read.csv("Raw/Weather Station/CPModel/data/Tavg_Allsite.csv")
names(All_avg) <-c("x", "Year", "All")
Fo_avg <- read.csv("Raw/Weather Station/CPModel/data/Tavg_Forsite.csv")
names(Fo_avg) <-c("x", "Year", "Fo")
Ag_avg <- read.csv("Raw/Weather Station/CPModel/data/Tavg_Agsite.csv")
names(Ag_avg) <-c("x", "Year", "Ag")

All_avg$x <- NULL
All_avg$Fo <- Fo_avg$Fo
All_avg$Ag <- Ag_avg$Ag
All_avg

#Tavg from above 
str(Fo_subset)
str(Ag_subset)
#Make dataset out of first row of both subsets
#Then run t-test to see if they're significantly different
Fo_subset[1,]
Ag_subset[1,]
t <- t.test(Fo_subset[1,], Ag_subset[1,])

t_function <- function(x){
  t <- t.test(Fo_subset[x,],Ag_subset[x,], paired=FALSE)
  p <- t$p.value
  dir <-t$estimate
  return(p)
}

list <- c(1:114)
l <- as.numeric(rbind(lapply(list, t_function)))

All_avg <- All_avg[1:114,]
All_avg$sig <- l
str(All_avg)
subset(All_avg, sig < 0.1)
label.df <- data.frame(Year = as.integer(c("1902", "1929", "1961", "1975",  "2006", "2011")),
                       value = c(2, 2, -2, -2, 2, 2))
str(All_avg)
str(label.df)
toplot2 <- melt(data = All_avg, id.vars = c("Year", "sig"), measure.vars = c("All", "Fo", "Ag"))
str(toplot2)
toplot2 <- subset(toplot2, variable=="Fo" | variable=="Ag")
str(toplot2)
p2 <- ggplot(data=toplot2)+
  geom_line(aes(y=value, colour=variable, x=Year))+
  scale_color_manual(labels = c("Forest", "Cropland", "All"), values = c("#276419", "#7fbc41", "black"))+
  labs(color = "Land Cover\n") +
  labs(title="Changepoint Line by Land Cover Type", 
       y="Temperature Anomaly", 
       x="Year")+
  ylim(-2, 2)+
  theme_grey(base_size = 16)+ 
  theme(plot.title = element_text(size=18))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"))


str(label.df)
p2 + geom_text(data=label.df, aes(x=Year, y=value, variable=NULL), label = "*")

#For max now -------
All_max <- read.csv("Raw/Weather Station/CPModel/data/Tmax_Allsite.csv")
names(All_max) <-c("x", "Year", "All")
Fo_max <- read.csv("Raw/Weather Station/CPModel/data/Tmax_Forsite.csv")
names(Fo_max) <-c("x", "Year", "Fo")
Ag_max <- read.csv("Raw/Weather Station/CPModel/data/Tmax_Agsite.csv")
names(Ag_max) <-c("x", "Year", "Ag")

All_max$x <- NULL
All_max$Fo <- Fo_max$Fo
All_max$Ag <- Ag_max$Ag
All_max

#Tmax from above 
str(Fo_subset)
str(Ag_subset)
#Make dataset out of first row of both subsets
#Then run t-test to see if they're significantly different
Fo_subset[1,]
Ag_subset[1,]
t <- t.test(Fo_subset[1,], Ag_subset[1,])

t_function <- function(x){
  t <- t.test(Fo_subset[x,],Ag_subset[x,], paired=FALSE)
  p <- t$p.value
  dir <-t$estimate
  return(p)
}

list <- c(1:114)
l <- as.numeric(rbind(lapply(list, t_function)))

All_max <- All_max[1:114,]
All_max$sig <- l
str(All_max)
subset(All_max, sig < 0.1)
label.df <- data.frame(Year = as.integer(c("1914", "1916", "1918", "1938",  "1946", "1961", "2008")),
                       value = c(-2, -2, -2, -2, -2, 2, 2))
str(All_max)
str(label.df)
toplot3 <- melt(data = All_max, id.vars = c("Year", "sig"), measure.vars = c("All", "Fo", "Ag"))
str(toplot3)
toplot3 <- subset(toplot2, variable=="Fo" | variable=="Ag")
str(toplot3)
p2 <- ggplot(data=toplot3)+
  geom_line(aes(y=value, colour=variable, x=Year))+
  scale_color_manual(labels = c("Forest", "Cropland", "All"), values = c("#276419", "#7fbc41", "black"))+
  labs(color = "Land Cover\n") +
  labs(title="Changepoint Line by Land Cover Type", 
       y="Temperature Anomaly", 
       x="Year")+
  ylim(-2, 2)+
  theme_grey(base_size = 16)+ 
  theme(plot.title = element_text(size=18))+
  theme(panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(size = 0.5, linetype = "solid",
                                 colour = "black"))+
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"))


str(label.df)
p2 + geom_text(data=label.df, aes(x=Year, y=value, variable=NULL), label = "*")



#---------------------
#Breaking up by latitude. Max by latitude 
#Latlongs
latlongs <- read.csv("Lily_Data/USHCNstationinformation_percentforest.csv")
str(latlongs)
latlongs$STA_NAME <- tolower(latlongs$STA_NAME)
latlongs$STA_NAME <- str_replace_all(latlongs$STA_NAME, " ", "")
latlongs$names <- substr(latlongs$STA_NAME, 1,10)

#Site lookup
T_site_lookup <- read.csv("Lily_Data/Lookup.csv")
str(T_site_lookup)
T_site_lookup$names <- tolower(T_site_lookup$names)
T_site_lookup$names
T_site_lookup$names <- substr(T_site_lookup$names, 1,10)

T_site_lookup$names
latlongs$names
#Check for diffs
check = setdiff(T_site_lookup$names, latlongs$names)
#beracolle spelled differently in latlongs vs. lookup
#There are two greenvilles, spelled the same. (?)
T_site_lookup[6,2] <- "beracolle"
T_site_lookup[119,2] <- "tuscaloosa"
T_site_lookup[43,2] <- "glenville"
T_site_lookup[115,2] <- "thomasvill"
T_site_lookup[60,2] <- "hillsboro"
T_site_lookup[51,2] <- "greenvilleoh"
latlongs[66,7] <- "greenvilleoh"
latlongs[13,7] <- "bowlinggre"
latlongs[117,7] <- "newman"
#Looks like I lost a few but don't have time to figure it out at the moment. 
merged <- merge(T_site_lookup, latlongs, by="names")
str(merged)
write.csv(merged, "Processed/MergedWeather.csv")
#Get rid of FL sites!
#merged <- subset(merged, !STATE=="FL")
mergedfo <- subset(merged, LC=="For")
mergedag <- subset(merged, LC=="Ag")
mergedhighlat <- subset(merged, LAT <43 & LAT > 37)
mergedmidlat <- subset(merged, LAT <37 & LAT >33 )
mergedlowlat <- subset(merged, LAT < 33)
#Headers
headers <- read.csv("Raw/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", header = F, nrows=1, as.is=T)
#Tmax data
Tmax <- read.csv("Raw/Weather Station/TMAX_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
names(Tmax) <- headers
Year <- Tmax$Year
#Going to just look at trends since 1960 now...
#Get z-scores (anomalies)
Tmax <- cbind(Year, as.data.frame(scale(Tmax[2:186], center = TRUE, scale = TRUE)))
str(Tmax)
Tmax_t <- as.data.frame(t(Tmax))
Tmax_t$names <- rownames(Tmax_t)
str(Tmax_t)
Tmax_t$names <- tolower(Tmax_t$names)
check = setdiff(merged$names, Tmax_t$names)
check
Tmax_t$names
Tmax_t[10,118] <- "beracolle"
Tmax_t[12,118] <- "bloomingto"
Tmax_t[14,118] <- "bowlinggre"
Tmax_t[18,118] <- "burkesgard"
Tmax_t[21,118] <- "cambridgem"
Tmax_t[25,118] <-"charlottes"
Tmax_t[36,118] <- "crystalspg"
Tmax_t[50,118] <- "fayettevil"
Tmax_t[51,118] <- "federalpoi"
Tmax_t[53,118] <-"fredericks"
Tmax_t[54,118] <- "ftlauderda"
Tmax_t[62,118] <- "greencastl"
Tmax_t[69,118] <- "hattiesbur"
Tmax_t[72,118] <- "hendersonn"
Tmax_t[73,118] <- "hendersonv"
Tmax_t[77,118] <- "hillsboro"
Tmax_t[80,118] <- "hopkinsvil"
Tmax_t[94,118] <- "lewisburgw"
Tmax_t[107,118] <- "mcminnvill"
Tmax_t[108,118] <- "milledgevi"
Tmax_t[133,118] <- "poplarvill"
Tmax_t[135,118] <- "princessan"
Tmax_t[139,118] <- "rogersvill"
Tmax_t[148,118] <- "shelbyvill"
Tmax_t[160,118] <- "thomasvill"
Tmax_t[166,118] <- "tuscaloosa"
Tmax_t[174,118] <- "watervalle"
Tmax_t[182,118] <- "williamsto"
#Select from lookup table by site name and transpose back 
forests_all <- as.data.frame(t(subset(Tmax_t, names %in% mergedfo$names)))
forests_all$year <- c(1901:2018)
forests_all$type <- "forest"
ag_all <- as.data.frame(t(subset(Tmax_t, names %in% mergedag$names)))
ag_all$year <- c(1901:2018)
ag_all$type <- "ag"

historic_toplotforest <- melt(forests_all, id.vars=c("year", "type"))
historic_toplotag <- melt(ag_all, id.vars=c("year", "type"))

#historic_toplot <- melt(all_historical, id.vars=c("year", "type"))
historic_toplot <- rbind(historic_toplotforest, historic_toplotag)
historic_toplot$year <- as.numeric(historic_toplot$year)
historic_toplot$value <- as.numeric(historic_toplot$value)
historic_toplot$type <- as.factor(historic_toplot$type)
historic_toplot
str(historic_toplot)

historic_toplot_post1960 <- subset(historic_toplot, year > 1945)

#OK not great, but what about when we subset by latitude (above) 
ggplot(historic_toplot, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover type", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  theme_classic()+
  ylim(-2,2)

ggplot(historic_toplot_post1960, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover type", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  theme_classic()+
  ylim(-2,2)


#when you want to switch back to anomalies, switch post1960_t to Tmax_t and the years to 1901:2018
highlatfo <- as.data.frame(t(subset(Tmax_t, names %in% mergedhighlat$names & names %in% mergedfo$names)))
highlatag <- as.data.frame(t(subset(Tmax_t, names %in% mergedhighlat$names & names %in% mergedag$names)))

midlatfo <- as.data.frame(t(subset(Tmax_t, names %in% mergedmidlat$names & names %in% mergedfo$names)))
midlatag <- as.data.frame(t(subset(Tmax_t, names %in% mergedmidlat$names & names %in% mergedag$names)))

lowlatfo <- as.data.frame(t(subset(Tmax_t, names %in% mergedlowlat$names & names %in% mergedfo$names)))
lowlatag <- as.data.frame(t(subset(Tmax_t, names %in% mergedlowlat$names & names %in% mergedag$names)))

highlatfo$year <- c(1901:2018)
highlatfo$type <- "forest"

highlatag$year <- c(1901:2018)

highlatag$type <- "ag"

midlatfo$year <- c(1901:2018)
midlatfo$type <- "forest"

midlatag$year <- c(1901:2018)
midlatag$type <- "ag"

lowlatfo$year <- c(1901:2018)
lowlatfo$type <- "forest"

lowlatag$year <- c(1901:2018)
lowlatag$type <- "ag"


highlat_toplot <- rbind(melt(highlatfo, id.vars=c("year", "type")), melt(highlatag, id.vars=c("year", "type")))
midlat_toplot <- rbind(melt(midlatfo, id.vars=c("year", "type")), melt(midlatag, id.vars=c("year", "type")))
lowlat_toplot <- rbind(melt(lowlatfo, id.vars=c("year", "type")), melt(lowlatag, id.vars=c("year", "type")))

highlat_toplot$year <- as.numeric(highlat_toplot$year)
highlat_toplot$value <- as.numeric(highlat_toplot$value)
highlat_toplot$type <- as.factor(highlat_toplot$type)

midlat_toplot$year <- as.numeric(midlat_toplot$year)
midlat_toplot$value <- as.numeric(midlat_toplot$value)
midlat_toplot$type <- as.factor(midlat_toplot$type)

lowlat_toplot$year <- as.numeric(lowlat_toplot$year)
lowlat_toplot$value <- as.numeric(lowlat_toplot$value)
lowlat_toplot$type <- as.factor(lowlat_toplot$type)

highlat_toplot_post1960 <- subset(highlat_toplot, year > 1945)
midlat_toplot_post1960 <- subset(midlat_toplot, year > 1945)
lowlat_toplot_post1960 <- subset(lowlat_toplot, year > 1945)

ggplot(highlat_toplot, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover type", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  ylim(-2,2)+
  theme_classic()

ggplot(midlat_toplot, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover type", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  ylim(-2,2)+
  theme_classic()

ggplot(lowlat_toplot, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover type", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  ylim(-2,2)+
  theme_classic()


ggplot(highlat_toplot_post1960, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover type", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  ylim(-2,2)+
  theme_classic()

ggplot(midlat_toplot_post1960, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover type", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  ylim(-2,2)+
  theme_classic()


ggplot(lowlat_toplot_post1960, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover type", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  ylim(-2,2)+
  theme_classic()

#Going to try instead to code sites by maps from 1992 vs. 1938. There will be 2 categories, 'reforested', 'deforested', and 'no change'. Then, I will run the change 
#Point analyses and Lily's key figure for these categories (instead of Urban, Crop, and Forest), and see what the results look like. 

#1 Recode land cover maps
#Categories (checked all): 
#0-no values,1-water/coastline, 2- urban (checked), 3-5 no pixels, 6 - mining, 7- barren, 8- Deciduous, 9 - evergreen, 10- Mixed, 11 - grassland, 12 - shrubland, 13-ag, 14-hay, 15 - herb wetland, 16 - woody wetland, 17 - ice/snow

#Coding raster: 
#16 is woody wetland and #15 is herbaceous wetland. Not sure if I should include.
# Forest: values = 8, 9, 10 
# Herbaceous: values = 11, 12, 13, 14

#Forests: code 1
#Herbaceous: code 0

Historical_LC <- raster("Raw/Other/FORE-SCE/Conus_Backcasting_y1938.tif")
extent(Historical_LC)
e2 <- extent(300000, 2200000, 177285, 2500000)
Historical_LC <- crop(Historical_LC, e2)
Historical_LC[Historical_LC <8 | Historical_LC > 14] <-NA
Historical_LC[Historical_LC>7 & Historical_LC<11] <-1
Historical_LC[Historical_LC>10 & Historical_LC < 15] <-0
hist(Historical_LC)

Later_LC <- raster("Raw/Other/FORE-SCE/Conus_Backcasting_y1992.tif")
Later_LC <- crop(Later_LC, e2)
Later_LC[Later_LC <8 | Later_LC>14] <-NA
Later_LC[Later_LC>7 & Later_LC<11] <-1
Later_LC[Later_LC>10 & Later_LC<15] <-0
hist(Later_LC)

#Couldn't get this to work, thought it might be faster. 
#Later_LC_class<-reclassify(Later_LC, c(-Inf, 7, NA,  8, 10, 1,  11, 14, 0, 15,Inf,NA))

Dif_LC <- Later_LC - Historical_LC
plot(Dif_LC)
#writeRaster(Dif_LC, "Processed/Change_LC_FORESCE.tif")
Dif_LC <- raster("Processed/Change_LC_FORESCE.tif")
hist(Dif_LC)
levels(Dif_LC)=data.frame(ID=-1:1, code=c('Deforest', 'Nochange', 'Reforest'))
levelplot(Dif_LC, col.regions=c('red', 'white', 'green'))  

#Need to reproject next

#Changepoint input
#Need to use nearest neighbor for reprojection since categorical variables - see here: https://stackoverflow.com/questions/15634882/why-the-values-of-my-raster-map-change-when-i-project-it-to-a-new-crs-projectra
#Diffs_reproj <- projectRaster(Dif_LC, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", method = "ngb" )
#writeRaster(Diffs_reproj, "Processed/Change_LC_proj.tif")

hist(Diffs_reproj)
levels(Diffs_reproj)=data.frame(ID=-1:1, code=c('Deforest', 'Nochange', 'Reforest'))
levelplot(Diffs_reproj, col.regions=c('red', 'white', 'green'))

#Starting here...-------------------
#Instead of forest, crop and urban, each site will be one of three categories. 'Reforest', 'Deforest', and 'Nochange'
Diffs_reproj <- raster("Processed/Change_LC_proj.tif")
merged <- read.csv("Processed/MergedWeather.csv")

xy <- cbind(merged$LONG, merged$LAT)
#Absmax, from here: https://stackoverflow.com/questions/24652771/finding-the-maximum-absolute-value-whilst-preserving-or-symbol
absmax <- function(x) { x[which.max( abs(x) )][1]}
LC_Change <- raster::extract(Diffs_reproj, xy, fun=absmax, buffer=600, df=T)
colnames(merged)[colnames(merged)=="X"] <- "ID"
all_reforest <- merge(merged, LC_Change, by="ID")
reforesting <- subset(all_reforest, Change_LC_proj==1)
nochange <- subset(all_reforest, Change_LC_proj==0)
deforesting <- subset(all_reforest, Change_LC_proj==-1)

#Plot TMax by group
reforesting_sites <- as.data.frame(t(subset(Tmax_t, names %in% reforesting$names)))
reforesting_sites$year <- c(1901:2018)
reforesting_sites$type <- "reforest"

nochange_sites <- as.data.frame(t(subset(Tmax_t, names %in% nochange$names)))
nochange_sites$year <- c(1901:2018)
nochange_sites$type <- "nochange"


deforesting_sites <- as.data.frame(t(subset(Tmax_t, names %in% deforesting$names)))
deforesting_sites$year <- c(1901:2018)
deforesting_sites$type <- "deforest"

historic_toplotreforest <- melt(reforesting_sites, id.vars=c("year", "type"))
historic_toplotdeforest <- melt(deforesting_sites, id.vars=c("year", "type"))
historic_toplotnochange <- melt(nochange_sites, id.vars=c("year", "type"))

historic_toplot <- rbind(historic_toplotreforest, historic_toplotdeforest, historic_toplotnochange)
#historic_toplot <- rbind(historic_toplotreforest, historic_toplotnochange)
historic_toplot$year <- as.numeric(historic_toplot$year)
historic_toplot$value <- as.numeric(historic_toplot$value)
historic_toplot$type <- as.factor(historic_toplot$type)
historic_toplot
str(historic_toplot)

historic_toplot_post1960 <- subset(historic_toplot, year > 1945)

#OK not great, but what about when we subset by latitude (above) 
ggplot(historic_toplot, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover change", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  theme_classic()+
  ylim(-2,2)

ggplot(historic_toplot_post1960, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover change", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  theme_classic()+
  ylim(-2,2)


#Try with forest height map ---------------
mean_na <- function(x) {
  mean(x,na.rm=T)
}

Forest_age_2019 <- raster("Raw/Other/Forest_Age_Conus.tif")
Fo_crop <- crop(Forest_age_2019, Diffs_reproj)
Fo_crop[Fo_crop < 18] <- NA

#Let's try: <40, 40-75, 80+ (?), and then NOforest
Fo_age <- raster::extract(Fo_crop, xy, fun=mean_na, buffer=1000, df=T)
colnames(merged)[colnames(merged)=="X"] <- "ID"
all_foage <- merge(merged, Fo_age, by="ID")

noforest <- subset(all_foage, is.na(Forest_Age_Conus))
youngforest <- subset(all_foage, Forest_Age_Conus <65)
#midforest <- subset(all_foage, Forest_Age_Conus >30 & Forest_Age_Conus < 65 )
oldforest <- subset(all_foage, Forest_Age_Conus >65)

#Plot TMax by group
noforest_sites <- as.data.frame(t(subset(Tmax_t, names %in% noforest$names)))
noforest_sites$year <- c(1901:2018)
noforest_sites$type <- "noforest"

youngforest_sites <- as.data.frame(t(subset(Tmax_t, names %in% youngforest$names)))
youngforest_sites$year <- c(1901:2018)
youngforest_sites$type <- "youngforest"


#midforest_sites <- as.data.frame(t(subset(Tmax_t, names %in% midforest$names)))
#midforest_sites$year <- c(1901:2018)
#midforest_sites$type <- "midforest"

oldforest_sites <- as.data.frame(t(subset(Tmax_t, names %in% oldforest$names)))
oldforest_sites$year <- c(1901:2018)
oldforest_sites$type <- "oldforest"

historic_toplotnoforest <- melt(noforest_sites, id.vars=c("year", "type"))
historic_toplotyoungforest <- melt(youngforest_sites, id.vars=c("year", "type"))
#historic_toplotmidforest <- melt(midforest_sites, id.vars=c("year", "type"))
historic_toplotoldforest <- melt(oldforest_sites, id.vars=c("year", "type"))

#historic_toplot <- rbind(historic_toplotnoforest, historic_toplotyoungforest, historic_toplotmidforest, historic_toplotoldforest)
historic_toplot <- rbind(historic_toplotnoforest, historic_toplotyoungforest,historic_toplotoldforest)
#historic_toplot <- rbind(historic_toplotreforest, historic_toplotnochange)
historic_toplot$year <- as.numeric(historic_toplot$year)
historic_toplot$value <- as.numeric(historic_toplot$value)
historic_toplot$type <- as.factor(historic_toplot$type)
historic_toplot
str(historic_toplot)

historic_toplot_post1960 <- subset(historic_toplot, year > 1945)

#OK not great, but what about when we subset by latitude (above) 
ggplot(historic_toplot, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by Forest Age", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  theme_classic()+
  ylim(-2,2)

ggplot(historic_toplot_post1960, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by Forest age", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  theme_classic()+
  ylim(-2,2)

#One more try: Vegetation height map --------
x <- raster("Raw/Other/LandFire/US_105evh/grid1/us_105evh/z001001.adf")
e2 <- extent(300000, 2200000, 177285, 2500000)
x <- crop(x, e2)
plot(x)
y <- reclassify(x, cbind(-Inf,0,NA), right=FALSE)
plot(y)
#Using nearest neighbor because it's a discrete (class) variable
#Taking FOREVER - run overnight. 
Heigh_reproj <- projectRaster(y, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", method = "ngb" )
writeraster("Processed/landfireheight.tif")

#Reproject y 

#Categories: of interest: 
#Forest height: 109 (5 to 10 meters), 110 (10 to 25 meters), 111 (25 to 50 meters), 112 (> 50 meters)
#Agriculture - General (80), Pasture/Hay (81), etc. etc. 



#Going back to Lily's RAW data------------
library(readxl)
library(Amelia)
getwd()
Format_Weather <- function(x){
  filename <- paste(x)
  station <- tolower(substr(filename, 1,10))
  station <- gsub('[0-9]+', '', station)
  station <- gsub('_', '', station)
  print(station)
  toread <- paste("Raw/Weather_Station_Raw", x, sep="/")
  y <- as.data.frame(read_excel(toread, na="-9999", skip=1))
  colnames(y)[colnames(y)=="TAVE (F)"] <- "Tavg"
  y$monthyear <- paste(y$Month, y$Year, sep="-")
  #FOR GROWING SEASON ONLY
  gs <- subset(y, Month == 6 | Month == 7 | Month == 8 | Month == 9)
  gs$Tavg <- as.numeric(gs$Tavg)
  z <- ddply(gs, .(Year), summarise, Tavg_gs=mean_na(Tavg),T75_gs=quantile(Tavg, probs=(0.75), na.rm=TRUE),T90_gs=quantile(Tavg, probs=(0.90), na.rm=TRUE))
  z$ID <- station
  return(z)
}

lxls <- list.files("Raw/Weather_Station_Raw", pattern= "\\.xlsx$")
test <- (lapply(lxls, Format_Weather))
All_Sites_Temps <- do.call(rbind, test)
write.csv(All_Sites_Temps, "Processed/All_Growingseason_Temps_gs.csv")

#Now to merge to get lat longs
#just for home computer
setwd("/Users/mallory/Documents/LST_Data")
#All_Sites_Temps <- read.csv("Processed/All_Growingseason_Temps_gs.csv")
#latlongs <- read.csv("Lily_Data/USHCNstationinformation_percentforest.csv")

All_Sites_Temps <- read.csv("All_Growingseason_Temps_gs.csv")
All_Sites_Temps$ID <- as.character(All_Sites_Temps$ID)
All_Sites_Temps[7738:7851,6] <- "greenville_oh"
All_Sites_Temps[7852:7960,6] <- "greenville_ms"
All_Sites_Temps[8304:8424,6] <- "henderson_nc"
All_Sites_Temps[8425:8517,6] <- "henderson_ky"
All_Sites_Temps[10455:10561,6] <- "laurel_ms"
All_Sites_Temps[10344:10454,6] <- "laurel_md"
All_Sites_Temps[10889:10998,6] <- "lewisburg_wv"
All_Sites_Temps[10999:11112,6] <- "lewisburg_tn"
All_Sites_Temps[11795:11904,6] <- "madison_fl"
All_Sites_Temps[11905:12018,6] <- "madison_in"
All_Sites_Temps[12019:12137,6] <- "marion_nc"
All_Sites_Temps[12138:12253,6] <- "marion_in"
All_Sites_Temps[16592:16694,6] <- "salisbury_md"
All_Sites_Temps[16471:16591,6] <- "salisbury_nc"
All_Sites_Temps[20603:20715,6] <- "washington_in"
All_Sites_Temps[20716:20824,6] <- "washington_ga"
All_Sites_Temps[22044:22143,6] <- "woodstock_md"
All_Sites_Temps[22144:22157,6] <- "woodstock_va"
All_Sites_Temps[21164:21275,6] <- "waynesboro_tn"

All_Sites_Temps$ID <- as.factor(All_Sites_Temps$ID)
All_Sites_Temps <- subset(All_Sites_Temps, Year >1900)
summary(All_Sites_Temps$ID)

latlongs <- read.csv("USHCNstationinformation_percentforest.csv")

str(latlongs)
latlongs$STA_NAME <- tolower(latlongs$STA_NAME)
latlongs$STA_NAME <- str_replace_all(latlongs$STA_NAME, " ", "")
latlongs$ID <- substr(latlongs$STA_NAME, 1,10)

head(All_Sites_Temps)
latlongs[180:195,]
#Check for diffs
check = setdiff(All_Sites_Temps$ID, latlongs$ID)
latlongs[3,7] <-"albany"
#latlongs[x,7] <- "anderson"
latlongs[9,7] <-"bareacolle"
#latlongs[x,7] <- "batesville"
#latlongs[x,7] <- "blackville"
latlongs[12,7] <-"boonville"
latlongs[13,7] <-"bowlinggre"
#latlongs[x,7] <- "brunswick"
latlongs[19,7] <- "cambrige"  
#latlongs[x,7] <- "charleston" 
#latlongs[x,7] <- "cheraw"     
#latlongs[x,7] <- "circlevill" 
latlongs[33,7] <- "cornith"    
latlongs[35,7] <-"crystalspr" 
latlongs[36,7] <- "dalonega"   
latlongs[41,7] <- "denton"    
#latlongs[x,7] <- "downtownfr" 
latlongs[44,7] <- "eastman"   
latlongs[48,7] <- "farmville" 
#latlongs[x,7] <- "fernandina" 
#latlongs[x,7] <- "georgetown" 
latlongs[58,7] <- "glennville"
latlongs[65,7] <- "greenville_ms"
latlongs[66,7] <- "greenville_oh"
#latlongs[x,7] <- "haitesvill" 
latlongs[70,7] <- "henderson_nc"
latlongs[71,7] <- "henderson_ky"
latlongs[81,7] <- "inverness"  
#latlongs[x,7] <- "kershaw"    
#latlongs[x,7] <- "kinston"   
#latlongs[x,7] <- "littlemtn" 
latlongs[87,7] <- "laurel_ms"
latlongs[88,7] <- "laurel_md"
latlongs[92,7] <- "lewisburg_wv"
latlongs[93,7] <- "lewisburg_tn"
latlongs[99,7] <- "madison_fl"
latlongs[100,7] <- "madison_in"
latlongs[101,7] <- "marion_in"
latlongs[104,7] <- "mcconelsvi"
latlongs[105,7] <- "mcminville"
latlongs[114,7] <- "mufreesbur"
latlongs[118,7] <- "newport"  
latlongs[120,7] <- "oakland"    
latlongs[124,7] <- "owens"    
#latlongs[x,7] <- "quitman" 
#latlongs[x,7] <- "rome."    
#latlongs[x,7] <- "salem"     
#latlongs[x,7] <- "saluda"   
#latlongs[x,7] <- "sandusky"  
latlongs[142,7] <- "salisbury_nc"
latlongs[141,7] <- "salisbury_md"
#latlongs[x,7] <- "slatesvill"
#latlongs[x,7] <- "stleo"     
#latlongs[x,7] <- "stuanton" 
#latlongs[x,7] <- "summervill"
latlongs[156,7] <- "tarboro"   
#latlongs[x,7] <- "tarponspri" 
latlongs[157,7] <- "thomasvile"
latlongs[161,7] <- "troy."     
latlongs[163,7] <- "tulcaloosa"
latlongs[164,7] <- "union city"
#latlongs[x,7] <- "walhalla" 
latlongs[170,7] <- "washington_in"
latlongs[174,7] <- "waynesboro_tn"
latlongs[175,7] <- "waynesboro_ms"
#latlongs[x,7] <- "whitestown"
#latlongs[x,7] <- "winnsburg"  
#latlongs[x,7] <- "winthropun" 
latlongs[181,7] <- "woodstock_md"
latlongs[182,7] <- "woodstock_va"
latlongs[184,7] <- "yemasse"   

check2= setdiff(latlongs$ID, All_Sites_Temps$ID)

merged_gs <- merge(All_Sites_Temps, latlongs, by="ID")
colnames(merged_gs)[2] <- "ID_no"
merged_gs <- merged_gs[order(merged_gs$ID_no),]
summary(merged_gs$ID)
write.csv(merged_gs, "to_fill_years.csv")
merged_gs<- read.csv("to_fill_years.csv")
#Need to gap fill
#Create dataframe with all site names, and then year values of 1900:2013 for all
#Then merge, keeping all.y, and then we'll be good! :D
#Split, then rbind the 1901-2013 to each element, then combine
merged_gs <- as.data.frame(droplevels(merged_gs))
X <- split(merged_gs, merged_gs$ID)
addyears <- function(x){
Year <-   as.data.frame(c(1901:2013))
colnames(Year) <- "Year"
y <- merge(Year, x, by="Year", all.x=TRUE)
y$ID <- as.character(y$ID)
print(x$ID[2])
y$ID <- x$ID[2]
y$ID <- as.factor(y$ID)
y$LAT <- x$LAT[2]
y$LONG <- x$LONG[2]
y$STA_NAME <- x$STA_NAME[2]
y$ID_no <- as.numeric(x$ID[2])
print(y$ID_no)
return(y)
}

test2 <- lapply(X, addyears)
All_Sites_Temps_clean <- do.call(rbind, test2)
summary(All_Sites_Temps_clean$ID)
write.csv(All_Sites_Temps_clean, "Allsites_gs_cleaned.csv")
#Diffs_reproj <- raster("Processed/Change_LC_proj.tif")
Diffs_LC <- raster("Change_LC_FORESCE.tif")
#Diffs_reproj <- projectRaster(Diffs_LC, crs="+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0", method = "ngb" )
#writeRaster(Diffs_reproj, "Change_LC_proj.tif")
Diffs_reproj <- raster("Change_LC_proj.tif")
plot(Diffs_reproj)
xy <- cbind(All_Sites_Temps_clean$LONG, All_Sites_Temps_clean$LAT)
xy2 <- unique(xy)
#Absmax, from here: https://stackoverflow.com/questions/24652771/finding-the-maximum-absolute-value-whilst-preserving-or-symbol
absmax <- function(x) { x[which.max( abs(x) )][1]}
LC_Change <- raster::extract(Diffs_reproj, xy2, fun=absmax, buffer=600, df=T)
str(All_Sites_Temps_clean)
str(LC_Change)
LC_Change <- plyr::rename(LC_Change, c("ID" = "ID_no"))
all_forest_gs <- merge(All_Sites_Temps_clean, LC_Change, by="ID_no")
#Hoping it worked 
all_forest_gs$type <- ifelse((all_forest_gs$Change_LC_proj == 1),
                             "reforest",
                     ifelse((all_forest_gs$Change_LC_proj ==-1),"deforest","nochange"))


historic_toplotreforest <- melt(reforesting_gs, id.vars=c("Year", "type"), measure.vars = c("LAT", "Tavg_gs", "T75_gs", "T90_gs"))
historic_toplotdeforest <- melt(deforesting_gs, id.vars=c("Year", "type"))
historic_toplotnochange <- melt(nochange_gs, id.vars=c("Year", "type"))

historic_toplot <- rbind(historic_toplotreforest, historic_toplotdeforest, historic_toplotnochange)
#historic_toplot <- rbind(historic_toplotreforest, historic_toplotnochange)
historic_toplot$year <- as.numeric(historic_toplot$year)
historic_toplot$value <- as.numeric(historic_toplot$value)
historic_toplot$type <- as.factor(historic_toplot$type)
historic_toplot
str(historic_toplot)

historic_toplot_post1960 <- subset(historic_toplot, year > 1945)

#OK not great, but what about when we subset by latitude (above) 
ggplot(historic_toplot, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover change", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  theme_classic()+
  ylim(-2,2)

ggplot(historic_toplot_post1960, aes(x=year, y=value, colour=type, group=type)) +
  geom_smooth(method="loess")+
  labs(title="Air temperature trend by land cover change", 
       y="Temperature Anomaly (Z score)", 
       x="Year")+
  theme_classic()+
  ylim(-2,2)



