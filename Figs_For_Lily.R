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
#Load file for plotting
df <- read.csv("/Users/mallory/Documents/Temp_Project/APPEARS_LST/for_plotting.csv")
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
allmeans <- read.csv("/Users/mallory/Documents/Temp_Project/Lily_Data/allmeans200mupdateddata.csv")
fomeans <- read.csv("/Users/mallory/Documents/Temp_Project/Lily_Data/allmeans200mupdateddata.csv")
agmeans <- read.csv("/Users/mallory/Documents/Temp_Project/Lily_Data/allmeans200mupdateddata.csv")

allmeans$category <- "all"
agmeans$category <- "ag"
fomeans$category <- "for"

alltoplot <- rbind(rbind(allmeans, agmeans), fomeans)
ggplot(alltoplot, aes(x=year, y=y, color=category))+
  geom_line()

alltoplot

#---------------Remaking Lily's figs
#Lookup table first (T_site_lookup will work for Tmax,Tmax, and Tmin)
Tavg_site <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", nrows=3, header=T)
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
write.csv(T_site_lookup, "/Users/mallory/Documents/Temp_Project/Lily_Data/Lookup.csv")
#Tavg---
headers <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", header = F, nrows=1, as.is=T)
Tavg <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
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
write.csv(All, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tavg_Allsite.csv")
write.csv(For_all, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tavg_Forsite.csv")
write.csv(Ag_all, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tavg_Agsite.csv")

For_all$x <- Ag_all$Agmean
names(For_all) <- c("year", "forest", "ag")
str(For_all)
toplot5 <- melt(For_all, id.vars = "year", measure.vars = c("forest", "ag"))
ggplot(toplot5, aes(x=year, y=value, colour=variable))+
  geom_line()
#Tmax---
Tmax<- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TMAX_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
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
write.csv(Allmax, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmax_Allsite.csv")
write.csv(For_allmax, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmax_Forsite.csv")
write.csv(Ag_allmax, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmax_Agsite.csv")

#Tmin--- 
Tmin <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TMIN_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
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
write.csv(Allmin, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmin_Allsite.csv")
write.csv(For_allmin, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmin_Forsite.csv")
write.csv(Ag_allmin, "/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmin_Agsite.csv")

#To-do's
#1: Remake star figure
#2: Remake CP lines figure (prob easier)
library(reshape2)
library(ggplot2)
#For Average Lines
All_line <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Avg_all_output_line.csv")
names(All_line) <-c("x", "All", "Year")
Fo_line <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Avg_for_output_line.csv")
names(Fo_line) <-c("x", "Fo", "Year")
Ag_line<- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Avg_ag_output_line.csv")
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
All_line <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Max_all_output_line.csv")
names(All_line) <-c("x", "All", "Year")
Fo_line <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Max_for_output_line.csv")
names(Fo_line) <-c("x", "Fo", "Year")
Ag_line<- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Max_ag_output_line.csv")
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
All_line <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Min_all_output_line.csv")
names(All_line) <-c("x", "All", "Year")
Fo_line <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Min_for_output_line.csv")
names(Fo_line) <-c("x", "Fo", "Year")
Ag_line<- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/Novick_Analyses/Min_ag_output_line.csv")
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
All_avg <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tavg_Allsite.csv")
names(All_avg) <-c("x", "Year", "All")
Fo_avg <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tavg_Forsite.csv")
names(Fo_avg) <-c("x", "Year", "Fo")
Ag_avg <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tavg_Agsite.csv")
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
All_max <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmax_Allsite.csv")
names(All_max) <-c("x", "Year", "All")
Fo_max <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmax_Forsite.csv")
names(Fo_max) <-c("x", "Year", "Fo")
Ag_max <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/CPModel/data/Tmax_Agsite.csv")
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
latlongs <- read.csv("/Users/mallory/Documents/Temp_Project/Lily_Da ta/USHCNstationinformation_percentforest.csv")
str(latlongs)
latlongs$STA_NAME <- tolower(latlongs$STA_NAME)
latlongs$STA_NAME <- str_replace_all(latlongs$STA_NAME, " ", "")
latlongs$names <- substr(latlongs$STA_NAME, 1,10)

#Site lookup
T_site_lookup <- read.csv("/Users/mallory/Documents/Temp_Project/Lily_Data/Lookup.csv")
str(T_site_lookup)
T_site_lookup$names <- tolower(T_site_lookup$names)
T_site_lookup$names
T_site_lookup$names <- substr(T_site_lookup$names, 1,10)

T_site_lookup$names
latlongs$names
#Looks like I lost a few but don't have time to figure it out at the moment. 
merged <- merge(T_site_lookup, latlongs, by="names")
#Get rid of FL sites!
merged <- subset(merged, !STATE=="FL")
mergedfo <- subset(merged, LC=="For")
mergedag <- subset(merged, LC=="Ag")
mergedhighlat <- subset(merged, LAT <43 & LAT > 37)
mergedmidlat <- subset(merged, LAT <37 & LAT >33 )
mergedlowlat <- subset(merged, LAT < 33)
#Headers
headers <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", header = F, nrows=1, as.is=T)
#Tmax data
Tmax <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TMAX_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
names(Tmax) <- headers
Year <- Tmax$Year
#Going to just look at trends since 1960 now...
#Get z-scores (anomalies)
Tmax <- cbind(Year, as.data.frame(scale(Tmax[2:186], center = TRUE, scale = TRUE)))
str(Tmax)
Tmax_t <- as.data.frame(t(Tmax))
Tmax_t$names <- rownames(Tmax_t)
str(Tmax_t$names)
Tmax_t$names <- tolower(Tmax_t$names)


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


