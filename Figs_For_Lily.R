#Figures for Lily's Undergraduate Thesis Defense: 
#Agreed-upon color scheme for the different land cover types: Color blind friendly!
#Urban "c51b7d"
#Forest "#276419"
#Grass "#7fbc41"
#Update 7/4/2019: Re-doing change point analyses!

#Load file for plotting
df <- read.csv("for_plotting.csv")
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

#---------------Remaking Lily's figs. 
headers <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", header = F, nrows=1, as.is=T)
Tavg <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TAVG_20190621_Allsiteswithnames200mbufferlandcover.csv", skip=4)
names(Tavg) <- headers
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
str(Tavg)
head(Tavg_site)
#Create lookup table 
T_site_lookup <- cbind(names = rownames(Tavg_site), Tavg_site)
rownames(T_site_lookup) <- c()
T_site_lookup <- as.data.frame(T_site_lookup[c(1,5)])
#Get list of forest sites 
Fo_list <- as.vector(T_site_lookup$names[T_site_lookup$LC=="For"])
df.subset <- Tavg[, Fo_list]
str(df.subset)
str(Fo_list)
allowedVars <- c("species", "family", "location")
Tmax<- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TMAX_20190621_Allsiteswithnames200mbufferlandcover.csv")
Tmin <- read.csv("/Users/mallory/Documents/Temp_Project/Weather Station/TMIN_20190621_Allsiteswithnames200mbufferlandcover.csv")
