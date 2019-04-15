#Figures for Lily's Undergraduate Thesis Defense: 
#Agreed-upon color scheme for the different land cover types: Color blind friendly!
#Urban "c51b7d"
#Forest "#276419"
#Grass "#7fbc41"

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
  geom_line() +
  scale_color_manual(labels = c("Forest", "Cropland", "Urban"), values = c("#276419", "#7fbc41", "#c51b7d"))+
  labs(color = "Land Cover\n") +
  geom_errorbar(aes(x=Month, ymin=value-error, ymax=value + error), width=0.2, size=0.5)+
  labs(title="Ta-Ts by Land Cover Type", 
       y="Ta-Ts (degrees C)", 
       x="Month")+
  ylim(-4.4,5)+
  scale_x_continuous(breaks=seq(1,12,3))+
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
  theme(legend.text = element_text(colour="black", size = 12, face = "bold"), legend.title = element_blank())

ggsave("Land_Cover_Change.pdf")
  

