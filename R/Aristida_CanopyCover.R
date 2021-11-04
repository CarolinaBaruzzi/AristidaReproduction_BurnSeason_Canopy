#Required libraries
library(tidyverse)
library(rcompanion) 

#Load data
Aristida_Canopy <-read.csv("Aristida_Canopy.csv")

#Filter open canopy data
canopy_open <- filter(Aristida_Canopy, Canopy_Cover == "Open")
#Check normality test for open canopy
qqnorm(canopy_open$Percent_Light_Transmited)
qqline(canopy_open$Percent_Light_Transmited, col="red")

#Filter partial canopy data
canopy_partial <- filter(canopy, Canopy_Cover == "Partial")
#Check normality test for parial canopy
qqnorm(canopy_partial$Percent_Light_Transmited)
qqline(canopy_partial$Percent_Light_Transmited, col="red")

#Run Wilcoxon rank sum test because residual don't appear to be normal
wilcox.test(Percent_Light_Transmited ~ Canopy_Cover, data = Aristida_Canopy)

#### Percent light transmited figure ####
#Order factors within the canopy cover category to have open plots first
Aristida_Canopy <- Aristida_Canopy %>% 
  mutate(Canopy_Cover = fct_relevel(Canopy_Cover,"Open", "Partial"))

#plot data
ggplot(Aristida_Canopy, aes(Burn_Season, Percent_Light_Transmited, color = Canopy_Cover)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = 0, size = 1.2,
               position = position_dodge(0.5)) +
  stat_summary(fun=mean, geom="point", size=4,
               position = position_dodge(0.5)) +
  scale_colour_manual(values = c("#29AF7FFF", "#453781FF")) +
  labs(y = "Light transmission (%)", x = "Burn season",
       colour = " ") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20),
        legend.text=element_text(size=20),
        legend.position = c(0.85, 0.6)) 
