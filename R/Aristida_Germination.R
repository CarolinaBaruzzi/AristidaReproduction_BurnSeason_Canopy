#Required libraries
library(lme4) #run generalized linear mixed effects models

#Load data
Aristida_Germination <-read.csv("Data/Aristida_Germination.csv")

#Filter June data
Aristida_Germination_June <- filter(Aristida_Germination, Burn_Season == "Early wet") 

#Run model
germ.fit <- glmer(Germinated ~ Canopy_Cover + (1 | Box_ID), data = Aristida_Germination_June, 
                  family = binomial())

#Summarize results
summary(germ.fit)

#### Germination probability figure ####

ggplot(Aristida_Germination, aes(Burn_Season, Germinated, color = Canopy_Cover)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = 0, size = 1.2,
               position = position_dodge(0.5)) +
  stat_summary(fun=mean, geom="point", size=4,
               position = position_dodge(0.5)) +
  scale_colour_manual(values = c("#29AF7FFF", "#453781FF")) +
  labs(y = "Germination probability", x = "Burn season", colour = " ") + 
  scale_y_continuous(limits=c(0,1)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18),
        text=element_text(size=24),
        legend.position = c(0.15, 0.8)) 
