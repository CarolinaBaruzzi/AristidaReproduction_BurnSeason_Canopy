#Required libraries
library(nlme) #run generalized linear mixed effects models
library(emmeans) #run pairwise comparisons of glmer models
library(rcompanion) #for normality plots

#### Wiregrass basal area analysis ####
#Check normality
plotNormalHistogram(Aristida_Flowering$Basal_Area)
qqnorm(Aristida_Flowering$Basal_Area)
qqline(Aristida_Flowering$Basal_Area, col="red")

#Log transform basal area
Aristida_Flowering$Basal_Area_log <- log(Aristida_Flowering$Basal_Area) 
plotNormalHistogram(Aristida_Flowering$Basal_Area_log)
qqnorm(Aristida_Flowering$Basal_Area_log)
qqline(Aristida_Flowering$Basal_Area_log, col="red")

#Run mixed effect linear model
fit.area <- lme(Basal_Area_log ~ Burn_Season * Canopy_Cover, random = ~ 1|Transect_ID, 
                data = Aristida_Flowering)
#Check homogeneity of variance
plot(fit.area) 

#Run pairwise comparisons
emmeans(fit.area, pairwise ~ Burn_Season * Canopy_Cover)

#### Wiregrass basal area figure ####

ggplot(Aristida_Flowering, aes(Burn_Season, Basal_Area, color =  Canopy_Cover)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = 0, size = 1.2,
               position = position_dodge(0.5)) +
  stat_summary(fun=mean, geom="point", size=4,
               position = position_dodge(0.5)) +
  scale_colour_manual(values = c("#29AF7FFF", "#453781FF")) +
  labs(y = "Basal area (cm²)", x = "Burn season", colour = " ") +
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 20),
        legend.text=element_text(size=20),
        legend.position = c(0.8, 0.8)) 
