#Required libraries
library(lme4) #run generalized linear mixed effects models
library(emmeans) #run pairwise comparisons of glmer models
library(glmmTMB) #run zero inflated glmer models
library(tidyverse) #for data manipulation

#Load data
Aristida_Flowering <-read.csv("Aristida_Flowering.csv")

#### Reproduction probability analysis #### 

#Rescale plant basal area data
Aristida_Flowering$Basal_Area2 <- scale(Aristida_Flowering$Basal_Area)

#Run model
fec.fit <- glmer(Fecundity ~ Canopy_Cover * Burn_Season + Basal_Area2 * Burn_Season +
                        (0 + Basal_Area2 | Transect_ID) + (1 | Transect_ID), family = binomial(), 
                      control = glmerControl(optimizer = "bobyqa",
                                             optCtrl = list(maxfun=2e5)), 
                      data = Aristida_Flowering)
#Summarize results
summary(fec.fit)

#Run pairwise comparisons
emmeans(fec.fit, pairwise ~ Burn_Season)
emmeans(fec.fit, pairwise ~ Burn_Season * Canopy_Cover)
emtrends(fec.fit, pairwise ~ Burn_Season, var = "Basal_Area2")

#### Reproduction probability figure #### 
#Order factors to plot the data
Aristida_Flowering$Burn_Season  <- factor(Aristida_Flowering$Burn_Season, 
                                           levels=c("Early dry", "Mid-dry", "Early wet"))

ggplot(Aristida_Flowering, aes(Burn_Season, Fecundity, color = Canopy_Cover)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = 0, size = 1.2,
               position = position_dodge(0.5)) +
  stat_summary(fun=mean, geom="point", size=4,
               position = position_dodge(0.5)) +
  scale_colour_manual(values = c("#29AF7FFF", "#453781FF")) +
  labs(y = "Probability of flowering", x = "Burn season", colour = " ") + 
  scale_y_continuous(limits=c(0,1)) + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 22, face = "bold"),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=18),
        text=element_text(size=24),
        legend.position = c(0.15, 0.80)) 

#### Number of inflorescences analysis #### 
#Remove early dry season data because there were almost no inflorescences produced in that season
Aristida_Flowering2 <- filter(Aristida_Flowering, Burn_Season != "Early dry")

#Run poisson, negative binomial, and respective zero-inflated models to find best distribution
fit.inflo.pois <- glmer(Inflorescence_Number ~ Canopy_Cover * Burn_Season + 
                          Basal_Area2 * Burn_Season +
                          (0 + Basal_Area2 | Transect_ID) + (1 | Transect_ID), family = poisson, 
                        data = Aristida_Flowering2)

fit.inflo.nb <- glmer.nb(Inflorescence_Number ~ Canopy_Cover * Burn_Season + 
                           Basal_Area2 * Burn_Season +
                           (0 + Basal_Area2 | Transect_ID) + (1 | Transect_ID), 
                         data = Aristida_Flowering2)

fit.inflo.zipois <- glmmTMB(Inflorescence_Number ~ Canopy_Cover * Burn_Season + 
                              Basal_Area2 * Burn_Season +
                              (0 + Basal_Area2 | Transect_ID) + (1 | Transect_ID), data = Aristida_Flowering2, 
                            ziformula= ~ 1, family = poisson)

fit.inflo.zinb <- glmmTMB(Inflorescence_Number ~ Canopy_Cover * Burn_Season + 
                            Basal_Area2 * Burn_Season +
                            (0 + Basal_Area2 | Transect_ID) + (1 | Transect_ID), 
                          data = Aristida_Flowering2, 
                          ziformula = ~ 1, family = nbinom1(link = "log")) #excluded for onvergence issues

#Compare models based on AIC
AIC(fit.inflo.pois)
AIC(fit.inflo.nb) #best
AIC(fit.inflo.zipois)

#Summrize model results
summary(fit.inflo.nb)

#Run pairwise comparisons
emmeans(fit.inflo.nb, pairwise ~ Burn_Season * Canopy_Cover)

#### Number of inflorescences figure #### 

ggplot(Aristida_Flowering2, aes(Burn_Season, Inflorescence_Number, color = Canopy_Cover)) +
  stat_summary(fun.data=mean_cl_boot, geom="errorbar", width = 0, size = 1.2,
               position = position_dodge(0.5)) +
  stat_summary(fun=mean, geom="point", size=4,
               position = position_dodge(0.5)) +
  scale_colour_manual(values = c("#29AF7FFF", "#453781FF")) +
  labs(y = "Number of inflorescences", x = "Burn season", colour = " ") + 
  theme_bw() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 22, face = "bold"),
        axis.title.y = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 18),
        legend.text=element_text(size=18),
        text=element_text(size=24),
        legend.position = c(0.2, 0.8)) 

