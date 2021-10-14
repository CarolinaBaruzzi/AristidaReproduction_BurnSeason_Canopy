#Required libraries
library(lme4) #run generalized linear mixed effects models
library(emmeans) #run pairwise comparisons of glmer models
library(glmmTMB) #run zero inflated glmer models
library(tidyverse) #for data manipulation

#Load data
Aristida_Flowering <-read.csv("Aristida_Flowering.csv")

#### Fecundity analysis #### 

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

#### Number of inflorescences analysis #### 
#Remove early dry season data because there were almost no inflorescences produced in that season
Aristida_Flowering2 <- filter(Aristida_Flowering, Burn_Season != "Early dry")

#Run poisson, negative binomial, and respective zero-inflated models to find best distribution
fit.inflo.pois <- glmer(Inflorescence_Number ~ Canopy_Cover * Burn_Season + Basal_Area2 * Burn_Season +
                          (0 + Basal_Area2 | Transect_ID) + (1 | Transect_ID), family = poisson, 
                        data = Aristida_Flowering2)

fit.inflo.nb <- glmer.nb(Inflorescence_Number ~ Canopy_Cover * Burn_Season + Basal_Area2 * Burn_Season +
                           (0 + Basal_Area2 | Transect_ID) + (1 | Transect_ID), 
                         data = Aristida_Flowering2)

fit.inflo.zipois <- glmmTMB(Inflorescence_Number ~ Canopy_Cover * Burn_Season + Basal_Area2 * Burn_Season +
                              (0 + Basal_Area2 | Transect_ID) + (1 | Transect_ID), data = Aristida_Flowering2, 
                            ziformula= ~ 1, family = poisson)

fit.inflo.zinb <- glmmTMB(Inflorescence_Number ~ Canopy_Cover * Burn_Season + Basal_Area2 * Burn_Season +
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
emmeans(fit.inflo.nb, pairwise ~ Burn.Month * CanopyCover)
emmip(fit.inflo.nb, CanopyCover ~ Burn.Month)
