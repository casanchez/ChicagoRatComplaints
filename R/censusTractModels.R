# models to predict rat complaints from different spatial predictors

# libraries---------------------------------------------------------------------
library(bbmle)
library(beepr)
library(corrplot)
library(fitdistrplus)
library(glmmTMB)
library(tidyverse)

# load data---------------------------------------------------------------------
rc <- read.csv("./Data/ratCompPredsCT.csv", header = TRUE) %>% 
#rc <- read.csv("./Data/ratCompPredsCT_baited.csv", header = TRUE) %>% 
  mutate_at(vars(year), as.factor) %>% 
  mutate(logPD = log(popDens)) %>%  # since there's one really high value
  dplyr::select(-c(totalPop, area, popDens))
  
# format tract as character, place leading zero if it's only five digits
rc$tract <- format(rc$tract, width = 6)
rc$tract <- gsub(" ", "0", rc$tract)

# models------------------------------------------------------------------------

# examine correlations between predictors
corrplot(cor(rc[, c(5:16)]), method = "number")
# pGradDegree and medHouseholdIncome have correlation of 0.7
# will examine VIFs in model

hist(rc$ratcomp)
descdist(rc$ratcomp, discrete = TRUE)
# looks like we need negative binomial distribution

# center and scale the predictors
cs <- function(x) {
  scale(x, center = TRUE, scale = TRUE)
}

rcScaled <- rc %>% 
  mutate_at(c(5:16), cs)

# set up models

Sys.time()
globalZINB <- glmmTMB(ratcomp ~ quarter + bPerm + dogFeces + garbage + food +
                        pGradDegr + medHouseholdInc + pVacantHU +
                        pOwnerOcc + pBuiltPre1950 + pCrowded +
                        pUnder5y + logPD + (1|year) + (1|tract), 
                      ziformula = ~logPD, family = nbinom1, data = rcScaled)
Sys.time()
beep(sound=2)

Sys.time()
# this one takes about 11 min
globalZINB2 <- glmmTMB(ratcomp ~ quarter + bPerm + dogFeces + garbage + food +
                         pGradDegr + medHouseholdInc + pVacantHU +
                         pOwnerOcc + pBuiltPre1950 + pCrowded +
                         pUnder5y + logPD + (1|year) + (1|tract),
                       ziformula = ~logPD, family = nbinom2, data = rcScaled)
Sys.time()
beep(sound=2)

AICtab(globalZINB, globalZINB2)
#             dAIC    df
# globalZINB2     0.0 21
# globalZINB    8371.1 21

# okay, so nbinom2 is best

# check multicollinearity
library(performance)
check_collinearity(globalZINB2, component = "count")
# all VIFs < 4


# now compare global model against other candidate models

# harborage: building age and vacant units
m1 <- glmmTMB(ratcomp ~ pBuiltPre1950 + pVacantHU + quarter + (1|year) + 
                (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)

# attractants: dog feces, garbage, restaurants
m2 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + quarter + (1|year) + 
                (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)

# disturbance: construction/demolition permits
m3 <- glmmTMB(ratcomp ~ bPerm + quarter + (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)
  
# human density: population density, overcrowding  
m4 <- glmmTMB(ratcomp ~ logPD + pCrowded + quarter + (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

# socioeconomics: household income, % graduate degree
m5 <- glmmTMB(ratcomp ~ medHouseholdInc + pGradDegr + quarter + (1|year) + 
                (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)

# demographics: % owner occupied, % under 5
m6 <- glmmTMB(ratcomp ~ pOwnerOcc + pUnder5y + quarter + (1|year) + 
                (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)


# harborage + attractants
m7 <- glmmTMB(ratcomp ~ pBuiltPre1950 + pVacantHU + dogFeces + garbage +
                food + quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled)

# harborage + disturbance
m8 <- glmmTMB(ratcomp ~ pBuiltPre1950 + pVacantHU + bPerm + quarter + 
                (1|year) + (1|tract), ziformula = ~logPD, family = nbinom2, 
              data = rcScaled)

# harborage + density
m9 <- glmmTMB(ratcomp ~ pBuiltPre1950 + pVacantHU + logPD + pCrowded +
                quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled) 

# harborage + socioecon
m10 <- glmmTMB(ratcomp ~ pBuiltPre1950 + pVacantHU + medHouseholdInc +
                 pGradDegr + quarter + (1|year) + (1|tract),
               ziformula = ~logPD, family = nbinom2, data = rcScaled)

# harborage + demographics
m11 <- glmmTMB(ratcomp ~ pBuiltPre1950 + pVacantHU + pOwnerOcc + 
                 pUnder5y + quarter + (1|year) + (1|tract), 
               ziformula = ~logPD, family = nbinom2, data = rcScaled)

# attractants + disturbance
m12 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + bPerm + quarter + 
                 (1|year) + (1|tract), ziformula = ~logPD, family = nbinom2, 
               data = rcScaled)

# attractants + density
m13 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + logPD + pCrowded + 
                quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled)

# attractants + socioecon
m14 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + medHouseholdInc + 
                 pGradDegr + quarter + (1|year) + (1|tract), 
               ziformula = ~logPD, family = nbinom2, data = rcScaled)

# attractants + demographics
m15 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + pOwnerOcc + pUnder5y +
                quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled)

# disturbance + density
m16 <- glmmTMB(ratcomp ~ bPerm + logPD + pCrowded + quarter + (1|year) + 
                 (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)

# disturbance + socioecon
m17 <- glmmTMB(ratcomp ~ bPerm + medHouseholdInc + pGradDegr + quarter + 
                 (1|year) + (1|tract), ziformula = ~logPD, family = nbinom2, 
               data = rcScaled)

# disturbance + demographics
m18 <- glmmTMB(ratcomp ~ bPerm + pOwnerOcc + pUnder5y + quarter + 
                 (1|year) + (1|tract), ziformula = ~logPD, family = nbinom2, 
               data = rcScaled)

# density + socioecon
m19 <- glmmTMB(ratcomp ~ logPD + pCrowded + medHouseholdInc + pGradDegr +
                 quarter + (1|year) + (1|tract), ziformula = ~logPD, 
               family = nbinom2, data = rcScaled)

# density + demographics
m20 <- glmmTMB(ratcomp ~ logPD + pCrowded + pOwnerOcc + pUnder5y +
                quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled)

# socioecon + demographics
m21 <- glmmTMB(ratcomp ~ medHouseholdInc + pGradDegr + pOwnerOcc +
                pUnder5y + quarter + (1|year) + (1|tract), 
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

# null model
m22 <- glmmTMB(ratcomp ~ 1 + (1|year) + (1|tract),
                     ziformula = ~logPD, family = nbinom2, data = rcScaled)


AICtab(globalZINB2, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14,
       m15, m16, m17, m18, m19, m20, m21, m22)

#             dAIC   df
# globalZINB2    0.0 21
# m7            35.4 14
# m13           69.5 14
# m12           82.6 13
# m15           90.3 14
# m2            93.3 12
# m14           95.0 14
# m9           288.8 13
# m8           292.1 12
# m11          304.9 13
# m10          306.1 13
# m1           309.4 11
# m16          314.7 12
# m20          322.3 13
# m4           329.4 11
# m19          330.5 13
# m18          345.7 12
# m17          346.9 12
# m3           347.0 10
# m6           362.0 11
# m5           362.2 11
# m21          362.9 13
# m22         5612.8 6 

bestmod <- globalZINB2

#saveRDS(bestmod, "./ModelOutput/bestmodel.rds")

#saveRDS(bestmod, "./ModelOutput/bestmodel_baited.rds")