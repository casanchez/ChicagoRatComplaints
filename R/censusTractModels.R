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
# percGradDegree and medHouseholdIncome have correlation of 0.7
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
                        percGradDegr + medHouseholdInc + percVacantHU +
                        percOwnerOcc + percBuilt1970on + percCrowded +
                        percUnder5y + logPD + (1|year) + (1|tract), 
                      ziformula = ~logPD, family = nbinom1, data = rcScaled)
Sys.time()
beep(sound=2)

Sys.time()
# this one takes about 11 min
globalZINB2 <- glmmTMB(ratcomp ~ quarter + bPerm + dogFeces + garbage + food +
                         percGradDegr + medHouseholdInc + percVacantHU +
                         percOwnerOcc + percBuilt1970on + percCrowded +
                         percUnder5y + logPD + (1|year) + (1|tract),
                       ziformula = ~logPD, family = nbinom2, data = rcScaled)
Sys.time()
beep(sound=2)

AICtab(globalZINB, globalZINB2)
#             dAIC    df
# globalZINB2     0.0 21
# globalZINB    835.7 21

# okay, so nbinom2 is best

# check multicollinearity
library(performance)
check_collinearity(globalZINB2, component = "count")
# all VIFs < 4


# now compare global model against other candidate models

# harborage: building age and vacant units
m1 <- glmmTMB(ratcomp ~ percBuilt1970on + percVacantHU + quarter + (1|year) + 
                (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)

# attractants: dog feces, garbage, restaurants
m2 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + quarter + (1|year) + 
                (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)

# disturbance: construction/demolition permits
m3 <- glmmTMB(ratcomp ~ bPerm + quarter + (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)
  
# human density: population density, overcrowding  
m4 <- glmmTMB(ratcomp ~ logPD + percCrowded + quarter + (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

# socioeconomics: household income, owner occupied
m5 <- glmmTMB(ratcomp ~ medHouseholdInc + percOwnerOcc + quarter + (1|year) + 
                (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)

# demographics: % graduate degree, % under 5
m6 <- glmmTMB(ratcomp ~ percGradDegr + percUnder5y + quarter + (1|year) + 
                (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)


# harborage + attractants
m7 <- glmmTMB(ratcomp ~ percBuilt1970on + percVacantHU + dogFeces + garbage +
                food + quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled)

# harborage + disturbance
m8 <- glmmTMB(ratcomp ~ percBuilt1970on + percVacantHU + bPerm + quarter + 
                (1|year) + (1|tract), ziformula = ~logPD, family = nbinom2, 
              data = rcScaled)

# harborage + density
m9 <- glmmTMB(ratcomp ~ percBuilt1970on + percVacantHU + logPD + percCrowded +
                quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled) 

# harborage + socioecon
m10 <- glmmTMB(ratcomp ~ percBuilt1970on + percVacantHU + medHouseholdInc +
                 percOwnerOcc + quarter + (1|year) + (1|tract),
               ziformula = ~logPD, family = nbinom2, data = rcScaled)

# harborage + demographics
m11 <- glmmTMB(ratcomp ~ percBuilt1970on + percVacantHU + percGradDegr + 
                 percUnder5y + quarter + (1|year) + (1|tract), 
               ziformula = ~logPD, family = nbinom2, data = rcScaled)

# attractants + disturbance
m12 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + bPerm + quarter + 
                 (1|year) + (1|tract), ziformula = ~logPD, family = nbinom2, 
               data = rcScaled)

# attractants + density
m13 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + logPD + percCrowded + 
                quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled)

# attractants + socioecon
m14 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + medHouseholdInc + 
                 percOwnerOcc + quarter + (1|year) + (1|tract), 
               ziformula = ~logPD, family = nbinom2, data = rcScaled)

# attractants + demographics
m15 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + percGradDegr + percUnder5y +
                quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled)

# disturbance + density
m16 <- glmmTMB(ratcomp ~ bPerm + logPD + percCrowded + quarter + (1|year) + 
                 (1|tract), ziformula = ~logPD, family = nbinom2, data = rcScaled)

# disturbance + socioecon
m17 <- glmmTMB(ratcomp ~ bPerm + medHouseholdInc + percOwnerOcc + quarter + 
                 (1|year) + (1|tract), ziformula = ~logPD, family = nbinom2, 
               data = rcScaled)

# disturbance + demographics
m18 <- glmmTMB(ratcomp ~ bPerm + percGradDegr + percUnder5y + quarter + 
                 (1|year) + (1|tract), ziformula = ~logPD, family = nbinom2, 
               data = rcScaled)

# density + socioecon
m19 <- glmmTMB(ratcomp ~ logPD + percCrowded + medHouseholdInc + percOwnerOcc +
                 quarter + (1|year) + (1|tract), ziformula = ~logPD, 
               family = nbinom2, data = rcScaled)

# density + demographics
m20 <- glmmTMB(ratcomp ~ logPD + percCrowded + percGradDegr + percOwnerOcc +
                quarter + (1|year) + (1|tract), ziformula = ~logPD, 
              family = nbinom2, data = rcScaled)

# socioecon + demographics
m21 <- glmmTMB(ratcomp ~ medHouseholdInc + percOwnerOcc + percGradDegr +
                percOwnerOcc + quarter + (1|year) + (1|tract), 
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

# null model
m22 <- glmmTMB(ratcomp ~ 1 + (1|year) + (1|tract),
                     ziformula = ~logPD, family = nbinom2, data = rcScaled)


AICtab(globalZINB2, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, m14,
       m15, m16, m17, m18, m19, m20, m21, m22)

#             dAIC   df
# globalZINB2    0.0 21
# m7            34.3 14
# m13           51.9 14
# m12           64.9 13
# m14           74.6 14
# m2            75.7 12
# m15           76.3 14
# m9           286.5 13
# m8           296.6 12
# m16          297.1 12
# m20          307.2 13
# m19          308.3 13
# m10          311.1 13
# m4           311.8 11
# m1           314.5 11
# m11          315.5 13
# m17          328.3 12
# m3           329.4 10
# m18          331.1 12
# m5           343.8 11
# m21          345.3 12
# m6           347.2 11
# m22         5595.1 6 


bestmod <- globalZINB2

#saveRDS(bestmod, "./ModelOutput/bestmodel.rds")

#saveRDS(bestmod, "./ModelOutput/bestmodel_baited.rds")