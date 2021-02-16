# Code to accompany Sanchez et al. 2021. Social and environmental correlates
# of rat complaints in Chicago. Journal of Urban Ecology. doi: 10.1093/jue/juab006

# This script runs various candidate models to predict the number of rat 
# complaints based on different combinations of environmental and social variables

rm(list = ls())

# load packages-----------------------------------------------------------------
library(bbmle)
library(beepr)
library(corrplot)
library(fitdistrplus)
library(glmmTMB)
library(performance)
library(tidyverse)

# load data---------------------------------------------------------------------
rc <- read.csv("./DataCleaned/dataformodels.csv", header = TRUE) %>% 
  mutate_at(vars(year), as.factor) %>% 
  mutate(logPD = log(popDens)) %>%  # since there's one really high value
  dplyr::select(-c(medHouseholdInc, totalPop, area, popDens)) %>% 
  # format tract as character, place leading zero if it's only five digits
  mutate(tract = format(tract, width = 6)) %>% 
  mutate(tract = gsub(" ", "0", tract))

# summary info------------------------------------------------------------------

sum(rc$ratcomp) # total rat complaints
summary(rc$ratcomp) # median, IQR, range

sum(rc$garbage) # total garbage complaints
sum(rc$dogFeces) # total dog feces complaints
sum(rc$bPerm) # total construction/demolition permits issued

# models------------------------------------------------------------------------

# examine correlations between predictors
corrplot(cor(rc[, c(5:16)]), method = "number", number.cex = 0.7)
# pGradDegree and adjHhIncome have correlation of 0.7
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

# global model with quasi-Poisson distribution
global_nb1 <- glmmTMB(ratcomp ~ quarter + bPerm + dogFeces + garbage + food +
                        pGradDegr + adjHhInc + pVacantHU + pOwnerOcc + 
                        pBuiltPre1950 + pOvercrowded + pUnder5y + logPD + 
                        (1|year) + (1|tract), ziformula = ~1, family = nbinom1, 
                      data = rcScaled)

# global model with negative binomial distribution
global_nb2 <- glmmTMB(ratcomp ~ quarter + bPerm + dogFeces + garbage + food +
                        pGradDegr + adjHhInc + pVacantHU + pOwnerOcc +
                        pBuiltPre1950 + pOvercrowded + pUnder5y + logPD + 
                        (1|year) + (1|tract), ziformula = ~1, family = nbinom2, 
                      data = rcScaled)

AICtab(global_nb1, global_nb2)
#             dAIC    df
# global_nb2     0.0 20
# global_nb1    823.8 20

# okay, so nbinom2 is best

# check multicollinearity among variables
check_collinearity(global_nb2, component = "count")
# all VIFs < 4


# now compare global model against other candidate models

# harborage: building age and vacant units
m1 <- glmmTMB(ratcomp ~ pBuiltPre1950 + pVacantHU + (1|year) + (1|tract), 
              ziformula = ~1, family = nbinom2, data = rcScaled)

# attractants: dog feces, garbage, restaurants
m2 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + (1|year) + (1|tract),
              ziformula = ~1, family = nbinom2, data = rcScaled)

# disturbance: construction/demolition permits
m3 <- glmmTMB(ratcomp ~ bPerm + (1|year) + (1|tract),
              ziformula = ~1, family = nbinom2, data = rcScaled)
  
# human density: population density, overcrowding  
m4 <- glmmTMB(ratcomp ~ logPD + pOvercrowded + (1|year) + (1|tract),
              ziformula = ~1, family = nbinom2, data = rcScaled)

# socioeconomics: household income, % graduate degree
m5 <- glmmTMB(ratcomp ~ adjHhInc + pGradDegr + (1|year) + (1|tract), 
              ziformula = ~1, family = nbinom2, data = rcScaled)

# demographics: % owner occupied, % under 5
m6 <- glmmTMB(ratcomp ~ pOwnerOcc + pUnder5y + (1|year) + (1|tract), 
              ziformula = ~1, family = nbinom2, data = rcScaled)

# season: quarter
m7 <- glmmTMB(ratcomp ~ quarter + (1|year) + (1|tract), 
              ziformula = ~1, family = nbinom2, data = rcScaled)

# all environmental variables (season, harborage, attractants, disturbance)
m8 <- glmmTMB(ratcomp ~ quarter + pBuiltPre1950 + pVacantHU + dogFeces + 
                garbage + food + bPerm + (1|year) + (1|tract), 
              ziformula = ~1, family = nbinom2, data = rcScaled)

# all social variables (human density, socioeconomics, demographics)
m9 <- glmmTMB(ratcomp ~ logPD + pOvercrowded + adjHhInc + pGradDegr + 
                pOwnerOcc + pUnder5y + (1|year) + (1|tract),
              ziformula = ~1, family = nbinom2, data = rcScaled)

AICtab(global_nb2, m1, m2, m3, m4, m5, m6, m7, m8, m9)

#            dAIC   df
# global_nb2    0.0 20
# m8           24.7 14
# m7          356.3 8 
# m2         4696.4 8 
# m3         5091.4 6 
# m1         5133.3 7 
# m9         5172.2 11
# m4         5174.8 7 
# m6         5212.6 7 
# m5         5213.7 7 

# save all models by modifying this format
#saveRDS(global_nb2, "./ModelOutput/global_nb2.rds")

# We don't have specific hypotheses about which variables would be most important
#   to include in the zi-portion
# To generate some hypotheses for what processes could be generating false zeros,
# we will put all variables into zi-portion
finalMod <- glmmTMB(ratcomp ~ quarter + bPerm + dogFeces + garbage + food + 
                 pGradDegr + adjHhInc + pVacantHU + pOwnerOcc + pBuiltPre1950 + 
                 pOvercrowded + pUnder5y + logPD + (1|year) + (1|tract),
               ziformula = ~quarter + bPerm + dogFeces + garbage + food + 
                 pGradDegr + adjHhInc + pVacantHU + pOwnerOcc + pBuiltPre1950 + 
                 pOvercrowded + pUnder5y + logPD, 
               family = nbinom2, data = rcScaled)

#saveRDS(finalMod, "./ModelOutput/finalModel.rds")

summary(finalMod)

# calculate exponentiated estimates for tables
modelOutput <- summary(finalMod)
round(exp(modelOutput$coefficients$cond[, "Estimate"]), 2) # IRR
round(exp(modelOutput$coefficients$cond[, "Estimate"] - 1.96*modelOutput$coefficients$cond[,"Std. Error"]), 2)  # lower CI
round(exp(modelOutput$coefficients$cond[, "Estimate"] + 1.96*modelOutput$coefficients$cond[,"Std. Error"]), 2)  # upper CI

round(exp(modelOutput$coefficients$zi[, "Estimate"]), 2) # OR
round(exp(modelOutput$coefficients$zi[, "Estimate"] - 1.96*modelOutput$coefficients$zi[,"Std. Error"]), 2)  # lower CI
round(exp(modelOutput$coefficients$zi[, "Estimate"] + 1.96*modelOutput$coefficients$zi[,"Std. Error"]), 2)  # upper CI
