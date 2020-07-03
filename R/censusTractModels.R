# models to predict rat complaints from different spatial predictors

# libraries---------------------------------------------------------------------
library(bbmle)
library(beepr)
library(corrplot)
library(fitdistrplus)
library(glmmTMB)
library(sjPlot)
library(tidyverse)

# load data---------------------------------------------------------------------
rc <- read.csv("./Data/ratCompPredsCT.csv", header = TRUE) %>% 
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
# percCrowding and percOwnerOcc have correlation of 0.75
# might need to drop some variables

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
                        percOwnerOcc + percBuilt1970on + percCrowding +
                        percUnder5y + logPD +
                        (1|year) + (1|tract), ziformula = ~logPD, 
                      family = nbinom1, data = rcScaled)
Sys.time()
beep(sound=2)


Sys.time()
# this one takes about 11 min
globalZINB2 <- glmmTMB(ratcomp ~ quarter + bPerm + dogFeces + garbage + food +
                         percGradDegr + medHouseholdInc + percVacantHU +
                         percOwnerOcc + percBuilt1970on + percCrowding +
                         percUnder5y + logPD +
                         (1|year) + (1|tract), ziformula = ~logPD, 
                       family = nbinom2, data = rcScaled)
Sys.time()
beep(sound=2)

AICtab(globalZINB, globalZINB2)
#             dAIC    df
# globalZINB2     0.0 21
# globalZINB    847.7 21

# okay, so nbinom2 is best

# check multicollinearity
library(performance)
check_collinearity(globalZINB2, component = "count")
# highest VIF is 1.21


# now compare global model against other candidate models

# harborage: building age and vacant units
m1 <- glmmTMB(ratcomp ~ percBuilt1970on + percVacantHU + quarter + 
                (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

# attractants: dog feces, garbage, restaurants
m2 <- glmmTMB(ratcomp ~ dogFeces + garbage + food + quarter + 
                (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

# disturbance: construction/demolition permits
m3 <- glmmTMB(ratcomp ~ bPerm + quarter +
                (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)
  
# human density: population density, overcrowding  
m4 <- glmmTMB(ratcomp ~ logPD + percCrowding + quarter +
                (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

# socioeconomics: household income, owner occupied
m5 <- glmmTMB(ratcomp ~ medHouseholdInc + percOwnerOcc + quarter + 
                (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

# demographics: household income, owner occupied
m6 <- glmmTMB(ratcomp ~ percGradDegr + percUnder5y + quarter + 
                  (1|year) + (1|tract),
                ziformula = ~logPD, family = nbinom2, data = rcScaled)

# null model
m7 <- glmmTMB(ratcomp ~ 1 + (1|year) + (1|tract),
                     ziformula = ~logPD, family = nbinom2, data = rcScaled)

AICtab(globalZINB2, m1, m2, m3, m4, m5, m6, m7)

#             dAIC   df
# globalZINB2    0.0 17
# m5           352.7 9 
# m2          4978.4 9 
# m4          5460.2 8 
# m3          5488.6 7 
# m6          5515.3 6 
# m1              NA 8 

modelOutput <- summary(globalZINB2)

# plot incident rate ratios
plot_model(globalZINB2, show.values = TRUE, value.offset = 0.4, vline.color = "gray") +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

exp(modelOutput$coefficients$cond[,1])


# plot marginal effects
plot_model(globalZINB2, type = "pred", terms = c("percentOcc", "quarter")) + 
  theme_bw() +
  xlab("% owner-occupied homes") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))


plot_model(globalZINB2, type = "eff", terms = "demolition", show.data = TRUE)

plot_model(globalZINB2, type = "eff", terms = "dogFeces", show.data = TRUE) + 
  theme_bw() +
  xlab("Dog feces complaints") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

plot_model(globalZINB2, type = "eff", terms = "food", show.data = TRUE) + 
  theme_bw() +
  xlab("Food establishments") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

plot_model(globalZINB2, type = "eff", terms = "garbage", show.data = TRUE) + 
  theme_bw() +
  xlab("Garbage complaints") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

plot_model(globalZINB2, type = "eff", terms = "logPD", show.data = TRUE)

plot_model(globalZINB2, type = "eff", terms = "quarter", show.data = TRUE)
