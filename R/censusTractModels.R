# models to predict rat complaints from different spatial predictors

# libraries---------------------------------------------------------------------
library(bbmle)
library(beepr)
library(effects)
library(fitdistrplus)
library(glmmTMB)
library(sjPlot)

# load data---------------------------------------------------------------------
rc <- read.csv("./Data/ratCompPredsCT.csv", header = TRUE)
rc <- rc[, -1]

rc$year <- as.factor(rc$year)

# format tract as character, place leading zero if it's only five digits
rc$tract <- format(rc$tract, width = 6)
rc$tract <- gsub(" ", "0", rc$tract)

# calculate population density
rc$popDens <- rc$pop / rc$area
# log transform since there's one really high value of population density
rc$logPD <- log(rc$popDens)

# models------------------------------------------------------------------------

# examine correlations between predictors
library(corrplot)
corrplot(cor(rc[, 5:16]), method = "number")

hist(rc$ratcomp)
descdist(rc$ratcomp, discrete = TRUE)


# center and scale the predictors
cs <- function(x) {
  scale(x, center = TRUE, scale = TRUE)
}

rcScaled <- rc %>% 
  mutate_at(c(5:16), cs)

# set up models
Sys.time()
globalZINB <- glmmTMB(ratcomp ~ construction + demolition + feces + food + 
                        garbage + medIncome + percentOcc + logPD +
                        quarter + (1|year) + (1|tract), ziformula = ~logPD, 
                      family = nbinom1, data = rcScaled)
Sys.time()
beep(sound=2)


Sys.time()
globalZINB2 <- glmmTMB(ratcomp ~ construction + demolition + feces + food + 
                         garbage + medIncome + percentOcc + logPD +
                         quarter + (1|year) + (1|tract), ziformula = ~logPD, 
                       family = nbinom2, data = rcScaled)
Sys.time()
beep(sound=2)
AICtab(globalZINB, globalZINB2)

# okay, so nbinom2 is best
# have to compare global against all other candidate models

m1 <- glmmTMB(ratcomp ~ construction + demolition + (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

m2 <- glmmTMB(ratcomp ~ feces + food + garbage + (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

m3 <- glmmTMB(ratcomp ~ logPD + (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

m4 <- glmmTMB(ratcomp ~ medIncome + percentOcc + (1|year) + (1|tract),
              ziformula = ~logPD, family = nbinom2, data = rcScaled)

m5 <- glmmTMB(ratcomp ~ quarter + (1|year) + (1|tract),
                      ziformula = ~logPD, family = nbinom2, data = rcScaled)

m6 <- glmmTMB(ratcomp ~ 1 + (1|year) + (1|tract),
                     ziformula = ~logPD, family = nbinom2, data = rcScaled)

AICtab(globalZINB2, m1, m2, m3, m4, m5, m6)

#             dAIC   df
# globalZINB2    0.0 17
# m5           384.5 9 
# m2          5008.9 9 
# m1          5419.1 8 
# m4          5488.4 8 
# m3          5510.3 7 
# m6          5547.2 6 

summary(globalZINB2)

# plot incident rate ratios
plot_model(globalZINB2, show.values = TRUE, value.offset = 0.4, vline.color = "gray") +
  theme_bw()


# plot marginal effects
plot_model(globalZINB2, type = "eff", terms = "construction", show.data = TRUE) + 
  theme_bw() +
  xlab("Building permits issued") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

plot_model(globalZINB2, type = "eff", terms = "demolition", show.data = TRUE)

plot_model(globalZINB2, type = "eff", terms = "feces", show.data = TRUE) + 
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

plot_model(globalZINB2, type = "eff", terms = "pop", show.data = TRUE)

plot_model(globalZINB2, type = "eff", terms = "quarter", show.data = TRUE)
