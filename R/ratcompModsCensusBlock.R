# analysis of rat complaint predictors as the census block scale

library(bbmle)
library(beepr)
library(effects)
library(fitdistrplus)
library(glmmTMB)

# load and format data----------------------------------------------------------

rc <- read.csv("./Data/ratCompPredsCB.csv", header = TRUE, na.strings = "NA")
rc <- rc[, -1]

rc$year <- as.factor(rc$year)

# full model--------------------------------------------------------------------

hist(rc$ratcomp)
descdist(rc$ratcomp, discrete = TRUE)
# kind of beyond even a ZINB, but we can try

# single zero inflation parameter applying to all observations
ZINB_full <- glmmTMB(ratcomp ~ build + demol + feces + food + garbage + logPop +
                quarter + (1|year), ziformula = ~1, family = nbinom2, data = rc)

# no zeros model----------------------------------------------------------------

noZero <- rc %>% 
  filter(ratcomp > 0)

hist(noZero$ratcomp)
descdist(noZero$ratcomp, discrete = TRUE)
# still highly skewed, so seems appropriate to keep using NB distribution
# will use truncated negative binomial model

# so that the intercept estimate is meaningful, mean-center continuous variables
center <- function(x) {
  scale(x, scale = FALSE)
}

noZeroCent <- noZero %>% 
  mutate_at(c(5:12), center)

# all predictors
# takes about 45 min to run
start_time <- Sys.time()
TNB_full <- glmmTMB(ratcomp ~ build + demol + feces + food + garbage + logPop +
                quarter + (1|year), family = truncated_nbinom2, 
                data = noZeroCent)
end_time <- Sys.time()
end_time - start_time
beep(sound = 2)

# only building and demolition permits
# ~20 min
start_time <- Sys.time()
TNB_constr <- glmmTMB(ratcomp ~ build + demol + (1|year), 
                      family = truncated_nbinom2, data = noZero)
end_time <- Sys.time()
end_time - start_time
beep(sound = 2)

# food and harborage
# ~30 min
# generated a warning message (In sqrt(diag(vcov)) : NaNs produced)
# didn't generate AIC value
start_time <- Sys.time()
TNB_foodHarbor <- glmmTMB(ratcomp ~ feces + food + garbage + (1|year), 
                          family = truncated_nbinom2, data = noZero)
end_time <- Sys.time()
end_time - start_time
beep(sound = 2)

# population size
# ~20 min
start_time <- Sys.time()
TNB_pop <- glmmTMB(ratcomp ~ logPop + (1|year), family = truncated_nbinom2, 
                    data = noZero)
end_time <- Sys.time()
end_time - start_time
beep(sound = 2)

# quarter
# ~30 min
start_time <- Sys.time()
TNB_quart <- glmmTMB(ratcomp ~ quarter + (1|year), family = truncated_nbinom2, 
                    data = noZero)
end_time <- Sys.time()
end_time - start_time
beep(sound = 2)

# null model
# ~ 17 min
start_time <- Sys.time()
TNB_null <- glmmTMB(ratcomp ~ 1 + (1|year), family = truncated_nbinom2, 
                     data = noZero)
end_time <- Sys.time()
end_time - start_time
beep(sound = 2)

# compare models with AIC
AICtab(TNB_full, TNB_constr, TNB_foodHarbor, TNB_pop, TNB_quart, TNB_null)

summary(TNB_full)
plot(allEffects(TNB_full))
plot_model(TNB_full)

library(performance)
check_collinearity(TNB_full)

# could also look at truncated poisson model, just to double check
start_time <- Sys.time()
TP_full <- glmmTMB(ratcomp ~ build + demol + feces + food + garbage + logPop +
                      quarter + (1|year), family = truncated_poisson, 
                   data = noZero)
end_time <- Sys.time()
end_time - start_time
beep(sound = 2)

# yep, negative binomial is definitely the way to go
AICtab(TNB_full, TP_full)


# https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
# to get incident rate ratios, exponentiate the model coefficients
# increasing predictor by 1 unit multiplies the mean value of rat complaints by exp(beta)
exp(TNB_full$fit$par)

library(DHARMa)
res <- simulateResiduals(TNB_full)
plot(res, rank = T)
