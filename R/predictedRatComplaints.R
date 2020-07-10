library(dplyr)
library(ggplot2)
library(sf)
library(sjPlot)
library(gridExtra)  

bestmod <- readRDS("./ModelOutput/bestmodel.rds")
#bestmod <- readRDS("./ModelOutput/bestmodel_baited.rds")

# quarterly rat complaints and quarterly preds----------------------------------

rc <- read.csv("./Data/ratCompPredsCT.csv", header = TRUE) %>% 
  #rc <- read.csv("./Data/ratCompPredsCT_baited.csv", header = TRUE) %>% 
  mutate_at(vars(year), as.factor) %>% 
  mutate(logPD = log(popDens)) %>%  # since there's one really high value
  dplyr::select(-c(totalPop, area, popDens))

# format tract as character, place leading zero if it's only five digits
rc$tract <- format(rc$tract, width = 6)
rc$tract <- gsub(" ", "0", rc$tract)

# plotting rat complaints by quarter and year
rc %>% 
  group_by(year, quarter) %>% 
  summarise(ratcomp = sum(ratcomp)) %>% 
  ggplot(., aes(x = quarter, y = ratcomp, group = year, color = year)) +
  geom_line(size = 2) +
  geom_point(color = "black") +
  theme_bw() -> p1

# garbage complaints
rc %>% 
  group_by(year, quarter) %>% 
  summarise(garbage = sum(garbage)) %>% 
  ggplot(., aes(x = quarter, y = garbage, group = year, color = year)) +
  geom_line(size = 2) +
  geom_point(color = "black") +
  theme_bw() -> p2

# dog feces complaints
rc %>% 
  group_by(year, quarter) %>% 
  summarise(dogFeces = sum(dogFeces)) %>% 
  ggplot(., aes(x = quarter, y = dogFeces, group = year, color = year)) +
  geom_line(size = 2) +
  geom_point(color = "black") +
  theme_bw() -> p3

# construction/demolition permits
rc %>% 
  group_by(year, quarter) %>% 
  summarise(bPerm = sum(bPerm)) %>% 
  ggplot(., aes(x = quarter, y = bPerm, group = year, color = year)) +
  geom_line(size = 2) +
  geom_point(color = "black") +
  theme_bw() -> p4

grid.arrange(p1, p2, p3, p4, nrow = 2)


# plot incident rate ratios-----------------------------------------------------

summary(bestmod)

plot_model(bestmod, show.values = TRUE, value.offset = 0.4, 
           vline.color = "gray") +
  theme_bw() +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

# exponentiated estimates
modelOutput <- summary(bestmod)
exp(modelOutput$coefficients$cond[,1])


# marginal effects--------------------------------------------------------------

# population density
p1 <- plot_model(bestmod, type = "eff", terms = c("logPD")) + 
  theme_bw() +
  xlab("Human population density") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

# restaurants
p2 <- plot_model(bestmod, type = "eff", terms = c("food")) + 
  theme_bw() +
  xlab("# of restaurants") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

# building age
p3 <- plot_model(bestmod, type = "eff", terms = c("percBuilt1970on")) + 
  theme_bw() +
  xlab("% structures built since 1970") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

# garbage complaints
p4 <- plot_model(bestmod, type = "eff", terms = "garbage") + 
  theme_bw() +
  xlab("Garbage complaints") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

grid.arrange(p1, p2, p3, p4, nrow = 2)

# owner occupied
plot_model(bestmod, type = "eff", terms = c("percOwnerOcc")) + 
  theme_bw() +
  xlab("% owner-occupied homes") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

# seasonal predictions map------------------------------------------------------

# pick all quarterly predictors to plot their relationship w/ rat complaints

qpred <- rc %>% 
  group_by(tract, quarter) %>% 
  dplyr::summarise(garbage = mean(garbage),
                   dogFeces = mean(dogFeces),
                   bPerm = mean(bPerm),
                   logPD = mean(logPD))

# center and scale predictors
qpred$garbage <- (qpred$garbage - mean(rc$garbage)) / sd(rc$garbage)
qpred$dogFeces <- (qpred$dogFeces - mean(rc$dogFeces)) / sd(rc$dogFeces)
qpred$bPerm <- (qpred$bPerm - mean(rc$bPerm)) / sd(rc$bPerm)
qpred$logPD <- (qpred$logPD - mean(rc$logPD)) / sd(rc$logPD)

# extract fixed effects estimates
beta.cond <- fixef(bestmod)$cond

# only pick out the estimates we need
beta.cond2 <- beta.cond[c("(Intercept)", "garbage", "dogFeces", "bPerm",
                          "quarterQ2", "quarterQ3", "quarterQ4")] 

# create empty matrix to hold predictor data
X.cond <- matrix(0, ncol = 7, nrow = nrow(qpred))

X.cond[, 1] <- 1
X.cond[, 2] <- qpred$garbage
X.cond[, 3] <- qpred$dogFeces
X.cond[, 4] <- qpred$bPerm
# dummy variables for quarter
X.cond[which(qpred$quarter == "Q2"), 5] <- 1
X.cond[which(qpred$quarter == "Q3"), 6] <- 1
X.cond[which(qpred$quarter == "Q4"), 7] <- 1

# make predictions for conditional model
pred.cond <- X.cond %*% beta.cond2

# now we want to do the bit for the zero-inflation model
# get fixed effect estimates for zero-inflation model
beta.zi <- fixef(bestmod)$zi

# make predictions
pred.zi <- cbind(1, qpred$logPD) %*% beta.zi

# we have generated estimates on the link scale
# logit(prob) and log(cond), not the predictions themselves
# so we transform to the response scale and multiply
pred.ucount <- exp(pred.cond)*(1-plogis(pred.zi))

# add predicted rat complaints back to the garbage, tract, and quarter dataset
qpred$preds <- pred.ucount

# plotting map of predicted rat complaints--------------------------------------

tracts_sf <- st_read("./Data/GIS/chicagoCensusTracts2010.shp")

# want a separate map for each quarter
Q1data <- left_join(tracts_sf, 
                    qpred %>% 
                      filter(quarter == "Q1") %>% 
                      dplyr::select(tract, preds),
                    by = c("tractce10" = "tract"))

Q2data <- left_join(tracts_sf, 
                    qpred %>% 
                      filter(quarter == "Q2") %>% 
                      dplyr::select(tract, preds),
                    by = c("tractce10" = "tract"))

Q3data <- left_join(tracts_sf, 
                    qpred %>% 
                      filter(quarter == "Q3") %>% 
                      dplyr::select(tract, preds),
                    by = c("tractce10" = "tract"))

Q4data <- left_join(tracts_sf, 
                    qpred %>% 
                      filter(quarter == "Q4") %>% 
                      dplyr::select(tract, preds),
                    by = c("tractce10" = "tract"))

# rat complaints
mytheme <- theme_bw() +
  theme(legend.position = c(0.18, 0.2))


p1 <- ggplot() + 
  mytheme +
  geom_sf(data = Q1data, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(qpred$preds)),
                       name = "Predicted\nrat complaints") +
  coord_sf()

p2 <- ggplot() + 
  mytheme +
  geom_sf(data = Q2data, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(qpred$preds)),
                       name = "Predicted\nrat complaints") +
  coord_sf()

p3 <- ggplot() + 
  mytheme +
  geom_sf(data = Q3data, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(qpred$preds)),
                       name = "Predicted\nrat complaints") +
  coord_sf()

p4 <- ggplot() + 
  mytheme +
  geom_sf(data = Q4data, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(qpred$preds)),
                       name = "Predicted\nrat complaints") +
  coord_sf()

# png("./Figures/quarterlyPlots.png", width = 10, height = 10, units = "in", 
#     res = 600)
grid.arrange(p1, p2, p3, p4, nrow = 2)
# dev.off()

# year predictions map----------------------------------------------------------

# pick all yearly predictors to plot their relationship w/ rat complaints

ypred <- rc %>% 
  group_by(tract) %>% 
  dplyr::summarise(food = mean(food),
                   percGradDegr = mean(percGradDegr),
                   medHouseholdInc = mean(medHouseholdInc),
                   percVacantHU = mean(percVacantHU),
                   percOwnerOcc = mean(percOwnerOcc),
                   percBuilt1970on = mean(percBuilt1970on),
                   percCrowded = mean(percCrowded),
                   percUnder5y = mean(percUnder5y),
                   logPD = mean(logPD))

# center and scale predictors
ypred$food <- (ypred$food - mean(rc$food)) / sd(rc$food)
ypred$percGradDegr <- (ypred$percGradDegr - mean(rc$percGradDegr)) / sd(rc$percGradDegr)
ypred$medHouseholdInc <- (ypred$medHouseholdInc - mean(rc$medHouseholdInc)) / sd(rc$medHouseholdInc)
ypred$percVacantHU <- (ypred$percVacantHU - mean(rc$percVacantHU)) / sd(rc$percVacantHU)
ypred$percOwnerOcc <- (ypred$percOwnerOcc - mean(rc$percOwnerOcc)) / sd(rc$percOwnerOcc)
ypred$percBuilt1970on <- (ypred$percBuilt1970on - mean(rc$percBuilt1970on)) / sd(rc$percBuilt1970on)
ypred$percCrowded <- (ypred$percCrowded - mean(rc$percCrowded)) / sd(rc$percCrowded)
ypred$percUnder5y <- (ypred$percUnder5y - mean(rc$percUnder5y)) / sd(rc$percUnder5y)
ypred$logPD <- (ypred$logPD - mean(rc$logPD)) / sd(rc$logPD)

# extract fixed effects estimates
beta.cond <- fixef(bestmod)$cond

# only pick out the estimates we need
beta.cond2 <- beta.cond[c("(Intercept)", "food", "percGradDegr",
                          "medHouseholdInc", "percVacantHU", "percOwnerOcc",
                          "percBuilt1970on", "percCrowded", "percUnder5y",
                          "logPD")] 

# create empty matrix to hold predictor data
X.cond <- matrix(0, ncol = 10, nrow = nrow(ypred))

X.cond[, 1] <- 1
X.cond[, 2] <- ypred$food
X.cond[, 3] <- ypred$percGradDegr
X.cond[, 4] <- ypred$medHouseholdInc
X.cond[, 5] <- ypred$percVacantHU
X.cond[, 6] <- ypred$percOwnerOcc
X.cond[, 7] <- ypred$percBuilt1970on
X.cond[, 8] <- ypred$percCrowded
X.cond[, 9] <- ypred$percUnder5y
X.cond[, 10] <- ypred$logPD

# make predictions for conditional model
pred.cond <- X.cond %*% beta.cond2

# now we want to do the bit for the zero-inflation model
# get fixed effect estimates for zero-inflation model
beta.zi <- fixef(bestmod)$zi

# make predictions
pred.zi <- cbind(1, ypred$logPD) %*% beta.zi

# we have generated estimates on the link scale
# logit(prob) and log(cond), not the predictions themselves
# so we transform to the response scale and multiply
pred.ucount <- exp(pred.cond)*(1-plogis(pred.zi))

# add predicted rat complaints back to the garbage, tract, and quarter dataset
ypred$preds <- pred.ucount

# plotting map of predicted rat complaints--------------------------------------

tracts_sf <- st_read("./Data/GIS/chicagoCensusTracts2010.shp")

# want a separate map for each quarter
ydata <- left_join(tracts_sf, 
                   ypred %>% 
                     dplyr::select(tract, preds),
                   by = c("tractce10" = "tract"))

# rat complaints
mytheme <- theme_bw() +
  theme(legend.position = c(0.18, 0.2))


ggplot() + 
  mytheme +
  geom_sf(data = ydata, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(ypred$preds)),
                       name = "Predicted\nrat complaints") +
  coord_sf()

# something weird going on with tract 839100
# this tract has 3x the restaurants as the tract w/next highest
# is this true characteristic?? doesn't seem like data processing error

# plot with artificial maximum
ggplot() + 
  mytheme +
  geom_sf(data = ydata, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, 9),
                       name = "Predicted\nrat complaints") +
  coord_sf()

# png("./Figures/yearPlot.png", width = 10, height = 10, units = "in", 
#     res = 600)
# grid.arrange(p1, p2, p3, p4, nrow = 2)
# dev.off()

# plotting map of random effects------------------------------------------------
tracts_sf <- st_read("./Data/GIS/chicagoCensusTracts2010.shp")

tractEff <- ranef(bestmod)$cond$tract
tractEff$tract <- rownames(tractEff)
rownames(tractEff) <- NULL
names(tractEff)[1] <- "effect"


tracts_sf <- left_join(tracts_sf, tractEff, by = c("tractce10" = "tract"))

ggplot() + 
  mytheme +
  geom_sf(data = tracts_sf, aes(fill = effect), color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0) +
  coord_sf()


