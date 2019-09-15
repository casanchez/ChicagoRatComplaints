# generate predictions across the city

# model that we want to make predictions from
globalZINB2 <- glmmTMB(ratcomp ~ construction + demolition + feces + food + 
                         garbage + medIncome + percentOcc + logPD +
                         quarter + (1|year) + (1|tract), ziformula = ~logPD, 
                       family = nbinom2, data = rcScaled)

# let's pick one predictor (garbage) to plot its relationhip w/ rat complaints
# because there was such a difference with quarters, we want quarterly data

garbageTQ <- rc %>% 
  group_by(tract, quarter) %>% 
  summarise(garbage = mean(garbage))

# center and scale garbage
garbageTQ$garbageSc <- (garbageTQ$garbage - mean(rc$garbage)) / sd(rc$garbage)

# extract fixed effects estimates
beta.cond <- fixef(globalZINB2)$cond

# only pick out the estimates we need
beta.cond2 <- beta.cond[c("(Intercept)", "garbage", "quarterQ2", "quarterQ3", "quarterQ4")] 

# create empty matrix to hold predictor data
X.cond <- matrix(0, ncol = 5, nrow = nrow(garbageTQ))

X.cond[, 1] <- 1
X.cond[, 2] <- garbageTQ$garbage
# dummy variables for quarter
X.cond[which(garbageTQ$quarter == "Q2"), 3] <- 1
X.cond[which(garbageTQ$quarter == "Q3"), 4] <- 1
X.cond[which(garbageTQ$quarter == "Q4"), 5] <- 1

# make predictions for conditional model
pred.cond <- X.cond %*% beta.cond2

# now we want to do the bit for the zero-inflation model
# population density was the only predictor there
garbageTQ <- left_join(garbageTQ, distinct(rcScaled[,c("tract", "logPD")]), 
                       by = "tract")

# get fixed effect estimates for zero-inflation model
beta.zi <- fixef(globalZINB2)$zi

# make predictions
pred.zi <- cbind(1, garbageTQ$logPD) %*% beta.zi

# we have generated estimates on the link scale
# logit(prob) and log(cond), not the predictions themselves
# so we transform to the response scale and multiply
pred.ucount <- exp(pred.cond)*(1-plogis(pred.zi))

# add predicted rat complaints back to the garbage, tract, and quarter dataset
garbageTQ$preds <- pred.ucount

# plotting map of predicted rat complaints--------------------------------------

tracts_sf <- st_read("./Data/GIS/chicagoCensusTracts2010.shp")

# want a separate map for each quarter
Q1data <- left_join(tracts_sf, 
                  garbageTQ %>% 
                    filter(quarter == "Q1") %>% 
                    dplyr::select(tract, preds),
                  by = c("tractce10" = "tract"))

Q2data <- left_join(tracts_sf, 
                    garbageTQ %>% 
                      filter(quarter == "Q2") %>% 
                      dplyr::select(tract, preds),
                    by = c("tractce10" = "tract"))

Q3data <- left_join(tracts_sf, 
                  garbageTQ %>% 
                    filter(quarter == "Q3") %>% 
                    dplyr::select(tract, preds),
                  by = c("tractce10" = "tract"))

Q4data <- left_join(tracts_sf, 
                    garbageTQ %>% 
                      filter(quarter == "Q4") %>% 
                      dplyr::select(tract, preds),
                    by = c("tractce10" = "tract"))

# rat complaints
p1 <- ggplot() + 
  theme_bw() +
  geom_sf(data = Q1data, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(garbageTQ$preds)),
                       name = "Predicted\nrat complaints") +
  theme(legend.position = c(0.2, 0.2)) +
  coord_sf()

p2 <- ggplot() + 
  theme_bw() +
  geom_sf(data = Q2data, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(garbageTQ$preds)),
                       name = "Predicted\nrat complaints") +
  theme(legend.position = c(0.2, 0.2)) +
  coord_sf()

p3 <- ggplot() + 
  theme_bw() +
  geom_sf(data = Q3data, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(garbageTQ$preds)),
                       name = "Predicted\nrat complaints") +
  theme(legend.position = c(0.2, 0.2)) +
  coord_sf()

p4 <- ggplot() + 
  theme_bw() +
  geom_sf(data = Q4data, aes(fill = preds), color = "black") +
  scale_fill_viridis_c(limits = c(0, max(garbageTQ$preds)),
                       name = "Predicted\nrat complaints") +
  theme(legend.position = c(0.2, 0.2)) +
  coord_sf()
  
library(gridExtra)  
  
grid.arrange(p1, p2, p3, p4, nrow = 2)
