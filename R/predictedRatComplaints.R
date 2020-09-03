library(dplyr)
library(ggplot2)
library(sf)
library(sjPlot)
library(gridExtra)  

bestmod <- readRDS("./ModelOutput/bestmodel.rds")
#bestmod <- readRDS("./ModelOutput/bestmodel_baited.rds")

rc <- read.csv("./DataCleaned/ratCompPredsCT.csv", header = TRUE) %>% 
  #rc <- read.csv("./Data/ratCompPredsCT_baited.csv", header = TRUE) %>% 
  mutate_at(vars(year), as.factor) %>% 
  mutate(logPD = log(popDens)) %>%  # since there's one really high value
  dplyr::select(-c(totalPop, area, popDens))

# format tract as character, place leading zero if it's only five digits
rc$tract <- format(rc$tract, width = 6)
rc$tract <- gsub(" ", "0", rc$tract)


# center and scale the predictors
cs <- function(x) {
  scale(x, center = TRUE, scale = TRUE)
}
rcScaled <- rc %>% 
  mutate_at(c(5:16), cs)

# FIGURE 1: quarterly rat complaints and quarterly preds------------------------

myblues <- RColorBrewer::brewer.pal(9, "Blues")[3:9]

#tiff("./Figures/Fig1.tiff", height = 5, width = 7, units = "in", res = 600)

# plotting rat complaints by quarter and year
rc %>% 
  group_by(year, quarter) %>% 
  summarise(ratcomp = sum(ratcomp)) %>% 
  ggplot(., aes(x = quarter, y = ratcomp, group = year, color = year)) +
  scale_color_manual(values = myblues) +
  guides(color = guide_legend(title = "Year")) +
  geom_line(size = 1.5) +
  geom_point(color = "black") +
  ylab("Rat complaints") + 
  theme_bw() -> p1

# garbage complaints
rc %>% 
  group_by(year, quarter) %>% 
  summarise(garbage = sum(garbage)) %>% 
  ggplot(., aes(x = quarter, y = garbage, group = year, color = year)) +
  scale_color_manual(values = myblues) +
  guides(color = guide_legend(title = "Year")) +
  geom_line(size = 1.5) +
  geom_point(color = "black") +
  ylab("Garbage complaints") + 
  theme_bw() -> p2

# dog feces complaints
rc %>% 
  group_by(year, quarter) %>% 
  summarise(dogFeces = sum(dogFeces)) %>% 
  ggplot(., aes(x = quarter, y = dogFeces, group = year, color = year)) +
  scale_color_manual(values = myblues) +
  guides(color = guide_legend(title = "Year")) +
  geom_line(size = 1.5) +
  geom_point(color = "black") +
  ylab("Dog feces complaints") + 
  theme_bw() -> p3

# construction/demolition permits
rc %>% 
  group_by(year, quarter) %>% 
  summarise(bPerm = sum(bPerm)) %>% 
  ggplot(., aes(x = quarter, y = bPerm, group = year, color = year)) +
  scale_color_manual(values = myblues) +
  guides(color = guide_legend(title = "Year")) +
  geom_line(size = 1.5) +
  geom_point(color = "black") +
  ylab("Construction and\n demolition permits issued") + 
  theme_bw() -> p4

grid.arrange(p1, p2, p3, p4, nrow = 2)
#dev.off()

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
exp(modelOutput$coefficients$cond[, "Estimate"]) # IRR
exp(modelOutput$coefficients$cond[, "Estimate"] - 1.96*modelOutput$coefficients$cond[,"Std. Error"])  # lower CI
exp(modelOutput$coefficients$cond[, "Estimate"] + 1.96*modelOutput$coefficients$cond[,"Std. Error"])  # upper CI

# plot RE coefs
plot_model(bestmod, type = "re")

test <- ranef(bestmod)[2]

# FIGURE 2: marginal effects----------------------------------------------------

#tiff("./Figures/Fig2.tiff", height = 5, width = 7, units = "in", res = 600)

mytheme <- theme_bw() +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "black"))

# population density
p1 <- plot_model(bestmod, type = "eff", terms = c("logPD", "quarter"), 
                 colors = "BrBG", line.size = 2) + 
  xlab("Human population density") +
  ylab("Rat complaints") +
  mytheme


# restaurants
p2 <- plot_model(bestmod, type = "eff", terms = c("food", "quarter"),
                 colors = "BrBG", line.size = 2) + 
  xlab("# of restaurants") +
  ylab("Rat complaints") +
  mytheme

# building age
p3 <- plot_model(bestmod, type = "eff", terms = c("pBuiltPre1950", "quarter"),
                 colors = "BrBG", line.size = 2) + 
  xlab("% structures built before 1950") +
  ylab("Rat complaints") +
  mytheme

# garbage complaints
p4 <- plot_model(bestmod, type = "eff", terms = c("garbage", "quarter"),
                 colors = "BrBG", line.size = 2) + 
  xlab("Garbage complaints") +
  ylab("Rat complaints") +
  mytheme


grid.arrange(p1, p2, p3, p4, nrow = 2)

#dev.off()

# owner occupied
# plot_model(bestmod, type = "eff", terms = c("pOwnerOcc")) + 
#   theme_bw() +
#   xlab("% owner-occupied homes") +
#   ylab("Rat complaints") +
#   theme(plot.title = element_blank(),
#         axis.title = element_text(size = 18),
#         axis.text = element_text(size = 16, color = "black"))

# predicted # of rat complaints by quarter--------------------------------------

# calculate avg rat complaints in a tract each quarter
qpred <- rc %>% 
  group_by(tract, quarter) %>% 
  dplyr::summarise_at(vars(-year), mean)

# center and scale predictors
# have to apply the same transformation as was done to the original data
# ie need the mean and sd of the rc dataset
qpred$garbage <- (qpred$garbage - mean(rc$garbage)) / sd(rc$garbage)
qpred$dogFeces <- (qpred$dogFeces - mean(rc$dogFeces)) / sd(rc$dogFeces)
qpred$bPerm <- (qpred$bPerm - mean(rc$bPerm)) / sd(rc$bPerm)
qpred$food <- (qpred$food - mean(rc$food)) / sd(rc$food)
qpred$pGradDegr <- (qpred$pGradDegr - mean(rc$pGradDegr)) / sd(rc$pGradDegr)
qpred$medHouseholdInc <- (qpred$medHouseholdInc - mean(rc$medHouseholdInc)) / sd(rc$medHouseholdInc)
qpred$pVacantHU <- (qpred$pVacantHU - mean(rc$pVacantHU)) / sd(rc$pVacantHU)
qpred$pOwnerOcc <- (qpred$pOwnerOcc - mean(rc$pOwnerOcc)) / sd(rc$pOwnerOcc)
qpred$pBuiltPre1950 <- (qpred$pBuiltPre1950 - mean(rc$pBuiltPre1950)) / sd(rc$pBuiltPre1950)
qpred$pCrowded <- (qpred$pCrowded - mean(rc$pCrowded)) / sd(rc$pCrowded)
qpred$pUnder5y <- (qpred$pUnder5y - mean(rc$pUnder5y)) / sd(rc$pUnder5y)
qpred$logPD <- (qpred$logPD - mean(rc$logPD)) / sd(rc$logPD)


# This part is based on Brooks et al. 2017
# "glmmTMB balances speed and flexibility among packages for zero-inflated
# generalized linear mixed modeling"
# extract fixed effects estimates for the conditional model
beta.cond <- fixef(bestmod)$cond

# create empty matrix to hold predictor data
# important that the order of the columns is the same as beta.cond
X.cond <- matrix(0, ncol = 16, nrow = nrow(qpred))

X.cond[, 1] <- 1 # intercept
X.cond[which(qpred$quarter == "Q2"), 2] <- 1 # dummy variables for quarter
X.cond[which(qpred$quarter == "Q3"), 3] <- 1 # dummy variables for quarter
X.cond[which(qpred$quarter == "Q4"), 4] <- 1 # dummy variables for quarter
X.cond[, 5] <- qpred$bPerm
X.cond[, 6] <- qpred$dogFeces
X.cond[, 7] <- qpred$garbage
X.cond[, 8] <- qpred$food
X.cond[, 9] <- qpred$pGradDegr
X.cond[, 10] <- qpred$medHouseholdInc
X.cond[, 11] <- qpred$pVacantHU
X.cond[, 12] <- qpred$pOwnerOcc
X.cond[, 13] <- qpred$pBuiltPre1950
X.cond[, 14] <- qpred$pCrowded
X.cond[, 15] <- qpred$pUnder5y
X.cond[, 16] <- qpred$logPD

# make predictions for conditional model
pred.cond <- X.cond %*% beta.cond

# now we want to do the bit for the zero-inflation model
# get fixed effect estimates for zero-inflation model
beta.zi <- fixef(bestmod)$zi

# make predictions
pred.zi <- cbind(1, qpred$logPD) %*% beta.zi

# we have generated estimates on the link scale
# logit(prob) and log(cond), not the predictions themselves
# so we transform to the response scale and multiply
pred.ucount <- exp(pred.cond)*(1-plogis(pred.zi))

# add predicted rat complaints back to the dataset
qpred$preds <- pred.ucount

# Fig 3:plotting quarterly maps of predicted rat complaints---------------------

tracts_sf <- st_read("./DataRaw/GIS/chicagoCensusTracts2010.shp")

# want only quarter 3

Q3data <- left_join(tracts_sf, 
                    qpred %>% 
                      filter(quarter == "Q3") %>% 
                      dplyr::select(tract, preds),
                    by = c("tractce10" = "tract"))


#tiff("./Figures/Fig3.tiff", height = 8, width = 6, units = "in", res = 600)

# rat complaints
mytheme <- theme_bw() +
  theme(legend.position = c(0.18, 0.2))

# plotted on a log scale
# ggplot() + 
#   mytheme +
#   geom_sf(data = Q3data, aes(fill = preds), color = "black") +
#   scale_fill_viridis_c(trans = "log10",
#                        name = "Predicted\nrat complaints") +
#   coord_sf()

# plot with percentiles instead
Fn <- ecdf(Q3data$preds)

ggplot() + 
  mytheme +
  geom_sf(data = Q3data, aes(fill = Fn(preds)), color = "black") +
  scale_fill_viridis_c(name = "Relative predicted\nrat complaints") +
  coord_sf()

dev.off()

# Fig. S1: plotting map of random effects---------------------------------------
tracts_sf <- st_read("./DataRaw/GIS/chicagoCensusTracts2010.shp")

tractEff <- ranef(bestmod)$cond$tract
tractEff$tract <- rownames(tractEff)
rownames(tractEff) <- NULL
names(tractEff)[1] <- "effect"


tracts_sf <- left_join(tracts_sf, tractEff, by = c("tractce10" = "tract"))

#tiff("./Figures/FigS1.tiff", height = 8, width = 6, units = "in", res = 600)

ggplot() + 
  mytheme +
  geom_sf(data = tracts_sf, aes(fill = effect), color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0) +
  coord_sf()

#dev.off()

#test <- left_join(Q3data, tractEff, by = c("tractce10" = "tract"))

#############################
tractEff <- ranef(bestmod)$cond$tract
tractEff2 <- tractEff[,1]
tractEff2 <- rep(tractEff2, each = 4)

# could also be written as exp(pred.cond)*exp(tractEff2)*(1-plogis(pred.zi))
predwRE <- exp(pred.cond + tractEff2)*(1-plogis(pred.zi))

# add predicted rat complaints back to the dataset
qpred$predwRE <- predwRE


tracts_sf <- st_read("./Data/GIS/chicagoCensusTracts2010.shp")

# want only quarter 3

Q3data <- left_join(tracts_sf, 
                    qpred %>% 
                      filter(quarter == "Q3") %>% 
                      dplyr::select(tract, preds, predwRE),
                    by = c("tractce10" = "tract"))

# rat complaints
mytheme <- theme_bw() +
  theme(legend.position = c(0.18, 0.2))

ggplot() + 
  mytheme +
  geom_sf(data = Q3data, aes(fill = preds - predwRE), color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                       midpoint = 0) +
  coord_sf()

# plot with percentiles instead
Fn <- ecdf(Q3data$preds)

ggplot() + 
  mytheme +
  geom_sf(data = Q3data, aes(fill = Fn(preds)), color = "black") +
  scale_fill_viridis_c(name = "Relative predicted\nrat complaints") +
  coord_sf()

