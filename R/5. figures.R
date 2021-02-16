# Code to accompany Sanchez et al. 2021. Social and environmental correlates
# of rat complaints in Chicago. Journal of Urban Ecology. doi: 10.1093/jue/juab006

# This script generates Fig. 1-3 and Fig. S1 for the paper

# load packages-----------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(sf)
library(sjPlot)
library(gridExtra)  
library(glmmTMB)

finalMod <- readRDS("./ModelOutput/finalModel.rds")

rc <- read.csv("./DataCleaned/dataformodels.csv", header = TRUE) %>% 
  mutate_at(vars(year), as.factor) %>% 
  mutate(logPD = log(popDens)) %>%  # since there's one really high value
  dplyr::select(-c(medHouseholdInc, totalPop, area, popDens)) %>% 
  # format tract as character, place leading zero if it's only five digits
  mutate(tract = format(tract, width = 6)) %>% 
  mutate(tract = gsub(" ", "0", tract))

# center and scale the predictors
cs <- function(x) {
  scale(x, center = TRUE, scale = TRUE)
}

rcScaled <- rc %>% 
  mutate_at(c(5:16), cs)

# FIGURE 1: quarterly rat complaints and quarterly preds------------------------

#tiff("./Figures/Fig1.tiff", height = 5, width = 7, units = "in", res = 300)

fig1dat <- rc %>% 
  group_by(year, quarter) %>% 
  summarise("Rat complaints" = sum(ratcomp), "Garbage complaints" = sum(garbage), 
            "Dog feces complaints" = sum(dogFeces), 
            "Construction/demolition permits" = sum(bPerm)) %>% 
  tidyr::pivot_longer(cols = -c(1:2), names_to = "outcome") %>%
  mutate(outcome = forcats::fct_relevel(outcome, "Rat complaints", 
                                        "Garbage complaints", 
                                        "Dog feces complaints"))

# plotting rat complaints by quarter and year
ggplot(fig1dat, aes(x = quarter, y = value)) +
  geom_boxplot(aes(group = quarter)) +
  facet_wrap(~outcome, scales = "free_y") +
  ylab("") + xlab("Quarter") +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 10),
        axis.title = element_text(size = 14),
        strip.text.x = element_text(size = 12))

#dev.off()

# FIGURE 2: marginal effects----------------------------------------------------

get_legend <- function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

#tiff("./Figures/Fig2.tiff", height = 5, width = 8, units = "in", res = 300)

mytheme <- theme_bw() +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "black"))

# to make the figures more readable, need to change the axis labels from the centered/scaled values back to the original scale

# population density
pdt <- function(x){(x-mean(rc$logPD))/sd(rc$logPD)}
p1 <- plot_model(finalMod, type = "eff", terms = c("logPD", "quarter"), 
                 colors = "BrBG", line.size = 2) + 
  scale_x_continuous(breaks = c(pdt(6), pdt(8), pdt(10), pdt(12)),
                     labels = c("6", "8", "10", "12")) +
  xlab("log(Human population density)") +
  ylab("Rat complaints") +
  mytheme + labs(color = "Quarter")

leg <- get_legend(p1) 
p1 <- p1 + theme(legend.position = "none")

# building age
bat <- function(x){(x-mean(rc$pBuiltPre1950))/sd(rc$pBuiltPre1950)}
p2 <- plot_model(finalMod, type = "eff", 
                 terms = c("pBuiltPre1950 [-2.55:1.95 by = 0.1]", "quarter"),
                 colors = "BrBG", line.size = 2) + 
  scale_x_continuous(breaks = c(bat(0), bat(25), bat(50), bat(75), bat(100)),
                     labels = c("0", "25", "50", "75", "100")) +
  xlab("% structures built before 1950") +
  ylab("Rat complaints") +
  mytheme + theme(legend.position = "none")

# restaurants
foodt <- function(x){(x-mean(rc$food))/sd(rc$food)}
p3 <- plot_model(finalMod, type = "eff", 
                 terms = c("food [-0.63:18.6 by = 0.5]", "quarter"),
                 colors = "BrBG", line.size = 2) + 
  scale_x_continuous(breaks = c(foodt(0), foodt(50), foodt(100), foodt(150),
                                foodt(200), foodt(250)),
                     labels = c("0", "50", "100", "150", "200", "250")) +
  xlab("Number of restaurants") +
  ylab("Rat complaints") +
  mytheme + theme(legend.position = "none")

# garbage complaints
gt <- function(x){(x-mean(rc$garbage))/sd(rc$garbage)}
p4 <- plot_model(finalMod, type = "eff", 
                 terms = c("garbage [-0.8:22.7 by = 0.1]", "quarter"),
                 colors = "BrBG", line.size = 2) + 
  scale_x_continuous(breaks = c(gt(0), gt(20), gt(40), gt(60), gt(80)),
                     labels = c("0", "20", "40", "60", "80")) +
  xlab("Garbage complaints") +
  ylab("Rat complaints") +
  mytheme + theme(legend.position = "none")


grid.arrange(p1, p2, p3, p4, leg, layout_matrix = rbind(c(1, 2, 5), c(3, 4, 5)), 
             widths = c(2.5, 2.5, 1))

#dev.off()

# predicted # of rat complaints by quarter (for use in Fig. 3)------------------

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
qpred$adjHhInc <- (qpred$adjHhInc - mean(rc$adjHhInc)) / sd(rc$adjHhInc)
qpred$pVacantHU <- (qpred$pVacantHU - mean(rc$pVacantHU)) / sd(rc$pVacantHU)
qpred$pOwnerOcc <- (qpred$pOwnerOcc - mean(rc$pOwnerOcc)) / sd(rc$pOwnerOcc)
qpred$pBuiltPre1950 <- (qpred$pBuiltPre1950 - mean(rc$pBuiltPre1950)) / sd(rc$pBuiltPre1950)
qpred$pOvercrowded <- (qpred$pOvercrowded - mean(rc$pOvercrowded)) / sd(rc$pOvercrowded)
qpred$pUnder5y <- (qpred$pUnder5y - mean(rc$pUnder5y)) / sd(rc$pUnder5y)
qpred$logPD <- (qpred$logPD - mean(rc$logPD)) / sd(rc$logPD)

# This part is based on Brooks et al. 2017
# "glmmTMB balances speed and flexibility among packages for zero-inflated
# generalized linear mixed modeling"

# extract fixed effects estimates for the conditional model
beta.cond <- fixef(finalMod)$cond

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
X.cond[, 10] <- qpred$adjHhInc
X.cond[, 11] <- qpred$pVacantHU
X.cond[, 12] <- qpred$pOwnerOcc
X.cond[, 13] <- qpred$pBuiltPre1950
X.cond[, 14] <- qpred$pOvercrowded
X.cond[, 15] <- qpred$pUnder5y
X.cond[, 16] <- qpred$logPD

# make predictions for conditional model
pred.cond <- X.cond %*% beta.cond


# now we want to do the bit for the zero-inflation model
# get fixed effect estimates for zero-inflation model
beta.zi <- fixef(finalMod)$zi

# create empty matrix to hold predictor data
# important that the order of the columns is the same as beta.cond
X.zi <- matrix(0, ncol = 16, nrow = nrow(qpred))

X.zi[, 1] <- 1 # intercept
X.zi[which(qpred$quarter == "Q2"), 2] <- 1 # dummy variables for quarter
X.zi[which(qpred$quarter == "Q3"), 3] <- 1 # dummy variables for quarter
X.zi[which(qpred$quarter == "Q4"), 4] <- 1 # dummy variables for quarter
X.zi[, 5] <- qpred$bPerm
X.zi[, 6] <- qpred$dogFeces
X.zi[, 7] <- qpred$garbage
X.zi[, 8] <- qpred$food
X.zi[, 9] <- qpred$pGradDegr
X.zi[, 10] <- qpred$adjHhInc
X.zi[, 11] <- qpred$pVacantHU
X.zi[, 12] <- qpred$pOwnerOcc
X.zi[, 13] <- qpred$pBuiltPre1950
X.zi[, 14] <- qpred$pOvercrowded
X.zi[, 15] <- qpred$pUnder5y
X.zi[, 16] <- qpred$logPD

# make predictions
pred.zi <-X.zi %*% beta.zi

# we have generated estimates on the link scale:
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

library(ggsn)
#tiff("./Figures/Fig3.tiff", height = 8, width = 6, units = "in", res = 300)

# rat complaints
mytheme <- theme_bw() +
  theme(legend.position = c(0.18, 0.2))

# plot with percentiles
Fn <- ecdf(Q3data$preds)

ggplot() + 
  mytheme +
  geom_sf(data = Q3data, aes(fill = Fn(preds)), color = "black") +
  scale_fill_viridis_c(name = "Relative predicted\nrat complaints") +
  coord_sf() +
  north(Q3data, location = "topright", symbol = 12) +
  scalebar(Q3data, dist = 5, dist_unit = "km", transform = TRUE, 
           model = "WGS84", location = "bottomleft", st.dist = 0.03, 
           st.bottom = FALSE) +
  ylab("") + xlab("") +
  theme(legend.position = c(0.2, 0.3),
        axis.text = element_text(color = "black"))

#dev.off()

# Fig. S1: plotting map of random effects---------------------------------------
tracts_sf <- st_read("./DataRaw/GIS/chicagoCensusTracts2010.shp")

tractEff <- ranef(finalMod)$cond$tract
tractEff$tract <- rownames(tractEff)
rownames(tractEff) <- NULL
names(tractEff)[1] <- "effect"

tracts_sf <- left_join(tracts_sf, tractEff, by = c("tractce10" = "tract"))

#tiff("./Figures/FigS1.tiff", height = 8, width = 6, units = "in", res = 300)

ggplot() + 
  mytheme +
  geom_sf(data = tracts_sf, aes(fill = effect), color = "black") +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red",
                        midpoint = 0) +
  coord_sf() +
  north(tracts_sf, location = "topright", symbol = 12) +
  scalebar(tracts_sf, dist = 5, dist_unit = "km", transform = TRUE, 
           model = "WGS84", location = "bottomleft", st.dist = 0.03, 
           st.bottom = FALSE) +
  ylab("") + xlab("") +
  theme(legend.position = c(0.2, 0.3),
        axis.text = element_text(color = "black"))

#dev.off()
