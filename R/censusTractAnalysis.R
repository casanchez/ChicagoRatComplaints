# load packages-----------------------------------------------------------------
library(dplyr)
library(sp)
library(rgdal)
library(fitdistrplus)
library(glmmTMB)
library(effects)

'%!in%' <- function(x,y)!('%in%'(x,y))

# load data---------------------------------------------------------------------

options(digits = 20)

# some are large files, can take a while

# https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Rodent-Baiting-No-Duplicates/uqhs-j723
rats <- read.csv("./Data/cleanedComplaints.csv", header = TRUE)

# https://data.cityofchicago.org/Buildings/Building-Permits/ydr8-5enu
bPermits <- read.csv("./Data/buildingPermits.csv", header = TRUE, 
                     na.strings = "")

# https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Sanitation-Code-Complaints-No/rccf-5427
san <- read.csv("./Data/sanitationComplaints.csv", header = TRUE, 
                na.strings = "")

# https://data.cityofchicago.org/Health-Human-Services/Food-Inspections/4ijn-s7e5

# consider only restaurants? https://data.cityofchicago.org/Health-Human-Services/Restaurant/5udb-dr6f 
food <- read.csv("./Data/foodInspections.csv", header = TRUE, na.strings = "")

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Blocks-2010/mfzt-js4n
blocks <- readOGR("./Data/GIS","chicagoCensusBlocks2010")

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Tracts-2010/5jrd-6zik
tracts <- readOGR("./Data/GIS","chicagoCensusTracts2010")

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Population-by-2010-Census-Block/5yjb-v3mj
pop <- read.csv("./Data/population2010CB.csv", header = TRUE, na.strings = "")
pop$CENSUS.BLOCK.FULL <- as.factor(pop$CENSUS.BLOCK.FULL)

# rat complaints data-----------------------------------------------------------

rats <- rats[, -1]

# change formats
rats$Creation.Date <- as.Date(rats$Creation.Date, "%m/%d/%Y") 

# add month and year columns for aggregation
rats$month <- format(rats$Creation.Date,"%m")
rats$quarter <- as.factor(rats$month)
levels(rats$quarter) <- list(Q1 = c("01", "02", "03"), 
                             Q2 = c("04", "05", "06"),
                             Q3 = c("07", "08", "09"),
                             Q4 = c("10", "11", "12"))
rats$year <- format(rats$Creation.Date,"%Y")

# shorten so we have only full years
ratShort <- rats %>% 
  filter(Creation.Date >= as.Date("2011-01-01")) %>% 
  filter(Creation.Date <= as.Date("2017-12-31"))

ratShort$year <- as.factor(ratShort$year)

# pull out coordinates of rat complaints
ratPts <- SpatialPoints(ratShort[, c("Longitude", "Latitude")], 
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

# extract census block that each rat complaint belongs to
ratPts2 <- over(ratPts, tracts)

# store unique census block id back with rat data
ratShort$tract <- ratPts2$tractce10

# remove points not assigned to a block
ratShort <- ratShort %>%
  filter(!is.na(tract))

# calculate number of rat complaints per community area, each quarter of each year
ratsCTQY <- ratShort %>% 
  group_by(tract, year, quarter, .drop = FALSE) %>% 
  summarize(ratcomp = n())

# building permit data----------------------------------------------------------

# change formats
bPermits$ISSUE_DATE <- as.Date(bPermits$ISSUE_DATE, "%m/%d/%Y")

# add month and year columns for aggregation
bPermits$month <- format(bPermits$ISSUE_DATE,"%m")
bPermits$quarter <- as.factor(bPermits$month)
levels(bPermits$quarter) <- list(Q1 = c("01", "02", "03"), 
                                 Q2 = c("04", "05", "06"),
                                 Q3 = c("07", "08", "09"),
                                 Q4 = c("10", "11", "12"))
bPermits$year <- format(bPermits$ISSUE_DATE,"%Y")

# subset relevant columns, construction types, dates
bpShort <- bPermits %>%
  dplyr::select(ID:WORK_DESCRIPTION, LATITUDE:Wards, month:year) %>%
  filter(PERMIT_TYPE %in% c("PERMIT - NEW CONSTRUCTION", 
                            "PERMIT - WRECKING/DEMOLITION")) %>%
  filter(ISSUE_DATE >= as.Date("2011-01-01")) %>% 
  filter(ISSUE_DATE <= as.Date("2017-12-31")) %>% 
  filter(!is.na(LONGITUDE))

# drop unused levels
bpShort$PERMIT_TYPE <- droplevels(bpShort$PERMIT_TYPE)
bpShort$WORK_DESCRIPTION <- droplevels(bpShort$WORK_DESCRIPTION)

# need to extract the community area
bpPts <- SpatialPoints(bpShort[, c("LONGITUDE", "LATITUDE")], 
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

bpPts2 <- over(bpPts, tracts)
bpShort$tract <- bpPts2$tractce10

# remove points not assigned to a block
bpShort <- bpShort %>%
  filter(!is.na(tract))

bpShort$year <- as.factor(bpShort$year)

# calculate number of permits issued per community area, each quarter of each year

buildCTQY <- bpShort %>% 
  filter(PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION") %>% 
  group_by(tract, year, quarter, .drop = FALSE) %>% 
  summarize(build = n())

demolCTQY <- bpShort %>% 
  filter(PERMIT_TYPE == "PERMIT - WRECKING/DEMOLITION") %>% 
  group_by(tract, year, quarter, .drop = FALSE) %>% 
  summarize(demol = n())

# sanitation complaint data-----------------------------------------------------

# re-categorize sanitation complaints
san$violType <- plyr::revalue(san$What.is.the.Nature.of.this.Code.Violation.,
                              c("Garbage in alley" = "Garbage",
                                "Garbage in yard" = "Garbage",
                                "Dumpster not being emptied" = "Garbage",
                                "Overflowing carts" = "Garbage",
                                "Dog feces in yard" = "Dog feces",
                                "Construction Site Cleanliness/Fence" = "Other",
                                "Graffiti Commercial Vehicle" = "Other",
                                "Standing water" = "Other",
                                "Other" = "Other"))
san$violType <- as.character(san$violType)
san$violType[is.na(san$violType)] <- "Not provided"
san$violType <- as.factor(san$violType)

# change formats
san$Creation.Date <- as.Date(san$Creation.Date, "%m/%d/%Y")

# add month and year columns for aggregation
san$month <- format(san$Creation.Date, "%m")
san$quarter <- as.factor(san$month)
levels(san$quarter) <- list(Q1 = c("01", "02", "03"), 
                            Q2 = c("04", "05", "06"),
                            Q3 = c("07", "08", "09"),
                            Q4 = c("10", "11", "12"))
san$year <- format(san$Creation.Date, "%Y")

# trim the timespan
sanShort <- san %>%
  filter(Creation.Date >= as.Date("2011-01-01")) %>% 
  filter(Creation.Date <= as.Date("2017-12-31")) %>% 
  filter(!is.na(Longitude))

# extract the census block
sanPts <- SpatialPoints(sanShort[, c("Longitude", "Latitude")], 
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
sanPts2 <- over(sanPts, tracts)
sanShort$tract <- sanPts2$tractce10

sanShort <- sanShort %>%
  filter(!is.na(tract))

sanShort$year <- as.factor(sanShort$year)

# calculate number of different sanitation complaints per community area, each quarter of each year
sancompsCTQY <- sanShort %>% 
  group_by(tract, year, quarter, violType, .drop = FALSE) %>% 
  summarize(nviol = n()) %>% 
  tidyr::spread(violType, nviol)

names(sancompsCTQY)[4:7] <- c("feces", "garbage", "sanNP", "sanOther")

# food inspections--------------------------------------------------------------

# using locations of food inspections as proxy for number of food places

# change formats
food$Inspection.Date <- as.Date(food$Inspection.Date, "%m/%d/%Y")

# add month and year columns for aggregation
food$month <- format(food$Inspection.Date,"%m")
food$quarter <- as.factor(food$month)
levels(food$quarter) <- list(Q1 = c("01", "02", "03"), 
                             Q2 = c("04", "05", "06"),
                             Q3 = c("07", "08", "09"),
                             Q4 = c("10", "11", "12"))
food$year <- format(food$Inspection.Date,"%Y")

# could consider only keeping restaurants, but for now just keep broad
foodShort <- food %>% 
  filter(Inspection.Date >= as.Date("2011-01-01")) %>% 
  filter(Inspection.Date <= as.Date("2017-12-31")) %>% 
  filter(Inspection.Type %!in% c("O.B.", "Out of Business", "OUT OF BUSINESS",
                                 "out ofbusiness")) %>%
  filter(Results != "Business Not Located") %>% 
  filter(!is.na(Longitude))                             

foodShort$year <- as.factor(foodShort$year)


foodPts <- SpatialPoints(foodShort[, c("Longitude", "Latitude")], 
                         proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
foodPts2 <- over(foodPts, tracts)
foodShort$tract <- foodPts2$tractce10

# only aggregating by year, not quarter
# keeping only distinct addresses within a year, to avoid double-counting
foodCTY <- foodShort %>% 
  filter(!is.na(tract)) %>% 
  group_by(tract, year, .drop = FALSE) %>%
  distinct(Address, .keep_all = TRUE) %>% 
  summarize(food = n())

# combine data together---------------------------------------------------------

fullDat <- dplyr::bind_cols(c(ratsCTQY, 
                              buildCTQY[, "build"], demolCTQY[, "demol"], 
                              sancompsCTQY[, c("feces", "garbage", "sanOther")]))

fullDat <- left_join(fullDat, foodCTY, 
                     by = c("tract" = "tract", "year" = "year"))


# want to add the populations for all the census blocks in a tract together
pop$tract <- substr(pop$CENSUS.BLOCK.FULL, 6, 11)

tractPop <- pop %>% 
  group_by(tract) %>% 
  summarise(pop = sum(TOTAL.POPULATION))

fullDat <- left_join(fullDat, tractPop, by = "tract")

fullDat$logPop <- log(fullDat$TOTAL.POPULATION + 1)

# remove any NA values
fullDat <- fullDat[complete.cases(fullDat), ]

# save dataset since it takes a little while to build from scratch
#write.csv(fullDat, "./Data/ratCompPredsCT.csv")

# models------------------------------------------------------------------------

# examine correlations between predictors
library(corrplot)
corrplot(cor(fullDat[, 5:12]), method = "number")
descdist(fullDat$ratcomp, discrete = TRUE)

# square-root transform population
fullDat$sqrtPop <- sqrt(fullDat$pop)

# center the predictors
center <- function(x) {
  scale(x, scale = FALSE)
}

fullDatCent <- fullDat %>% 
  mutate_at(c(5:12), center)

# set up models
globalZIP <- glmmTMB(ratcomp ~ build + demol + feces + food + garbage + sqrtPop 
                     + quarter + (1|year) + (1|tract), ziformula = ~sqrtPop, 
                     family = poisson, data = fullDat)

globalZINB <- glmmTMB(ratcomp ~ build + demol + feces + food + garbage + 
                        sqrtPop + quarter + (1|year) + (1|tract), 
                      ziformula = ~sqrtPop, family = nbinom1, data = fullDat)

globalZINB2 <- glmmTMB(ratcomp ~ build + demol + feces + food + garbage + 
                         sqrtPop + quarter + (1|year) + (1|tract),
                       ziformula = ~sqrtPop, family = nbinom2, data = fullDat)

library(bbmle)
AICtab(globalZIP, globalZINB, globalZINB2)

# okay, so negative binomial model is best
# have to compare global against all other candidate models

constrZINB2 <- glmmTMB(ratcomp ~ build + demol + (1|year) + (1|tract),
                       ziformula = ~sqrtPop, family = nbinom2, data = fullDat)
# non-positive-definite Hessian matrix

fhZINB2 <- glmmTMB(ratcomp ~ feces + food + garbage + (1|year) + (1|tract),
                   ziformula = ~sqrtPop, family = nbinom2, data = fullDat)

popZINB2 <- glmmTMB(ratcomp ~ sqrtPop + (1|year) + (1|tract),
                    ziformula = ~sqrtPop, family = nbinom2, data = fullDat)
# non-positive-definite Hessian matrix

quartZINB2 <- glmmTMB(ratcomp ~ quarter + (1|year) + (1|tract),
                      ziformula = ~sqrtPop, family = nbinom2, data = fullDat)

nullZINB2 <- glmmTMB(ratcomp ~ 1 + (1|year) + (1|tract),
                     ziformula = ~sqrtPop, family = nbinom2, data = fullDat)

AICtab(globalZINB2, constrZINB2, fhZINB2, popZINB2, quartZINB2, nullZINB2)

summary(globalZINB2)

# plot incident rate ratios
plot_model(globalZINB2, show.values = TRUE, value.offset = 0.4, vline.color = "gray") +
  theme_bw()


# plot marginal effects
plot_model(globalZINB2, type = "eff", terms = "build", show.data = TRUE) + 
  theme_bw() +
  xlab("Building permits issued") +
  ylab("Rat complaints") +
  theme(plot.title = element_blank(),
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16, color = "black"))

plot_model(globalZINB2, type = "eff", terms = "demol", show.data = TRUE)

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

plot_model(globalZINB2, type = "eff", terms = "sqrtPop", show.data = TRUE)

plot_model(globalZINB2, type = "eff", terms = "quarter", show.data = TRUE)
