
# load packages-----------------------------------------------------------------
library(dplyr)
library(sp)
library(rgdal)
library(fitdistrplus)
library(glmmTMB)
library(effects)

# load data---------------------------------------------------------------------

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
food <- read.csv("./Data/foodInspections.csv", header = TRUE, na.strings = "")

# https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2
socio <- read.csv("./Data/socioecInd.csv", header = TRUE, na.strings = "")

pop <- read.csv("./Data/CMAP.csv", header = TRUE, na.strings = c("", "n/a"))

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Community-Areas-current-/cauq-8yn6
comms <- readOGR("./Data/GIS","chicagoCommAreas")

# rat complaints data-----------------------------------------------------------

rats <- rats[, -1]

# change formats
rats$Creation.Date <- as.Date(rats$Creation.Date, "%m/%d/%Y") 
rats$Community.Area <- as.factor(rats$Community.Area)

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

# I think community areas are good for this dataset, but we're extracting them for all the others, so might as well be consistent
ratPts <- SpatialPoints(ratShort[, c("Longitude", "Latitude")], 
                         proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
ratPts2 <- over(ratPts, comms)
ratShort$CAcalc <- ratPts2$area_num_1

ratShort$CAcalc <- factor(ratShort$CAcalc, levels = c(1:77))

ratShort$Community.Area == ratShort$CAcalc

ratShort <- ratShort %>%
  filter(!is.na(CAcalc))

# calculate number of rat complaints per community area, each quarter of each year
ratsCAQY <- ratShort %>% 
  group_by(CAcalc, year, quarter, .drop = FALSE) %>% 
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

bpPts2 <- over(bpPts, comms)
bpShort$CAcalc <- bpPts2$area_num_1

# reorder community area levels
bpShort$CAcalc <- factor(bpShort$CAcalc, levels = c(1:77))

# remove NA community areas 
# nearly all are 11601 W TOUHY AVE (part of the airport, but technically falls outside the CA boundary)--exclude or assign to 76?
bpShort <- bpShort %>%
  filter(!is.na(CAcalc))

bpShort$year <- as.factor(bpShort$year)

# calculate number of permits issued per community area, each quarter of each year

buildCAQY <- bpShort %>% 
  filter(PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION") %>% 
  group_by(CAcalc, year, quarter, .drop = FALSE) %>% 
  summarize(build = n())

demolCAQY <- bpShort %>% 
  filter(PERMIT_TYPE == "PERMIT - WRECKING/DEMOLITION") %>% 
  group_by(CAcalc, year, quarter, .drop = FALSE) %>% 
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
san$month <- format(san$Creation.Date,"%m")
san$quarter <- as.factor(san$month)
levels(san$quarter) <- list(Q1 = c("01", "02", "03"), 
                                 Q2 = c("04", "05", "06"),
                                 Q3 = c("07", "08", "09"),
                                 Q4 = c("10", "11", "12"))
san$year <- format(san$Creation.Date,"%Y")

# trim the timespan
sanShort <- san %>%
  filter(Creation.Date >= as.Date("2011-01-01")) %>% 
  filter(Creation.Date <= as.Date("2017-12-31")) %>% 
  filter(!is.na(Community.Area)) %>% 
  filter(!is.na(Longitude))

# extract the community area
sanPts <- SpatialPoints(sanShort[, c("Longitude", "Latitude")], 
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
sanPts2 <- over(sanPts, comms)
sanShort$CAcalc <- sanPts2$area_num_1

# reorder community area levels
sanShort$CAcalc <- factor(sanShort$CAcalc, levels = c(1:77))

# the calculated and given CAs mostly match up, but probably better for consistency to use the calculated ones
sanShort$CAcalc == sanShort$Community.Area

sanShort <- sanShort %>%
  filter(!is.na(CAcalc))

sanShort$year <- as.factor(sanShort$year)

# calculate number of different santiation complaints per community area, each quarter of each year
sancompsCAQY <- sanShort %>% 
  group_by(CAcalc, year, quarter, violType, .drop = FALSE) %>% 
  summarize(nviol = n()) %>% 
  tidyr::spread(violType, nviol)

names(sancompsCAQY)[4:7] <- c("feces", "garbage", "sanNP", "sanOther")

# food inspections--------------------------------------------------------------

# using locations of food inspections as proxy for number of food places in each CA

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


foodShort <- food %>% 
  filter(Inspection.Date >= as.Date("2011-01-01")) %>% 
  filter(Inspection.Date <= as.Date("2017-12-31")) %>% 
  filter(Results != "Business Not Located") %>% 
  filter(!is.na(Longitude)) 

foodShort$year <- as.factor(foodShort$year)


foodPts <- SpatialPoints(foodShort[, c("Longitude", "Latitude")], 
                     proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
foodPts2 <- over(foodPts, comms)
foodShort$CAcalc <- foodPts2$area_num_1

foodShort$CAcalc <- factor(foodShort$CAcalc, levels = c(1:77))

# only aggregating by year, not quarter
# keeping only distinct addresses within a year, to avoid double-counting
foodCAY <- foodShort %>% 
  filter(!is.na(CAcalc)) %>% 
  group_by(CAcalc, year) %>% 
  distinct(Address, .keep_all = TRUE) %>% 
  summarize(food = n())

# community areas---------------------------------------------------------------

areas <- as.data.frame(matrix(NA, nrow = 77, ncol = 2))
names(areas) <- c("Community.Area", "sqm")
areas$Community.Area <- comms$area_num_1
areas$sqm <- comms$shape_area
areas$sqkm <- areas$sqm/1000000

# combine data together---------------------------------------------------------

fullDat <- dplyr::bind_cols(c(ratsCAQY, buildCAQY[, "build"], 
                              demolCAQY[, "demol"], 
                       sancompsCAQY[, c("feces", "garbage", "sanOther")]))

fullDat <- left_join(fullDat, foodCAY, by = c("CAcalc" = "CAcalc", 
                                              "year" = "year"))

socio$Community.Area.Number <- as.factor(socio$Community.Area.Number)

fullDat <- left_join(fullDat, socio, by = c("CAcalc" = "Community.Area.Number"))

fullDat <- left_join(fullDat, pop[, c("GEOG", "TOT_POP")], by = c("COMMUNITY.AREA.NAME" = "GEOG"))

fullDat <- left_join(fullDat, areas[, c("Community.Area", "sqkm")], 
                     by = c("CAcalc" = "Community.Area"))

fullDat$CAcalc <- as.factor(fullDat$CAcalc)

# remove CA 76 (Ohare airport)
fullDat <- filter(fullDat, CAcalc != "76")

fullDat$CAcalc <- droplevels(fullDat$CAcalc)

fullDat$CAcalc <- factor(fullDat$CAcalc, levels = c(1:75, 77))

# save dataset since it takes a little while to build from scratch
#write.csv(fullDat, "./Data/ratCompPredsCA.csv")

# models------------------------------------------------------------------------

hist(fullDat$ratcomp)

descdist(fullDat$ratcomp, discrete = TRUE)
# suggests a negative binomial distribution

fit.nb <- fitdist(fullDat$ratcomp, "nbinom")
plot(fit.nb)

# including total population (untransformed) throws a warning, so I have sqrt-transformed it
m1 <- glmmTMB(ratcomp ~ build + demol + feces + garbage + sanOther + food + 
                PERCENT.HOUSEHOLDS.BELOW.POVERTY + 
                quarter + sqrt(TOT_POP) + sqkm + (1|CAcalc) + (1|year), 
              family = nbinom2, data = fullDat)
summary(m1)
plot(allEffects(m1))

# looks like there could be outliers with feces and garbage

noOut <- fullDat %>% 
  filter(feces < 25) %>% 
  filter(garbage < 180)

m2 <- glmmTMB(ratcomp ~ build + demol + feces + garbage + sanOther + food + 
                PERCENT.HOUSEHOLDS.BELOW.POVERTY + quarter + sqrt(TOT_POP) + 
                sqkm + (1|CAcalc) + (1|year), 
              family = nbinom2, data = noOut)
summary(m2)
plot(allEffects(m2))

# results are still robust, so that's good

# dropping other sanitation complaints as pred
m3 <- glmmTMB(ratcomp ~ build + demol + feces + garbage + food + 
                PERCENT.HOUSEHOLDS.BELOW.POVERTY + 
                quarter + sqrt(TOT_POP) + sqkm + (1|CAcalc) + (1|year), 
              family = nbinom2, data = fullDat)
summary(m3)
plot(allEffects(m3))
# doesn't change anything


# https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/
# to get incident rate ratios, exponentiate the model coefficients
# increasing predictor by 1 unit multiplies the mean value of rat complaints by exp(beta)
exp(m1$fit$par)

library(DHARMa)
res <- simulateResiduals(m1)
plot(res, rank = T)


# running original model with different package (a lot slower, and throws warnings)
# lets us see correlation matrix of predictor variables
library(lme4)
m1b <- glmer.nb(ratcomp ~ build + demol + feces + garbage + sanOther + food + 
                 PERCENT.HOUSEHOLDS.BELOW.POVERTY + quarter + sqrt(TOT_POP) + 
                 sqkm + (1|CAcalc) + (1|year), data = fullDat)
summary(m1b)
vcov(m1b)
plot(allEffects(m1b))


