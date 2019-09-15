# load packages-----------------------------------------------------------------
library(dplyr)
library(sp)
library(rgdal)
library(fitdistrplus)
library(glmmTMB)
library(effects)

'%!in%' <- function(x,y)!('%in%'(x,y))

# load data---------------------------------------------------------------------

options(digits = 6)

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

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Blocks-2010/mfzt-js4n
blocks <- readOGR("./Data/GIS","chicagoCensusBlocks2010")

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Tracts-2010/5jrd-6zik
tracts <- readOGR("./Data/GIS","chicagoCensusTracts2010")

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Population-by-2010-Census-Block/5yjb-v3mj
pop <- read.csv("./Data/population2010CB.csv", header = TRUE, na.strings = "")

# socioeconomic data
socio1 <- read.csv("./Data/Summary of Census Socioeconomic Data 2011-2012.csv")
socio2 <- read.csv("./Data/Summary of Census Socioeconomic Data 2012-2013.csv")
socio3 <- read.csv("./Data/Summary of Census Socioeconomic Data 2013-2014.csv")
socio4 <- read.csv("./Data/Summary of Census Socioeconomic Data 2014-2015.csv")
socio5 <- read.csv("./Data/Summary of Census Socioeconomic Data 2015-2016.csv")
socio6 <- read.csv("./Data/Summary of Census Socioeconomic Data 2016-2017.csv")
socio7 <- read.csv("./Data/Summary of Census Socioeconomic Data 2017-2018.csv")

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

# extract census tract that each rat complaint belongs to
ratPts2 <- over(ratPts, tracts)

# store unique census tract id back with rat data
ratShort$tract <- ratPts2$tractce10

# remove points not assigned to a tract
ratShort <- ratShort %>%
  filter(!is.na(tract))

# calculate number of rat complaints per tract, each quarter of each year
ratsCTQY <- ratShort %>% 
  group_by(tract, year, quarter, .drop = FALSE) %>% 
  summarize(ratcomp = n())

# looking at rat complaints by quarter of each year (for table/figure)
ratsQY <- ratShort %>% 
  group_by(year, quarter, .drop = FALSE) %>% 
  summarize(ratcomp = n())

ggplot(ratsQY) + 
  geom_line(aes(x = quarter, y = ratcomp, group = year, color = year), size = 2) +
  theme_bw()

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

# need to extract the census tract
bpPts <- SpatialPoints(bpShort[, c("LONGITUDE", "LATITUDE")], 
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

bpPts2 <- over(bpPts, tracts)
bpShort$tract <- bpPts2$tractce10

# remove points not assigned to a tract
bpShort <- bpShort %>%
  filter(!is.na(tract))

bpShort$year <- as.factor(bpShort$year)

# calculate number of permits issued per tract, each quarter of each year

buildCTQY <- bpShort %>% 
  #filter(PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION") %>% 
  group_by(tract, year, quarter, PERMIT_TYPE, .drop = FALSE) %>% 
  summarize(bPerm = n()) %>% 
  tidyr::spread(PERMIT_TYPE, bPerm)

names(buildCTQY)[4:5] <- c("construction", "demolition")

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

# extract the census tract
sanPts <- SpatialPoints(sanShort[, c("Longitude", "Latitude")], 
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
sanPts2 <- over(sanPts, tracts)
sanShort$tract <- sanPts2$tractce10

sanShort <- sanShort %>%
  filter(!is.na(tract))

sanShort$year <- as.factor(sanShort$year)

# calculate number of different sanitation complaints per tract, each quarter of each year
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

# only keeping restaurants
foodShort <- food %>% 
  filter(Facility.Type == "Restaurant") %>% 
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

# socioeconomic data------------------------------------------------------------

# have to deal with the first dataset differently, has different formatting
socio1$year <- 2011

# also has different dimensions than the other datasets...
# could suggest different numbering of census tracts?

socio1 <- socio1 %>% 
  dplyr::select(TRACT, year, Estimated.Median.Family.Income, 
                Percent.of.Owner.Occupied.Homes)

names(socio1) <- c("tract", "year", "medIncome", "percentOcc")

# get rid of some special characters
socio1$medIncome <- gsub("[,$]", "", socio1$medIncome)
socio1$medIncome <- as.numeric(gsub(" ", "", socio1$medIncome))

socio1$percentOcc <- gsub("\\%", "", socio1$percentOcc)
socio1$percentOcc <- as.numeric(socio1$percentOcc)

socio1$tract <- format(socio1$tract, width = 6)
socio1$tract <- gsub(" ", "0", socio1$tract)


# now deal with rest of the data
socio2$year <- 2012
socio3$year <- 2013
socio4$year <- 2014
socio5$year <- 2015
socio6$year <- 2016
socio7$year <- 2017

allSocio <- rbind(socio2[, c("TRACT", "year", "Estimated.Median.Family.Income",
                             "Percent.of.Owner.Occupied.Homes")],
                  socio3[, c("TRACT", "year", "Estimated.Median.Family.Income",
                             "Percent.of.Owner.Occupied.Homes")],
                  socio4[, c("TRACT", "year", "Estimated.Median.Family.Income",
                             "Percent.of.Owner.Occupied.Homes")],
                  socio5[, c("TRACT", "year", "Estimated.Median.Family.Income",
                             "Percent.of.Owner.Occupied.Homes")],
                  socio6[, c("TRACT", "year", "Estimated.Median.Family.Income",
                             "Percent.of.Owner.Occupied.Homes")],
                  socio7[, c("TRACT", "year", "Estimated.Median.Family.Income",
                             "Percent.of.Owner.Occupied.Homes")])

allSocio <- allSocio %>% 
  dplyr::select(TRACT, year, Estimated.Median.Family.Income, 
                Percent.of.Owner.Occupied.Homes)

names(allSocio) <- c("tract", "year", "medIncome", "percentOcc")

# get rid of some special characters
allSocio$medIncome <- gsub("[,$]", "", allSocio$medIncome)
allSocio$medIncome <- as.numeric(gsub(" ", "", allSocio$medIncome))

allSocio$percentOcc <- gsub("\\%", "", allSocio$percentOcc)
allSocio$percentOcc <- as.numeric(allSocio$percentOcc)

allSocio$tract <- format(allSocio$tract, width = 6)
allSocio$tract <- gsub("\\.", "", allSocio$tract)
allSocio$tract <- gsub(" ", "0", allSocio$tract)

allSocio$year <- as.factor(allSocio$year)

allSocio <- rbind(socio1, allSocio)

# census tracts-----------------------------------------------------------------

# calculate area of each tract
#https://gis.stackexchange.com/questions/200420/calculate-area-for-each-polygon-in-r
tracts$area_sqkm <- area(tracts) / 1000000

tractAreas <- as.data.frame(tracts$tractce10)
tractAreas$area <- tracts$area_sqkm
names(tractAreas)[1] <- "tract"

# calculate population of each tract (from 2010 census block data)
# want to add the populations for all the census blocks in a tract together
pop$CENSUS.BLOCK.FULL <- as.factor(pop$CENSUS.BLOCK.FULL)
pop$tract <- substr(pop$CENSUS.BLOCK.FULL, 6, 11)

tractPop <- pop %>% 
  group_by(tract) %>% 
  summarise(pop = sum(TOTAL.POPULATION))

# combine data together---------------------------------------------------------

# use bind_cols to just bind because these all are in same order and same rows
fullDat <- dplyr::bind_cols(c(ratsCTQY, 
                              buildCTQY[, c("construction", "demolition")], 
                              sancompsCTQY[, c("feces", "garbage", "sanOther")]))

# add in food data (by year)
fullDat <- left_join(fullDat, foodCTY, 
                     by = c("tract" = "tract", "year" = "year"))

# add socioeconomic data (by tract and year)
fullDat <- left_join(fullDat, allSocio, 
                     by = c("tract" = "tract", "year" = "year"))

# add human population
fullDat <- left_join(fullDat, tractPop, by = "tract")

# add tract area (sqkm)
fullDat <- left_join(fullDat, tractAreas, by = "tract")

# remove any NA values
# there are two tracts (980000 and 980100) that are part of Ohare and Midway
# no socioeconomic data for them
fullDat <- fullDat[complete.cases(fullDat), ]

# one tract in 2011 has 0 population...let's remove
fullDat <- fullDat %>% 
  filter(pop > 0)

# save dataset to use for models
#write.csv(fullDat, "./Data/ratCompPredsCT.csv")

# plotting rat complaints and predictors (in 2017)------------------------------
library(ggplot2)
library(sf)

CA_sf <- st_read("./Data/GIS/chicagoCommAreas.shp")

# rat complaints
ggplot() + 
  geom_sf(data = CA_sf, color = "black", fill = "gray90") + 
  geom_point(data = subset(ratShort, year == 2017), 
             aes(x = Longitude, y = Latitude), 
             shape = 19, color = "red", size = 0.5) +
  ggtitle("Rat complaints 2017") +
  theme_bw() +
  coord_sf() 

# ggsave(filename = "Rat complaints 2017.png", device = "png",
#        path = "./Figures/", dpi = 600, width = 7, height = 7, units = "in")

# restaurants and other food establishments
ggplot() + 
  geom_sf(data = CA_sf, color = "black", fill = "gray90") + 
  geom_point(data = subset(foodShort, year == 2017), 
             aes(x = Longitude, y = Latitude), 
             shape = 19, color = "red", size = 0.5) +
  ggtitle("Restaurants & food establishments 2017") +
  theme_bw() +
  coord_sf() 

# ggsave(filename = "Food establishments 2017.png", device = "png",
#        path = "./Figures/", dpi = 600, width = 7, height = 7, units = "in")

# construction and demolition permits
ggplot() + 
  theme_bw() +
  geom_sf(data = CA_sf, color = "black", fill = "gray90") + 
  geom_point(data = subset(bpShort, year == 2017), 
             aes(x = LONGITUDE, y = LATITUDE, colour = PERMIT_TYPE),  
             shape = 19, size = 1) +
  scale_color_manual(values = c("#984ea3", "#4daf4a"),
                     labels = c("Construction", "Demolition")) +
  ggtitle("Construction and demolition permits 2017") +
  theme(legend.position = c(0.25, 0.16)) +
  coord_sf() 

# ggsave(filename = "Construction and demolition permits 2017.png",
#        device = "png", path = "./Figures/", dpi = 600, width = 7, height = 7,
#        units = "in")

# sanitation complaints
ggplot() + 
  theme_bw() +
  geom_sf(data = CA_sf, color = "black", fill = "gray90") + 
  geom_point(data = sanShort %>% 
               filter(year == 2017) %>% 
               filter(violType == "Garbage"), 
             aes(x = Longitude, y = Latitude),  
             shape = 19, size = 1, color = "yellow") +
  geom_point(data = sanShort %>% 
               filter(year == 2017) %>% 
               filter(violType == "Dog feces"), 
             aes(x = Longitude, y = Latitude),  
             shape = 19, size = 1, color = "red") +
  scale_color_manual(labels = c("Garbage", "Dog feces")) + 
  ggtitle("Sanitation complaints 2017") +
  theme(legend.position = c(0.23, 0.16)) +
  coord_sf() 


# ggplot() + 
#   theme_bw() +
#   geom_sf(data = CA_sf, color = "black", fill = "gray90") + 
#   geom_point(data = sanShort %>% 
#                filter(year == 2017) %>% 
#                filter(violType == "Garbage" | violType == "Dog feces"), 
#              aes(x = Longitude, y = Latitude, color = violType),  
#              shape = 19, size = 1) +
#   scale_color_manual(values = c("red", "yellow"),
#                      labels = c("Dog feces", "Garbage")) + 
#   ggtitle("Sanitation complaints 2017") +
#   theme(legend.position = c(0.23, 0.16)) +
#   coord_sf() 

# ggsave(filename = "Sanitation complaints 2017.png",
#        device = "png", path = "./Figures/", dpi = 600, width = 7, height = 7,
#        units = "in")
