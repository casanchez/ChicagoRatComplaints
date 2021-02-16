# Code to accompany Sanchez et al. 2021. Social and environmental correlates
# of rat complaints in Chicago. Journal of Urban Ecology. doi: 10.1093/jue/juab006

# This script cleans complaint data (rat complaints, garbage complaints, dog 
# feces complaints), restaurant inspection data, and building permit data.
# It generates the number of rat complaints in each Chicago census tract 
# per quarter, per year, from 2011-2017

# The raw data are not uploaded to Github because they are publicly available
# Links are provided here and in the manuscript
# But the cleaned datasets are in the DataCleaned folder

# load packages-----------------------------------------------------------------
library(dplyr)
library(sp)
library(rgdal)
library(fitdistrplus)
library(glmmTMB)
library(forcats)
library(ggplot2)
library(sf)

'%!in%' <- function(x,y)!('%in%'(x,y))

# load data---------------------------------------------------------------------

options(digits = 6) # helps for viewing census tract numbers

# https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Rodent-Baiting-No-Duplicates/uqhs-j723
# downloaded 6/28/2019
rats <- read.csv("./DataRaw/311_Service_Requests_-_Rodent_Baiting_-_No_Duplicates.csv", 
                 header = TRUE, na.strings = "")

# https://data.cityofchicago.org/Buildings/Building-Permits/ydr8-5enu
# downloaded 6/27/2019
bPermits <- read.csv("./DataRaw/Building_Permits.csv", header = TRUE, 
                     na.strings = "")

# https://data.cityofchicago.org/Service-Requests/311-Service-Requests-Sanitation-Code-Complaints-No/rccf-5427
# downloaded 6/28/2019
san <- read.csv("./DataRaw/311_Service_Requests_-_Sanitation_Code_Complaints_-_No_Duplicates.csv", 
                header = TRUE, na.strings = "")

# https://data.cityofchicago.org/Health-Human-Services/Food-Inspections/4ijn-s7e5
# downloaded 8/1/2019
food <- read.csv("./DataRaw/Food_Inspections.csv", header = TRUE, 
                 na.strings = "")

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Tracts-2010/5jrd-6zik
# downloaded 8/13/2019
tracts <- readOGR("./DataRaw/GIS", "chicagoCensusTracts2010")

# rat complaints data-----------------------------------------------------------

# add month/quarter/year cols, restrict dates
# restrict to most recent action = inspected and baited
ratShort <- rats %>% 
  rename(Creation.Date = ï..Creation.Date) %>% 
  filter(!is.na(Latitude)) %>% 
  mutate_at(vars(Creation.Date), ~as.Date(., "%m/%d/%Y")) %>% 
  filter(Creation.Date >= as.Date("2011-01-01") &
           Creation.Date <= as.Date("2017-12-31")) %>% 
  filter(Most.Recent.Action == "Inspected and baited") %>% 
  mutate(month = format(Creation.Date, "%m")) %>% 
  mutate(quarter = as.factor(month)) %>% 
  mutate(quarter = fct_collapse(quarter, 
                                Q1 = c("01", "02", "03"), 
                                Q2 = c("04", "05", "06"),
                                Q3 = c("07", "08", "09"),
                                Q4 = c("10", "11", "12"))) %>% 
  mutate(year = format(Creation.Date, "%Y")) %>% 
  mutate_at(vars(year), as.factor)

# pull out coordinates of rat complaints
ratPts <- SpatialPoints(ratShort[, c("Longitude", "Latitude")], 
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

# extract census tract that each rat complaint belongs to
ratPts2 <- over(ratPts, tracts)

# store unique census tract id back with rat data
ratShort$tract <- ratPts2$tractce10

# calculate number of rat complaints per tract, each quarter of each year
ratsCTQY <- ratShort %>% 
  filter(!is.na(tract)) %>%  # remove points not assigned to a tract
  group_by(tract, year, quarter, .drop = FALSE) %>% 
  summarize(ratcomp = n())

#write.csv(ratsCTQY, "./DataCleaned/ratsCTQY.csv", row.names = FALSE)

# building permit data----------------------------------------------------------

bpShort <- bPermits %>% 
  mutate_at(vars(ISSUE_DATE), ~as.Date(., "%m/%d/%Y")) %>% 
  mutate(month = format(ISSUE_DATE, "%m")) %>% 
  mutate(quarter = as.factor(month)) %>% 
  mutate(quarter = fct_collapse(quarter, 
                                Q1 = c("01", "02", "03"), 
                                Q2 = c("04", "05", "06"),
                                Q3 = c("07", "08", "09"),
                                Q4 = c("10", "11", "12"))) %>% 
  mutate(year = format(ISSUE_DATE, "%Y")) %>% 
  filter(PERMIT_TYPE %in% c("PERMIT - NEW CONSTRUCTION", 
                            "PERMIT - WRECKING/DEMOLITION")) %>%
  mutate(PERMIT_TYPE = fct_drop(PERMIT_TYPE)) %>% 
  filter(ISSUE_DATE >= as.Date("2011-01-01") &
           ISSUE_DATE <= as.Date("2017-12-31")) %>% 
  filter(!is.na(LONGITUDE)) %>% 
  mutate_at(vars(year), as.factor) %>% 
  dplyr::select(PERMIT_TYPE, LATITUDE, LONGITUDE, month:year)

# need to extract the census tract
bpPts <- SpatialPoints(bpShort[, c("LONGITUDE", "LATITUDE")], 
                       proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

bpPts2 <- over(bpPts, tracts)
bpShort$tract <- bpPts2$tractce10

# calculate number of permits issued per tract, each quarter of each year
buildCTQY <- bpShort %>% 
  filter(!is.na(tract)) %>% 
  group_by(tract, year, quarter, .drop = FALSE) %>% 
  summarize(bPerm = n())

#write.csv(buildCTQY, "./DataCleaned/buildCTQY.csv", row.names = FALSE)

# sanitation complaint data-----------------------------------------------------

sanShort <- san %>% 
  mutate(violType = fct_collapse(What.is.the.Nature.of.this.Code.Violation.,
                                 garbage = c("Garbage in alley", 
                                             "Garbage in yard",
                                             "Dumpster not being emptied",
                                             "Overflowing carts"),
                                 dogFeces = "Dog feces in yard",
                                 other = c("Construction Site Cleanliness/Fence",
                                           "Graffiti Commercial Vehicle",
                                           "Standing water",
                                           "Other"))) %>% 
  filter(violType %in% c("garbage", "dogFeces")) %>% 
  mutate(violType = fct_drop(violType)) %>% 
  mutate_at(vars(Creation.Date), ~as.Date(., "%m/%d/%Y")) %>% 
  mutate(month = format(Creation.Date, "%m")) %>% 
  mutate(quarter = as.factor(month)) %>% 
  mutate(quarter = fct_collapse(quarter, 
                                Q1 = c("01", "02", "03"), 
                                Q2 = c("04", "05", "06"),
                                Q3 = c("07", "08", "09"),
                                Q4 = c("10", "11", "12"))) %>% 
  mutate(year = format(Creation.Date, "%Y")) %>% 
  filter(Creation.Date >= as.Date("2011-01-01") & 
           Creation.Date <= as.Date("2017-12-31")) %>% 
  filter(!is.na(Longitude)) %>% 
  mutate_at(vars(year), as.factor) %>% 
  dplyr::select(violType, Latitude, Longitude, month:year)

# extract the census tract
sanPts <- SpatialPoints(sanShort[, c("Longitude", "Latitude")], 
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
sanPts2 <- over(sanPts, tracts)
sanShort$tract <- sanPts2$tractce10

# calculate number of different sanitation complaints per tract, each quarter of each year
sancompsCTQY <- sanShort %>% 
  filter(!is.na(tract)) %>% 
  group_by(tract, year, quarter, violType, .drop = FALSE) %>% 
  summarize(nviol = n()) %>% 
  tidyr::spread(violType, nviol)

#write.csv(sancompsCTQY, "./DataCleaned/sancompsCTQY.csv", row.names = FALSE)

# food inspections--------------------------------------------------------------

# using locations of food inspections as proxy for number of food places

foodShort <- food %>% 
  mutate_at(vars(Inspection.Date), ~as.Date(., "%m/%d/%Y")) %>% 
  mutate(month = format(Inspection.Date, "%m")) %>% 
  mutate(quarter = as.factor(month)) %>% 
  mutate(quarter = fct_collapse(quarter, 
                                Q1 = c("01", "02", "03"), 
                                Q2 = c("04", "05", "06"),
                                Q3 = c("07", "08", "09"),
                                Q4 = c("10", "11", "12"))) %>% 
  mutate(year = format(Inspection.Date, "%Y")) %>% 
  filter(Facility.Type == "Restaurant" &
           Inspection.Date >= as.Date("2011-01-01") &
           Inspection.Date <= as.Date("2017-12-31") &
           Inspection.Type %!in% c("O.B.", "Out of Business", "OUT OF BUSINESS",
                                 "out ofbusiness") &
           Results != "Business Not Located") %>% 
  filter(!is.na(Longitude)) %>% 
  mutate_at(vars(year), as.factor) %>% 
  dplyr::select(Address, Longitude, Latitude, month:year)

foodPts <- SpatialPoints(foodShort[, c("Longitude", "Latitude")], 
                         proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
foodPts2 <- over(foodPts, tracts)
foodShort$tract <- foodPts2$tractce10

# only aggregating by year b/c there probably isn't much quarterly change
# keeping only distinct addresses within a year, to avoid double-counting
# ie if there were two inspections of the same place within the same year
foodCTY <- foodShort %>% 
  filter(!is.na(tract)) %>% 
  group_by(tract, year, .drop = FALSE) %>%
  distinct(Address, .keep_all = TRUE) %>% 
  summarize(food = n())

#write.csv(foodCTY, "./DataCleaned/foodCTY.csv", row.names = FALSE)