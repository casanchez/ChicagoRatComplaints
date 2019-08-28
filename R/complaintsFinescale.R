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
food <- read.csv("./Data/foodInspections.csv", header = TRUE, na.strings = "")

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Blocks-2010/mfzt-js4n
blocks <- readOGR("./Data/GIS","chicagoCensusBlocks2010")

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
ratPts2 <- over(ratPts, blocks)

# store unique census block id back with rat data
ratShort$block <- ratPts2$geoid1

# remove points not assigned to a block
ratShort <- ratShort %>%
  filter(!is.na(block))

# calculate number of rat complaints per community area, each quarter of each year
ratsCBQY <- ratShort %>% 
  group_by(block, year, quarter, .drop = FALSE) %>% 
  summarize(ratcomp = n())

# calculate number of rat complaints per community area, each quarter
# ratsCBQ <- ratShort %>% 
#   group_by(block, quarter, .drop = FALSE) %>% 
#   summarize(ratcomp = n())

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

bpPts2 <- over(bpPts, blocks)
bpShort$block <- bpPts2$geoid1

# remove points not assigned to a block
bpShort <- bpShort %>%
  filter(!is.na(block))

bpShort$year <- as.factor(bpShort$year)

# calculate number of permits issued per community area, each quarter of each year

buildCBQY <- bpShort %>% 
  filter(PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION") %>% 
  group_by(block, year, quarter, .drop = FALSE) %>% 
  summarize(build = n())

demolCBQY <- bpShort %>% 
  filter(PERMIT_TYPE == "PERMIT - WRECKING/DEMOLITION") %>% 
  group_by(block, year, quarter, .drop = FALSE) %>% 
  summarize(demol = n())

# collapsing over years
# buildCBQ <- bpShort %>% 
#   filter(PERMIT_TYPE == "PERMIT - NEW CONSTRUCTION") %>% 
#   group_by(block, quarter, .drop = FALSE) %>% 
#   summarize(build = n())
# 
# demolCBQ <- bpShort %>% 
#   filter(PERMIT_TYPE == "PERMIT - WRECKING/DEMOLITION") %>% 
#   group_by(block, quarter, .drop = FALSE) %>% 
#   summarize(demol = n())

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
sanPts2 <- over(sanPts, blocks)
sanShort$block <- sanPts2$geoid1

sanShort <- sanShort %>%
  filter(!is.na(block))

sanShort$year <- as.factor(sanShort$year)

# calculate number of different sanitation complaints per community area, each quarter of each year
sancompsCBQY <- sanShort %>% 
  group_by(block, year, quarter, violType, .drop = FALSE) %>% 
  summarize(nviol = n()) %>% 
  tidyr::spread(violType, nviol)

names(sancompsCBQY)[4:7] <- c("feces", "garbage", "sanNP", "sanOther")

# collapsing over years
# sancompsCBQ <- sanShort %>% 
#   group_by(block, quarter, violType, .drop = FALSE) %>% 
#   summarize(nviol = n()) %>% 
#   tidyr::spread(violType, nviol)
# 
# names(sancompsCBQ)[3:6] <- c("feces", "garbage", "sanNP", "sanOther")

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
foodPts2 <- over(foodPts, blocks)
foodShort$block <- foodPts2$geoid1

# only aggregating by year, not quarter
# keeping only distinct addresses within a year, to avoid double-counting
foodCBY <- foodShort %>% 
  filter(!is.na(block)) %>% 
  group_by(block, year, .drop = FALSE) %>%
  distinct(Address, .keep_all = TRUE) %>% 
  summarize(food = n())

# community areas---------------------------------------------------------------

# areas <- as.data.frame(matrix(NA, nrow = 77, ncol = 2))
# names(areas) <- c("Community.Area", "sqm")
# areas$Community.Area <- comms$area_num_1
# areas$sqm <- comms$shape_area
# areas$sqkm <- areas$sqm/1000000

# combine data together---------------------------------------------------------

fullDat <- dplyr::bind_cols(c(ratsCBQY, 
                              buildCBQY[, "build"], demolCBQY[, "demol"], 
                              sancompsCBQY[, c("feces", "garbage", "sanOther")]))

fullDat <- left_join(fullDat, foodCBY, 
                     by = c("block" = "block", "year" = "year"))

fullDat <- left_join(fullDat, pop[, c("CENSUS.BLOCK.FULL", "TOTAL.POPULATION")],
                     by = c("block" = "CENSUS.BLOCK.FULL"))

fullDat$logPop <- log(fullDat$TOTAL.POPULATION + 1)

# remove any NA values
fullDat <- fullDat[complete.cases(fullDat), ]

# save dataset since it takes a little while to build from scratch
#write.csv(fullDat, "./Data/ratCompPredsCB.csv")

# plotting rat complaints and predictors (in 2017)------------------------------
library(ggplot2)
library(sf)


CA_sf <- st_read("./Data/GIS/chicagoCommAreas.shp")

# rat complaints
ggplot() + 
  geom_sf(data = CA_sf, color = "black", fill = "gray") + 
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
  geom_sf(data = CA_sf, color = "black", fill = "gray") + 
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
  geom_sf(data = CA_sf, color = "black", fill = "gray") + 
  geom_point(data = subset(bpShort, year == 2017), 
             aes(x = LONGITUDE, y = LATITUDE, colour = PERMIT_TYPE),  
             shape = 19, size = 1) +
  ggtitle("Construction and demolition permits 2017") +
  scale_color_discrete(labels = c("Construction", "Demolition")) +
  theme(legend.position = c(0.25, 0.16)) +
  coord_sf() 

# ggsave(filename = "Construction and demolition permits 2017.png",
#        device = "png", path = "./Figures/", dpi = 600, width = 7, height = 7,
#        units = "in")

# sanitation complaints
ggplot() + 
  theme_bw() +
  geom_sf(data = CA_sf, color = "black", fill = "gray") + 
  geom_point(data = sanShort %>% 
               filter(year == 2017) %>% 
               filter(violType == "Garbage"), 
             aes(x = Longitude, y = Latitude),  
             shape = 19, size = 1, color = "blue") +
  geom_point(data = sanShort %>% 
               filter(year == 2017) %>% 
               filter(violType == "Dog feces"), 
             aes(x = Longitude, y = Latitude),  
             shape = 19, size = 1, color = "red") +
  ggtitle("Sanitation complaints 2017") +
  theme(legend.position = c(0.23, 0.16)) +
  coord_sf() 


ggplot(df) +
  geom_point(aes(x = x, y = y, color = label,  size = size)) +
  geom_point(data = subset(df, label == 'point'),
             aes(x = x, y = y, color = label, size = size))

# ggsave(filename = "Sanitation complaints 2017.png",
#        device = "png", path = "./Figures/", dpi = 600, width = 7, height = 7,
#        units = "in")
