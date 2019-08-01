# complaints data---------------------------------------------------------------
complaints <- read.csv("./Data/cleanedComplaints.csv", header = TRUE)

complaints <- complaints[, -1]

# change formats
complaints$Creation.Date <- as.Date(complaints$Creation.Date, "%m/%d/%Y") 
complaints$Community.Area <- as.factor(complaints$Community.Area)

# add month and year columns for aggregation
complaints$month <- format(complaints$Creation.Date,"%m")
complaints$quarter <- as.factor(complaints$month)
levels(complaints$quarter) <- list(Q1 = c("01", "02", "03"), 
                                 Q2 = c("04", "05", "06"),
                                 Q3 = c("07", "08", "09"),
                                 Q4 = c("10", "11", "12"))
complaints$year <- format(complaints$Creation.Date,"%Y")

# shorten so we have only full years
compShort <- complaints %>% 
  filter(Creation.Date >= as.Date("2011-01-01")) %>% 
  filter(Creation.Date <= as.Date("2017-12-31"))

complaints$year <- as.factor(complaints$year)

# calculate number of rat complaints per community area, each quarter of each year
ratcompsCAQY <- compShort %>% 
  group_by(Community.Area, year, quarter, .drop = FALSE) %>% 
  summarize(ratcomp = n())

# building permit data----------------------------------------------------------

# big file, takes a while to load
bPermits <- read.csv("./Data/buildingPermits.csv", header = TRUE, 
                     na.strings = c(""))

# change formats
bPermits$ISSUE_DATE <- as.Date(bPermits$ISSUE_DATE, "%m/%d/%Y")
bPermits$Community.Areas <- as.factor(bPermits$Community.Areas)

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
  filter(!is.na(Community.Areas)) %>% 
  filter(!is.na(LONGITUDE))

bPermits$year <- as.factor(bPermits$year)
  
# drop unused levels
bpShort$PERMIT_TYPE <- droplevels(bpShort$PERMIT_TYPE)
bpShort$WORK_DESCRIPTION <- droplevels(bpShort$WORK_DESCRIPTION)

# calculate number of permits issued per community area, each quarter of each year
bpermsCAQY <- bpShort %>% 
  group_by(Community.Areas, year, quarter, .drop = FALSE) %>% 
  summarize(bperm = n())

# sanitation complaint data-----------------------------------------------------
sanitation <- read.csv("./Data/sanitationComplaints.csv", header = TRUE, 
                       na.strings = c(""))

# re-categorize sanitation complaints
sanitation$violType <- plyr::revalue(sanitation$What.is.the.Nature.of.this.Code.Violation.,
                                     c("Garbage in alley" = "Garbage",
                                       "Garbage in yard" = "Garbage",
                                       "Dumpster not being emptied" = "Garbage",
                                       "Overflowing carts" = "Garbage",
                                       "Dog feces in yard" = "Dog feces",
                                       
                                       "Construction Site Cleanliness/Fence" = "Other",
                                       "Graffiti Commercial Vehicle" = "Other",
                                       "Standing water" = "Other",
                                       "Other" = "Other"))
sanitation$violType <- as.character(sanitation$violType)
sanitation$violType[is.na(sanitation$violType)] <- "Not provided"
sanitation$violType <- as.factor(sanitation$violType)

# change formats
sanitation$Creation.Date <- as.Date(sanitation$Creation.Date, "%m/%d/%Y")

# add month and year columns for aggregation
sanitation$month <- format(sanitation$Creation.Date,"%m")
sanitation$quarter <- as.factor(sanitation$month)
levels(sanitation$quarter) <- list(Q1 = c("01", "02", "03"), 
                                 Q2 = c("04", "05", "06"),
                                 Q3 = c("07", "08", "09"),
                                 Q4 = c("10", "11", "12"))
sanitation$year <- format(sanitation$Creation.Date,"%Y")


# trim the timespan
sanShort <- sanitation %>%
  filter(Creation.Date >= as.Date("2011-01-01")) %>% 
  filter(Creation.Date <= as.Date("2017-12-31")) %>% 
  filter(!is.na(Community.Area)) %>% 
  filter(Community.Area > 0) %>% 
  filter(!is.na(Longitude))

sanShort$Community.Area <- as.factor(sanShort$Community.Area)
sanitation$year <- as.factor(sanitation$year)

# calculate number of different santiation complaints per community area, each quarter of each year
sancompsCAQY <- sanShort %>% 
  group_by(Community.Area, year, quarter, violType, .drop = FALSE) %>% 
  summarize(nviol = n()) %>% 
  spread(violType, nviol)

names(sancompsCAQY)[4:7] <- c("feces", "garbage", "sanNP", "sanOther")

# food inspections--------------------------------------------------------------

# using locations of inspections as proxy to count numbers of food places in each CA

# COMMUNITY AREAS NOT ACCURATE

food <- read.csv("./Data/foodInspections.csv", header = TRUE, na.strings = c(""))

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



food$year <- as.factor(food$year)



library(sp)
library(rgdal)
CAs <- readOGR("./Data/GIS","chicagoCommAreas")
pts <- SpatialPoints(foodShort[, c("Longitude", "Latitude")], 
                     proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))

pts2 <- over(pts, CAs)
foodShort$CAcalc <- pts2$area_num_1

foodCAY <- foodShort %>% 
  filter(!is.na(CAcalc)) %>% 
  group_by(CAcalc, year) %>% 
  distinct(Address, .keep_all = TRUE) %>% 
  summarize(nfood = n())

# socioeconomic data------------------------------------------------------------

# https://data.cityofchicago.org/Health-Human-Services/Census-Data-Selected-socioeconomic-indicators-in-C/kn9c-c2s2
socio <- read.csv("./Data/socioecInd.csv", header = TRUE, na.strings = c(""))

# combine data together---------------------------------------------------------

fullDat <- dplyr::bind_cols(c(ratcompsCAQY, bpermsCAQY[, "bperm"], 
                       sancompsCAQY[, c("feces", "garbage", "sanOther")]))

fullDat <- left_join(fullDat, socio, by = c("Community.Area" = "Community.Area.Number"))

# models------------------------------------------------------------------------

library(fitdistrplus)


library(glmmTMB)
m1 <- glmmTMB(ratcomp ~ bperm + feces + garbage + sanOther + (1|Community.Area), data = fullDat)
summary(m1)
