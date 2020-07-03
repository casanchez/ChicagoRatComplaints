# load packages-----------------------------------------------------------------
library(dplyr)
library(sp)
library(rgdal)
library(fitdistrplus)
library(glmmTMB)
library(forcats)

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

# https://data.cityofchicago.org/Facilities-Geographic-Boundaries/Boundaries-Census-Tracts-2010/5jrd-6zik
tracts <- readOGR("./Data/GIS","chicagoCensusTracts2010")

# rat complaints data-----------------------------------------------------------

# add month/quarter/year cols, restrict dates
ratShort <- rats %>% 
  dplyr::select(-X) %>% 
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

#write.csv(ratsCTQY, "./Data/ratsCTQY.csv", row.names = FALSE)

# looking at rat complaints by quarter of each year (for table/figure)
# ratsQY <- ratShort %>%
#   filter(!is.na(tract)) %>%
#   group_by(year, quarter, .drop = FALSE) %>%
#   summarize(ratcomp = n())
# 
# ggplot(ratsQY) +
#   geom_line(aes(x = quarter, y = ratcomp, group = year, color = year), 
#             size = 2) +
#   theme_bw()

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

#write.csv(buildCTQY, "./Data/buildCTQY.csv", row.names = FALSE)

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

#write.csv(sancompsCTQY, "./Data/sancompsCTQY.csv", row.names = FALSE)

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

# only aggregating by year, not quarter,
# there probably isn't much quarter to quarter change
# keeping only distinct addresses within a year, to avoid double-counting
# ie if there were two inspections of the same place within the same year
foodCTY <- foodShort %>% 
  filter(!is.na(tract)) %>% 
  group_by(tract, year, .drop = FALSE) %>%
  distinct(Address, .keep_all = TRUE) %>% 
  summarize(food = n())

#write.csv(foodCTY, "./Data/foodCTY.csv", row.names = FALSE)


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
