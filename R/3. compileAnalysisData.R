# Code to accompany Sanchez et al. 2021. Social and environmental correlates
# of rat complaints in Chicago. Journal of Urban Ecology. doi: 10.1093/jue/juab006

# This script assembles all of the data from the first two scripts
# so that it is ready for analysis

rm(list = ls())

library(rgdal)

# load cleaned datasets---------------------------------------------------------
ratsCTQY <- read.csv("./DataCleaned/ratsCTQY.csv")
buildCTQY <- read.csv("./DataCleaned/buildCTQY.csv")
sancompsCTQY <- read.csv("./DataCleaned/sancompsCTQY.csv")
foodCTY <- read.csv("./DataCleaned/foodCTY.csv")
allACS <- read.csv("./DataCleaned/allACS.csv")
tracts <- readOGR("./DataRaw/GIS", "chicagoCensusTracts2010")

# census tract areas------------------------------------------------------------

# calculate area of each tract
#https://gis.stackexchange.com/questions/200420/calculate-area-for-each-polygon-in-r
tracts$area_sqkm <- raster::area(tracts) / 1000000

tractAreas <- as.data.frame(tracts$tractce10)
tractAreas$area <- tracts$area_sqkm
names(tractAreas)[1] <- "tract"

# combine data together---------------------------------------------------------

fullDat <- plyr::join_all(list(ratsCTQY, buildCTQY, sancompsCTQY),
                          by = c("tract", "year", "quarter"), type = "left")

# add in restaurant data (by tract and year)
fullDat <- left_join(fullDat, foodCTY, by = c("tract", "year"))

# add ACS socioeconomic data (by tract and year)
fullDat <- left_join(fullDat, allACS, by = c("tract", "year"))


# add tract area (sqkm)
# need to format tract as character, place leading zero if it's only five digits
fullDat$tract <- format(fullDat$tract, width = 6)
fullDat$tract <- gsub(" ", "0", fullDat$tract)
fullDat <- left_join(fullDat, tractAreas, by = "tract")

# remove two tracts (980000 and 980100) that are part of O'Hare and Midway airports
# no socioeconomic data for them
final <- fullDat %>% 
  filter(!(tract %in% c("980000", "980100"))) %>% 
  filter(totalPop > 0) %>%  # remove any tract with no people
  mutate(popDens = totalPop/area) # calculate population density

# remove any remaining NAs
final <- final[complete.cases(final), ]

# save dataset to use for models
#write.csv(final, "./DataCleaned/dataformodels.csv", row.names = FALSE)