# libraries---------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# load building permit data-----------------------------------------------------
# big file, takes a while
building <- read.csv("./Data/buildingPermits.csv", header = TRUE, 
                       na.strings = c(""))

# convert dates to date format
building$ISSUE_DATE <- as.Date(building$ISSUE_DATE, "%m/%d/%Y")

# subset relevant columns, construction types, dates
buildingShort <- building %>%
  dplyr::select(ID:WORK_DESCRIPTION, LATITUDE:Wards) %>%
  filter(PERMIT_TYPE %in% c("PERMIT - NEW CONSTRUCTION", 
                            "PERMIT - WRECKING/DEMOLITION",
                            "PERMIT - PORCH CONSTRUCTION")) %>%
  filter(ISSUE_DATE >= as.Date("2011-01-01") & ISSUE_DATE <= as.Date("2018-11-30"))

#PERMIT TYPE: "New Construction and Renovation" includes new projects or rehabilitations of existing buildings; "Other Construction" includes items that require plans such as cell towers and cranes; "Easy Permit" includes minor repairs that require no plans; "Wrecking/Demolition" includes private demolition of buildings and other structures; "Electrical Wiring" includes major and minor electrical work both permanent and temporary; "Sign Permit" includes signs, canopies and awnings both on private property and over the public way; "Porch Permit" includes new porch construction and renovation (defunct permit type porches are now issued under "New Construction and Renovation" directly); "Reinstate Permit" includes original permit reinstatements; "Extension Permits" includes extension of original permit when construction has not started within six months of original permit issuance.

# remove permits without lat/long coordinates
buildingShort <- buildingShort[!is.na(buildingShort$LONGITUDE), ]
buildingShort <- buildingShort[!is.na(buildingShort$LATITUDE), ]

# exploratory plots-------------------------------------------------------------

# number of permits over time, colored by permit type 
ggplot(data = buildingShort, aes(x = ISSUE_DATE, fill = PERMIT_TYPE)) +
  geom_bar()

# autoplot(chicago_proj) +
#   geom_point(data = buildingShort, 
#              aes(x = LONGITUDE, y = LATITUDE, colour = PERMIT_TYPE), 
#              shape = 21, size = 0.5) +
#   labs(title = "Building permit issue dates",
#        subtitle = "Date:{frame_time}") +
#   transition_time(ISSUE_DATE) +
#   ease_aes('linear')

# not quite working because it skips over days where there weren't permits issued
# might need to aggregate data by month instead of day by day

# transition_time() which can be used with continuous variables such as year. With this transition it is not necessary to provide transition and state length as the “transition variable” provides this directly (e.g. it should take twice as long to transition between 1980 and 1990 compared to 2000 to 2005). 

# add a month/year column for aggregation
#buildingShort$monthyear <- format(buildingShort$ISSUE_DATE,"%m-%Y")

# rasterizing point data--------------------------------------------------------
# https://rspatial.org/analysis/8-pointpat.html

#devtools::install_github('rspatial/rspatial')
#library(rspatial)

library(rgdal)

# how to get the right "res" values to end up with a grid cell size of 150 m when projected
chicago_shp <- readOGR("./Data/GIS","chicagoBoundary")
chicago_rast <- raster(chicago_shp)
res(chicago_rast) <- c(0.01, 0.01)
chicago_rast <- rasterize(chicago_shp, chicago_rast)
chicago_proj <- projectRaster(chicago_rast, crs = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
newres <- 150/(res(chicago_proj)/0.01)

# r <- raster(nrow=292, ncol=242)
# extent(r) <- extent(chicago_shp)
# r2 <- rasterize(chicago_shp, r)

# load shapefile as spatial polygons data frame
chicago_shp <- readOGR("./Data/GIS","chicagoBoundary")

# create rasterLayer object from shapefile
chicago_rast <- raster(chicago_shp)

# change resolution so that grid cells will end up 150m by 150m (approx range of rat) once projected
res(chicago_rast) <- newres

# transfer values associated with object type spatial data (points, lines, polygons) to raster cells
# now the raster has attributes
chicago_rast <- rasterize(chicago_shp, chicago_rast)

# reproject the raster
chicago_proj <- projectRaster(chicago_rast, crs = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# I think these should be all yellow, like chicago_rast
# something might be going on...
plot(chicago_proj)

# locations where building permits were issued
permitPoints <- SpatialPoints(coords = buildingShort[, c("LONGITUDE", 
                                                         "LATITUDE")], 
                              proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))

# project to same crs so we can overlay
permPtsProj <- spTransform(permitPoints, CRS = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# looks like there is an issue...not aligning
plot(chicago_proj)
points(permPtsProj)

# use rasterize to count number of permits in each raster cell
# background = 0: value to put in the cells that are not covered by any of the features of permPtsProj
nPermCell <- rasterize(coordinates(permPtsProj), chicago_proj, fun = 'count', background = 0)

# plot number of permits, overlay chicago boundary
plot(nPermCell)
#plot(chicago_shp, add = TRUE)

test <- spTransform(chicago_shp, CRS = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")

# exclude the areas outside chicago boundary
nPermCell2 <- mask(nPermCell, chicago_proj)
plot(nPermCell2)
#plot(chicago_shp, add = TRUE)

# calculate frequency of permits/cell
freqPerm <- freq(nPermCell2, useNA = 'no')
freqPerm

# extract values to points------------------------------------------------------

# should be able to now overlay rat complaints and associate each point with the number of building permits for the corresponding quadrat

# would need to decide how to aggregate the data
# right now, all building permits from all years are overlaid
# but we could do month by month, for each year

# what would the code look like to loop through and extract values to points?
# also look at JP's Ebola code

for(i in 2011:2018){
   
  yearSubset <- filter(compShort, year == i)
  
  for(j in 1:12){
    monthSubset <- filter(compShort, month == j)
    
    rasterIJ <- paste0("sanitation_", j, "_", i, ".tif")
    
    r <- raster(rasterIJ)
    
    something <- raster::extract(r, monthSubset[, c("Longitude", "Latitude")])
    
    monthSubset <- cbind(monthSubset, something)
   
    listoflists[[i]][[j]] <- monthSubset
     
  }
}
