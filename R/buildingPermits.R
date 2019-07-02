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

autoplot(chicago_proj) +
  geom_point(data = buildingShort, 
             aes(x = LONGITUDE, y = LATITUDE, colour = PERMIT_TYPE), 
             shape = 21, size = 0.5) +
  labs(title = "Building permit issue dates",
       subtitle = "Date:{frame_time}") +
  transition_time(ISSUE_DATE) +
  ease_aes('linear')

# not quite working because it skips over days where there weren't permits issued
# might need to aggregate data by month instead of day by day

# transition_time() which can be used with continuous variables such as year. With this transition it is not necessary to provide transition and state length as the “transition variable” provides this directly (e.g. it should take twice as long to transition between 1980 and 1990 compared to 2000 to 2005). 

# add a month/year column for aggregation
#buildingShort$monthyear <- format(buildingShort$ISSUE_DATE,"%m-%Y")

# number of points in quadrats--------------------------------------------------
# https://rspatial.org/analysis/8-pointpat.html

#devtools::install_github('rspatial/rspatial')
library(rspatial)

library(rgdal)


# need to figure out the height and width of chicago, so that I can figure out how many 150 cells fit it, and then use that to set up the dimensions of the raster


r <- raster(ncol=150, nrow=150)
extent(r) <- extent(chicago_sp)
r2 <- rasterize(chicago_sp, r)

# load shapefile as spatial polygons data frame
chicago_sp <- readOGR("./Data/GIS","chicagoBoundary")

# create rasterLayer object from shapefile
chicago_rast <- raster(chicago_sp)

# change resolution of raster to get more quadrats
res(chicago_rast) <- 0.00180723

# transfer values associated with object type spatial data (points, lines, polygons) to raster cells
# now the raster has attributes
chicago_rast <- rasterize(chicago_sp, chicago_rast)

# I want to reproject so we can get some units of measurement
# but can't project a raster that has no values
# chicago_proj <- projectRaster(chicago_rast, crs = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
# 
# res(chicago_rast) <- 150
# 
# chicago_rast <- rasterize(chicago_sp, chicago_rast)

plot(chicago_rast)

# create quadrats (spatial polygons) from the raster
quads <- as(chicago_rast, 'SpatialPolygons')

# overlay the quadrats on the map
plot(quads, add = TRUE)

# add locations of building permits
points(x = buildingShort$LONGITUDE, y = buildingShort$LATITUDE, col='red', cex=.5)

permitPoints <- SpatialPoints(coords = buildingShort[, c("LONGITUDE", 
                                                         "LATITUDE")], 
                              proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))

# use rasterize to count number of permits in each raster cell
# background = 0 assigns other points a zero value
nPermCell <- rasterize(coordinates(permitPoints), chicago_rast, fun = 'count', background = 0)

# plot number of permits, overlay chicago boundary
plot(nPermCell)
plot(chicago_sp, add = TRUE)

# exclude the areas outside chicago boundary
nPermCell2 <- mask(nPermCell, chicago_rast)
plot(nPermCell2)
plot(chicago_sp, add = TRUE)

# calculate frequency of permits/cell
freqPerm <- freq(nPermCell2, useNA = 'no')
freqPerm

# extract values to points------------------------------------------------------

# should be able to now overlay rat complaints and associate each point with the number of building permits for the corresponding quadrat

# would need to decide how to aggregate the data
# right now, all building permits from all years are overlaid
# but we could do month by month, for each year

data2011 <- filter(compShort, year == 2011)

ratpts2011 <- SpatialPoints(coords = data2011[, c("Longitude", "Latitude")], 
                                proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))

data2011$buildingPermits <- raster::extract(nPermCell2, ratpts2011)


