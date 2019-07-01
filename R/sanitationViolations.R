sanitation <- read.csv("./Data/sanitationComplaints.csv", header = TRUE, 
                     na.strings = c(""))

summary(sanitation$What.is.the.Nature.of.this.Code.Violation.)
# which sanitation issues are most likely to attract rats?

# are these groups too broad?
sanitation$violType <- plyr::revalue(sanitation$What.is.the.Nature.of.this.Code.Violation.,
                        c("Garbage in alley" = "Garbage",
                          "Garbage in yard" = "Garbage",
                          "Dumpster not being emptied" = "Garbage",
                          "Overflowing carts" = "Garbage",
                          "Construction Site Cleanliness/Fence" = "Other",
                          "Graffiti Commercial Vehicle" = "Other",
                          "Standing water" = "Other",
                          "Dog feces in yard" = "Other",
                          "Other" = "Other"))
sanitation$violType <- as.character(sanitation$violType)
sanitation$violType[is.na(sanitation$violType)] <- "Not provided"
sanitation$violType <- as.factor(sanitation$violType)

# convert dates to date format
sanitation$Creation.Date <- as.Date(sanitation$Creation.Date, "%m/%d/%Y")
sanitation$Completion.Date <- as.Date(sanitation$Completion.Date, "%m/%d/%Y")

# trim the timespan
sanShort <- sanitation %>%
  filter(Creation.Date >= as.Date("2011-01-01") & Creation.Date <= as.Date("2018-11-30"))

# calculate time between complaint and response
sanShort$Response.Time <- difftime(sanShort$Completion.Date, 
                                   sanShort$Creation.Date, units = "days")
sanShort$Response.Time <- as.numeric(sanShort$Response.Time)

quantile(sanShort$Response.Time)

# remove complaints without lat/long coordinates
sanShort <- sanShort[!is.na(sanShort$Longitude), ]
sanShort <- sanShort[!is.na(sanShort$Latitude), ]

# plotting complaints spatially-------------------------------------------------

# create spatial points object
sanPoints <- SpatialPoints(coords = sanShort[, c("Longitude", "Latitude")], 
                        proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))

# use open street maps to get local Chicago map
chicago_open <- openmap(upperLeft = c(bbox(extent(points)*1.1)[4], 
                                      bbox(extent(points)*1.1)[1]),
                        lowerRight = c(bbox(extent(points)*1.1)[2],
                                       bbox(extent(points)*1.1)[3]),
                        type = "bing")

# use UTM 16 projection
# test <- openproj(chicago_open, projection = "+proj=utm +zone=16 +ellps=WGS84 +datum=WGS84 +units=m +no_defs") 

# open map is in mercator projection by default
# project the map into lat long
chicago_proj <- openproj(chicago_open)

# plot complaints, colored by original code violation
autoplot(chicago_proj) +
  geom_point(data = sanShort, 
             aes(x = Longitude, y = Latitude, colour = What.is.the.Nature.of.this.Code.Violation.), 
             shape = 21, size = 0.5)

# plot complaints, colored by grouped code violation
autoplot(chicago_proj) +
  geom_point(data = sanShort, 
             aes(x = Longitude, y = Latitude, colour = violType), 
             shape = 21, size = 1)
