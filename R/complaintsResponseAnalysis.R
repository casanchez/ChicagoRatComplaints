# analysis of rat baitings in response to complaints

# libraries---------------------------------------------------------------------
library(dplyr)
library(ggmap)
library(ggplot2)
library(sp)
library(sf)
library(OpenStreetMap)
library(raster)

#devtools::install_github("dgrtwo/gganimate")
#devtools::install_github("thomasp85/transformr")
#devtools::install_github("thomasp85/patchwork")
library(gganimate)
library(gifski)
library(magick)
library(patchwork)
library(transformr)
library(png)


# load rat complaint and baiting data-------------------------------------------
# large dataset, takes a few seconds to load

complaints <- read.csv("./Data/ratComplaintsResponses.csv", header = TRUE, 
                       na.strings = c(""))

# data formatting---------------------------------------------------------------

names(complaints)[1] <- "Creation.Date"

complaints$Community.Area <- as.factor(complaints$Community.Area)
complaints$Number.of.Premises.Baited <- as.numeric(complaints$Number.of.Premises.Baited)
complaints$Number.of.Premises.with.Garbage <- as.numeric(complaints$Number.of.Premises.with.Garbage)
complaints$Number.of.Premises.with.Rats <- as.numeric(complaints$Number.of.Premises.with.Rats)

# need to concatenate "most recent action" categories
summary(complaints$Most.Recent.Action)

complaints$MRA2 <- plyr::revalue(complaints$Most.Recent.Action,
                  c("Area Baited" = "Baited",
                    "Backyard serviced, contact made" = "Baited",
                    "Inspected and baited" = "Baited",
                    "Completed" = "Baited",
                    "Area inspected, no cause and no baiting" = "No cause",
                    "No contact, left door hanger" = "No contact",
                    "No contact/gate locked; left door hanger." = "No contact",
                    "Area inspected, no baiting, owner responsibility" = "Owner responsibility",
                    "Create Work Order" = "Other",
                    "Refer to Sanitation for Inspection" = "Other"))
complaints$MRA2 <- as.factor(complaints$MRA2)
        
# change complaint and responses to date format
complaints$Creation.Date <- as.Date(complaints$Creation.Date, "%m/%d/%Y") 
complaints$Completion.Date <- as.Date(complaints$Completion.Date, "%m/%d/%Y") 

# restrict time range
compShort <- complaints %>%
  filter(Creation.Date  >= as.Date("2011-01-01") & Creation.Date  <= as.Date("2018-11-30"))

# there are multiple records with NA values
# just exclude? only ~2% of dataset
compShort <- compShort[complete.cases(compShort), ]

# # add a month/year column for aggregation
# compShort$monthyear <- format(compShort$Creation.Date,"%m-%Y")
# add a year column for aggregation
compShort$year <- format(compShort$Creation.Date,"%Y")

# calculate time between complaint and response
compShort$Response.Time <- difftime(compShort$Completion.Date, 
                                    compShort$Creation.Date, units = "days")
compShort$Response.Time <- as.numeric(compShort$Response.Time)

baitings <- dplyr::filter(compShort, MRA2 == "Baited")
  
# summary stats about dataset---------------------------------------------------

# total number of complaints
dim(compShort)[1]

# quantiles of response times
quantile(compShort$Response.Time)

# summary of what the response is
summary(compShort$MRA2)/ dim(compShort)[1] * 100

# when baiting did occur, how many premises were baited?
compShort %>%
  filter(MRA2 == "Baited") %>%
  summarise(medPremBaited = median(Number.of.Premises.Baited),
            minPremBaited = min(Number.of.Premises.Baited),
            maxPremBaited = max(Number.of.Premises.Baited))

# summarize complaint and response data by community area
commAreas <- compShort %>%
  group_by(Community.Area) %>%
  summarise(totComplaints = n(),
            avgPremBaited = mean(Number.of.Premises.Baited),
            avgPremGarbage = mean(Number.of.Premises.with.Garbage),
            avgPremRats = mean(Number.of.Premises.with.Rats)) %>%
  arrange(desc(totComplaints))

# exploratory graphs: overall dataset-------------------------------------------

# barchart showing complaints by date
ggplot(data = compShort, aes(x = Creation.Date)) +
  geom_bar()

ggplot(data = baitings, aes(x = Creation.Date)) +
  geom_bar()

# histogram of response times
hist(compShort$Response.Time)

# barchart showing response times over time
# this is cumulative response time for complaints on each day, so not exactly what I want
# but it does seem like response times have gone down recently
ggplot(data = compShort, aes(x = Creation.Date, y = Response.Time)) +
  geom_bar(stat = "identity")

# trends in number of overall complaints
# only 11 months of data for 2018
ggplot(data = compShort, aes(x = year)) +
  geom_bar()

# correlations between premises baited, with garbage, and with rats
# by community area
plot(commAreas$avgPremBaited~commAreas$avgPremGarbage)
plot(commAreas$avgPremBaited~commAreas$avgPremRats)
plot(commAreas$avgPremRats~commAreas$avgPremGarbage)
# all positively correlated as we'd expect

# exploratory graphs: by community area-----------------------------------------
# a lot of these are hard to interpret because there are so many CAs
# also just the CA number is meaningless, would be better to tie it to something socioeconomic etc

# barchart showing complaints by community area
ggplot(data = commAreas, aes(x = reorder(Community.Area, -totComplaints), 
                             y = totComplaints)) +
  geom_col() +
  xlab("Community Area")

# stacked barchart showing proportion of complaints from each CA by year
ggplot(data = compShort, aes(x = year, fill = Community.Area)) +
  geom_bar()

# barchart showing complaints over time BY community area
# 77 community areas, so need to choose some subset
ggplot(data = subset(compShort, Community.Area %in% c(1:12)), 
       aes(x = Creation.Date)) +
  geom_bar() +
  facet_wrap(~Community.Area)

# plotting complaints spatially (using chicago boundary shapefile)--------------

chicagoBoundary <- st_read("./Data/GIS/chicagoBoundary.shp")

#options(gganimate.nframes = 100, gganimate.duration = 60)

# plot complaints
ggplot() + 
  geom_sf(data = chicagoBoundary, color = "black", fill = "gray") + 
  geom_point(data = compShort, 
             aes(x = Longitude, y = Latitude), 
             shape = 21, color = "red", size = 0.5) +
  coord_sf() 

# there have been complaints basically everywhere from 2011-2018, so not super informative
# let's make animated plot to show changes over time

# https://github.com/thomasp85/gganimate
# https://www.datanovia.com/en/blog/gganimate-how-to-create-plots-with-beautiful-animation-in-r/
# http://lenkiefer.com/2018/01/17/simple-animated-line-plot/
# https://towardsdatascience.com/create-animated-bar-charts-using-r-31d09e5841da 
# animated plot of complaints by date
# takes a little while to load
ggplot() + 
  geom_sf(data = chicagoBoundary, color = "black", fill = "gray") + 
  geom_point(data = compShort, 
             aes(x = Longitude, y = Latitude), 
             shape = 21, color = "red", size = 0.5) +
  coord_sf() +
  labs(title = "Rat complaints",
       subtitle = "Date:{frame_time}") +
  transition_time(Creation.Date) 

# next thing: would be nice to have side by side plot showing the seasonal, sinusoidal nature of complaints

# concatenate complaints by when they were created, calculate daily sum
compbyCD <- compShort %>%
  group_by(Creation.Date) %>%
  summarise(numComplaints = n())

# to have one animation over a static frame, need to call data from separate data frames
# https://stackoverflow.com/questions/51919498/latest-gganimate-how-to-have-a-fixed-plot-in-the-background

compbyCD2 <- compbyCD
names(compbyCD2) <- c("V1", "V2")

# points show up gradually
ggplot(data = compbyCD, aes(x = Creation.Date, y = numComplaints)) +
  geom_point(aes(group = seq_along(Creation.Date))) +
  transition_reveal(Creation.Date) 

# these are working animation wise, but aren't showing exactly what I need data wise:

# line is gradually revealed
ggplot(data = compbyCD, aes(x = Creation.Date, y = numComplaints)) +
  geom_line() +
  transition_reveal(Creation.Date)

# grey line static underneath, red line moves along
ggplot(data = compbyCD, aes(x = Creation.Date, y = numComplaints)) +
  geom_line(data = compbyCD2, aes(x = V1, y = V2), color = "grey") +
  geom_line(color = "red") +
  transition_time(Creation.Date)

# grey line static underneath, red line moves along and covers
ggplot(data = compbyCD, aes(x = Creation.Date, y = numComplaints)) +
  geom_path(data = compbyCD2, aes(x = V1, y = V2), color = "grey") +
  geom_path(color = "red") +
  transition_reveal(Creation.Date)

# plot two animations simultaneously--------------------------------------------

# https://github.com/thomasp85/gganimate/wiki/Animation-Composition

p1 <- ggplot() + 
  geom_sf(data = chicagoBoundary, color = "black", fill = "gray") + 
  geom_point(data = compShort, 
             aes(x = Longitude, y = Latitude), 
             shape = 21, color = "red", size = 0.5) +
  coord_sf() +
  labs(title = "Rat complaints",
       subtitle = "Date:{frame_time}") +
  transition_time(Creation.Date) 

p1_gif <- animate(p1, nframes = 300, duration = 60, width = 240, height = 240)

p2 <- ggplot(data = compbyCD, aes(x = Creation.Date, y = numComplaints)) +
  geom_point(aes(group = seq_along(Creation.Date))) +
  transition_reveal(Creation.Date) 

p2_gif <- animate(p2, nframes = 300, duration = 60, width = 240, height = 240)

p1_mgif <- image_read(p1_gif)
p2_mgif <- image_read(p2_gif)

new_gif <- image_append(c(p1_mgif[1], p2_mgif[1]))
for(i in 2:300){
  combined <- image_append(c(p1_mgif[i], p2_mgif[i]))
  new_gif <- c(new_gif, combined)
}

new_gif

# plotting complaints spatially (using openmap)-----------------------

# # create spatial points object
# points <- SpatialPoints(coords = compShort[, c("Longitude", "Latitude")], 
#                         proj4string = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +towgs84=0,0,0"))
# 
# # use open street maps to get local Chicago map
# chicago_open <- openmap(upperLeft = c(bbox(extent(points)*1.1)[4], 
#                                       bbox(extent(points)*1.1)[1]),
#                         lowerRight = c(bbox(extent(points)*1.1)[2],
#                                        bbox(extent(points)*1.1)[3]),
#                         type = "bing")
# 
# # open map is in mercator projection by default
# # project the map into lat long
# chicago_proj <- openproj(chicago_open)
# 
# # plot cases
# autoplot(chicago_proj) +
#   geom_point(data = compShort, 
#              aes(x = Longitude, y = Latitude), 
#              shape = 21, color = "red", size = 0.5)

# testing for spatial autocorrelation-------------------------------------------

# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/ 

# plotting hotspots (kernel density)--------------------------------------------
#https://stackoverflow.com/questions/45694234/hotspots-map-using-kernel-density-estimation-in-r

library(spatstat) 

# this is year to year, which is too coarse but code works at least

chicago_owin <- as(chicago_sp, "owin") 

# need to fix the color scale, because it changes from plot to plot
# this is getting there, but not quite there yet

densMin <- vector()
densMax <- vector()

for(i in 2011:2018){
  data <- filter(compShort, year == i)
  
  # create a point pattern
  dta <- ppp(data$Longitude, data$Latitude, 
             window = chicago_owin)
  
  dta <- density(dta)
  
  densMin <- c(densMin, min(dta))
  densMax <- c(densMin, max(dta))
}

breakpoints <- seq(min(densMin), max(densMax), length.out = 7)

for(i in 2011:2018){
  data <- filter(compShort, year == i)
  
  # create a point pattern
  dta <- ppp(data$Longitude, data$Latitude, 
             window = chicago_owin)
  
  dta <- density(dta)
  
  plot(dta, main = paste("Density plot of rat complaints:", i), 
       col = terrain.colors(6), breaks = breakpoints)
  plot(chicago_sp, add = TRUE)
}

# what happens post-baiting-----------------------------------------------------

# general idea of what we want to do:

# have a baiting point
# choose some buffer (around 150m, could play with it) to match rodent movement
# choose some time window
# measure the number of complaints in the buffer, in the time window before and after the baiting event