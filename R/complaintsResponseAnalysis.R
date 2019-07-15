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

# remove commas
complaints[, 'Number.of.Premises.Baited'] <- gsub(",","", complaints[, 'Number.of.Premises.Baited']) 
complaints[, 'Number.of.Premises.with.Garbage'] <- gsub(",","", complaints[, 'Number.of.Premises.with.Garbage']) 
complaints[, 'Number.of.Premises.with.Rats'] <- gsub(",","", complaints[, 'Number.of.Premises.with.Rats']) 

# change from character to numeric
complaints$Number.of.Premises.Baited <- as.numeric(complaints$Number.of.Premises.Baited)
complaints$Number.of.Premises.with.Garbage <- as.numeric(complaints$Number.of.Premises.with.Garbage)
complaints$Number.of.Premises.with.Rats <- as.numeric(complaints$Number.of.Premises.with.Rats)

# there are some issues going on between descriptions and data
# can have "inspected and baited" but 0 premises baited

# concatenate "most recent action" categories
summary(complaints$Most.Recent.Action)

# complaints$MRA2 <- plyr::revalue(complaints$Most.Recent.Action,
#                   c("Area Baited" = "Baited",
#                     "Backyard serviced, contact made" = "Baited",
#                     "Inspected and baited" = "Baited",
#                     "Completed" = "Baited",
#                     "Area inspected, no cause and no baiting" = "No cause",
#                     "No contact, left door hanger" = "No contact",
#                     "No contact/gate locked; left door hanger." = "No contact",
#                     "Area inspected, no baiting, owner responsibility" = "Owner responsibility",
#                     "Create Work Order" = "Other",
#                     "Refer to Sanitation for Inspection" = "Other"))
# complaints$MRA2 <- as.factor(complaints$MRA2)

xtabs(~complaints$Number.of.Premises.Baited + complaints$Most.Recent.Action)
        
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

# subset only complaints where the reponse was a baiting
baitings <- dplyr::filter(compShort, Number.of.Premises.Baited >= 1)
  
# summary stats about dataset---------------------------------------------------

# total number of complaints
dim(compShort)[1]

# quantiles of response times
quantile(compShort$Response.Time)

# summary of what the response is
summary(compShort$MRA2)/ dim(compShort)[1] * 100

# when baiting did occur, how many premises were baited?
baitings %>%
  summarise(medPremBaited = median(Number.of.Premises.Baited),
            minPremBaited = min(Number.of.Premises.Baited),
            maxPremBaited = max(Number.of.Premises.Baited))

# there are some crazy high numbers of premises baited...either some kind of data entry error or something else. should probably exclude at some point

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
plot(commAreas$avgPremBaited ~ commAreas$avgPremGarbage, ylim = c(0, 10))
plot(commAreas$avgPremBaited ~ commAreas$avgPremRats, ylim = c(0, 10))
plot(commAreas$avgPremRats ~ commAreas$avgPremGarbage)
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
# very hard to interpret with current color scheme
ggplot(data = compShort, aes(x = year, fill = Community.Area)) +
  geom_bar()

# barchart showing complaints over time BY community area
# 77 community areas, so need to choose some subset
# at first glance seems like the seasonal pattern over all Chicago can be seen in some of the community areas
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

# might need to have some kind of factor to account for expected changes in complaints based on season?
# ie if you make a complaint at peak complaint time, does the decrease reflect a typical seasonal change, or is it due to the baiting?
library(geosphere)

# set several time windows to examine how complaints change pre/post baiting (days)
twindow <- c(7, 14, 30)

# set distance (meters)
radius <- 150

baitings$compPreMed <- 0
baitings$compPreShort <- 0
baitings$compPostShort <- 0
baitings$compPostMed <- 0
baitings$compPostLong <- 0


# let's restrict the minimum date to May 2016 (10 more crews were supposedly added to DSS in April)
# and restrict the max date to the same time, two years later May 2018
# and then let's sample rows randomly, so we hopefully get a balanced picture through time

rowCt <- 4200

set.seed(26)
baitings2 <- baitings %>%
  filter(Creation.Date <= as.Date("2018-05-01") &
         Creation.Date >= as.Date("2016-05-01")) %>% 
  sample_n(rowCt)

# let's also trim compShort so the loop won't have to search through as much
compShort <- compShort %>% 
  filter(Creation.Date <= as.Date("2018-04-01") &
           Creation.Date >= as.Date("2016-06-01"))

longs <- baitings2[, "Longitude"]
lats <- baitings2[, "Latitude"]
compDates <- baitings2[, "Completion.Date"]


start_time <- Sys.time()
for(i in 1:nrow(baitings2)){
  
  # obtain coordinates for focal baiting
  targetXY <- c(longs[i], lats[i])
  
  # when did the baiting occur?
  baitDate <- compDates[i]
  
  # complaints before the baiting (doesn't include day of baiting)
  bigWindowPre <- compShort %>% 
    filter(Creation.Date >= baitDate - twindow[2] & Creation.Date < baitDate)
  
  if(dim(bigWindowPre)[1] > 0){
    
    distMPre <- distm(targetXY, bigWindowPre[, c("Longitude", "Latitude")], 
                      fun = distGeo)
    
    # if you set "& distMPre > 0", will exclude the focal complaint)
    radPtsPre <- bigWindowPre[which(distMPre <= radius), ]
    
    baitings2$compPreMed[i] <- nrow(radPtsPre)
    
    # filter to shorter time window
    radPtsPre2 <- radPtsPre %>% 
      filter(Creation.Date >= baitDate - twindow[1])
    
    baitings2$compPreShort[i] <- nrow(radPtsPre2)
  }
  
  # complaints after the baiting occured
  bigWindowPost <- compShort %>% 
    filter(Creation.Date > baitDate & Creation.Date <= baitDate + twindow[3])
  
  if(dim(bigWindowPost)[1] > 0){
    
    distMPost <- distm(targetXY, bigWindowPost[, c("Longitude", "Latitude")], 
                       fun = distGeo)
    
    radPtsPost <- bigWindowPost[which(distMPost <= radius), ]
    
    baitings2$compPostLong[i] <- nrow(radPtsPost)
    
    # filter to medium time window
    radPtsPost2 <- radPtsPost %>% 
      filter(Creation.Date <= baitDate + twindow[2])
    
    baitings2$compPostMed[i] <- nrow(radPtsPost2)
    
    # filter to short time window
    radPtsPost3 <- radPtsPost %>% 
      filter(Creation.Date <= baitDate + twindow[1])
    
    baitings2$compPostShort[i] <- nrow(radPtsPost3)
  }
}
end_time <- Sys.time()

end_time - start_time


par(mfrow = c(1, 1))
boxplot(baitings2[, c(29:33)])

library(vioplot)
vioplot(baitings2[, c(29:33)])

# # set up non-baitings-----------------------------------------------------------
# 
# baitings3 <- baitings2 %>% 
#   dplyr::select(one_of(c("Longitude", "Latitude", "Completion.Date", 
#                          "Community.Area", "compPreMed", "compPreShort", 
#                          "compPostShort", "compPostMed", "compPostLong")))
# baitings3$baited <- "Yes"
# 
# # set up empty data frame to store the non-intervention (non-baiting) points
# nonBaitings <- as.data.frame(matrix(0, nrow = rowCt, ncol = 10))
# names(nonBaitings) <- c("Longitude", "Latitude", "Completion.Date", 
#                         "Community.Area", "compPreMed", "compPreShort", 
#                         "compPostShort", "compPostMed", "compPostLong", "baited")
# nonBaitings$baited <- "No"
# 
# # randomly sample points from within Chicago boundary
# # we'll have to refine this later
# library(rgdal)
# Chicago <- readOGR("./Data/GIS","chicagoBoundary")
# bgPts <- spsample(Chicago, rowCt, type = "random")
# nonBaitings[, 1:2] <- bgPts@coords
# 
# # obtain community area for each location
# CAs <- readOGR("./Data/GIS","chicagoCommAreas")
# pts <- SpatialPoints(bgPts@coords, proj4string = CRS("+proj=longlat +ellps=WGS84 +no_defs"))
# pts2 <- over(pts, CAs)
# nonBaitings$Community.Area <- pts2$area_num_1
# 
# # remove NA values (ie no community area)
# nonBaitings <- nonBaitings[complete.cases(nonBaitings), ]
# 
# # assign date to each point
# # at some point need to refine this to reflect non-uniform complaint creation during the year
# startDate <- min(baitings3$Completion.Date)
# endDate <- max(baitings3$Completion.Date)
# datesVec <- seq(startDate, endDate, by = 1)
# for (i in 1:nrow(nonBaitings)){
#   d <- sample(datesVec, 1)
#   nonBaitings$Completion.Date[i] <- d
# }
# nonBaitings$Completion.Date <- as.Date(nonBaitings$Completion.Date)
# 
# # calculate pre/post complaints
# 
# longs <- nonBaitings[, "Longitude"]
# lats <- nonBaitings[, "Latitude"]
# compDates <- nonBaitings[, "Completion.Date"]
# 
# 
# for(i in 1:nrow(nonBaitings)){
#   
#   # obtain coordinates for focal baiting
#   targetXY <- c(longs[i], lats[i])
#   
#   # when did the baiting occur?
#   baitDate <- compDates[i]
#   
#   # complaints before the baiting (doesn't include day of baiting)
#   bigWindowPre <- compShort %>% 
#     filter(Creation.Date >= baitDate - twindow[2] & Creation.Date < baitDate)
#   
#   if(dim(bigWindowPre)[1] > 0){
#     
#     distMPre <- distm(targetXY, bigWindowPre[, c("Longitude", "Latitude")], 
#                       fun = distGeo)
#     
#     # if you set "& distMPre > 0", will exclude the focal complaint)
#     radPtsPre <- bigWindowPre[which(distMPre <= radius), ]
#     
#     nonBaitings$compPreMed[i] <- nrow(radPtsPre)
#     
#     # filter to shorter time window
#     radPtsPre2 <- radPtsPre %>% 
#       filter(Creation.Date >= baitDate - twindow[1])
#     
#     nonBaitings$compPreShort[i] <- nrow(radPtsPre2)
#   }
#   
#   # complaints after the baiting occured
#   bigWindowPost <- compShort %>% 
#     filter(Creation.Date > baitDate & Creation.Date <= baitDate + twindow[3])
#   
#   if(dim(bigWindowPost)[1] > 0){
#     
#     distMPost <- distm(targetXY, bigWindowPost[, c("Longitude", "Latitude")], 
#                        fun = distGeo)
#     
#     radPtsPost <- bigWindowPost[which(distMPost <= radius), ]
#     
#     nonBaitings$compPostLong[i] <- nrow(radPtsPost)
#     
#     # filter to medium time window
#     radPtsPost2 <- radPtsPost %>% 
#       filter(Creation.Date <= baitDate + twindow[2])
#     
#     nonBaitings$compPostMed[i] <- nrow(radPtsPost2)
#     
#     # filter to short time window
#     radPtsPost3 <- radPtsPost %>% 
#       filter(Creation.Date <= baitDate + twindow[1])
#     
#     nonBaitings$compPostShort[i] <- nrow(radPtsPost3)
#   }
# }
# 
# boxplot(nonBaitings[, c(5:9)])
# vioplot(nonBaitings[, c(5:9)])
# 
# # regression modeling-----------------------------------------------------------
# allData <- rbind(baitings3, nonBaitings)
# allData$baited <- as.factor(allData$baited)
# 
# temps <- read.csv("./Data/dailyTemp.csv", header = TRUE, na.strings = "")
# temps$DATE <- as.Date(temps$DATE, format = "%m/%d/%Y")
# 
# allData2 <- left_join(allData, temps[, c("DATE", "TAVG")], 
#                       by = c("Completion.Date" = "DATE"))
# 
# library(glmmTMB)
# library(effects)
# 
# 
# M1 <- glmmTMB(compPostShort ~ compPreShort + baited + TAVG + (1|Community.Area), 
#               zi = ~ compPreShort + baited + TAVG,
#               family = poisson, data = allData2)
# summary(M1)
# plot(allEffects(M1))
# 
# M2 <- glmmTMB(compPostShort ~ compPreMed + baited + TAVG + (1|Community.Area), 
#               zi = ~ compPreMed + baited + TAVG,
#               family = poisson, data = allData2)
# summary(M2)
# 
# # interestingly, initially showing opposite effect of baiting than expected
# # could be that there are just no rats in the randomly selected points, whereas since the rat complaints are based on perception, the post-baiting number of rats is still going to be higher than in a place where there weren't any rats to begin with
# 
# # what if instead of the random points (which we haven't optimized how to choose), we use the natural data of non-interventions? Ie when a complaint is made but baiting doesn't occur. there wouldn't be as many cases, but it would probably be a more realistic comparison. and avoids all the issues with trying to generate semi-random complaints. but it brings us back to the problem of descriptions not matching up to numbers

# set up non-baitings-----------------------------------------------------------

baitings3 <- baitings2 %>% 
  dplyr::select(one_of(c("Longitude", "Latitude", "Completion.Date", 
                         "Community.Area", "compPreMed", "compPreShort", 
                         "compPostShort", "compPostMed", "compPostLong")))
baitings3$baited <- "Yes"

# non-intervention (non-baiting) points
# for now, will ignore the "most recent action" column and search for 0 "number of premises baited" and non-zero "number of premises with rats"
# can they bait without making contact?

nonBaitings <- complaints %>% 
  filter(Creation.Date <= as.Date("2018-05-01") &
           Creation.Date >= as.Date("2016-05-01")) %>% 
  filter(Number.of.Premises.Baited == 0) %>% 
  filter(Number.of.Premises.with.Rats > 0) 

# %>% 
#   filter(Most.Recent.Action == "Area inspected, no baiting, owner responsibility" | Most.Recent.Action == "No contact/gate locked; left door hanger." | Most.Recent.Action == "No contact, left door hanger")


nonBaitings$compPreMed <- 0
nonBaitings$compPreShort <- 0
nonBaitings$compPostShort <- 0
nonBaitings$compPostMed <- 0
nonBaitings$compPostLong <- 0
nonBaitings$baited <- "No"

# calculate pre/post response complaints

longs <- nonBaitings[, "Longitude"]
lats <- nonBaitings[, "Latitude"]
compDates <- nonBaitings[, "Completion.Date"]


for(i in 1:nrow(nonBaitings)){
  
  # obtain coordinates for focal baiting
  targetXY <- c(longs[i], lats[i])
  
  # when did the baiting occur?
  baitDate <- compDates[i]
  
  # complaints before the baiting (doesn't include day of baiting)
  bigWindowPre <- compShort %>% 
    filter(Creation.Date >= baitDate - twindow[2] & Creation.Date < baitDate)
  
  if(dim(bigWindowPre)[1] > 0){
    
    distMPre <- distm(targetXY, bigWindowPre[, c("Longitude", "Latitude")], 
                      fun = distGeo)
    
    # if you set "& distMPre > 0", will exclude the focal complaint)
    radPtsPre <- bigWindowPre[which(distMPre <= radius), ]
    
    nonBaitings$compPreMed[i] <- nrow(radPtsPre)
    
    # filter to shorter time window
    radPtsPre2 <- radPtsPre %>% 
      filter(Creation.Date >= baitDate - twindow[1])
    
    nonBaitings$compPreShort[i] <- nrow(radPtsPre2)
  }
  
  # complaints after the baiting occured
  bigWindowPost <- compShort %>% 
    filter(Creation.Date > baitDate & Creation.Date <= baitDate + twindow[3])
  
  if(dim(bigWindowPost)[1] > 0){
    
    distMPost <- distm(targetXY, bigWindowPost[, c("Longitude", "Latitude")], 
                       fun = distGeo)
    
    radPtsPost <- bigWindowPost[which(distMPost <= radius), ]
    
    nonBaitings$compPostLong[i] <- nrow(radPtsPost)
    
    # filter to medium time window
    radPtsPost2 <- radPtsPost %>% 
      filter(Creation.Date <= baitDate + twindow[2])
    
    nonBaitings$compPostMed[i] <- nrow(radPtsPost2)
    
    # filter to short time window
    radPtsPost3 <- radPtsPost %>% 
      filter(Creation.Date <= baitDate + twindow[1])
    
    nonBaitings$compPostShort[i] <- nrow(radPtsPost3)
  }
}

nonBaitings <- nonBaitings %>% 
  dplyr::select(one_of(c("Longitude", "Latitude", "Completion.Date", 
                         "Community.Area", "compPreMed", "compPreShort", 
                         "compPostShort", "compPostMed", "compPostLong", "baited")))


boxplot(nonBaitings[, c(5:9)])
vioplot(nonBaitings[, c(5:9)])

# regression modeling-----------------------------------------------------------
allData <- rbind(baitings3, nonBaitings)
allData$baited <- as.factor(allData$baited)

temps <- read.csv("./Data/dailyTemp.csv", header = TRUE, na.strings = "")
temps$DATE <- as.Date(temps$DATE, format = "%m/%d/%Y")

allData2 <- left_join(allData, temps[, c("DATE", "TAVG")], 
                      by = c("Completion.Date" = "DATE"))

library(glmmTMB)
library(effects)


M1 <- glmmTMB(compPostShort ~ compPreShort + baited + TAVG + (1|Community.Area), 
              zi = ~ compPreShort + baited + TAVG,
              family = poisson, data = allData2)
summary(M1)
plot(allEffects(M1))

M2 <- glmmTMB(compPostShort ~ compPreMed + baited + TAVG + (1|Community.Area), 
              zi = ~ compPreMed + baited + TAVG,
              family = poisson, data = allData2)
summary(M2)
plot(allEffects(M2))
