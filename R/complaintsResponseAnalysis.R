
# libraries---------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# load rat complaint and baiting data-------------------------------------------
complaints <- read.csv("./Data/ratComplaintsResponses.csv", header = TRUE, 
                       na.strings = c("", " ", "NA"))

# data formatting---------------------------------------------------------------
names(complaints)[1] <- "Creation.Date"

complaints$Community.Area <- as.factor(complaints$Community.Area)
complaints$Number.of.Premises.Baited <- as.numeric(complaints$Number.of.Premises.Baited)
complaints$Number.of.Premises.with.Garbage <- as.numeric(complaints$Number.of.Premises.with.Garbage)
complaints$Number.of.Premises.with.Rats <- as.numeric(complaints$Number.of.Premises.with.Rats)

# remove cases that weren't responded to
complaints <- complaints[!is.na(complaints$Completion.Date), ]

# only four complaints before 12/27/2010: remove
complaints <- subset(complaints, !(Service.Request.Number %in% c("10-01545326",
                                                                 "10-01233874",
                                                                 "06-01365993",
                                                                 "99-01925037")))

# there are multiple records with NA values...need to deal with these later
complaints <- complaints[complete.cases(complaints),]

# change complaint and responses to date format
complaints$Creation.Date <- as.Date(complaints$Creation.Date, "%m/%d/%Y") 
complaints$Completion.Date <- as.Date(complaints$Completion.Date, "%m/%d/%Y") 

# add a month/year column for aggregation
complaints$monthyear <- format(complaints$Creation.Date,"%m-%Y")
# add a year column for aggregation
complaints$year <- format(complaints$Creation.Date,"%Y")

# calculate time between complaint and response
complaints$Response.Time <- difftime(complaints$Completion.Date, 
                                     complaints$Creation.Date, units = "days")
complaints$Response.Time <- as.numeric(complaints$Response.Time)

# summary stats about dataset---------------------------------------------------

# total number of complaints
dim(complaints)[1]

# timespan of complaints
range(complaints$Creation.Date)

# quantiles of response times
quantile(complaints$Response.Time)

# summarize complaint and response data by community area
commAreas <- complaints %>%
  group_by(Community.Area) %>%
  summarise(totComplaints = n(),
            avgPremBaited = mean(Number.of.Premises.Baited),
            avgPremGarbage = mean(Number.of.Premises.with.Garbage),
            avgPremRats = mean(Number.of.Premises.with.Rats)) %>%
  arrange(desc(totComplaints))

test <- complaints %>%
  group_by(Creation.Date, Community.Area) %>%
  summarise(numComplaints = n())

# exploratory graphs: overall dataset-------------------------------------------

# barchart showing complaints over time
ggplot(data = complaints, aes(x = Creation.Date)) +
  geom_bar()

# barchart showing response times over time
# this is cumulative response time for complaints on each day, so not exactly what I want
# but it does seem like response times have gone down recently
ggplot(data = complaints, aes(x = Creation.Date, y = Response.Time)) +
  geom_bar(stat = "identity")

# trends in number of overall complaints
# don't have full year's worth of data for 2010 and 2018
ggplot(data = complaints, aes(x = year)) +
  geom_bar()

# correlations between premises baited, with garbage, and with rats
# by community area
plot(commAreas$avgPremBaited~commAreas$avgPremGarbage)
plot(commAreas$avgPremBaited~commAreas$avgPremRats)
plot(commAreas$avgPremRats~commAreas$avgPremGarbage)
# all positively correlated as we'd expect

# exploratory graphs: by community area-----------------------------------------

# barchart showing complaints by community area
ggplot(data = commAreas, aes(x = reorder(Community.Area, -totComplaints), y = totComplaints)) +
  geom_col()

# barchart showing complaints over time BY community area
# 77 community areas, so need to choose some subset
ggplot(data = subset(complaints, Community.Area %in% c(1:12)), 
       aes(x = Creation.Date)) +
  geom_bar() +
  facet_wrap(~Community.Area)