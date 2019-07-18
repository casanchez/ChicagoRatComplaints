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


# functions---------------------------------------------------------------------

# https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#overdispersion
overdisp_fun <- function(model) {
  rdf <- df.residual(model)
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}

# load rat complaint and baiting data-------------------------------------------

complaints <- read.csv("./Data/cleanedComplaints.csv", header = TRUE)

complaints <- complaints[, -1]

complaints$ZIP.Code <- as.factor(complaints$ZIP.Code)
complaints$Ward <- as.factor(complaints$Ward)
complaints$Police.District <- as.factor(complaints$Police.District)
complaints$Community.Area <- as.factor(complaints$Community.Area)

# date formatting
complaints$Creation.Date <- as.Date(complaints$Creation.Date, "%m/%d/%Y") 
complaints$Completion.Date <- as.Date(complaints$Completion.Date, "%m/%d/%Y") 

# calculate time between complaint and response
complaints$Response.Time <- difftime(complaints$Completion.Date, 
                                     complaints$Creation.Date, units = "days")
complaints$Response.Time <- as.numeric(complaints$Response.Time)

# add a year column for aggregation
complaints$year <- format(complaints$Creation.Date,"%Y")

# data subsets------------------------------------------------------------------

# 10 additional crews were added to DSS some time in April 2016
# so for now, let's examine only complaints from May 2016 onwards
# (in case responses changed drastically after adding more crews)
# and since we don't have full data for all of Dec 2016, let's cut short at Oct
compShort <- complaints %>% 
  filter(Completion.Date >= as.Date("2016-05-01")) %>% 
  filter(Completion.Date <= as.Date("2018-10-31"))

# "baiting" = rats observed, at least 1 premise baited and "Most.Recent.Action" is "inspected and baited"
baitings <- compShort %>% 
  filter(Number.of.Premises.with.Rats > 0) %>% 
  filter(Number.of.Premises.Baited >= 1) %>% 
  filter(Most.Recent.Action == "Inspected and baited")

# "non-baiting" = rats observed, 0 premises baited
# will ignore Most.Recent.Action for now
nonBaitings <- compShort %>% 
  filter(Number.of.Premises.Baited == 0) %>% 
  filter(Number.of.Premises.with.Rats > 0) 

# NOTE: there are some issues going on between descriptions and data
# can have "inspected and baited" but 0 premises baited
# or "no cause" but rats observed

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
plot(commAreas$avgPremBaited ~ commAreas$avgPremGarbage, ylim = c(0, 15))
plot(commAreas$avgPremBaited ~ commAreas$avgPremRats, ylim = c(0, 15))
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

library(geosphere)

# set several time windows to examine how complaints change pre/post baiting (days)
twindow <- c(7, 14, 30)

# set distance (meters)
radius <- 150

baitings$baited = "Yes"
nonBaitings$baited = "No"

combined <- rbind(baitings, nonBaitings)
combined$baited <- as.factor(combined$baited)

combined$preMed <- 0
combined$preShort <- 0
combined$postShort <- 0
combined$postMed <- 0
combined$postLong <- 0

# trim the complaints dataset to a month before and after the baitings
searchComps <- complaints %>% 
  filter(Creation.Date >= as.Date("2016-04-01")) %>% 
  filter(Creation.Date <= as.Date("2018-11-30"))
           
longs <- combined[, "Longitude"]
lats <- combined[, "Latitude"]
compDates <- combined[, "Completion.Date"]

start_time <- Sys.time()
for(i in 1:nrow(combined)){
  
  # obtain coordinates for focal baiting
  targetXY <- c(longs[i], lats[i])
  
  # when did the baiting occur?
  baitDate <- compDates[i]
  
  # complaints before the baiting (doesn't include day of baiting)
  bigWindowPre <- searchComps %>% 
    filter(Creation.Date >= baitDate - twindow[2] & Creation.Date < baitDate)
  
  if(dim(bigWindowPre)[1] > 0){
    
    distMPre <- distm(targetXY, bigWindowPre[, c("Longitude", "Latitude")], 
                      fun = distGeo)
    
    # if you set "& distMPre > 0", will exclude the focal complaint)
    radPtsPre <- bigWindowPre[which(distMPre <= radius), ]
    
    combined$preMed[i] <- nrow(radPtsPre)
    
    # filter to shorter time window
    radPtsPre2 <- radPtsPre %>% 
      filter(Creation.Date >= baitDate - twindow[1])
    
    combined$preShort[i] <- nrow(radPtsPre2)
  }
  
  # complaints after the baiting occured
  bigWindowPost <- searchComps %>% 
    filter(Creation.Date > baitDate & Creation.Date <= baitDate + twindow[3])
  
  if(dim(bigWindowPost)[1] > 0){
    
    distMPost <- distm(targetXY, bigWindowPost[, c("Longitude", "Latitude")], 
                       fun = distGeo)
    
    radPtsPost <- bigWindowPost[which(distMPost <= radius), ]
    
    combined$postLong[i] <- nrow(radPtsPost)
    
    # filter to medium time window
    radPtsPost2 <- radPtsPost %>% 
      filter(Creation.Date <= baitDate + twindow[2])
    
    combined$postMed[i] <- nrow(radPtsPost2)
    
    # filter to short time window
    radPtsPost3 <- radPtsPost %>% 
      filter(Creation.Date <= baitDate + twindow[1])
    
    combined$postShort[i] <- nrow(radPtsPost3)
  }
}
end_time <- Sys.time()

end_time - start_time


boxplot(combined[, c(19:23)])

library(vioplot)
vioplot(combined[, c(19:23)])

# add temperature data----------------------------------------------------------

# need to have some kind of factor to account for expected changes in complaints based on season
# ie if you make a complaint at peak complaint time, does the decrease reflect a typical seasonal change, or is it due to the baiting?
# we'll use daily temperature for now, since it roughly mirrors the cyclic nature of complaints

temps <- read.csv("./Data/dailyTemp.csv", header = TRUE, na.strings = "")
temps$DATE <- as.Date(temps$DATE, format = "%m/%d/%Y")

final <- left_join(combined, temps[, c("DATE", "TAVG")], 
                      by = c("Completion.Date" = "DATE"))

# write.csv(final, "./Data/prepostComplaints.csv")

# regression modeling-----------------------------------------------------------
library(glmmTMB)
library(effects)
library(sjPlot)

# NOTE: since it can take a while for rats to die, should we look at complaints 2 weeks post-baiting instead of one week?

# we have count data for our response variable (number of complaints post-baiting)
# usually modeled with Poisson or negative binomial distribution

# poisson
mPois <- glmmTMB(postShort ~ preShort + baited + TAVG + 
                   (1|Community.Area), family = poisson, data = final)
overdisp_fun(mPois)
# indicates overdispersion, suggesting negative binomial might be better

library(fitdistrplus)
hist(final$postShort)
x <- final$postShort
descdist(x, discrete = TRUE)
fit.nb <- fitdist(x, "nbinom")
plot(fit.nb)
# also suggests a negative binomial distribution

mNB <- glmmTMB(postShort ~ preShort + baited + TAVG + 
                   (1|Community.Area), family = nbinom2, data = final)
overdisp_fun(mNB)
# seems to be a lot better

# we also have a lot of zero values
# are they false or true zeros?

# true zero: no complaints because the baiting worked

# false zero: rats still there, but not observed. or rats there, but not complained about


# "Zero-inflated poisson regression is used to model count data that has an excess of zero counts. Further, theory suggests that the excess zeros are generated by a separate process from the count values and that the excess zeros can be modeled independently."

# "if the overdispersion in a Poisson GLM is caused by the excessive number of zeros, then the ZIP will take care of the overdispersion. But if the overdispersion is not caused by the zeros, then the ZIP is not the appropriate model either"

# suspect that ZINB will be better than ZIP, but can compare with likelihood ratio test

# zero inflated poisson
mZIP <- glmmTMB(postShort ~ preShort + baited + TAVG + (1|Community.Area), 
              zi = ~ preShort + baited + TAVG,
              family = poisson, data = final)

# zero inflated negative binomial
# nbinom2: variance increases quadratically with the mean
mZINB <- glmmTMB(postShort ~ preShort + baited + TAVG + (1|Community.Area), 
                zi = ~ preShort + baited + TAVG,
                family = nbinom2, data = final)

library(lmtest)
lrtest(mZIP, mZINB)

# indicates the ZINB is better

summary(mZINB)
# the zero-inflation model estimates the probability of an extra zero such that a positive contrast indicates a higher chance of absence (e.g. baitedYes < 0 means fewer absences in sites that were baited); this is the opposite of the conditional model where a positive contrast indicates a higher abundance (e.g. baitedYes >0 means higher abundances in sites that were baited)

plot_model(mZINB)
plot(allEffects(mZINB))


# can also fit hurdle models (zero-inflated)
hZIP <- glmmTMB(postShort ~ preShort + baited + TAVG + (1|Community.Area), 
               zi = ~ preShort + baited + TAVG,
               family = truncated_poisson, data = final)

hZINB <- glmmTMB(postShort ~ preShort + baited + TAVG + (1|Community.Area), 
                   zi = ~ preShort + baited + TAVG,
                   family = truncated_nbinom2, data = final)


library(bbmle)
# compare all the GLMMs
AICtab(mPois, mNB, mZIP, mZINB, hZIP, hZINB)

# zero-inflated negative binomial model is best by AIC

# plotting model results--------------------------------------------------------

# https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html

## quick and dirty plot
# To avoid marginalizing over or conditioning on random effects, we can refit the best model without the random effect of site; however, this is not ideal because it ignores the correlation within sites.
mZINBfe <- glmmTMB(postShort ~ preShort + baited + TAVG, 
                  zi = ~preShort + baited + TAVG, 
                  family= nbinom2, data = final)
newdata0 <- newdata <- unique(final[, c("preShort", "baited", "TAVG")])
# the predict function has a parameter "type" that specifies whether you want predictions from the conditional model, the zero-inflation model, or the expected response that combines both parts of the model
temp <- predict(mZINBfe, newdata, se.fit = TRUE, type = "response")
newdata$predFE = temp$fit
newdata$predFE.min = temp$fit-1.98*temp$se.fit
newdata$predFE.max = temp$fit+1.98*temp$se.fit

real <- plyr::ddply(final, ~ Community.Area + preShort + baited + TAVG, 
                    summarize, m = mean(postShort))

ggplot(newdata, aes(preShort, predFE)) + geom_point() +
  facet_wrap(~baited) +
  geom_errorbar(aes(ymin = predFE.min, ymax = predFE.max)) +
  geom_point(data=real, colour = "orange", aes(x=preShort, y=m)) +
  ylab("Average abundance \n including presences and absences")+
  xlab("pre Complaints")
# points represent site-specific average counts

## alternative prediction method
# we can predict at the population mode, by setting the random effects to zero

X.cond = model.matrix(lme4::nobars(formula(mZINB)[-2]), newdata0)
beta.cond = fixef(mZINB)$cond
pred.cond = X.cond %*% beta.cond

ziformula = mZINB$modelInfo$allForm$ziformula
X.zi = model.matrix(lme4::nobars(ziformula), newdata0)
beta.zi = fixef(mZINB)$zi
pred.zi = X.zi %*% beta.zi

#These are estimates of the linear predictors (i.e., predictions on the link scale: logit(prob) and log(cond)), not the predictions themselves. The easiest thing to do for the point estimates of the unconditional count (ucount) is to transform to the response scale and multiply:
pred.ucount = exp(pred.cond)*(1-plogis(pred.zi))

# For the standard errors/confidence intervals, we could use posterior predictive simulations (i.e. draw MVN samples from the parameter for the fixed effects). This conditions on/ignores uncertainty in the random-effect parameters.
library(MASS)
set.seed(101)
pred.condpar.psim = mvrnorm(1000, mu=beta.cond, Sigma=vcov(mZINB)$cond)
pred.cond.psim = X.cond %*% t(pred.condpar.psim)
pred.zipar.psim = mvrnorm(1000,mu=beta.zi,Sigma=vcov(mZINB)$zi)
pred.zi.psim = X.zi %*% t(pred.zipar.psim)
pred.ucount.psim = exp(pred.cond.psim)*(1-plogis(pred.zi.psim))
ci.ucount = t(apply(pred.ucount.psim,1,quantile,c(0.025,0.975)))
ci.ucount = data.frame(ci.ucount)
names(ci.ucount) = c("ucount.low","ucount.high")
pred.ucount = data.frame(newdata0, pred.ucount, ci.ucount)

# These predicted counts should be close to the median counts, so we plot them together to compare.
real.count = plyr::ddply(final, ~preShort + baited + TAVG, summarize, m=median(postShort), mu=mean(postShort))

ggplot(pred.ucount, aes(x=preShort, y=pred.ucount))+
  facet_wrap(~baited) +
  geom_point(shape=1, size=2)+
  geom_errorbar(aes(ymin=ucount.low, ymax=ucount.high))+
  geom_point(data=real.count, aes(x=preShort, y=m, colour=baited), 
             shape=0, size=2)+
  # geom_point(data=real.count, aes(x=preShort, y=mu, colour=baited), 
  #            shape=5, size=2)+
  ylab("Abundance \n including presences and absences")+
  xlab("pre Complaints")
# circles represent predicted unconditional counts at the mode (ie community area effect = 0) and error bars represent 95% CIs for that mode. Squares represent the observed median and diamonds represent observed means calculated across samples and community areas



## simulating from a fitted model 

# look at the distribution of simulated values from the best fitted model. for this we use the function simulate.glmmTMB


#https://cran.r-project.org/web/packages/glmmTMB/vignettes/sim.html
sims <- simulate(mZINB, seed = 1)
simdat <- final
simdat$postShort <- sims[[1]]
# simdata <- transform(simdat,
#                      )




sims <- simulate(mZINB, seed = 1, nsim = 10)
# this function returns a list of vectors. the list has one element for each simulation (nsim) and the vectors are the same shape as our response variable
simdatlist=lapply(sims, function(postShort){
  cbind(postShort, final[, c('preShort', 'baited', 'TAVG', 'Community.Area')])
})

# takes forever to run if nsim is high
simdatsums=lapply(simdatlist, function(x){
  plyr::ddply(x, ~preShort+baited+TAVG, summarize,
        absence=mean(postShort==0),
        mu=mean(postShort))
})

ssd=do.call(rbind, simdatsums)

# then we can plot them with the observations summarized in the same way
real = plyr::ddply(final, ~preShort+baited+TAVG, summarize,
             absence=mean(postShort==0),
             mu=mean(postShort))

ggplot(ssd, aes(x=absence, color=baited))+
  facet_wrap(~baited) +
  geom_density(adjust=4)+
  geom_point(data=real, aes(x=absence, y=preShort, color=baited), size=2)+
  xlab("Probability that complaints are not made (rats are not observed)") +
  ylab("pre complaints")
# simulated zero counts. densities are values from 10 datasets simulated from best fit model. Points represent the observed data

ggplot(ssd, aes(x=mu, color=baited))+
  geom_density(adjust=4)+
  geom_point(data=real, aes(x=mu, y=.5, color=baited), size=2)+
  xlab("Complaints including zeros")+ylab(NULL)

