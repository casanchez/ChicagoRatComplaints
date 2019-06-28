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


