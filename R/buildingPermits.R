# libraries---------------------------------------------------------------------
library(dplyr)
library(ggplot2)

# load building permit data-----------------------------------------------------
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

# remove permits without lat/long coordinates
buildingShort <- buildingShort[!is.na(buildingShort$LONGITUDE), ]
buildingShort <- buildingShort[!is.na(buildingShort$LATITUDE), ]


#PERMIT TYPE: "New Construction and Renovation" includes new projects or rehabilitations of existing buildings; "Other Construction" includes items that require plans such as cell towers and cranes; "Easy Permit" includes minor repairs that require no plans; "Wrecking/Demolition" includes private demolition of buildings and other structures; "Electrical Wiring" includes major and minor electrical work both permanent and temporary; "Sign Permit" includes signs, canopies and awnings both on private property and over the public way; "Porch Permit" includes new porch construction and renovation (defunct permit type porches are now issued under "New Construction and Renovation" directly); "Reinstate Permit" includes original permit reinstatements; "Extension Permits" includes extension of original permit when construction has not started within six months of original permit issuance.

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
buildingShort$monthyear <- format(buildingShort$ISSUE_DATE,"%m-%Y")
test <- as.Date(buildingShort$monthyear, "%m-%Y")

"%m/%d/%Y"

test <- buildingShort %>%
  group_by()
