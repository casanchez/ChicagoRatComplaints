# Code to accompany Sanchez et al. 2021. Social and environmental correlates
# of rat complaints in Chicago. Journal of Urban Ecology. doi: 10.1093/jue/juab006

# This script compiles data from the American Community Survey

# load packages-----------------------------------------------------------------
library(dplyr)
library(tidyr)

setwd("./DataRaw/ACS")

# housing data------------------------------------------------------------------

# SELECTED HOUSING CHARACTERISTICS 
# Survey/Program: American Community Survey
# TableID: DP04
# Product:
#   2018: ACS 5-Year Estimates Data Profiles 

#https://data.census.gov/cedsci/table?q=DP04%3A%20SELECTED%20HOUSING%20CHARACTERISTICS&g=0500000US17031.140000&tid=ACSDP5Y2018.DP04&hidePreview=true
# NOTE: by choosing % buildings built 1970 onward, 
# this changes the span of years depending on the year
# ie in 2011, 1970-2011 is 41 years
# but in 2017, the span is 47 years

# note that some of the category names change over the years
# which slightly changes which variables are selected

# year 2011-2014
# DP04_0003PE = Percent!!HOUSING OCCUPANCY!!Vacant housing units
# DP04_0024PE = Percent!!YEAR STRUCTURE BUILT!!Built 1940 to 1949
# DP04_0025PE = Percent!!YEAR STRUCTURE BUILT!!Built 1939 or earlier
# DP04_0045PE = Percent!!HOUSING TENURE!!Owner-occupied
# DP04_0078PE = Percent!!OCCUPANTS PER ROOM!!1.51 or more

# year 2015, 2016, 2017
# DP04_0003PE = Percent!!HOUSING OCCUPANCY!!Total housing units!!Vacant housing units
# DP04_0025PE = Percent!!YEAR STRUCTURE BUILT!!Built 1940 to 1949
# DP04_0026PE = Percent!!YEAR STRUCTURE BUILT!!Built 1939 or earlier
# DP04_0046PE = Percent!!HOUSING TENURE!!Occupied housing units!!Owner-occupied
# DP04_0079PE = Percent!!OCCUPANTS PER ROOM!!Occupied housing units!!1.51 or more


temp <- list.files(pattern = "DP04_data_with")

myfunc <- function(file){
  df <- read.csv(file, header = TRUE, na.strings = c("-", "(X)", "**"), 
                 stringsAsFactors = FALSE) %>% 
    dplyr::slice(-1)
  
  if(grepl("2011|2012|2013|2014", file)){
    df <- df %>% 
      dplyr::select(GEO_ID, DP04_0003PE, DP04_0024PE, DP04_0025PE, DP04_0045PE,
                    DP04_0078PE) %>% 
      mutate_at(vars(DP04_0003PE:DP04_0078PE), as.numeric) %>% 
      mutate(pBuiltPre1950 = DP04_0024PE + DP04_0025PE) %>% 
      rename(pVacantHU = DP04_0003PE, pOwnerOcc = DP04_0045PE, 
             pOvercrowded = DP04_0078PE)
  }
  
  else{
    df <- df %>% 
      dplyr::select(GEO_ID, DP04_0003PE, DP04_0025PE, DP04_0026PE, DP04_0046PE,
                    DP04_0079PE) %>% 
      mutate_at(vars(DP04_0003PE:DP04_0079PE), as.numeric) %>% 
      mutate(pBuiltPre1950 = DP04_0025PE + DP04_0026PE) %>% 
      rename(pVacantHU = DP04_0003PE, pOwnerOcc = DP04_0046PE, 
             pOvercrowded = DP04_0079PE)
  }
  
  df <- df %>%  
    mutate(tract = substr(GEO_ID, 15, 20)) %>% 
    mutate(year = substr(file, 8, 11)) %>% 
    dplyr::select(pVacantHU, pOwnerOcc, pBuiltPre1950, pOvercrowded,
                  tract, year)
}

DP04 <- lapply(temp, myfunc) %>% 
   do.call("rbind", .)

# demographic data--------------------------------------------------------------

# ACS DEMOGRAPHIC AND HOUSING ESTIMATES 
# Survey/Program: American Community Survey
# TableID: DP05
# Product:
#   2018: ACS 5-Year Estimates Data Profiles 


#https://data.census.gov/cedsci/table?g=0500000US17031.140000&d=ACS%205-Year%20Estimates%20Data%20Profiles&tid=ACSDP5Y2018.DP05&hidePreview=true

# in 2017 the % estimate for under 5 years is marked as DP05_0005PE

temp <- list.files(pattern = "DP05_data_with")

myfunc <- function(file){
  df <- read.csv(file, header = TRUE, na.strings = c("-", "(X)", "**"), 
                 stringsAsFactors = FALSE) %>% 
    dplyr::slice(-1)
  
  if(grepl("2017", file, fixed = TRUE)){
    df <- df %>% 
      dplyr::select(GEO_ID, DP05_0001E, DP05_0005PE) %>% 
      mutate_at(vars(-GEO_ID), as.numeric) %>% 
      rename(totalPop = DP05_0001E, pUnder5y = DP05_0005PE)
  }
  
  else{
    df <- df %>% 
      dplyr::select(GEO_ID, DP05_0001E, DP05_0004PE) %>% 
      mutate_at(vars(-GEO_ID), as.numeric) %>% 
      rename(totalPop = DP05_0001E, pUnder5y = DP05_0004PE)
  }
  
  df <- df %>% 
    mutate(tract = substr(GEO_ID, 15, 20)) %>% 
    mutate(year = substr(file, 8, 11)) %>% 
    dplyr::select(-GEO_ID)
}

DP05 <- lapply(temp, myfunc) %>% 
  do.call("rbind", .)

# social data-------------------------------------------------------------------

# SELECTED SOCIAL CHARACTERISTICS IN THE UNITED STATES 
# Survey/Program: American Community Survey
# TableID: DP02
# Product:
#   2018: ACS 5-Year Estimates Data Profiles 

# https://data.census.gov/cedsci/table?g=0400000US17.140000&d=ACS%205-Year%20Estimates%20Data%20Profiles&tid=ACSDP5Y2018.DP02&hidePreview=true

temp <- list.files(pattern = "DP02_data_with")

myfunc <- function(file){
  df <- read.csv(file, header = TRUE, na.strings = c("-", "(X)", "**"), 
                 stringsAsFactors = FALSE) %>% 
    dplyr::slice(-1) %>% 
    dplyr::select(GEO_ID, DP02_0065PE) %>% 
    mutate_at(vars(DP02_0065PE), as.numeric) %>% 
    rename(pGradDegr = DP02_0065PE) %>% 
    mutate(tract = substr(GEO_ID, 15, 20)) %>% 
    mutate(year = substr(file, 8, 11)) %>% 
    dplyr::select(-GEO_ID)
}

DP02 <- lapply(temp, myfunc) %>% 
  do.call("rbind", .)

# economic data-----------------------------------------------------------------

# SELECTED ECONOMIC CHARACTERISTICS 
# Survey/Program: American Community Survey
# TableID: DP03
# Product:
#   2018: ACS 5-Year Estimates Data Profiles 

#https://data.census.gov/cedsci/table?g=0500000US17031.140000&d=ACS%205-Year%20Estimates%20Data%20Profiles&tid=ACSDP5Y2018.DP03&hidePreview=true

temp <- list.files(pattern = "DP03_data_with")

myfunc <- function(file){
  df <- read.csv(file, header = TRUE, na.strings = c("-", "(X)", "**"), 
                 stringsAsFactors = FALSE) %>% 
    dplyr::slice(-1) %>% 
    dplyr::select(GEO_ID, DP03_0062E) %>% 
    mutate_at(vars(DP03_0062E), as.numeric) %>% 
    rename(medHouseholdInc = DP03_0062E) %>% 
    mutate(tract = substr(GEO_ID, 15, 20)) %>% 
    mutate(year = substr(file, 8, 11)) %>% 
    dplyr::select(-GEO_ID)
}

DP03 <- lapply(temp, myfunc) %>% 
  do.call("rbind", .)

# adjust median household income for inflation: to 2017 dollars
# consumer price index data available here:
# https://www.bls.gov/cpi/research-series/r-cpi-u-rs-home.htm

CPI <- read.csv("./r-cpi-u-rs-allitems.csv")

CPI <- CPI %>% 
  dplyr::slice(40:46) %>% 
  dplyr::select(1, 14) %>% 
  rename(year = Consumer.Price.Index.Research.Series.Using.Current.Methods..CPI.U.RS.,
         annuAvg = X.12) %>% 
  mutate_at(vars(annuAvg), ~as.numeric(as.character(.x)))

DP03 <- DP03 %>% 
  mutate(adjHhInc = case_when(
    year == "2011" ~medHouseholdInc*(CPI[7, "annuAvg"]/CPI[1, "annuAvg"]),
    year == "2012" ~medHouseholdInc*(CPI[7, "annuAvg"]/CPI[2, "annuAvg"]),
    year == "2013" ~medHouseholdInc*(CPI[7, "annuAvg"]/CPI[3, "annuAvg"]),
    year == "2014" ~medHouseholdInc*(CPI[7, "annuAvg"]/CPI[4, "annuAvg"]),
    year == "2015" ~medHouseholdInc*(CPI[7, "annuAvg"]/CPI[5, "annuAvg"]),
    year == "2016" ~medHouseholdInc*(CPI[7, "annuAvg"]/CPI[6, "annuAvg"]),
    year == "2017" ~medHouseholdInc
  ))

# join all tract-level ACS data-------------------------------------------------

allACS <- plyr::join_all(list(DP02, DP03, DP04, DP05), by = c("tract", "year"),
                         type = "full")

setwd("./../..")

#write.csv(allACS, "./DataCleaned/allACS.csv", row.names = FALSE)
