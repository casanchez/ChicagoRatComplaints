# compile data from the American Community Survey

library(dplyr)
library(tidyr)

setwd("./Data/ACS")

# SELECTED HOUSING CHARACTERISTICS 
# Survey/Program: American Community Survey
# TableID: DP04
# Product:
#   2018: ACS 5-Year Estimates Data Profiles 

#https://data.census.gov/cedsci/table?q=DP04%3A%20SELECTED%20HOUSING%20CHARACTERISTICS&tid=ACSDP5Y2018.DP04&hidePreview=true&vintage=2018 

# NOTE: by choosing % buildings built 1970 onward, 
# this changes the span of years depending on the year
# ie in 2011, 1970-2011 is 41 years
# but in 2017, the span is 47 years

# note that some of the category names change over the years
# which slightly changes which variables are selected

# year 2011
# DP04_0003PE = Percent!!HOUSING OCCUPANCY!!Vacant housing units
# DP04_0017PE = Percent!!YEAR STRUCTURE BUILT!!Built 2005 or later
# DP04_0018PE = Percent!!YEAR STRUCTURE BUILT!!Built 2000 to 2004
# DP04_0019PE = Percent!!YEAR STRUCTURE BUILT!!Built 1990 to 1999
# DP04_0020PE = Percent!!YEAR STRUCTURE BUILT!!Built 1980 to 1989
# DP04_0021PE = Percent!!YEAR STRUCTURE BUILT!!Built 1970 to 1979
# DP04_0045PE = Percent!!HOUSING TENURE!!Owner-occupied
# DP04_0077PE = Percent!!OCCUPANTS PER ROOM!!1.01 to 1.50
# DP04_0078PE = Percent!!OCCUPANTS PER ROOM!!1.51 or more
 
# year 2012, 2013, 2014
# DP04_0003PE = Percent!!HOUSING OCCUPANCY!!Vacant housing units
# DP04_0017PE = Percent!!YEAR STRUCTURE BUILT!!Built 2010 or later
# DP04_0018PE = Percent!!YEAR STRUCTURE BUILT!!Built 2000 to 2009
# DP04_0019PE = Percent!!YEAR STRUCTURE BUILT!!Built 1990 to 1999
# DP04_0020PE = Percent!!YEAR STRUCTURE BUILT!!Built 1980 to 1989
# DP04_0021PE = Percent!!YEAR STRUCTURE BUILT!!Built 1970 to 1979
# DP04_0045PE = Percent!!HOUSING TENURE!!Owner-occupied
# DP04_0077PE = Percent!!OCCUPANTS PER ROOM!!1.01 to 1.50
# DP04_0078PE = Percent!!OCCUPANTS PER ROOM!!1.51 or more

# year 2015, 2016, 2017
# DP04_0003PE = Percent!!HOUSING OCCUPANCY!!Total housing units!!Vacant housing units
# DP04_0017PE = Percent!!YEAR STRUCTURE BUILT!!Total housing units!!Built 2014 or later
# DP04_0018PE = Percent!!YEAR STRUCTURE BUILT!!Total housing units!!Built 2010 to 2013
# DP04_0019PE = Percent!!YEAR STRUCTURE BUILT!!Total housing units!!Built 2000 to 2009
# DP04_0020PE = Percent!!YEAR STRUCTURE BUILT!!Total housing units!!Built 1990 to 1999
# DP04_0021PE = Percent!!YEAR STRUCTURE BUILT!!Total housing units!!Built 1980 to 1989
# DP04_0022PE = Percent!!YEAR STRUCTURE BUILT!!Total housing units!!Built 1970 to 1979
# DP04_0046PE = Percent!!HOUSING TENURE!!Occupied housing units!!Owner-occupied
# DP04_0078PE = Percent!!OCCUPANTS PER ROOM!!Occupied housing units!!1.01 to 1.50
# DP04_0079PE = Percent!!OCCUPANTS PER ROOM!!Occupied housing units!!1.51 or more


temp <- list.files(pattern = "DP04_data_with")

myfunc <- function(file){
  df <- read.csv(file, header = TRUE, na.strings = c("-", "(X)", "**"), 
                 stringsAsFactors = FALSE) %>% 
    dplyr::slice(-1)
  
  if(grepl("2011|2012|2013|2014", file)){
    df <- df %>% 
      dplyr::select(GEO_ID, DP04_0003PE, DP04_0017PE, DP04_0018PE, DP04_0019PE,
                    DP04_0020PE, DP04_0021PE, DP04_0045PE, DP04_0077PE, 
                    DP04_0078PE) %>% 
      mutate_at(vars(DP04_0003PE:DP04_0078PE), as.numeric) %>% 
      mutate(percBuilt1970on = DP04_0017PE + DP04_0018PE + DP04_0019PE +
               DP04_0020PE + DP04_0021PE,
             percCrowded = DP04_0077PE + DP04_0078PE) %>% 
      rename(percVacantHU = DP04_0003PE, percOwnerOcc = DP04_0045PE)
  }
  
  else{
    df <- df %>% 
      dplyr::select(GEO_ID, DP04_0003PE, DP04_0017PE, DP04_0018PE, DP04_0019PE,
                    DP04_0020PE, DP04_0021PE, DP04_0022PE, DP04_0046PE,
                    DP04_0078PE, DP04_0079PE) %>% 
      mutate_at(vars(DP04_0003PE:DP04_0079PE), as.numeric) %>% 
      mutate(percBuilt1970on = DP04_0017PE + DP04_0018PE + DP04_0019PE +
               DP04_0020PE + DP04_0021PE + DP04_0022PE,
             percCrowded = DP04_0078PE + DP04_0079PE) %>% 
      rename(percVacantHU = DP04_0003PE, percOwnerOcc = DP04_0046PE)
  }
  
  df <- df %>%  
    mutate(tract = substr(GEO_ID, 15, 20)) %>% 
    mutate(year = substr(file, 8, 11)) %>% 
    dplyr::select(percVacantHU, percOwnerOcc, percBuilt1970on, percCrowded,
                  tract, year)
}

DP04 <- lapply(temp, myfunc) %>% 
   do.call("rbind", .)

                
# ACS DEMOGRAPHIC AND HOUSING ESTIMATES 
# Survey/Program: American Community Survey
# TableID: DP05
# Product:
#   2018: ACS 5-Year Estimates Data Profiles 


#https://data.census.gov/cedsci/table?d=ACS%205-Year%20Estimates%20Data%20Profiles&tid=ACSDP5Y2018.DP05&hidePreview=true

# in 2017 the % estimate for under 5 years is marked as DP05_0005PE

temp <- list.files(pattern = "DP05_data_with")

myfunc <- function(file){
  df <- read.csv(file, header = TRUE, na.strings = c("-", "(X)", "**"), 
                 stringsAsFactors = FALSE) %>% 
    dplyr::slice(-1)
  
  if(grepl("2017", file, fixed = TRUE)){
    df <- df %>% 
      dplyr::select(GEO_ID, DP05_0001E, DP05_0005PE) %>% 
      mutate_at(vars(DP05_0001E, DP05_0005PE), as.numeric) %>% 
      rename(totalPop = DP05_0001E, percUnder5y = DP05_0005PE)
  }
  
  else{
    df <- df %>% 
      dplyr::select(GEO_ID, DP05_0001E, DP05_0004PE) %>% 
      mutate_at(vars(DP05_0001E, DP05_0004PE), as.numeric) %>% 
      rename(totalPop = DP05_0001E, percUnder5y = DP05_0004PE)
  }
  
  df <- df %>% 
    mutate(tract = substr(GEO_ID, 15, 20)) %>% 
    mutate(year = substr(file, 8, 11)) %>% 
    dplyr::select(totalPop, percUnder5y, tract, year)
}

DP05 <- lapply(temp, myfunc) %>% 
  do.call("rbind", .)


# SELECTED SOCIAL CHARACTERISTICS IN THE UNITED STATES 
# Survey/Program: American Community Survey
# TableID: DP02
# Product:
#   2018: ACS 5-Year Estimates Data Profiles 

# https://data.census.gov/cedsci/table?d=ACS%205-Year%20Estimates%20Data%20Profiles&g=0400000US17.140000&tid=ACSDP5Y2018.DP02&hidePreview=true

temp <- list.files(pattern = "DP02_data_with")

myfunc <- function(file){
  df <- read.csv(file, header = TRUE, na.strings = c("-", "(X)", "**"), 
                 stringsAsFactors = FALSE) %>% 
    dplyr::slice(-1) %>% 
    dplyr::select(GEO_ID, DP02_0065PE) %>% 
    mutate_at(vars(DP02_0065PE), as.numeric) %>% 
    rename(percGradDegr = DP02_0065PE) %>% 
    mutate(tract = substr(GEO_ID, 15, 20)) %>% 
    mutate(year = substr(file, 8, 11)) %>% 
    dplyr::select(percGradDegr, tract, year)
}

DP02 <- lapply(temp, myfunc) %>% 
  do.call("rbind", .)

# SELECTED ECONOMIC CHARACTERISTICS 
# Survey/Program: American Community Survey
# TableID: DP03
# Product:
#   2018: ACS 5-Year Estimates Data Profiles 

#https://data.census.gov/cedsci/table?d=ACS%205-Year%20Estimates%20Data%20Profiles&tid=ACSDP5Y2018.DP03&hidePreview=true&vintage=2018

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
    dplyr::select(medHouseholdInc, tract, year)
}

DP03 <- lapply(temp, myfunc) %>% 
  do.call("rbind", .)

# join all tract-level ACS data-------------------------------------------------

allACS <- plyr::join_all(list(DP02, DP03, DP04, DP05), by = c("tract", "year"),
                         type = "full")

setwd("./../..")

#write.csv(allACS, "./Data/allACS.csv", row.names = FALSE)
