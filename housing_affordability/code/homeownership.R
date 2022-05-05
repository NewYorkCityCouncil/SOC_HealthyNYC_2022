## LIBRARIES -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor", "sf", "leaflet", "leaflet.extras", 
                      "htmlwidgets", "RSocrata", "tidycensus")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)

# returns TRUE if package was loaded successfully




library(tidycensus)
library(dplyr)
library(stringr)
library(janitor)
library(jsonlite)
library(RSocrata)
library(tidyr)

### REQUEST ------------
# Map of Homeownership rates across the city 

## OUTLINE TEXT for Reference ##

# Foreclosure - affordable homeownership tie in


# Homeownership rates across the city council district or NTA (Ben, Brook)
# Focus on home ownership or condos?
#   Rpad has condos
# Race and ownership
# How many people per geographic unit own homes? How many people own the space they are living in?
#   What types of homes are available in that region? In what numbers?
#   Is the data on owning the home you live in acs?
#   

# resources: https://cnycn.org/new-york-foreclosure-data-explained/
# https://furmancenter.org/thestoop/entry/snapshot-of-homeownership-in-new-york-city
# https://fred.stlouisfed.org/graph/?g=OPNG
# https://fred.stlouisfed.org/series/HOWNRATEACS036005#0
# https://coredata.nyc/
# https://wherewelive.cityofnewyork.us/explore-data/housing-conditions/
# https://therealdeal.com/new-research/industry-reports/nyc-homeownership-rate-by-racial-ethnic-group-streeteasy/
# maybe Housing Vacancy Survey has homeownership by race for each puma area....

census_api_key(tidy_key, install = TRUE)


rm(list=ls())


v20 <- load_variables(2020, "acs5", cache = TRUE)


#' relevant variables:
#' B07013_002	Estimate!!Total:!!Householder lived in owner-occupied housing units	
#'                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR CURRENT RESIDENCE IN THE UNITED STATES
#' 
#' B25003_002	Estimate!!Total:!!Owner occupied	TENURE
#' B25106_024	Estimate!!Total:!!Renter-occupied housing units:	TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
#' 

city_raw <- get_acs(geography = "tract", 
                 state = 'NY',
                 county = c('Kings County', 'Queens County', 
                            'Richmond County', 'Bronx County',
                            'New York County'), 
                 variables = c('B25106_024', 'B25003_002'),
                 year = 2020,
                 survey = 'acs5') %>% 
  clean_names()

# make each vraiable a column, rename
city_wide <- city_raw %>% 
  select(-moe) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate') %>% 
  rename(owner_occupied = B25003_002,
         renter_occupied = B25106_024)

city_wide$owned_rate <- round(replace_na(city_wide$owner_occupied / (city_wide$owner_occupied + city_wide$renter_occupied), 0), 4) * 100



# 2	B25032_002	Estimate!!Total:!!Owner-occupied housing units:	TENURE BY UNITS IN STRUCTURE
# 3	B25032_003	Estimate!!Total:!!Owner-occupied housing units:!!1, detached	TENURE BY UNITS IN STRUCTURE
# 4	B25032_004	Estimate!!Total:!!Owner-occupied housing units:!!1, attached	TENURE BY UNITS IN STRUCTURE
# 5	B25032_005	Estimate!!Total:!!Owner-occupied housing units:!!2	TENURE BY UNITS IN STRUCTURE
# 6	B25032_006	Estimate!!Total:!!Owner-occupied housing units:!!3 or 4	TENURE BY UNITS IN STRUCTURE
# 7	B25032_007	Estimate!!Total:!!Owner-occupied housing units:!!5 to 9	TENURE BY UNITS IN STRUCTURE
# 8	B25032_008	Estimate!!Total:!!Owner-occupied housing units:!!10 to 19	TENURE BY UNITS IN STRUCTURE
# 9	B25032_009	Estimate!!Total:!!Owner-occupied housing units:!!20 to 49	TENURE BY UNITS IN STRUCTURE
# 10	B25032_010	Estimate!!Total:!!Owner-occupied housing units:!!50 or more	TENURE BY UNITS IN STRUCTURE


# 13	B25032_013	Estimate!!Total:!!Renter-occupied housing units:	TENURE BY UNITS IN STRUCTURE
# 14	B25032_014	Estimate!!Total:!!Renter-occupied housing units:!!1, detached	TENURE BY UNITS IN STRUCTURE
# 15	B25032_015	Estimate!!Total:!!Renter-occupied housing units:!!1, attached	TENURE BY UNITS IN STRUCTURE
# 16	B25032_016	Estimate!!Total:!!Renter-occupied housing units:!!2	TENURE BY UNITS IN STRUCTURE
# 17	B25032_017	Estimate!!Total:!!Renter-occupied housing units:!!3 or 4	TENURE BY UNITS IN STRUCTURE
# 18	B25032_018	Estimate!!Total:!!Renter-occupied housing units:!!5 to 9	TENURE BY UNITS IN STRUCTURE
# 19	B25032_019	Estimate!!Total:!!Renter-occupied housing units:!!10 to 19	TENURE BY UNITS IN STRUCTURE
# 20	B25032_020	Estimate!!Total:!!Renter-occupied housing units:!!20 to 49	TENURE BY UNITS IN STRUCTURE
# 21	B25032_021	Estimate!!Total:!!Renter-occupied housing units:!!50 or more	TENURE BY UNITS IN STRUCTURE



# building-size

size_vars = c('B25032_002', 'B25032_003', 'B25032_004', 'B25032_005', 'B25032_006', 'B25032_007', 'B25032_008', 'B25032_009', 'B25032_009',
              'B25032_013', 'B25032_014', 'B25032_015', 'B25032_016', 'B25032_017', 'B25032_018', 'B25032_019', 'B25032_020', 'B25032_021')

size_vars_rename <- c('owner_1_detach', 'owner_1_attach', 'owner_2', 'owner_3_4', 'owner_5_9', 'owner_10_19', 'owner_20_49', 'owner_50_plus',
                      'renter_1_detach', 'renter_1_attach', 'renter_2', 'renter_3_4', 'renter_5_9', 'renter_10_19', 'renter_20_49', 'renter_50_plus')



by_size <- get_acs(geography = "tract", 
                   state = 'NY',
                   county = c('Kings County', 'Queens County', 
                              'Richmond County', 'Bronx County',
                              'New York County'), 
                   variables = size_vars,
                   year = 2020,
                   survey = 'acs5') %>% 
  clean_names()

by_size_wide <- by_size %>% 
  select(-moe) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate')


for(i in 1:length(size_vars)-1) names(by_size_wide)[names(by_size_wide) == size_vars[i]] = size_vars_rename[i]
