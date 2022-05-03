library(censusapi)
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

# check in with Brook, she may have data and analysis or be aware of resources on this topic
# resources: https://cnycn.org/new-york-foreclosure-data-explained/
# https://furmancenter.org/thestoop/entry/snapshot-of-homeownership-in-new-york-city
# https://fred.stlouisfed.org/graph/?g=OPNG
# https://fred.stlouisfed.org/series/HOWNRATEACS036005#0
# https://coredata.nyc/
# https://wherewelive.cityofnewyork.us/explore-data/housing-conditions/
# https://therealdeal.com/new-research/industry-reports/nyc-homeownership-rate-by-racial-ethnic-group-streeteasy/
# maybe Housing Vacancy Survey has homeownership by race for each puma area....