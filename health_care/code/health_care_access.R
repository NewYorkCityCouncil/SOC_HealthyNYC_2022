library(censusapi)
library(dplyr)
library(stringr)
library(janitor)
library(jsonlite)
library(RSocrata)
library(tidyr)

### REQUEST ------------
# Map of H+H Health Care Access deserts or health care access inequities

## OUTLINE TEXT for Reference ##

# Expand H+H Health Care Access Points (Council has already begun to advocate initial efforts on this, but deeper commitment needed by the City) 

# > Need to expand with 4 community health and ambulatory centers (preventative care, women’s health/OBGYN, pediatric, mental health care) and a center in Far Rockaway with a trauma unit that the Majority Whip is advocating for in this year’s budget as just the start. 
# > We need to do more to expand health care access – call on H+H to establish a capital plan for expanding health access points that has a rational and transparent criteria to address health care access inequities across the City 
# > From health delivery standpoint, the pandemic has brought us some good models of how to bring access to health care to where people are, in their communities – that’s a model H+H should invest in, where health access is community-based in every essence. Council can support this through initiative to demonstrate leadership.  


# TASK ---
# do some digging on what is considered health access, what health care facilities to include, like clinics, hospitals, etc.
# reach out to the health, hospitals committees for guidance
# goal is to identify health care access deserts
# use https://capitalplanning.nyc.gov/facilities to search for health facilties
# EDC did some work on this already, can be helpful 
# https://edc.nyc/insights/access-to-health-care-in-nyc-borough-inequality-pandemic-effect
# DOHMH has a map on https://a816-healthpsi.nyc.gov/NYCHealthMap