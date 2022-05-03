library(censusapi)
library(dplyr)
library(stringr)
library(janitor)
library(jsonlite)
library(RSocrata)
library(tidyr)

### REQUEST ------------
# map overlaying supply gap data and EFAP locations

## OUTLINE TEXT for Reference ##

# Food Access + Security 

# Ensure food assistance access for all communities that need it, 
# including the undocumented ineligible for SNAP 

# > Call on state to match funding for a Council-initiated program for food benefits for those not eligible for existing benefits, including undocumented 
# > Pilot program to provide food pantries and fresh food boxes at schools or shelters in high-need areas 
# > Expand Grow NYC greenmarkets in communities that lack healthy food access – high-need areas of Bronx, Brooklyn, Queens, Upper Manhattan 
# > Older adult food assistance items in Preliminary Budget Response 

# Links 
# https://www1.nyc.gov/site/foodpolicy/reports-and-data/supply-gap.page
# https://www1.nyc.gov/assets/hra/downloads/pdf/services/efap/EFAP_ACTIVE.pdf
# https://www1.nyc.gov/assets/dsny/contact/services/COVID-19GetFoodNYCHistDist.shtml

# I pulled the supply gap data, its at the NTA level
# check if the efap pdf is the latest vs what is quarterly reported to us