library(censusapi)
library(dplyr)
library(stringr)
library(janitor)
library(jsonlite)
library(RSocrata)
library(tidyr)
library(sf)
library(leaflet)
library(htmltools)
library(htmlwidgets)
library(mapview)
library(ggplot2)
library(leaflet.extras)
library(classInt)

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
# Note: Use the tableau video data after selecting and hovering all zip codes to download ^
# https://www1.nyc.gov/assets/hra/downloads/pdf/services/efap/EFAP_ACTIVE.pdf
# https://www1.nyc.gov/assets/dsny/contact/services/COVID-19GetFoodNYCHistDist.shtml
# https://council.nyc.gov/data/emergency-food-in-nyc/

# I pulled the supply gap data, its at the NTA level
# check if the efap pdf is the latest vs what is quarterly reported to us

### Functions ----------------------------------

unzip_sf <- function(zip_url) {
  temp <- tempfile()
  temp2 <- tempfile()
  #download the zip folder from the internet save to 'temp' 
  download.file(zip_url, temp)
  #unzip the contents in 'temp' and save unzipped content in 'temp2'
  unzip(zipfile = temp, exdir = temp2)
  #if returns "character(0), then .shp may be nested within the folder
  your_SHP_file <- ifelse(!identical(list.files(temp2, pattern = ".shp$",full.names=TRUE), character(0)), 
                          list.files(temp2, pattern = ".shp$",full.names=TRUE), 
                          list.files(list.files(temp2, full.names=TRUE), pattern = ".shp$", full.names = TRUE))
  unlist(temp)
  unlist(temp2)
  return(your_SHP_file)
}

### Load data ----------------------------------

nta_url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nynta2010_22a.zip"
x <- read_sf(unzip_sf(nta_url)) 

nta <- x %>%
  st_transform("+proj=longlat +datum=WGS84") 

neigh_prio <- read.csv("food_access/data/Neighborhood_prioritization.csv") %>% 
  clean_names() %>%
  right_join(nta %>% select(NTACode, geometry), 
            by=c("nta" = "NTACode")) %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 
  
efap <- read.csv("food_access/data/efap_geo.csv") %>% clean_names() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84")

efap <- efap %>%
  left_join(
    (efap_mult <- efap %>%
       st_drop_geometry() %>%
       group_by(distadd) %>%
       summarise(num_programs = n(), 
                 prop_legend = ifelse(num_programs >=2, "Two or Three", "One"))), 
    by = "distadd"
  ) %>% 
  mutate(locations = rep("EFAP Locations", n()))

efap_nta <- efap %>%
  st_join(nta %>% select(NTACode, NTAName, BoroName, geometry)) %>%
  group_by(NTACode, NTAName, BoroName) %>%
  summarise(num_programs = n()) 

efap_neigh <- efap %>%
  st_join(nta %>% select(NTACode, NTAName, BoroName, geometry)) %>%
  st_drop_geometry() %>%
  group_by(NTACode) %>%
  summarise(num_programs = n()) %>%
  right_join(
    (neigh_prio %>%
       st_drop_geometry() %>%
       select(fy22_weighted_score, nta, nta_name)), 
    by = c("NTACode" = "nta")
  ) %>%
  mutate(num_programs = ifelse(is.na(num_programs), 0, num_programs))



