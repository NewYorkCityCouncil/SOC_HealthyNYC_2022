library(stringr)
library(jsonlite)
library(dplyr)
library(classInt)
library(sf)
library(tidycensus)
library(tidyverse)
sf::sf_use_s2(FALSE)

# function to unzip shapefile -----------

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

#########

df <- fromJSON("https://health.data.ny.gov/resource/875v-tpc8.json?$limit=999999999")
nyc_df <- df %>%
  filter(county %in% c("New York", "Kings","Bronx","Queens","Richmond"))

#HH_df <- nyc_df %>%
#  filter(operator_name == "New York City Health and Hospital Corporation",
#         !is.na(longitude))

#mb_access_token("pk.eyJ1Ijoiand1Y291bmNpbCIsImEiOiJjbDJ0NWl0bnIwMTEyM2NuMXc0b2tkNG03In0.VL6giDofMqwuVlWFaNWcog", install = TRUE)

isos=list()
system.time(
  for(i in 1:nrow(nyc_df)){
    Sys.sleep(0.75)
    if(is.na(nyc_df$longitude[i]) | is.na(nyc_df$latitude[i])){
      longlat_temp <- tryCatch(
        mb_geocode(paste0(nyc_df$address1[i], ", ", nyc_df$county[i])),
        error = function(e)
          print(paste("Failed to find long lats for", i))
      )
      
      if(!inherits(longlat_temp, "error")){
        nyc_df$longitude[i] <- longlat_temp[1]
        nyc_df$latitude[i] <- longlat_temp[2]
        
        isos[[i]] <- mb_isochrone(location = c(nyc_df$longitude[i], 
                                               nyc_df$latitude[i]),
                                  profile = "walking",
                                  time = 10)
      }else{
        next()
      }

    } else{
      isos[[i]] <- mb_isochrone(location = c(nyc_df$longitude[i], 
                                             nyc_df$latitude[i]),
                                profile = "walking",
                                time = 10)
    }
    print(i)
  }
)


mapping_data_full <- cbind(st_geometry(bind_rows(isos)),
                      nyc_df %>% 
                        select(facility_name,longitude,latitude,description,address1,city,state,fac_zip)) %>%
  st_as_sf %>%
  st_transform("+proj=longlat +datum=WGS84")

desc_to_keep <- c("Diagnostic and Treatment Center","Diagnostic and Treatment Center Extension Clinic","Hospital","Hospital Extension Clinic","Mobile Diagnostic and Treatment Center Extension Clinic","School Based Diagnostic and Treatment Center Extension Clinic","School Based Hospital Extension Clinic")
mapping_data <- mapping_data_full %>%
  filter(description %in% desc_to_keep)

## download census tract shapefile from Department of Planning
#url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyct2020_22a.zip"
#ct_shp <- sf::read_sf(unzip_sf(url)) %>%
#  mutate(
#    # match county numbers with those from the acs data 
#    county = case_when(
#      BoroCode == "1" ~ "061", 
#      BoroCode == "2" ~ "005", 
#      BoroCode == "3" ~ "047", 
#      BoroCode == "4" ~ "081", 
#      BoroCode == "5" ~ "085" 
#    ), 
#    # create GEO_ID to match acs data 
#    GEO_ID = paste0("1400000US36", county, CT2020)
#  )

### Demographics ----------------------------------------------------------------------
# Note: you must get a census API key to use getCensus functions

# Income data to pull from ACS
# https://api.census.gov/data/2020/acs/acs5/subject/variables.html
# Household median income: S1901_C01_012E
#inc_pop_col <- c("NAME", "GEO_ID", "S0101_C01_001E", paste0("S1901_C01_0",formatC(1:13, width = 2, flag = "0"), "E")) 

## Income data at census tract level
#ct_inc_pop <- getCensus(
#  name = "acs/acs5/subject",
#  vintage = 2020,
#  vars = inc_pop_col, 
#  region = "tract:*", 
#  regionin = "state:36+county:005,047,081,085,061") %>%
#  naniar::replace_with_na_all(condition = ~.x < 0) # Input NA for annotated values in numeric columns 

## Get puma shp file
#puma_shp <- read_sf(unzip_sf("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nypuma2010_22a.zip")) %>%
#  mutate(PUMA = paste0(0,PUMA)) %>%
#  # Get PUMA Names
#  left_join(pumas(state = "NY", cb = TRUE, year = 2019) %>%
#              filter(str_detect(NAME10, "NYC")) %>%
#              st_drop_geometry() %>%
#              select(NAME10,PUMACE10) %>%
#              rename(PUMA = PUMACE10))
#inc_pop_col <- c("HINCP","ADJINC","PUMA")
#
#puma_inc_pop <- get_pums(
#  variables = inc_pop_col,
#  state = "NY",
#  survey = "acs5",
#  year = 2020,
#  recode = TRUE
#)
## Filter PUMS data to only the pumas in the puma shapefile + clean it
#hh_nyc <- puma_inc_pop %>%
#  filter(PUMA %in% puma_shp$PUMA) %>% # include only nyc pumas
#  mutate(HINCP = ifelse(HINCP == "-60000",NA,HINCP), 
#         HINCP1 = HINCP * as.numeric(ADJINC),
#         WGTP = as.numeric(WGTP)) %>%
#  filter(SPORDER == 1) %>% # since we only care about households, we need 1 representation per household
#  uncount(WGTP, .remove = F) # replicate each one based on household weight
## Median household income by PUMA merged with shapefile
#puma_df <- hh_nyc %>%
#  group_by(PUMA) %>%
#  summarise(med_inc = median(HINCP1),
#            num_households = n()) %>%
#  left_join(puma_shp,.) %>%
#  st_transform("+proj=longlat +datum=WGS84")

nta_shp <- read_sf(unzip_sf("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nynta2020_22a.zip")) %>%
  st_transform("+proj=longlat +datum=WGS84") 

nta_df <- nta_shp %>%
  # Read in median income data/NTA
  left_join(read.csv("../data/NTA2020_med_income.csv")) %>%
  # Clean the uncertains and -9999
  mutate(med_income = ifelse(!is.na(med_income_cv),as.numeric(gsub(",","",med_income)),NA))
