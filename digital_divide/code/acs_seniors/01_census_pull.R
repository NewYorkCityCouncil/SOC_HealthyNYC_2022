library(censusapi)
library(dplyr)
library(stringr)
library(janitor)
library(jsonlite)
library(RSocrata)
library(tidyr)
library(sf)
library(leaflet)
library(mapview)


### REQUEST ------------
# map of senior population and internet metric 

## OUTLINE TEXT for Reference ##
# Close the Digital Divide to advance health and opportunity for youth, older adults and NYers with disabilities 
# > Device Donation Program – Connect old equipment from city agencies and private donors to those in need (based on programs in San Francisco and New Orleans) 
# > Digital Literacy Programming at DFTA 
# > Digital Literacy and Assistive Technology Investments for NYers with disabilities 

# check ACS 2019 & 2020 5 Year Tables at the Census Tract level;
# check PUMS 2019 & 2020, it has sample individual level data;
# check what internet metrics are there;
# then decide whether to aggregate up to NTA, CDTA, or PUMA


# example PUMS code -----------
# https://github.com/NewYorkCityCouncil/Racial_Disparity_Report/blob/4e87b005f5baf1579450cb435b55006e131c28a3/code/rm_acs_pull.R

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

### Pull and clean data -----------
### AGE BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD 2020 -----------
# Note: "Categorically, the ACS considers all desktops, laptops, tablets, and smartphones as computers, 
# along with selected computing technologies such as smart home devices and single board computers such as RaspberryPi 
# and Arduino boards compiled from write-in responses."

# get variables available in B28005 group of ACS5 2020
# also here: https://api.census.gov/data/2020/acs/acs5/variables.html
group_B28005 <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2020,
  type = "variables",
  group = "B28005")

# relevant "AGE BY PRESENCE OF A COMPUTER AND TYPES OF INTERNET SUBSCRIPTION IN HOUSEHOLD" variables
vars <- c("NAME", "GEO_ID", "B28002_001E", paste0("B28005_01",formatC(4:9, width = 1, flag = "0"), "E")) 

# get available geographies for ACS5 2020
geos <- listCensusMetadata(name = "acs/acs5", vintage = 2020, type = "geographies")

### data by census tract (2020) -----------

# get variables for NYC by census tract
age_internet_ct <- getCensus(
  # must add a census api key
  key = Sys.getenv("KEY"),
  name = "acs/acs5",
  vintage = 2020,
  vars = vars, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061")

# download census tract shapefile from Department of Planning
url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nyct2020_22a.zip"
ct_shp <- sf::read_sf(unzip_sf(url)) %>%
  mutate(
    # match county numbers with those from the acs data 
    county = case_when(
      BoroCode == "1" ~ "061", 
      BoroCode == "2" ~ "005", 
      BoroCode == "3" ~ "047", 
      BoroCode == "4" ~ "081", 
      BoroCode == "5" ~ "085" 
    ), 
    # create GEO_ID to match acs data 
    GEO_ID = paste0("1400000US36", county, CT2020)
  )

# join acs data with shapefile and create variables for proportion without broadband and without internet
ct_acs <- age_internet_ct %>%
  left_join(ct_shp %>% select(!c("county", "BoroCT2020", "BoroCode")), by = "GEO_ID") %>% 
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  mutate(
    # 1 - proportion of seniors with both computer and broadband access
    no_broadband = 1 - (B28005_017E / B28005_014E), 
    # proportion of seniors with a computer but no internet or without a computer
    no_internet = (B28005_018E + B28005_019E) / B28005_014E
  )


### data by NTA (2020) -----------

# download nta shapefile from Department of Planning 
nta_url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nynta2020_22a.zip"
x <- read_sf(unzip_sf(nta_url)) 

nta <- x %>%
  st_transform("+proj=longlat +datum=WGS84") 

# aggregate from census tract to NTA and join NTA shapefile
nta_acs <- ct_acs %>%
  st_drop_geometry() %>%
  group_by(NTA2020, NTAName, BoroName) %>%
  # create variables by summing nested census tracts to NTA
  summarise(
    Population = sum(B28002_001E), 
    Pop_65plus = sum(B28005_014E), 
    HasComp = sum(B28005_015E), 
    HasComp_DialUp = sum(B28005_016E), 
    HasComp_Broadband = sum(B28005_017E), 
    HasComp_NoInternet = sum(B28005_018E), 
    NoComp = sum(B28005_019E)
  ) %>%
  mutate(
    # 1 - proportion of seniors with both computer and broadband access
    No_Broadband = 1 - (HasComp_Broadband / Pop_65plus), 
    # proportion of seniors with a computer but no internet or without a computer
    No_Internet = (HasComp_NoInternet + NoComp) / Pop_65plus
  ) %>% 
  left_join(nta, by = "NTA2020") %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 

### data by PUMA (2020) -----------

# get variables for NYC by PUMA
age_internet_puma <- getCensus(
  key = Sys.getenv("KEY"),
  name = "acs/acs5",
  vintage = 2020,
  vars = vars, 
  region = "public use microdata area:*", 
  regionin = "state:36")

# subset to NYC
age_internet_puma <- age_internet_puma[grep("NYC-", age_internet_puma$NAME),]

