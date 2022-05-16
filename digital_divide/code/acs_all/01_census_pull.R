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
# For the internet access map, they would like it to show broadband access overall, not specific population and 
# perhaps numbers or bar chart seeing which groups (seniors, disabled, low-income, nonwhite, etc) 
# have lower rates compared to the city overall.

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
### PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD -----------
# Note: "Categorically, the ACS considers all desktops, laptops, tablets, and smartphones as computers, 
# along with selected computing technologies such as smart home devices and single board computers such as RaspberryPi 
# and Arduino boards compiled from write-in responses."

# get variables available in B28002 group of ACS5 2020
# also here: https://api.census.gov/data/2020/acs/acs5/variables.html
group_B28002 <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2020,
  type = "variables",
  group = "B28002")

# relevant "PRESENCE OF A COMPUTER AND TYPE OF INTERNET SUBSCRIPTION IN HOUSEHOLD" variables
# Note: overall population is "broadband such as cable, fiber optic or DSL" while others are "broadband subscription"
# Note: age category is slightly different from others -- "has a computer with broadband subscription" instead of just "broadband subscription"
vars <- c("NAME", "GEO_ID", 
          ## these are household data (e.g. "B28002_001E" = 3.192 households in nyc)
          # broadband for overall population (total, broadband such as cable, fiber optic or DSL)
          "B28002_001E", "B28002_007E", 
          # broadband by income (total, less than <50k with broadband)
          "B28004_001E", "B28004_004E", "B28004_008E", "B28004_012E", "B28004_016E", 
          # broadband by age (total 65+, 65+ and has a computer with broadband)
          "B28005_014E", "B28005_017E", 
          # broadband by race (total non-hispanic white, non-hispanic white with broadband)
          "B28009H_001E", "B28009H_004E"
          ) 

# get available geographies for ACS5 2020
geos <- listCensusMetadata(name = "acs/acs5", vintage = 2020, type = "geographies")

### data by census tract (2020) -----------

# get variables for NYC by census tract
internet_ct <- getCensus(
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
ct_acs <- internet_ct %>%
  left_join(ct_shp %>% select(!c("county", "BoroCT2020", "BoroCode")), by = "GEO_ID") %>% 
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  mutate(
    # 1 - proportion of with broadband such as cable, fiber optic or DSL
    no_broadband = 1 - (B28002_007E / B28002_001E)
    # have to add other variables here
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
    population = sum(B28002_001E), 
    broadband = sum(B28002_007E)
    # have to add other variables here
  ) %>%
  mutate(
    # 1 - proportion of with broadband such as cable, fiber optic or DSL
    no_broadband = 1 - (broadband / population)
    # have to add other variables here
  ) %>% 
  left_join(nta, by = "NTA2020") %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 

### data by PUMA (2020) [in case want to use PUMA instead] -----------

# get variables for NYC by PUMA
internet_puma <- getCensus(
  key = Sys.getenv("KEY"),
  name = "acs/acs5",
  vintage = 2019,
  vars = vars, 
  region = "public use microdata area:*", 
  regionin = "state:36")

# subset to NYC
internet_puma <- internet_puma[grep("NYC-", internet_puma$NAME),]

internet_puma %>%
  group_by(state) %>%
  summarise(
    hi_speed_pct = sum(B28002_007E) / sum(B28002_001E) 
  )

