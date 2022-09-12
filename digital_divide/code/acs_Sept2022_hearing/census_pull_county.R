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
vars <- c("NAME", "GEO_ID", 
          ## these are household data (e.g. "B28002_001E" = 3.192 households in nyc)
          "B28002_001E",
          # broadband for overall population (total, broadband such as cable, fiber optic or DSL)
          "B28002_007E", 
          # %s of households with only a mobile phone internet connection, only a home internet connection
          "B28002_005E", "B28002_006E", "B28002_008E", "B28002_010E", "B28002_011E", 
          # % of households citywide that have no internet access at home at all
          "B28002_013E"
          ) 

# get available geographies for ACS5 2020
geos <- listCensusMetadata(name = "acs/acs5", vintage = 2020, type = "geographies")

### data by census tract (2020) -----------

# get variables for NYC by census tract
internet_county <- getCensus(
  # must add a census api key
  key = Sys.getenv("KEY"),
  name = "acs/acs5",
  vintage = 2020,
  vars = vars, 
  region = "county:005,047,081,085,061", 
  regionin = "state:36")

internet_nyc <- internet_county %>%
  group_by(state) %>%
  summarise(
    # no internet access (note: it is access not subscription because there is another category for access but no subscription)
    no_access = sum(B28002_013E) / sum(B28002_001E),
    # no broadband subscription: 1 - proportion of with broadband such as cable, fiber optic or DSL
    no_broadband = 1 - (sum(B28002_007E) / sum(B28002_001E)),
    # cellular data plan and other internet subscription
    cell_and_int = (sum(B28002_005E) - sum(B28002_006E)) / sum(B28002_001E),
    # only cellular data plan with no other type of Internet subscription
    cell_only = sum(B28002_006E) / sum(B28002_001E), 
    # only home internet connection (broadband + satellite + other subscription)
    homeint_only = (sum(B28002_008E) + sum(B28002_010E) + sum(B28002_011E)) / sum(B28002_001E),
    # neither home broadband nor cell 

  )



