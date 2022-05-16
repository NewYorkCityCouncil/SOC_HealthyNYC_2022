# https://walker-data.com/tidycensus/articles/pums-data.html

library(tidyverse)
library(tidycensus)
library(survey)
library(sf)
library(jsonlite)
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

pums_vars_2019 <- pums_variables %>% 
  filter(year == 2019, survey == "acs5")

# pums_variables contains both the variables as well as their possible values. So let’s just look at the unique variables.
pums_vars_2019 %>% 
  distinct(var_code, var_label, data_type, level)

### HISPEED ------------------

# puma shapefile
ny_puma <- map("NY", tigris::pumas, class = "sf", cb = TRUE, year = 2019) %>% 
  reduce(rbind)

nyc_puma <- ny_puma[grep("NYC-", ny_puma$NAME),]

ny_pums_hispeed <- get_pums(
  variables = c("PUMA", "HISPEED", ),
  state = "NY",
  survey = "acs5",
  year = 2019, 
  # recode character variables
  recode = TRUE, 
  # for standard errors: replicate weights are used to simulate multiple samples from the single PUMS sample and can be used 
  # to calculate more precise standard errors. PUMS data contains both person- and housing-unit-level replicate weights.
#  rep_weights = "housing"
)

# nyc totals
nyc_puma %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = c("STATEFP10" = "ST", "PUMACE10" = "PUMA")) %>%
  summarize(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop, 
    no_speed = sum(WGTP[HISPEED == "2"]),
    no_speed_pct = no_speed / total_pop, 
    na_speed = sum(WGTP[HISPEED == "b"]),
    na_speed_pct = na_speed / total_pop, 
  )

# nyc pums broadband 
nyc_pums_hispeed <- nyc_puma %>%
  select(STATEFP10, PUMACE10, NAME10) %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = c("STATEFP10" = "ST", "PUMACE10" = "PUMA")) %>%
  group_by(PUMACE10, NAME10) %>%
  summarise(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop
  ) %>%
  left_join(nyc_puma %>% select(PUMACE10), by = "PUMACE10") %>%
  st_as_sf()

### by race ------ 

nyc_pums_hispeed <- nyc_puma %>%
  select(STATEFP10, PUMACE10, NAME10) %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = c("STATEFP10" = "ST", "PUMACE10" = "PUMA")) %>%
  group_by(PUMACE10, NAME10) %>%
  summarise(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop
  ) %>%
  left_join(nyc_puma %>% select(PUMACE10), by = "PUMACE10") %>%
  st_as_sf()



  
