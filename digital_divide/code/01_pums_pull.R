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
url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nypuma2010_22a.zip"
nyc_puma <- read_sf(unzip_sf(url)) %>%
  mutate(
    PUMA = paste0("0", PUMA)
  )


ny_pums_hispeed <- get_pums(
  variables = c("PUMA", "HISPEED", "RAC1P", "HISP", "AGEP", "POVPIP"),
  state = "NY",
  survey = "acs5",
  year = 2019, 
  # recode character variables
  recode = TRUE, 
  # for standard errors: replicate weights are used to simulate multiple samples from the single PUMS sample and can be used 
  # to calculate more precise standard errors. PUMS data contains both person- and housing-unit-level replicate weights.
#  rep_weights = "housing"
  ) %>%
  mutate(
    race_ethnicity = case_when(
      HISP != "01" ~ "Hispanic",
      HISP == "01" & RAC1P == "1" ~ "White",
      HISP == "01" & RAC1P == "2" ~ "Black",
      TRUE ~ "Other"
    ), 
    age65plus = case_when(
      AGEP >= 65 ~ "Senior",
      TRUE ~ "Other"
    ), 
    poverty = case_when(
      POVPIP < 200 ~ "low-income",
      TRUE ~ "Other"
    )
  )

# nyc totals
nyc_puma %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = "PUMA") %>%
  summarize(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop, 
    no_speed = sum(WGTP[HISPEED == "2"]),
    no_speed_pct = no_speed / total_pop, 
    na_speed = sum(WGTP[HISPEED == "b"]),
    na_speed_pct = na_speed / total_pop, 
  )

# nyc pums broadband -> use for map in 02_pums_map.R
nyc_pums_hispeed <- nyc_puma %>%
  select(PUMA) %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = "PUMA") %>%
  group_by(PUMA) %>%
  summarise(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop
  ) %>%
  left_join(nyc_puma %>% select(PUMA), by = "PUMA") %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 
  

### by race ------ 

# nyc total
nyc_puma %>%
  select(PUMA) %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = "PUMA") %>%
  group_by(race_ethnicity) %>%
  summarise(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop
  ) 

# puma [prob don't need]
nyc_race_hispeed <- nyc_puma %>%
  select(PUMA) %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = "PUMA") %>%
  group_by(PUMA, race_ethnicity) %>%
  summarise(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop
  ) %>%
  left_join(nyc_puma %>% select(PUMA), by = "PUMA") %>%
  st_as_sf()

### by age ------ 

# nyc total
nyc_puma %>%
  select(PUMA) %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = "PUMA") %>%
  group_by(age65plus) %>%
  summarise(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop
  ) 

  
### by poverty ------ 

# nyc total
nyc_puma %>%
  select(PUMA) %>%
  st_drop_geometry() %>%
  left_join(ny_pums_hispeed, by = "PUMA") %>%
  group_by(poverty) %>%
  summarise(
    total_pop = sum(WGTP),
    hi_speed = sum(WGTP[HISPEED == "1"]),
    hi_speed_pct = hi_speed / total_pop
  ) 
