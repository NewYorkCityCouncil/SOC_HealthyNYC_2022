# https://walker-data.com/tidycensus/articles/pums-data.html

library(tidyverse)
library(tidycensus)
library(survey)
library(sf)
library(jsonlite)
library(leaflet)
library(mapview)
library(classInt)
library(leaflet.extras)

### REQUEST -------------------
# map of senior population and internet metric 

## OUTLINE TEXT for Reference ##
# Close the Digital Divide to advance health and opportunity for youth, older adults and NYers with disabilities 
# For the internet access map, they would like it to show broadband access overall, not specific population and 
# perhaps numbers or bar chart seeing which groups (seniors, disabled, low-income, nonwhite, etc) 
# have lower rates compared to the city overall.

# Similar to the digital divide, a significant language access barrier remains in our city because of insufficient 
# translation services for city government. Despite our city having the greatest language diversity of any in the world, 
# we are ill-equipped to serve the 1.8 million New Yorkers with limited English proficiency. This is a major gap that undermines 
# health and well-being by blocking access to key services and opportunities. The City must expand language access in three ways: 
# increasing access to language services in city agencies, improving the quality of translations by creating a Community Interpreter 
# Bank, and providing employment and business opportunities for immigrant communities by developing worker-owned cooperatives. 

# function to unzip shapefile -------------------

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

### Pull and clean data -------------------

# puma shapefile from nyc planning
url <- "https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nypuma2010_22a.zip"
nyc_puma <- read_sf(unzip_sf(url)) %>%
  mutate(
    PUMA = paste0("0", PUMA)
  )

# get list of nyc pumas 
nyc_puma_list <- nyc_puma %>%
  select(PUMA) %>%
  st_drop_geometry()

# grab LANP and LANX (Language other than English spoken at home) 
og_pums <- get_pums(
  variables = c("PUMA", "LANP", "LANX"),
  state = "NY",
  survey = "acs5",
  year = 2019, 
  # recode character variables
  recode = TRUE, 
  # for standard errors: replicate weights are used to simulate multiple samples from the single PUMS sample and can be used 
  # to calculate more precise standard errors. PUMS data contains both person- and housing-unit-level replicate weights.
#  rep_weights = "person"
  ) 

# og_pums %>% select(LANP_label) %>% unique() %>% View()

# cleand and mutate pums download
ny_pums_language <- og_pums %>%
  # remove NA children
  filter(LANX_label != "N/A (less than 5 years old)") %>%
  # include english in language variable and indicate top languages 
  # from pg. 2: https://www1.nyc.gov/assets/civicengagement/downloads/pdf/nyccec-poll-site-language-assistance-program-methodology.pdf
  mutate(
    LANP_label = ifelse(LANX_label == "No, speaks only English", "English", as.character(LANP_label)),
    top_lang = ifelse(
        LANP_label == "English" |
          LANP_label == "Spanish" |
          LANP_label == "Arabic" |
          LANP_label == "Chinese" |
          LANP_label == "Cantonese" |
          LANP_label == "Mandarin" |
          LANP_label == "French" |
          LANP_label == "Haitian" |
          LANP_label == "Italian" |
          LANP_label == "Korean" |
          LANP_label == "Polish" |
          LANP_label == "Russian" |
          LANP_label == "Urdu" |
          LANP_label == "Yiddish", "yes", "no"
          ), 
    # consolidate "chinese"
    LANP_label = ifelse(
      LANP_label == "Chinese" | LANP_label == "Cantonese" | LANP_label == "Mandarin", "Chinese (Cantonese, Mandarin)", LANP_label
      )
  )

### Totals for State and City -------------------

# new york state totals
ny_pums_language %>%
  count(top_lang, wt = PWGTP) %>%
  mutate(prop = n / sum(n))

nyc_puma_list %>%
  left_join(ny_pums_language, by = "PUMA") %>%
  count(LANX_label, wt = PWGTP) %>%
  mutate(prop = n / sum(n))

# top languages after top 13 in nyc -> 03_pums_plot.R
top_langs <- nyc_puma_list %>%
  left_join(ny_pums_language, by = "PUMA") %>%
  count(LANP_label, top_lang, wt = PWGTP) %>%
  mutate(prop_lang = n / sum(n)) %>%
  arrange(-n) 
  
### Proportion In/Out Top 13 by PUMS -------------------

# nyc number and proportion in/out top languages by pums
nyc_pums_top <- nyc_puma_list %>%
  left_join(ny_pums_language, by = "PUMA") %>%
  group_by(PUMA) %>%
  count(top_lang, wt = PWGTP) %>%
  mutate(prop_lang = n / sum(n)) %>%
  left_join(nyc_puma %>% select(PUMA), by = "PUMA") %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84")

# subset to no for mapping -> 02_pums_map.R
nyc_pums_toplang <- nyc_pums_top %>%
  filter(top_lang == "no")

### Each Language by PUMS -------------------

# nyc number and proportion of all languages by pums
nyc_pums_lang <- nyc_puma_list %>%
  left_join(ny_pums_language, by = "PUMA") %>%
  group_by(PUMA) %>%
  count(LANP_label, top_lang, wt = PWGTP) %>%
  mutate(prop_lang = n / sum(n)) %>%
  left_join(nyc_puma %>% select(PUMA), by = "PUMA") %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 



