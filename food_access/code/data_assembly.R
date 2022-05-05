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

neigh_prio <- read.csv("food_access/data/Neighborhood_prioritization.csv") %>% clean_names() %>%
  left_join(nta %>% select(NTACode, geometry), by=c("nta" = "NTACode")) %>%
  st_as_sf() %>%
  st_transform("+proj=longlat +datum=WGS84") 
  
efap <- read.csv("food_access/data/efap_geo.csv") %>% clean_names() %>%
  st_as_sf(coords = c("lon", "lat"), crs = "+proj=longlat +datum=WGS84") %>%
  left_join(
    (efap_mult <- efap %>%
       st_drop_geometry() %>%
       group_by(distadd) %>%
       summarise(num_programs = n(), 
                 prop_legend = ifelse(num_programs >=2, "Two or Three", "One"))), 
    by = "distadd"
  )

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


### Map efap locations ----------------------------------

leaflet(efap) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addTiles() %>%
  addCircleMarkers(popup = lapply(paste("<p>","Address: ",efap$distadd,"</p>", 
                                        "<p>",paste0("Boro: ",efap$distboro),"</p>"), HTML), 
                   radius = 0.1)


pal_nta <- colorBin(
  palette = "YlOrRd",
  domain = neigh_prio$fy22_weighted_score,
  na.color = "transparent")

pal_efap <- colorFactor(c("navy", "purple"), domain = c("One", "Two or Three"))

# highest 25% of supply gap 
recode <- neigh_prio %>%
  mutate(bottom25 = ifelse(fy22_weighted_score >= quantile(neigh_prio$fy22_weighted_score, seq(0,1,0.05), na.rm = TRUE)[16] %>% as.numeric(), 1, 0))

map <- leaflet(efap) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data = neigh_prio, 
              weight = 1,
              fillColor = ~pal_nta(fy22_weighted_score),
              color="#E6E6E6",
              fillOpacity = 0.6, 
              popup = lapply(paste("<p>","NTA: ",neigh_prio$nta_name,"</p>", 
                                   "<p>",paste0("Score: ",neigh_prio$fy22_weighted_score),"</p>"), HTML)) %>%
  # addPolygons(data = recode, weight = 2, fill=F, opacity = 1,
  #             color = "#8744BC", stroke = recode$bottom25) %>% 
  addCircleMarkers(popup = lapply(paste("<p>","Address: ",efap$distadd,"</p>", 
                                        "<p>",paste0("Boro: ",efap$distboro),"</p>"), HTML), 
                   radius = 0.1, 
                   color = ~pal_efap(prop_legend)) %>%
  addLegend("bottomright", pal = pal_efap, values = efap$prop_legend,
            title = "Number of Programs</br>at Location",
            opacity = 0.6) %>%
  addLegend("bottomright", pal = pal_nta, values = neigh_prio$fy22_weighted_score,
            title = "Supply Gap Score</br>(Lowest to Highest</br>Level of Need)",
            opacity = 0.6) 

mapshot(map, file = "food_access/visual/efap_score.png", 
        vwidth = 900, vheight = 870)
  
### Relationships between efap locations and score ----------------------------------

ggplot(data = efap_neigh, aes(x = fy22_weighted_score, y = num_programs)) + geom_point() + ylim(0,15)


