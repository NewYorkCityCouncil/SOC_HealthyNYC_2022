library(censusapi)
library(stringr)
library(jsonlite)
library(dplyr)
library(mapboxapi)
library(sf)
sf::sf_use_s2(FALSE)

df <- fromJSON("https://health.data.ny.gov/resource/875v-tpc8.json?$limit=999999999")
nyc_df <- df %>%
  filter(county %in% c("New York", "Kings","Bronx","Queens","Richmond"))

HH_df <- nyc_df %>%
  filter(operator_name == "New York City Health and Hospital Corporation",
         !is.na(longitude))

#mb_access_token("pk.eyJ1Ijoiand1Y291bmNpbCIsImEiOiJjbDJ0NWl0bnIwMTEyM2NuMXc0b2tkNG03In0.VL6giDofMqwuVlWFaNWcog", install = TRUE)

isos=list()
system.time(
  for(i in 1:nrow(HH_df)){
    Sys.sleep(0.5)
    isos[[i]] <- mb_isochrone(location = c(HH_df$longitude[i], 
                                           HH_df$latitude[i]),
                              profile = "walking",
                              time = 10)
    print(i)
  }
)

#mapping_data <- st_set_geometry(HH_df, st_geometry(bind_rows(isos)))
mapping_data <- cbind(st_geometry(bind_rows(isos)),HH_df %>% select(facility_name,longitude,latitude,description,address1,city,state,fac_zip)) %>%
  st_as_sf %>%
  st_transform("+proj=longlat +datum=WGS84")

nycctbounds <- st_read("https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/NYC_Census_Tracts_for_2020_US_Census/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=pgeojson") %>%
  st_transform("+proj=longlat +datum=WGS84") %>%
  mutate(OBJECTID_area = st_area(.),
         # match county numbers with those from the acs data below
         county = case_when(
           BoroCode == "1" ~ "061", 
           BoroCode == "2" ~ "005", 
           BoroCode == "3" ~ "047", 
           BoroCode == "4" ~ "081", 
           BoroCode == "5" ~ "085" 
         ),
         # create GEO_ID to match acs data below
         GEO_ID = paste0("1400000US36", county, CT2020)
  )

### Demographics ----------------------------------------------------------------------
# Note: you must get a census API key to use getCensus functions

# Income data to pull from ACS
# https://api.census.gov/data/2020/acs/acs5/subject/variables.html
# Household median income: S1901_C01_012E
inc_pop_col <- c("NAME", "GEO_ID", "S0101_C01_001E", paste0("S1901_C01_0",formatC(1:13, width = 2, flag = "0"), "E")) 

# Income data at census tract level
ct_inc_pop <- getCensus(
  name = "acs/acs5/subject",
  vintage = 2019,
  vars = inc_pop_col, 
  region = "tract:*", 
  regionin = "state:36+county:005,047,081,085,061") %>%
  naniar::replace_with_na_all(condition = ~.x < 0) # Input NA for annotated values in numeric columns 

# Combine the census tract data with the census tract shapefile
ct_acs <- nycctbounds %>%
  left_join(ct_inc_pop %>% select(!c("state", "county", "tract", "NAME")), by = "GEO_ID")

# Map it
library(leaflet)
library(htmltools)

# Add column of how many overlaps
ct_acs$num_intersect <- apply(st_intersects(ct_acs, mapping_data, sparse = FALSE),1,sum)

# Calculate area and tidy up
intersect_pct <- st_intersection(ct_acs, st_union(st_geometry(mapping_data))) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(OBJECTID, intersect_area, OBJECTID_area) %>%   # only select columns needed to merge
  st_drop_geometry() %>%  # drop geometry as we don't need it
  mutate(perc_overlap_area = as.numeric(intersect_area/OBJECTID_area)) 

ct_df <- merge(ct_acs,intersect_pct %>% select(OBJECTID,perc_overlap_area),by="OBJECTID",all.x = T)
#prox_to_something <- apply(st_intersects(nycctbounds, mapping_data, sparse = FALSE),1,sum)

labels <- paste("<h3>",mapping_data$facility_name,"</h3>",
                "<p>",mapping_data$address1,"<br>", mapping_data$city, ",", mapping_data$state, mapping_data$fac_zip,"</p>",
                "<p>","Description: ",mapping_data$description,"</p>"
                #"<hr>",
                #"<p><b> Top Insurances Accepted </b></p>",
                #"<ol type = \"1\" >",
                #"<li>","yes","</li>",
                #"<li>","No","</li>",
                #"<li>","Maybe, so","</li>",
                #"</ol>"
                )

# Make the chloropleth!
markerCol <- colorFactor(palette = 'Spectral', mapping_data$description)
proxCol <- colorBin(
  # five green (positive)
  palette = c('#ff0000','#83b48d','#007534'),
  bins = c(0,0.1,0.25,max(ct_df$perc_overlap_area, na.rm = T)),
  na.color = '#ff0000',
  domain = ct_df$perc_overlap_area)
pal_inc = colorBin(
  palette = c('#d5dded', '#afb9db', '#8996ca', '#6175b8', '#2f56a6'),
  bins = c(0,55000,70000,95000,130000,260000),
  domain = ct_df$S1901_C01_013E, 
  na.color = "#CACACA"
)

leaflet() %>%
  setView(-73.984865,40.710542,10.5) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=ct_df,
              weight = 4,
              color = ~proxCol(ct_df$perc_overlap_area),
              fillColor = ~pal_inc(ct_df$S1901_C01_013E),
              fillOpacity = 0.9) %>%
  addPolygons(data = mapping_data,
              color = ~markerCol(description),
              weight = 4,
              fillOpacity = 0.1
              ) %>%
  addCircleMarkers(data = mapping_data,
                   lng = ~as.numeric(longitude), 
                   lat = ~as.numeric(latitude),
                   popup = ~lapply(labels,HTML),
                   color = ~markerCol(description),
                   radius = 5,
                   group = "circles"
                   #clusterOptions = markerClusterOptions()
                   )  %>%
  addLegend(title = "Type of Facility (H+H)",
            pal = markerCol,
            values = HH_df$description,
            opacity = 0.5,
            group = "circles",
            'bottomright')

