### Map

# Load data to start
# Have to add census api first
source("digital_divide/code/acs_all/01_census_pull.R")


### map by census tract ------------
pal_ct = colorNumeric(
  palette = "YlOrRd",
  domain = ct_acs$no_broadband, 
  na.color = "Grey"
)

map <- leaflet(ct_acs) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_ct(no_broadband),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal_ct, 
            opacity = 0.9,
            values = ct_acs$no_broadband,
            title =  "Proportion of Population</br>Without Broadband")

mapshot(map, file = "digital_divide/visual/acs_all/map_ct.png", 
        vwidth = 900, vheight = 870)


### map by NTA ------------
pal_nta = colorNumeric(
  palette = "YlOrRd",
  domain = nta_acs$no_broadband, 
  na.color = "Grey"
)

map <- leaflet(nta_acs) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_nta(no_broadband),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal_nta, 
            opacity = 0.9,
            values = nta_acs$no_broadband,
            title =  "Proportion of Seniors (65+)</br>Without Internet Access")

mapshot(map, file = "digital_divide/visual/acs_all/map_nta.png", 
        vwidth = 900, vheight = 870)

