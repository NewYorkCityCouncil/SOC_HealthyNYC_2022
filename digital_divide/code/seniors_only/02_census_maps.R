### Map

# Load data to start
# Have to add census api first
source("digital_divide/code/seniors_only/01_census_pull.R")


### map by census tract ------------
pal_ct = colorNumeric(
  palette = "YlOrRd",
  domain = ct_acs$no_internet, 
  na.color = "Grey"
)

map <- leaflet(ct_acs) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_ct(no_internet),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal_ct, 
            opacity = 0.9,
            values = ct_acs$no_internet,
            title =  "Proportion of Seniors (65+)</br>Without Internet Access")

mapshot(map, file = "digital_divide/visual/seniors_only/map_ct.png", 
        vwidth = 900, vheight = 870)


### map by NTA ------------
pal_nta = colorNumeric(
  palette = "YlOrRd",
  domain = nta_acs$No_Internet, 
  na.color = "Grey"
)

map <- leaflet(nta_acs) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_nta(No_Internet),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal_nta, 
            opacity = 0.9,
            values = nta_acs$No_Internet,
            title =  "Proportion of Seniors (65+)</br>Without Internet Access")

mapshot(map, file = "digital_divide/visual/seniors_only/map_nta.png", 
        vwidth = 900, vheight = 870)

