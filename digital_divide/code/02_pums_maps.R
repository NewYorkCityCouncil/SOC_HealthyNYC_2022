### Map

# Load data to start
# Have to add census api first
source("digital_divide/code/01_pums_pull.R")


### map by PUMA ------------
# histogram of data to choose breaks
hist(1 - nyc_pums_hispeed$hi_speed_pct, breaks = 15)

pal_puma = colorBin(
  palette = "YlOrRd",
  bins = c(0.1,0.2,0.3,0.4,0.5),
  domain = 1 - nyc_pums_hispeed$hi_speed_pct, 
  na.color = "Grey"
)

map <- leaflet(nyc_pums_hispeed) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addProviderTiles("CartoDB.Positron") %>%
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_puma(1- hi_speed_pct),
              fillOpacity = 0.9) %>% 
  addLegend(position ="bottomright", 
            pal = pal_puma, 
            opacity = 0.9,
            values = 1 - nyc_pums_hispeed$hi_speed_pct,
            title =  "Proportion of NYC Households Without</br>High Speed Broadband Access")

mapshot(map, file = "digital_divide/visual/map_puma.png", 
        vwidth = 900, vheight = 870)

