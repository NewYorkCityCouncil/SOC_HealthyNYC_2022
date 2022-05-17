### Map

# Load data to start
# Have to add census api first
source("digital_divide/code/01_pums_pull.R")


### map by PUMA ------------
# histogram of data to choose breaks
hist(1 - nyc_pums_hispeed$hi_speed_pct, breaks = 15)

nat_intvl_hispeed = classIntervals((1 - nyc_pums_hispeed$hi_speed_pct) * 100, n = 5, style = 'jenks')

pal_puma = colorBin(
  palette = c('#d5dded', '#afb9db', '#8996ca', '#6175b8', '#2f56a6'),
  bins = c(round(nat_intvl_hispeed$brks,0)[1]-1, round(nat_intvl_hispeed$brks,0)[2:5], round(nat_intvl_hispeed$brks,0)[6]+1),
  domain = 1 - nyc_pums_hispeed$hi_speed_pct, 
  na.color = "Grey"
)

map <- leaflet(nyc_pums_hispeed) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_puma((1 - nyc_pums_hispeed$hi_speed_pct) * 100),
              fillOpacity = 0.9) %>% 
  addLegend(position ="topleft", 
            pal = pal_puma, 
            opacity = 0.9,
            values = (1 - nyc_pums_hispeed$hi_speed_pct) * 100,
            title =  "Percent of NYC Households Without</br>High Speed Broadband Access",
            labFormat = labelFormat(suffix = "%")) %>%
  setMapWidgetStyle(list(background= "white"))
  

map

mapshot(map, file = "digital_divide/visual/map_puma.png", 
        vwidth = 900, vheight = 870)

