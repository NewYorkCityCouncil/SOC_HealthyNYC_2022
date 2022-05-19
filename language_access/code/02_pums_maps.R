### Map

# Load data to start
# Have to add census api first
source("language_access/code/01_pums_pull.R")


### map by PUMA ------------
# histogram of data to choose breaks
hist(nyc_pums_toplang$prop_lang, breaks = 15)

nat_intvl_top = classIntervals(nyc_pums_toplang$prop_lang * 100, n = 5, style = 'jenks')

pal_puma = colorBin(
  palette = c('#d5dded', '#afb9db', '#8996ca', '#6175b8', '#2f56a6'),
  bins = c(round(nat_intvl_top$brks,0)[1]-1, round(nat_intvl_top$brks,0)[2:5], round(nat_intvl_top$brks,0)[6]+1),
  domain = nyc_pums_toplang$prop_lang * 100, 
  na.color = "Grey"
)

map <- leaflet(nyc_pums_toplang) %>%
  setView(-73.941281,40.704103, zoom=11) %>% 
  addPolygons(weight = 1,
              color = "grey",
              stroke = FALSE,
              fillColor = ~pal_puma(nyc_pums_toplang$prop_lang * 100),
              fillOpacity = 0.9) %>% 
  addLegend(position ="topleft", 
            pal = pal_puma, 
            opacity = 0.9,
            values = nyc_pums_toplang$prop_lang * 100,
            title =  "Percent of NYC Residents Speaking</br>Language Outside of Top 13 at Home",
            labFormat = labelFormat(suffix = "%")) %>%
  setMapWidgetStyle(list(background= "white"))
  

map

mapshot(map, file = "language_access/visual/map_puma.png", 
        vwidth = 900, vheight = 870)

