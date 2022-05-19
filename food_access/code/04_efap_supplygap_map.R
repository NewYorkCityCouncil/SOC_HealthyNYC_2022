### Map

# Load data and libraries to start
source("food_access/code/03_data_assembly.R")

boro <- read_sf("https://data.cityofnewyork.us/api/geospatial/tqmj-j8zm?method=export&format=GeoJSON") %>% 
  st_transform("+proj=longlat +datum=WGS84") %>% 
  st_simplify(dTolerance = .00001)

# check appropriate binning for the map
# https://gistbok.ucgis.org/bok-topics/statistical-mapping-enumeration-normalization-classification#Classification
plot(density(efap_neigh$fy22_weighted_score))
# there is a skew, trying jenks

### Map efap locations ----------------------------------

leaflet(efap) %>%
  setView(-73.984865,40.710542,10.5) %>%
  setMapWidgetStyle(list(background= "white")) %>% 
  addCircleMarkers(popup = lapply(paste("<p>",
                                        "Address: ",
                                        efap$distadd,
                                        "</p>", "<p>",
                  paste0("Boro: ",efap$distboro),"</p>"), HTML), 
                   radius = 0.1)

int_nta <- classIntervals(neigh_prio$fy22_weighted_score, n = 5, style = 'fisher')

pal_nta <- colorBin(
  palette = c('#d5dded', '#afb9db', '#8996ca', '#6175b8', '#2f56a6'),
  bins = int_nta$brks,
  domain = neigh_prio$fy22_weighted_score, 
  na.color = "#e5f1ea"
)

pal_efap <- colorFactor(c("#BF8D37"), domain = c("EFAP Locations"))

# highest need break
recode <- neigh_prio %>%
  mutate(bottom25 = case_when(fy22_weighted_score>=int_nta$brks[5] ~1,  TRUE ~ 0))

# supply gap legend
# 
addLegendCustom <- function(map, colors, labels, sizes, shapes, borders, opacity = 1){

make_shapes <- function(colors, sizes, borders, shapes) {
  shapes <- gsub("circle", "50%", shapes)
  paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
}
make_labels <- function(sizes, labels) {
  paste0("<div style='display: inline-block;height: ", 
         sizes, "px;margin-top: 4px;line-height: ", 
         sizes, "px;'>", labels, "</div>")
}

legend_colors <- make_shapes(colors, sizes, borders, shapes)
legend_labels <- unique(efap$locations)

return(addLegend(map, colors = legend_colors, labels = legend_labels, opacity = opacity, position = "bottomright"))

}


map <- leaflet() %>%
  setView(-73.984865,40.710542,10.5) %>%
  setMapWidgetStyle(list(background= "white")) %>% 
  addPolygons(data=boro, stroke = T, fill=F, color = "#666666", weight = 1) %>%
  addPolygons(data = neigh_prio, 
              weight = 1, opacity = 0.6, 
              color = ~pal_nta(fy22_weighted_score),
              fillColor = ~pal_nta(fy22_weighted_score),
              fillOpacity = 0.9, 
              popup = lapply(paste("<p>","NTA: ",neigh_prio$nta_name,"</p>", 
                                   "<p>",paste0("Score: ",neigh_prio$fy22_weighted_score),"</p>"), HTML)) %>%
  addPolygons(data = recode, weight = 2, fill=F, opacity = 1,
              color = "#23417D", stroke = recode$bottom25) %>%
  addCircleMarkers(data = efap,
    popup = lapply(paste("<p>","Address: ",
                       efap$distadd,"</p>", "<p>",
                  paste0("Boro: ",efap$distboro),"</p>"), HTML),
    radius = 3, stroke = F, fillColor = "#BF8D37",
    fillOpacity = 1) %>%
  addLegend("topleft", pal = pal_nta, 
            values = neigh_prio$fy22_weighted_score,
            title = "<strong>Supply Gap Score</strong><hr><small>Level of Need</br>Low to High</small>",
            opacity = 10.9,
            labFormat = labelFormat(digits = 1)) %>%
  #addLegendCustom(colors, labels, sizes, shapes, borders) %>%
addLegend(data = efap, "topleft", pal = pal_efap,
          values = efap$locations)

mapshot(map, file = "food_access/visual/efap_score.png", 
        vwidth = 900, vheight = 870)

### Relationships between efap locations and score ----------------------------------

ggplot(data = efap_neigh, aes(x = fy22_weighted_score, y = num_programs)) + geom_point() + ylim(0,15)
