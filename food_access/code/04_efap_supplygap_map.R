### Map

# Load data and libraries to start
source("food_access/code/03_data_assembly.R")

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
  addLegend("topleft", pal = pal_nta, values = neigh_prio$fy22_weighted_score,
            title = "Supply Gap Score</br>(Lowest to Highest</br>Level of Need)",
            opacity = 0.6) %>%
  addLegend("topleft", pal = pal_efap, values = efap$prop_legend,
            title = "Number of Programs</br>at Location",
            opacity = 0.6)

mapshot(map, file = "food_access/visual/efap_score.png", 
        vwidth = 900, vheight = 870)

### Relationships between efap locations and score ----------------------------------

ggplot(data = efap_neigh, aes(x = fy22_weighted_score, y = num_programs)) + geom_point() + ylim(0,15)
