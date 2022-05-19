library(mapview)
source("03. Mapping.R")

grep("East New York",nta_df$NTAName)

interested_locs <- c(216,72,97,54,19,62,252)
nta_df$NTAName[19]

for(i in interested_locs){
  # Title of Map legend
  title_legend <- HTML(paste0("<h2>",nta_df$NTA2020[i],": ",nta_df$NTAName[i],"</h2>"))

  map <- leaflet() %>%
    setView(nta_df$centroid_lon[i],nta_df$centroid_lat[i],14) %>%
    addPolygons(data=nta_df,
                weight = 1,
                opacity = 0,
                fillColor = ~pal_area(nta_df$perc_overlap_area * 100),
                fillOpacity = 1,
                popup = ~lapply(map_labels, HTML)) %>%
    addLegend(position ="topleft", 
              pal = pal_area, 
              opacity = 0.9,
              values = nta_df$perc_overlap_area*100,
              title =  "Percent of area close to <br>Hospitals/Clinics <br> <small>(within 10 min walk)</small><hr>",
              labFormat = labelFormat(suffix = "%")) %>%
    # Overlay groups
    addPolygons(data = nta_df %>% filter(med_income <= quant_income$brks[2]),
                weight = 2,
                fill=F,
                stroke = TRUE,
                opacity = 1,
                color = "#B63F26",
                group = "Bottom 25% Median Income") %>%
    addMarkers(data = mapping_data,
               lng = ~as.numeric(longitude), 
               lat = ~as.numeric(latitude),
               popup = ~lapply(icon_labels,HTML),
               icon = ~IconSet[icon],
               group = "Add Hospitals/Clinics"
    ) %>%
    # Layers control
    addLayersControl(
      overlayGroups = c("Add Hospitals/Clinics"),
      options = layersControlOptions(collapsed = TRUE),
      position = "topright"
    ) %>%
    setMapWidgetStyle(list(background= "white")) %>%
    addControl(med_income_legend, position = "topleft") %>%
    addControl(source_legend, position = "bottomright") %>%
    addControl(title_legend, position = "bottomleft")
  
  mapshot(map, file = paste0("../visual/",nta_df$NTA2020[i],"_",gsub(" ","_",nta_df$NTAName[i]),".png"), 
          vwidth = 900, vheight = 870)
}

