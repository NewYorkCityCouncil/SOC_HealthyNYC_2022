# Load all the variables and packages
source("02. Data Wrangling.R")

library(classInt)
library(leaflet)
library(htmltools)
library(leaflet.extras)

# Add column of how many overlaps
nta_df$num_intersect <- apply(st_intersects(nta_df, mapping_data, sparse = FALSE),1,sum)

# Add NTA area
nta_df$NTA_area <- st_area(nta_df)

# Calculate area and tidy up
intersect_pct <- st_intersection(nta_df, st_union(st_geometry(mapping_data))) %>% 
  mutate(intersect_area = st_area(.)) %>%   # create new column with shape area
  dplyr::select(NTA2020, intersect_area, NTA_area) %>%   # only select columns needed to merge
  st_drop_geometry() %>%  # drop geometry as we don't need it
  mutate(perc_overlap_area = as.numeric(intersect_area/NTA_area)) 

nta_df <- merge(nta_df,intersect_pct %>% select(NTA2020,perc_overlap_area),by="NTA2020",all.x = T) %>%
  # Replace NAs with 0
  mutate(perc_overlap_area = ifelse(is.na(perc_overlap_area),0,perc_overlap_area))

# Add clinic or hospital classification for icon marker
mapping_data <- mapping_data %>%
  mutate(icon = ifelse(grepl("hospital",description, ignore.case = T),"hospital","clinic"))

IconSet <- iconList(
  hospital = makeIcon(iconUrl = "../data/icons/hospitals.png", iconWidth = 18, iconHeight = 18),
  clinic = makeIcon(iconUrl = "../data/icons/clinics.png", iconWidth = 19, iconHeight = 19)
)

icon_labels <- paste("<h3>",mapping_data$facility_name,"</h3>",
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

# source legend
source_legend <- HTML('<small> Source: NYC Planning, NYS DOH </small>')

# Pop-ups for NTAs
map_labels <- paste("<h3>",nta_df$NTA2020,"<br>",nta_df$NTAName,"</h3><hr>","<p><b> Median Household Income:</b>", scales::dollar(nta_df$med_income),
                    "<br>","<b>Percent of NTA within 10 min walk:</b>",scales::percent(nta_df$perc_overlap_area, accuracy = 2),"</p>")

# Make the chloropleth!
# Color binning
# Method 1: Access represented by polygon fill / Med Income represented by border color
nat_perc_area = classIntervals(nta_df$perc_overlap_area * 100, n = 4, style = 'jenks')
pal_area <- colorBin(
  palette = c('#d5dded', '#afb9db', '#8996ca', '#2f56a6'),
  bins = c(c(round(nat_perc_area$brks,0)[1], round(nat_perc_area$brks,0)[2:4], round(nat_perc_area$brks,0)[5])),
  na.color = "Grey",
  domain = nta_df$perc_overlap_area * 100)

pal_income <- colorBin(
  palette = c('#B63F26','#CACACA','#007534'),
  bins = c(0,55000,100000,150000),
  domain = nta_df$med_income, 
  na.color = "#CACACA"
)

# median income legend  
med_income_legend <- HTML('<div> <strong style="color: #B63F26">Low Income</strong> <br>
                          <strong style="color: #CACACA">Medium Income</strong> <br>
                          <strong style="color: #007534">High Income</strong></div>')

leaflet() %>%
  setView(-73.984865,40.710542,10.5) %>%
  #addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=nta_df,
              weight = 2,
              color = ~pal_income(nta_df$med_income),
              fillColor = ~pal_area(nta_df$perc_overlap_area * 100),
              fillOpacity = 0.9,
              popup = ~lapply(map_labels, HTML),
              group = "Median Income") %>%
  addLegend(position ="topright", 
            pal = pal_area, 
            opacity = 0.9,
            values = nta_df$perc_overlap_area*100,
            title =  "Percent of area close to <br>Hospitals/Clinics <br> <small>(within 10 min walk)</small><hr>",
            labFormat = labelFormat(suffix = "%"), 
            group = "Median Income") %>%
  # Overlay groups
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
    position = "bottomleft"
  ) %>%
  # Show markers only when zoomLevels are at 12.5+
  groupOptions("Add Hospitals/Clinics", zoomLevels = 12.5:20) %>%
  setMapWidgetStyle(list(background= "white")) %>%
  addControl(med_income_legend, position = "topright") %>%
  addControl(source_control, position = "bottomright")

# Method 2: Med Income represented by polygon fill / Access represented by border color
pal_income <- colorBin(
  palette = c('#d5dded', '#afb9db', '#8996ca', '#6175b8', '#2f56a6'),
  bins = c(0,55000,70000,95000,130000,max(nta_df$med_income, na.rm = T)),
  na.color = "Grey",
  domain = nta_df$med_income)
nat_perc_area = classIntervals(nta_df$perc_overlap_area * 100, n = 2, style = 'jenks')
pal_area <- colorBin(
  palette = c('#B63F26','#007534'),
  bins = c(c(round(nat_perc_area$brks,0)[1], round(nat_perc_area$brks,0)[2], round(nat_perc_area$brks,0)[3])),
  na.color = "Grey",
  domain = nta_df$perc_overlap_area * 100)

# Health access legend  
med_income_legend <- HTML('<div> <strong style="color: #B63F26">Low Access To Hospitals/Clinics</strong> <br> 
<small style="color: #B63F26"> (Less than 54% of area is within a ten minute walk to nearest Hospital/Clinic) </small> <br>
<strong style="color: #007534">High Access To Hospitals/Clinics</strong> <br> 
<small style="color: #007534"> (More than 54% of area is within a ten minute walk to nearest Hospital/Clinic) </small></div>')

leaflet() %>%
  setView(-73.984865,40.710542,10.5) %>%
  #addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=nta_df,
              weight = 2,
              color = ~pal_area(nta_df$perc_overlap_area * 100),
              fillColor = ~pal_income(nta_df$med_income),
              fillOpacity = 0.9,
              popup = ~lapply(map_labels, HTML),
              group = "Median Income") %>%
  addControl(med_income_legend, position = "topright") %>%
  addLegend(position ="topright", 
            pal = pal_income, 
            opacity = 0.9,
            values = nta_df$med_income,
            title =  "Median Household Income<hr>",
            labFormat = labelFormat(prefix = "$",big.mark = ","), 
            group = "Median Income") %>%
  # Overlay groups
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
    position = "bottomleft"
  ) %>%
  setMapWidgetStyle(list(background= "white")) %>%
  addControl(source_control, position = "bottomright")

