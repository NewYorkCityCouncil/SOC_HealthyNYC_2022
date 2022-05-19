# Load all the variables and packages
source("02. Data Wrangling.R")

library(classInt)
library(leaflet)
library(htmltools)
library(leaflet.extras)


############
# Map It!
#
# Making several decisions here including the bins of both proportion of area covered and income
############

# Add clinic or hospital classification for icon marker
mapping_data <- mapping_data %>%
  mutate(icon = ifelse(grepl("hospital",description, ignore.case = T),"hospital","clinic"))

IconSet <- iconList(
  hospital = makeIcon(iconUrl = "../data/icons/hospitals.png", iconWidth = 18, iconHeight = 18),
  clinic = makeIcon(iconUrl = "../data/icons/clinics.png", iconWidth = 19, iconHeight = 19)
)

# Labels when clicking on the markers
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

# Labels when clicking on the map
map_labels <- paste("<h3>",nta_df$NTA2020,"<br>",nta_df$NTAName,"</h3><hr>","<p><b> Median Household Income:</b>", scales::dollar(nta_df$med_income),
                    "<br>","<b>Percent of NTA within 10 min walk:</b>",scales::percent(nta_df$perc_overlap_area, accuracy = 2),"</p>")
# Source legend
source_legend <- HTML('<small> Source: NYC Planning, NYS DOH </small>')

# Make the chloropleth!
# Color binning
# Method 1: Access represented by polygon fill / Med Income represented by border color
nat_perc_area <- classIntervals(nta_df$perc_overlap_area * 100, n = 4, style = 'jenks') # Natural binning
pal_area <- colorBin(
  palette = c("#D1DFF6","#A4B2EC","#7087E1","#1D5FD6"),
  bins = c(c(round(nat_perc_area$brks,0)[1], round(nat_perc_area$brks,0)[2:4], round(nat_perc_area$brks,0)[5])),
  na.color = "Grey",
  domain = nta_df$perc_overlap_area * 100)

quant_income <- classIntervals(nta_df$med_income, n = 4, style = 'quantile') #Quantile binning
pal_income <- colorBin(
  palette = c('#B63F26'),
  bins = c(0,quant_income$brks[2] + 1),
  domain = nta_df$med_income, 
  na.color = "#CACACA"
)

# Median income legend  
med_income_legend <- HTML('<div> <strong style="color: #9B3D2C">Bottom 25% Income</strong> <br>
<small>(Median Household Income less than $47,572)</small></div>')

#<strong style="color: #CACACA">Medium Income</strong> <br>
#<strong style="color: #007534">High Income</strong></div>')

leaflet() %>%
  setView(-73.984865,40.710542,10.5) %>%
  #addProviderTiles("CartoDB.Positron") %>%
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
              color = "#9B3D2C",
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
  # Hide Markers Default
  hideGroup("Add Hospitals/Clinics") %>%
  setMapWidgetStyle(list(background= "white")) %>%
  addControl(med_income_legend, position = "topleft") %>%
  addControl(source_legend, position = "bottomright")

#########

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
              fillOpacity = 1,
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

