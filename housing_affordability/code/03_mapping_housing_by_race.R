## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.
#' 


list.of.packages <- c("tidyverse", "janitor", "sf", "leaflet", "leaflet.extras", 
                      "htmlwidgets", "RSocrata", "tidycensus", "jsonlite", "remotes",
                      'classInt', 'mapview')

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]


# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)



remotes::install_github("austensen/mapreliability")
library(mapreliability)


# returns TRUE if package was loaded successfully

year = 2019

census_api_key(tidy_key, install = TRUE)


rm(list=ls())


v19 <- load_variables(year, "acs5", cache = TRUE)


### Background ------------
# Map of Homeownership rates across the city 

## OUTLINE TEXT for Reference ##

# Foreclosure - affordable homeownership tie in


# Homeownership rates across the city council district or NTA (Ben, Brook)
# Focus on home ownership or condos?
#   Rpad has condos
# Race and ownership
# How many people per geographic unit own homes? How many people own the space they are living in?
#   What types of homes are available in that region? In what numbers?
#   Is the data on owning the home you live in acs?
#   

# resources: https://cnycn.org/new-york-foreclosure-data-explained/
# https://furmancenter.org/thestoop/entry/snapshot-of-homeownership-in-new-york-city
# https://fred.stlouisfed.org/graph/?g=OPNG
# https://fred.stlouisfed.org/series/HOWNRATEACS036005#0
# https://coredata.nyc/
# https://wherewelive.cityofnewyork.us/explore-data/housing-conditions/
# https://therealdeal.com/new-research/industry-reports/nyc-homeownership-rate-by-racial-ethnic-group-streeteasy/
# maybe Housing Vacancy Survey has homeownership by race for each puma area....




# Load Census Tracts and NTA Dataset ----------------------------------------------

# https://data.cityofnewyork.us/City-Government/2010-Census-Tracts/fxpq-c8ku

tracts <- read_csv('housing_affordability/data/nyct2010.csv') %>% 
  clean_names() %>% 
  select(boro_code, nta_code, ct2010) %>% 
  mutate(ct2010 = as.numeric(ct2010))

# tracts_geo <- read_sf('housing_affordability/data/2010 Census Tracts/geo_export_e7ed8d89-3627-472d-869d-d0c868bc6e6d.shp') %>% 
#   st_transform('+proj=longlat +datum=WGS84')

# https://data.cityofnewyork.us/City-Government/NTA-map/d3qk-pfyz

nta <- read_sf('housing_affordability/data/NTA map/geo_export_65ba306d-0eae-432f-bc2c-241064a40e43.shp') %>% 
  st_transform('+proj=longlat +datum=WGS84') %>% 
  select(ntacode, geometry) %>% 
  rename(nta_code = ntacode)


# Ownership/Renting -------------------------------------------------------




#' relevant variables:
#' B07013_002	Estimate!!Total:!!Householder lived in owner-occupied housing units	
#'                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR CURRENT RESIDENCE IN THE UNITED STATES
#' 
#' B25003_002	Estimate!!Total:!!Owner occupied	TENURE
#' B25106_024	Estimate!!Total:!!Renter-occupied housing units:	TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS
#' 

city_raw <- get_acs(geography = "tract", 
                    state = 'NY',
                    county = c('Kings County', 'Queens County', 
                               'Richmond County', 'Bronx County',
                               'New York County'), 
                    variables = c('B25106_024', 'B25003_002'),
                    year = year,
                    survey = 'acs5') %>% 
  clean_names()

# make each vraiable a column, rename
city_wide <- city_raw %>% 
  pivot_wider(names_from = 'variable', values_from = c('estimate', 'moe') ) %>% 
  rename(estimate_owner_occupied = estimate_B25003_002,
         estimate_renter_occupied = estimate_B25106_024,
         moe_owner_occupied = moe_B25003_002,
         moe_renter_occupied = moe_B25106_024)

# join with tracts
# first need to extract borough and ct from county and geoid respectively

city_wide$ct2010 <- as.numeric(str_sub(city_wide$geoid, start = -6, end = -1))

city_wide$boro <- str_extract(tolower(city_wide$name), "(?<=, ).+(?= county)")

city_wide$boro_code <- as.numeric(plyr:: mapvalues(city_wide$boro, 
                                 from=c("bronx", "kings", "new york", "queens", "richmond"), 
                                 to=c(2, 3, 1, 4, 5)))

city_wide_tract <- left_join(tracts, city_wide)

city_wide_nta <- left_join(city_wide_tract, nta)


cwnta_group <- city_wide_nta %>% 
  group_by(nta_code) %>% 
  summarise(estimate_owner_occupied = sum(estimate_owner_occupied),
            estimate_renter_occupied = sum(estimate_renter_occupied),
            moe_owner_occupied = sqrt(sum(moe_owner_occupied ** 2)),
            moe_renter_occupied = sqrt(sum(moe_renter_occupied ** 2)))



#city_wide$owned_rate <- round(replace_na(city_wide$owner_occupied / (city_wide$owner_occupied + city_wide$renter_occupied), 0), 4) * 100







# race/ethnicity and ownership/renting ----------------------------------------------


#' To Do
#' labor force by race to look at race/eth ownership  as percentage of race/eth labor force
#' rename categories based on pay equity names
#' calculate white latin/hisp and non-white latin/hisp
#' 
#' 
#' make map add up to nta (nick might have code)
#' check moe's

race_eth_vars = c('B25003A_001', 'B25003A_002', 'B25003A_003', 'B25003B_001', 'B25003B_002', 'B25003B_003', 'B25003C_001', 'B25003C_002', 'B25003C_003',
                  'B25003D_001', 'B25003D_002', 'B25003D_003', 'B25003E_001', 'B25003E_002', 'B25003E_003', 'B25003F_001', 'B25003F_002', 'B25003F_003',
                  'B25003G_001', 'B25003G_002', 'B25003G_003', 'B25003H_001', 'B25003H_002', 'B25003H_003', 'B25003I_001', 'B25003I_002', 'B25003I_003')

race_eth_vars_rename <- c('white_total', 'white_owner', 'white_renter', 'black_total', 'black_owner', 'black_renter','indigenous_total', 'indigenous_owner', 'indigenous_renter',
                          'asian_total', 'asian_owner', 'asian_renter','hawaiian_pi_total', 'hawaiian_pi_owner', 'hawaiian_pi_renter','other_total', 'other_owner', 'other_renter',
                          'two_or_more_total', 'two_or_more_owner', 'two_or_more_renter','white_non_lat_total', 'white_non_lat_owner', 'white_non_lat_renter','lat_hisp_total', 'lat_hisp_owner', 'lat_hisp_renter')



by_race_eth <- get_acs(geography = "tract", 
                       state = 'NY',
                       county = c('Kings County', 'Queens County', 
                                  'Richmond County', 'Bronx County',
                                  'New York County'), 
                       variables = race_eth_vars,
                       year = year,
                       survey = 'acs5') %>% 
  clean_names()


by_race_eth_wide <- by_race_eth %>% 
  pivot_wider(names_from = 'variable', values_from = c('estimate', 'moe'))

acs_nums <- c('estimate_', 'moe_')


# rename columnn vars
for(j in 1:length(acs_nums)) {
  for(i in 1:length(race_eth_vars)) {
    names(by_race_eth_wide)[names(by_race_eth_wide) == paste0(acs_nums[j], race_eth_vars[i])] = paste0(acs_nums[j],race_eth_vars_rename[i])}
}

# create estimate of other (will need to calculate new moe later)
# remove white (combines with hispanic)
brew <- by_race_eth_wide %>% 
  select(-estimate_white_total, -estimate_white_owner, -estimate_white_renter,
         -moe_white_total, -moe_white_owner, -moe_white_renter) %>% 
  mutate(estimate_other_races_total = estimate_indigenous_total + estimate_hawaiian_pi_total +
           estimate_two_or_more_total + estimate_other_total,
         estimate_other_races_owner = estimate_indigenous_owner + estimate_hawaiian_pi_owner +
           estimate_two_or_more_owner + estimate_other_owner,
         estimate_other_races_renter = estimate_indigenous_renter + estimate_hawaiian_pi_renter +
           estimate_two_or_more_renter + estimate_other_renter,
         moe_other_races_total = sqrt(moe_indigenous_total ** 2 + moe_hawaiian_pi_total  ** 2 +
           moe_two_or_more_total  ** 2 + moe_other_total  ** 2),
         moe_other_races_owner = sqrt(moe_indigenous_owner  ** 2 + moe_hawaiian_pi_owner  ** 2 +
           moe_two_or_more_owner  ** 2 + moe_other_owner  ** 2),
         moe_other_races_renter = sqrt(moe_indigenous_renter  ** 2 + moe_hawaiian_pi_renter  ** 2 +
           moe_two_or_more_renter  ** 2 + moe_other_renter  ** 2))



# join with tracts
# first need to extract borough and ct from county and geoid respectively

brew$ct2010 <- as.numeric(str_sub(brew$geoid, start = -6, end = -1))

brew$boro <- str_extract(tolower(brew$name), "(?<=, ).+(?= county)")

brew$boro_code <- as.numeric(plyr:: mapvalues(brew$boro, 
                                                   from=c("bronx", "kings", "new york", "queens", "richmond"), 
                                                   to=c(2, 3, 1, 4, 5)))

brew_tracts <- left_join(tracts, brew)


brew_nta <- left_join(brew_tracts, nta) %>% 
  replace_na()


brew_nta_group <- brew_nta %>% 
  group_by(nta_code) %>% 
  summarise(estimate_black_total = sum(estimate_black_total),
            estimate_black_owner = sum(estimate_black_owner),
            estimate_asian_total = sum(estimate_asian_total),
            estimate_asian_owner = sum(estimate_asian_owner),
            estimate_white_non_lat_total = sum(estimate_white_non_lat_total),
            estimate_white_non_lat_owner = sum(estimate_white_non_lat_owner),
            estimate_lat_hisp_total = sum(estimate_lat_hisp_total),
            estimate_lat_hisp_owner = sum(estimate_lat_hisp_owner),
            estimate_other_races_total = sum(estimate_other_races_total),
            estimate_other_races_owner = sum(estimate_other_races_owner),
            moe_black_total = sqrt(sum(moe_black_total ** 2)),
            moe_black_owner = sqrt(sum(moe_black_owner ** 2)),
            moe_asian_total = sqrt(sum(moe_asian_total ** 2)),
            moe_asian_owner = sqrt(sum(moe_asian_owner ** 2)),
            moe_white_non_lat_total = sqrt(sum(moe_white_non_lat_total ** 2)),
            moe_white_non_lat_owner = sqrt(sum(moe_white_non_lat_owner ** 2)),
            moe_lat_hisp_total = sqrt(sum(moe_lat_hisp_total ** 2)),
            moe_lat_hisp_owner = sqrt(sum(moe_lat_hisp_owner ** 2)),
            moe_other_races_total = sqrt(sum(moe_other_races_total ** 2)),
            moe_other_races_owner = sqrt(sum(moe_other_races_owner ** 2)))


rate_calc <- function(est_tot, est_own, moe_tot, moe_own) {
  ifelse(moe_tot >= est_tot | moe_own >= est_own,
         'Insufficient Data',
         paste0(as.character(round(est_own/est_tot * 100, 0)), "%"))
}

brew_nta_group <- brew_nta_group %>% 
  mutate(black_owner_rate = rate_calc(estimate_black_total, estimate_black_owner, moe_black_total, moe_black_total),
         asian_owner_rate = rate_calc(estimate_asian_total, estimate_asian_owner, moe_asian_total, moe_asian_total),
         white_owner_rate = rate_calc(estimate_white_non_lat_total, estimate_white_non_lat_owner, moe_white_non_lat_total, moe_white_non_lat_total),
         lat_hisp_owner_rate = rate_calc(estimate_lat_hisp_total, estimate_lat_hisp_owner, moe_lat_hisp_total, moe_lat_hisp_total),
         other_races_owner_rate = rate_calc(estimate_other_races_total, estimate_other_races_owner, moe_other_races_total, moe_other_races_total))



# combine all relevant data
# reintroduce geometry


all_dat <- left_join(nta, cwnta_group) %>% 
  left_join(brew_nta_group)


all_dat$all_owner_rate <- all_dat$estimate_owner_occupied / (all_dat$estimate_owner_occupied + all_dat$estimate_renter_occupied) * 100


# leaflet map of ownership rate

plot(density(all_dat$all_owner_rate, na.rm = TRUE))

classints <- classIntervals(na.omit(all_dat$all_owner_rate), n = 5, style = 'equal')

city_wide_pal <- colorBin(palette = c('#d5dded', '#afb9db', '#8996ca', '#6175b8', '#2f56a6'),
                          domain = all_dat$all_owner_rate, na.color = "transparent",
                          bins = classints$brks)



# popup by race/eth


popup <- paste0('<h3>Ownership Rate by Race/Ethnicity</h3>',
                '<hr>',
                '<strong>Black Only:</strong> ', all_dat$black_owner_rate, '<br>',
                '<strong>Asian Only:</strong> ', all_dat$asian_owner_rate, '<br>',
                '<strong>White Only:</strong> ', all_dat$white_owner_rate, '<br>',
                '<strong>Hispanic/Latin Any Race:</strong> ', all_dat$lat_hisp_owner_rate, '<br>',
                '<strong>Other Only:</strong> ', all_dat$other_races_owner_rate)





map <- leaflet() %>% 
  setView(-73.97, 40.718, zoom = 10.5) %>% 
  addPolygons(data = all_dat,
              color = ~city_wide_pal(all_owner_rate),
              weight = 0,
              fillOpacity = 1,
              popup = popup) %>% 
  addLegend(position ="topleft", 
            pal = city_wide_pal,
            opacity = 1,
            values = city_wide_geo$owned_rate,
            title = paste("Home Ownership Rate by NTA"),
            labFormat = labelFormat(digits = 0, suffix = "%", between = "% - ")) %>% 
  setMapWidgetStyle(list(background= "white"))

map

saveWidget(map, file="housing_affordability/visual/race_eth_home_ownership_map.html")

mapshot(map, file="housing_affordability/visual/race_eth_home_ownership_map.png", 
        vwidth = 900, vheight = 870, remove_controls = 'zoomControl')
