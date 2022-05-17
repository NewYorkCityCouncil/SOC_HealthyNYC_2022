## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor", "sf", "leaflet", "leaflet.extras", 
                      "htmlwidgets", "RSocrata", "tidycensus", "jsonlite", "survey", "srvyr")

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)

# returns TRUE if package was loaded successfully

year = 2020

census_api_key(tidy_key, install = TRUE)


rm(list=ls())


v20 <- load_variables(year, "acs5", cache = TRUE)


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




# Load Census Tracts Dataset ----------------------------------------------

tracts <- read_sf('housing_affordability/data/2020 Census Tracts - Tabular/geo_export_c2cde8a0-e3cc-46bd-97a1-c9d18c356d24.shp') %>% 
  st_transform('+proj=longlat +datum=WGS84')




# race/ethnicity and ownership/renting ----------------------------------------------



# Estimate!!Total:	TENURE (WHITE ALONE HOUSEHOLDER)
# 136	B25003A_002	Estimate!!Total:!!Owner occupied	TENURE (WHITE ALONE HOUSEHOLDER)
# 137	B25003A_003	Estimate!!Total:!!Renter occupied	TENURE (WHITE ALONE HOUSEHOLDER)
# 138	B25003B_001	Estimate!!Total:	TENURE (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
# 139	B25003B_002	Estimate!!Total:!!Owner occupied	TENURE (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
# 140	B25003B_003	Estimate!!Total:!!Renter occupied	TENURE (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)
# 141	B25003C_001	Estimate!!Total:	TENURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
# 142	B25003C_002	Estimate!!Total:!!Owner occupied	TENURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
# 143	B25003C_003	Estimate!!Total:!!Renter occupied	TENURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)
# 144	B25003D_001	Estimate!!Total:	TENURE (ASIAN ALONE HOUSEHOLDER)
# 145	B25003D_002	Estimate!!Total:!!Owner occupied	TENURE (ASIAN ALONE HOUSEHOLDER)
# 146	B25003D_003	Estimate!!Total:!!Renter occupied	TENURE (ASIAN ALONE HOUSEHOLDER)
# 147	B25003E_001	Estimate!!Total:	TENURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
# 148	B25003E_002	Estimate!!Total:!!Owner occupied	TENURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
# 149	B25003E_003	Estimate!!Total:!!Renter occupied	TENURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)
# 150	B25003F_001	Estimate!!Total:	TENURE (SOME OTHER RACE ALONE HOUSEHOLDER)
# 151	B25003F_002	Estimate!!Total:!!Owner occupied	TENURE (SOME OTHER RACE ALONE HOUSEHOLDER)
# 152	B25003F_003	Estimate!!Total:!!Renter occupied	TENURE (SOME OTHER RACE ALONE HOUSEHOLDER)
# 153	B25003G_001	Estimate!!Total:	TENURE (TWO OR MORE RACES HOUSEHOLDER)
# 154	B25003G_002	Estimate!!Total:!!Owner occupied	TENURE (TWO OR MORE RACES HOUSEHOLDER)
# 155	B25003G_003	Estimate!!Total:!!Renter occupied	TENURE (TWO OR MORE RACES HOUSEHOLDER)
# 156	B25003H_001	Estimate!!Total:	TENURE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
# 157	B25003H_002	Estimate!!Total:!!Owner occupied	TENURE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
# 158	B25003H_003	Estimate!!Total:!!Renter occupied	TENURE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)
# 159	B25003I_001	Estimate!!Total:	TENURE (HISPANIC OR LATINO HOUSEHOLDER)
# 160	B25003I_002	Estimate!!Total:!!Owner occupied	TENURE (HISPANIC OR LATINO HOUSEHOLDER)
# 161	B25003I_003	Estimate!!Total:!!Renter occupied	TENURE (HISPANIC OR LATINO HOUSEHOLDER)



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
  select(-moe) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate')


for(i in 1:length(race_eth_vars)) names(by_race_eth_wide)[names(by_race_eth_wide) == race_eth_vars[i]] = race_eth_vars_rename[i]


# make adjustments to columns to reflect race/eth categories used in pay equity analyses

#' Asian (Non-Hispanic or Latino)
#' Black or African American (Non-Hispanic or Latino)
#' Hispanic or Latino 
#' Other (Non-Hispanic or Latino) 
#'  •	 American Indian or Alaska Native 
#'  •	 Native Hawaiian or Pacific Islander 
#'  •	 Two or More Races 
#'  •	 Other Race 
#'  White (Non-Hispanic or Latino) 


by_race_eth_wide <- by_race_eth_wide %>% 
  mutate(other_races_total = indigenous_total + hawaiian_pi_total +
           two_or_more_total + other_total,
         other_races_owner = indigenous_owner + hawaiian_pi_owner +
           two_or_more_owner + other_owner,
         other_races_renter = indigenous_renter + hawaiian_pi_renter +
           two_or_more_renter + other_renter) %>% 
  select(-indigenous_owner, -indigenous_renter, -indigenous_total,
         -hawaiian_pi_total, -hawaiian_pi_owner, -hawaiian_pi_renter,
         -other_owner, -other_total, -other_renter,
         -two_or_more_total, -two_or_more_owner, - two_or_more_renter,
         -white_total, -white_renter, -white_owner)



# add labor force data, ages 16+, pulled from acs online

labor <- read_csv('housing_affordability/data/labor_force.csv') %>% 
  select(race_ethnicity, total) %>% 
  pivot_wider(names_from = race_ethnicity, values_from = total) %>% 
  clean_names() %>% 
  mutate(other = native_hawaiian_and_other_pacific_islander_alone + 
           two_or_more_races +
           some_other_race_alone +
           american_indian_and_alaska_native_alone) %>% 
  select(-native_hawaiian_and_other_pacific_islander_alone, 
         -two_or_more_races, -some_other_race_alone,
         -american_indian_and_alaska_native_alone,
         -white_alone)

# get city-wide rate

values_sum <- list(values = colSums(Filter(is.numeric, by_race_eth_wide)))
column_names <- colnames(Filter(is.numeric, by_race_eth_wide))

by_race_city <- as.data.frame(do.call(rbind, values_sum))


ownership_rates <- c(by_race_ownership_rate_city$black_owner/labor$black_or_african_american_alone,
                     by_race_ownership_rate_city$asian_owner/labor$asian_alone,
                     by_race_ownership_rate_city$white_non_lat_owner/labor$white_alone_not_hispanic_or_latino,
                     by_race_ownership_rate_city$lat_hisp_owner/labor$hispanic_or_latino_origin_of_any_race,
                     by_race_ownership_rate_city$other_races_owner/labor$other,
                     1 - by_race_ownership_rate_city$black_owner/labor$black_or_african_american_alone,
                     1 - by_race_ownership_rate_city$asian_owner/labor$asian_alone,
                     1 - by_race_ownership_rate_city$white_non_lat_owner/labor$white_alone_not_hispanic_or_latino,
                     1 - by_race_ownership_rate_city$lat_hisp_owner/labor$hispanic_or_latino_origin_of_any_race,
                     1 - by_race_ownership_rate_city$other_races_owner/labor$other)

variable_names <- c('Black Only', 'Asian Only', 'White Only', 'Hispanic/Latin Any Race', 'Other Only',
                    'Black Only', 'Asian Only', 'White Only', 'Hispanic/Latin Any Race', 'Other Only')


fill_names = c('Owns Home', 'Owns Home', 'Owns Home', 'Owns Home', 'Owns Home',
               'Rents Home', 'Rents Home', 'Rents Home', 'Rents Home', 'Rents Home')


by_race_city <- tibble(variable_names, ownership_rates, fill_names)


stacked_bar_fill_palette <- c('#a73226', '#23417d')

ggplot(by_race_city, aes(x = variable_names, y = ownership_rates, fill = fill_names)) +
  geom_bar(stat = 'identity', position = position_fill(reverse = TRUE)) +
  labs(x = "Race/Ethnicity", y = "Percent of Population", 
       title = "Home Ownership Rate By Race/Ethnicity",
       fill = "Home Status") +
  theme_minimal() +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_fill_manual(values=stacked_bar_fill_palette)



