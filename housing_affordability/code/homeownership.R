## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "janitor", "sf", "leaflet", "leaflet.extras", 
                      "htmlwidgets", "RSocrata", "tidycensus", "jsonlite")

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
  select(-moe) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate') %>% 
  rename(owner_occupied = B25003_002,
         renter_occupied = B25106_024)

city_wide$owned_rate <- round(replace_na(city_wide$owner_occupied / (city_wide$owner_occupied + city_wide$renter_occupied), 0), 4) * 100


# histogram of ownership rate by census tract

ggplot(data = city_wide, aes(x = owned_rate)) + 
  geom_histogram()

# leaflet map of ownership rate

city_wide_pal <- colorBin(palette = "Blues",
                domain = city_wide$owned_rate, na.color = "transparent")

city_wide_geo <- left_join(tracts, city_wide)

leaflet(data = city_wide_geo) %>% 
  addProviderTiles("CartoDB.Positron") %>% 
  addPolygons(color = ~city_wide_pal(owned_rate),
              weight = 0,
              fillOpacity = 1) %>% 
  addLegend(position ="topleft", 
            pal = city_wide_pal,
            opacity = 1,
            values = city_wide_geo$owned_rate,
            title = paste("Home Ownership Rate, by Tract")) 



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


# calculate owner rate by race/ethnicity

by_race_ownership_rate_tract <- by_race_eth_wide %>% 
  mutate(white_owner_rate = round(white_owner/white_total, 2),
         black_owner_rate = round(black_owner/black_total, 2),
         indigenous_owner_rate = round(indigenous_owner/indigenous_total, 2),
         asian_owner_rate = round(asian_owner/asian_total, 2),
         hawaiian_pi_owner_rate = round(hawaiian_pi_owner/hawaiian_pi_total, 2),
         other_owner_rate = round(other_owner/other_total, 2),
         two_or_more_owner_rate = round(two_or_more_owner/two_or_more_total, 2),
         white_non_lat_owner_rate = round(white_non_lat_owner/white_non_lat_total, 2),
         lat_hisp_owner_rate = round(lat_hisp_owner/lat_hisp_total, 2))


# join to tract data

tract_by_race_ownershp_geo <- left_join(tracts, by_race_ownership_rate_tract)


# create one row df of ownership total owners and renters by race/ethnicity

values_sum <- list(values = colSums(Filter(is.numeric, by_race_eth_wide)))
column_names <- colnames(Filter(is.numeric, by_race_eth_wide))

by_race_ownership_rate_city <- as.data.frame(do.call(rbind, values_sum))




# calculate rate of ownership by race/ethnicity [ owners / (owners + renters) ]

by_race_ownership_rate_city <- as_tibble(by_race_ownership_rate_city) %>% 
  mutate(white_owner_rate = round(white_owner/white_total, 2),
         black_owner_rate = round(black_owner/black_total, 2),
         indigenous_owner_rate = round(indigenous_owner/indigenous_total, 2),
         asian_owner_rate = round(asian_owner/asian_total, 2),
         hawaiian_pi_owner_rate = round(hawaiian_pi_owner/hawaiian_pi_total, 2),
         other_owner_rate = round(other_owner/other_total, 2),
         two_or_more_owner_rate = round(two_or_more_owner/two_or_more_total, 2),
         white_non_lat_owner_rate = round(white_non_lat_owner/white_non_lat_total, 2),
         lat_hisp_owner_rate = round(lat_hisp_owner/lat_hisp_total, 2)) %>% 
  select(white_owner_rate, black_owner_rate, indigenous_owner_rate, asian_owner_rate,
         hawaiian_pi_owner_rate, other_owner_rate, two_or_more_owner_rate,
         white_non_lat_owner_rate, lat_hisp_owner_rate) 

# pivot data from horizontal to vertical for plotting

brorc_pivot <- by_race_ownership_rate_city %>% 
  rename('White' = white_owner_rate,
         'Black' = black_owner_rate,
         'Indigenous' = indigenous_owner_rate,
         'Asian' = asian_owner_rate,
         'Hawaiian/Pacific Islander' = hawaiian_pi_owner_rate,
         'Other Race' = other_owner_rate,
         'Two or More Races' = two_or_more_owner_rate,
         'White Non-Latin/Hispanic' = white_non_lat_owner_rate,
         'Latin/Hispanic' = lat_hisp_owner_rate) %>% 
  pivot_longer(cols = 1:length(colnames(by_race_ownership_rate_city)))


# plot ownership rate in bar graph

ggplot(data = brorc_pivot, aes(x = name, y = value, fill = name)) +
  geom_bar(stat = 'identity') +
  labs(x = 'Race/Ethnicity', y = 'Ownership Rate', 
       title = 'Rate of Home Ownership by Race/Ethnicity, NYC') +
  theme_minimal() +
  guides(x = guide_axis(n.dodge = 2))



# Home Ownership as Percentage of Work Force by Race  ---------------------


work_force <- read_csv('housing_affordability/data/labor_force.csv')
  



















# building-size ownership/renting -----------------------------------------



# 2	B25032_002	Estimate!!Total:!!Owner-occupied housing units:	TENURE BY UNITS IN STRUCTURE
# 3	B25032_003	Estimate!!Total:!!Owner-occupied housing units:!!1, detached	TENURE BY UNITS IN STRUCTURE
# 4	B25032_004	Estimate!!Total:!!Owner-occupied housing units:!!1, attached	TENURE BY UNITS IN STRUCTURE
# 5	B25032_005	Estimate!!Total:!!Owner-occupied housing units:!!2	TENURE BY UNITS IN STRUCTURE
# 6	B25032_006	Estimate!!Total:!!Owner-occupied housing units:!!3 or 4	TENURE BY UNITS IN STRUCTURE
# 7	B25032_007	Estimate!!Total:!!Owner-occupied housing units:!!5 to 9	TENURE BY UNITS IN STRUCTURE
# 8	B25032_008	Estimate!!Total:!!Owner-occupied housing units:!!10 to 19	TENURE BY UNITS IN STRUCTURE
# 9	B25032_009	Estimate!!Total:!!Owner-occupied housing units:!!20 to 49	TENURE BY UNITS IN STRUCTURE
# 10	B25032_010	Estimate!!Total:!!Owner-occupied housing units:!!50 or more	TENURE BY UNITS IN STRUCTURE


# 13	B25032_013	Estimate!!Total:!!Renter-occupied housing units:	TENURE BY UNITS IN STRUCTURE
# 14	B25032_014	Estimate!!Total:!!Renter-occupied housing units:!!1, detached	TENURE BY UNITS IN STRUCTURE
# 15	B25032_015	Estimate!!Total:!!Renter-occupied housing units:!!1, attached	TENURE BY UNITS IN STRUCTURE
# 16	B25032_016	Estimate!!Total:!!Renter-occupied housing units:!!2	TENURE BY UNITS IN STRUCTURE
# 17	B25032_017	Estimate!!Total:!!Renter-occupied housing units:!!3 or 4	TENURE BY UNITS IN STRUCTURE
# 18	B25032_018	Estimate!!Total:!!Renter-occupied housing units:!!5 to 9	TENURE BY UNITS IN STRUCTURE
# 19	B25032_019	Estimate!!Total:!!Renter-occupied housing units:!!10 to 19	TENURE BY UNITS IN STRUCTURE
# 20	B25032_020	Estimate!!Total:!!Renter-occupied housing units:!!20 to 49	TENURE BY UNITS IN STRUCTURE
# 21	B25032_021	Estimate!!Total:!!Renter-occupied housing units:!!50 or more	TENURE BY UNITS IN STRUCTURE




size_vars = c('B25032_002', 'B25032_003', 'B25032_004', 'B25032_005', 'B25032_006', 'B25032_007', 'B25032_008', 'B25032_009', 'B25032_009',
              'B25032_013', 'B25032_014', 'B25032_015', 'B25032_016', 'B25032_017', 'B25032_018', 'B25032_019', 'B25032_020', 'B25032_021')

size_vars_rename <- c('owner_total', 'owner_1_detach', 'owner_1_attach', 'owner_2', 'owner_3_4', 'owner_5_9', 'owner_10_19', 'owner_20_49', 'owner_50_plus',
                      'renter_total', 'renter_1_detach', 'renter_1_attach', 'renter_2', 'renter_3_4', 'renter_5_9', 'renter_10_19', 'renter_20_49', 'renter_50_plus')



by_size <- get_acs(geography = "tract", 
                   state = 'NY',
                   county = c('Kings County', 'Queens County', 
                              'Richmond County', 'Bronx County',
                              'New York County'), 
                   variables = size_vars,
                   year = year,
                   survey = 'acs5') %>% 
  clean_names()

by_size_wide <- by_size %>% 
  select(-moe) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate')


for(i in 1:length(size_vars)) names(by_size_wide)[names(by_size_wide) == size_vars[i]] = size_vars_rename[i]







# Age and ownership/renting -----------------------------------------------

# B25007_001	Estimate!!Total:	TENURE BY AGE OF HOUSEHOLDER
# 163	B25007_002	Estimate!!Total:!!Owner occupied:	TENURE BY AGE OF HOUSEHOLDER
# 164	B25007_003	Estimate!!Total:!!Owner occupied:!!Householder 15 to 24 years	TENURE BY AGE OF HOUSEHOLDER
# 165	B25007_004	Estimate!!Total:!!Owner occupied:!!Householder 25 to 34 years	TENURE BY AGE OF HOUSEHOLDER
# 166	B25007_005	Estimate!!Total:!!Owner occupied:!!Householder 35 to 44 years	TENURE BY AGE OF HOUSEHOLDER
# 167	B25007_006	Estimate!!Total:!!Owner occupied:!!Householder 45 to 54 years	TENURE BY AGE OF HOUSEHOLDER
# 168	B25007_007	Estimate!!Total:!!Owner occupied:!!Householder 55 to 59 years	TENURE BY AGE OF HOUSEHOLDER
# 169	B25007_008	Estimate!!Total:!!Owner occupied:!!Householder 60 to 64 years	TENURE BY AGE OF HOUSEHOLDER
# 170	B25007_009	Estimate!!Total:!!Owner occupied:!!Householder 65 to 74 years	TENURE BY AGE OF HOUSEHOLDER
# 171	B25007_010	Estimate!!Total:!!Owner occupied:!!Householder 75 to 84 years	TENURE BY AGE OF HOUSEHOLDER
# 172	B25007_011	Estimate!!Total:!!Owner occupied:!!Householder 85 years and over	TENURE BY AGE OF HOUSEHOLDER
# 173	B25007_012	Estimate!!Total:!!Renter occupied:	TENURE BY AGE OF HOUSEHOLDER
# 174	B25007_013	Estimate!!Total:!!Renter occupied:!!Householder 15 to 24 years	TENURE BY AGE OF HOUSEHOLDER
# 175	B25007_014	Estimate!!Total:!!Renter occupied:!!Householder 25 to 34 years	TENURE BY AGE OF HOUSEHOLDER
# 176	B25007_015	Estimate!!Total:!!Renter occupied:!!Householder 35 to 44 years	TENURE BY AGE OF HOUSEHOLDER
# 177	B25007_016	Estimate!!Total:!!Renter occupied:!!Householder 45 to 54 years	TENURE BY AGE OF HOUSEHOLDER
# 178	B25007_017	Estimate!!Total:!!Renter occupied:!!Householder 55 to 59 years	TENURE BY AGE OF HOUSEHOLDER
# 179	B25007_018	Estimate!!Total:!!Renter occupied:!!Householder 60 to 64 years	TENURE BY AGE OF HOUSEHOLDER
# 180	B25007_019	Estimate!!Total:!!Renter occupied:!!Householder 65 to 74 years	TENURE BY AGE OF HOUSEHOLDER
# 181	B25007_020	Estimate!!Total:!!Renter occupied:!!Householder 75 to 84 years	TENURE BY AGE OF HOUSEHOLDER
# 182	B25007_021	Estimate!!Total:!!Renter occupied:!!Householder 85 years and over	TENURE BY AGE OF HOUSEHOLDER



#' TO DO
#' 

age_vars = c('B25007_001', 'B25007_002', 'B25007_003', 'B25007_004', 'B25007_005', 'B25007_006', 'B25007_007',
              'B25007_008', 'B25007_009', 'B25007_010', 'B25007_011', 'B25007_012', 'B25007_013', 'B25007_014',
              'B25007_015', 'B25007_016', 'B25007_017', 'B25007_018', 'B25007_019', 'B25007_020', 'B25007_021')

age_vars_rename <- c('total', 'total_owner', 'total_owner_15_24', 'total_owner_25_34', 'total_owner_35_44', 'total_owner_45_54', 'total_owner_55_59',
                     'total_owner_60_64', 'total_owner_65_74', 'total_owner_75_84', 'total_owner_85_plus', 'total_renter', 'total_renter_15_24', 'total_renter_25_34', 
                     'total_renter_35_44', 'total_renter_45_54', 'total_renter_55_59', 'total_renter_60_64', 'total_renter_65_74', 'total_renter_75_84', 'total_renter_85_plus')



by_age <- get_acs(geography = "tract", 
                   state = 'NY',
                   county = c('Kings County', 'Queens County', 
                              'Richmond County', 'Bronx County',
                              'New York County'), 
                   variables = age_vars,
                   year = year,
                   survey = 'acs5') %>% 
  clean_names()

by_age_wide <- by_age %>% 
  select(-moe) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate')


for(i in 1:length(age_vars)) names(by_age_wide)[names(by_age_wide) == age_vars[i]] = age_vars_rename[i]



# Population ownership/rental ---------------------------------------------

# B25008_001	Estimate!!Total:	TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE
# 184	B25008_002	Estimate!!Total:!!Owner occupied	TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE
# 185	B25008_003	Estimate!!Total:!!Renter occupied	TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE


pop_vars = c('B25008_001', 'B25008_002', 'B25008_003')

pop_vars_rename <- c('total_pop_housed', 'total_pop_in_owned', 'total_pop_in_rented')



by_total_pop <- get_acs(geography = "tract", 
                   state = 'NY',
                   county = c('Kings County', 'Queens County', 
                              'Richmond County', 'Bronx County',
                              'New York County'), 
                   variables = pop_vars,
                   year = year,
                   survey = 'acs5') %>% 
  clean_names()

by_total_pop_wide <- by_total_pop %>% 
  select(-moe) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate')


for(i in 1:length(pop_vars)-1) names(by_total_pop_wide)[names(by_total_pop_wide) == pop_vars[i]] = pop_vars_rename[i]


# Get population Variables ------------------------------------------------

# 1	B01001_001	Estimate!!Total:	SEX BY AGE
# 2	B01001_002	Estimate!!Total:!!Male:	SEX BY AGE

# 11	B01001_011	Estimate!!Total:!!Male:!!25 to 29 years	SEX BY AGE
# 12	B01001_012	Estimate!!Total:!!Male:!!30 to 34 years	SEX BY AGE
# 13	B01001_013	Estimate!!Total:!!Male:!!35 to 39 years	SEX BY AGE
# 14	B01001_014	Estimate!!Total:!!Male:!!40 to 44 years	SEX BY AGE
# 15	B01001_015	Estimate!!Total:!!Male:!!45 to 49 years	SEX BY AGE
# 16	B01001_016	Estimate!!Total:!!Male:!!50 to 54 years	SEX BY AGE
# 17	B01001_017	Estimate!!Total:!!Male:!!55 to 59 years	SEX BY AGE
# 18	B01001_018	Estimate!!Total:!!Male:!!60 and 61 years	SEX BY AGE
# 19	B01001_019	Estimate!!Total:!!Male:!!62 to 64 years	SEX BY AGE
# 20	B01001_020	Estimate!!Total:!!Male:!!65 and 66 years	SEX BY AGE
# 21	B01001_021	Estimate!!Total:!!Male:!!67 to 69 years	SEX BY AGE
# 22	B01001_022	Estimate!!Total:!!Male:!!70 to 74 years	SEX BY AGE
# 23	B01001_023	Estimate!!Total:!!Male:!!75 to 79 years	SEX BY AGE
# 24	B01001_024	Estimate!!Total:!!Male:!!80 to 84 years	SEX BY AGE
# 25	B01001_025	Estimate!!Total:!!Male:!!85 years and over	SEX BY AGE
# 26	B01001_026	Estimate!!Total:!!Female:	SEX BY AGE

# 35	B01001_035	Estimate!!Total:!!Female:!!25 to 29 years	SEX BY AGE
# 36	B01001_036	Estimate!!Total:!!Female:!!30 to 34 years	SEX BY AGE
# 37	B01001_037	Estimate!!Total:!!Female:!!35 to 39 years	SEX BY AGE
# 38	B01001_038	Estimate!!Total:!!Female:!!40 to 44 years	SEX BY AGE
# 39	B01001_039	Estimate!!Total:!!Female:!!45 to 49 years	SEX BY AGE
# 40	B01001_040	Estimate!!Total:!!Female:!!50 to 54 years	SEX BY AGE
# 41	B01001_041	Estimate!!Total:!!Female:!!55 to 59 years	SEX BY AGE
# 42	B01001_042	Estimate!!Total:!!Female:!!60 and 61 years	SEX BY AGE
# 43	B01001_043	Estimate!!Total:!!Female:!!62 to 64 years	SEX BY AGE
# 44	B01001_044	Estimate!!Total:!!Female:!!65 and 66 years	SEX BY AGE
# 45	B01001_045	Estimate!!Total:!!Female:!!67 to 69 years	SEX BY AGE
# 46	B01001_046	Estimate!!Total:!!Female:!!70 to 74 years	SEX BY AGE
# 47	B01001_047	Estimate!!Total:!!Female:!!75 to 79 years	SEX BY AGE
# 48	B01001_048	Estimate!!Total:!!Female:!!80 to 84 years	SEX BY AGE
# 49	B01001_049	Estimate!!Total:!!Female:!!85 years and over	SEX BY AGE


age_sex_vars = c('B01001_011', 'B01001_012', 'B01001_013', 'B01001_014', 'B01001_015',
                 'B01001_016', 'B01001_017', 'B01001_018', 'B01001_019', 'B01001_020',
                 'B01001_021', 'B01001_022', 'B01001_023', 'B01001_024', 'B01001_025',
                 'B01001_035', 'B01001_036', 'B01001_037', 'B01001_038', 'B01001_039', 
                 'B01001_040', 'B01001_041', 'B01001_042', 'B01001_043', 'B01001_044',
                 'B01001_045', 'B01001_046', 'B01001_047', 'B01001_048', 'B01001_049')
age_sex_vars_rename <- c('total_male_25_29', 'total_male_30_34', 'total_male_35_39', 'total_male_40_44', 'total_male_45_49',
                         'total_male_50_54', 'total_male_55_59', 'total_male_60_61', 'total_male_62_64', 'total_male_65_66', 
                         'total_male_67_69', 'total_male_70_74', 'total_male_75_79', 'total_male_80_84', 'total_male_85_plus',
                         'total_female_25_29', 'total_female_30_34', 'total_female_35_39', 'total_female_40_44', 'total_female_45_49',
                         'total_female_50_54', 'total_female_55_59', 'total_female_60_61', 'total_female_62_64', 'total_female_65_66', 
                         'total_female_67_69', 'total_female_70_74', 'total_female_75_79', 'total_female_80_84', 'total_female_85_plus')



ct_ages <- get_acs(geography = "tract", 
                        state = 'NY',
                        county = c('Kings County', 'Queens County', 
                                   'Richmond County', 'Bronx County',
                                   'New York County'), 
                        variables = age_sex_vars,
                        year = year,
                        survey = 'acs5') %>% 
  clean_names()

ct_ages_wide <- ct_ages %>% 
  select(-moe) %>% 
  pivot_wider(names_from = 'variable', values_from = 'estimate')


for(i in 1:length(age_sex_vars)) names(ct_ages_wide)[names(ct_ages_wide) == age_sex_vars[i]] = age_sex_vars_rename[i]


ct_ages_wide <- ct_ages_wide %>% 
  mutate(total_female_25_34 = total_female_25_29 + total_female_30_34,
         total_female_35_44 = total_female_35_39 + total_female_40_44,
         total_female_45_54 = total_female_50_54 + total_female_55_59,
         total_female_60_64 = total_female_60_61 + total_female_62_64,
         total_female_65_74 = total_female_65_66 + total_female_67_69 + total_female_70_74,
         total_female_75_84 = total_female_75_79 + total_female_80_84,
         total_female_85_pl = total_female_85_plus,
         total_male_25_34 = total_male_25_29 + total_male_30_34,
         total_male_35_44 = total_male_35_39 + total_male_40_44,
         total_male_45_54 = total_male_50_54 + total_male_55_59,
         total_male_60_64 = total_male_60_61 + total_male_62_64,
         total_male_65_74 = total_male_65_66 + total_male_67_69 + total_male_70_74,
         total_male_75_84 = total_male_75_79 + total_male_80_84,
         total_male_85_pl = total_male_85_plus) %>% 
  select(-age_sex_vars_rename)
         
         






# # Race by Census Tract ----------------------------------------------------

########

# 
# 
# # B02001_001	Estimate!!Total:	RACE
# # 70	B02001_002	Estimate!!Total:!!White alone	RACE
# # 71	B02001_003	Estimate!!Total:!!Black or African American alone	RACE
# # 72	B02001_004	Estimate!!Total:!!American Indian and Alaska Native alone	RACE
# # 73	B02001_005	Estimate!!Total:!!Asian alone	RACE
# # 74	B02001_006	Estimate!!Total:!!Native Hawaiian and Other Pacific Islander alone	RACE
# # 75	B02001_007	Estimate!!Total:!!Some other race alone	RACE
# # 76	B02001_008	Estimate!!Total:!!Two or more races:	RACE
# # 77	B02001_009	Estimate!!Total:!!Two or more races:!!Two races including Some other race	RACE
# # 78	B02001_010	Estimate!!Total:!!Two or more races:!!Two races excluding Some other race, and three or more races	RACE
# 
# 
# ct_race_vars <- c('B02001_002', 'B02001_003', 'B02001_004', 'B02001_005',
#                   'B02001_006', 'B02001_007', 'B02001_008', 'B02001_009',
#                   'B02001_010')
# 
# ct_race_vars_rename <- c('white', 'black', 'indigenous', 'asian', 'hawaiian_PI')

