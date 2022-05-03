library(censusapi)
library(dplyr)
library(stringr)
library(janitor)
library(jsonlite)
library(RSocrata)
library(tidyr)

### REQUEST ------------
# map of senior population and internet metric 

## OUTLINE TEXT for Reference ##
# Close the Digital Divide to advance health and opportunity for youth, older adults and NYers with disabilities 
# > Device Donation Program – Connect old equipment from city agencies and private donors to those in need (based on programs in San Francisco and New Orleans) 
# > Digital Literacy Programming at DFTA 
# > Digital Literacy and Assistive Technology Investments for NYers with disabilities 

# check ACS 2019 & 2020 5 Year Tables at the Census Tract level;
# check PUMS 2019 & 2020, it has sample individual level data;
# check what internet metrics are there;
# then decide whether to aggregate up to NTA, CDTA, or PUMA


# example PUMS code -----------
# https://github.com/NewYorkCityCouncil/Racial_Disparity_Report/blob/4e87b005f5baf1579450cb435b55006e131c28a3/code/rm_acs_pull.R

# hispanic origin/race at puma level for 2019 ----
group_B03002 <- listCensusMetadata(
  name = "acs/acs5",
  vintage = 2019,
  type = "variables",
  group = "B03002")
group_B03002 <- group_B03002[,1:2]
group_B03002 <- group_B03002[grep("Estimate!!Total:",group_B03002$label),]
group_B03002 <- group_B03002 %>% filter(!str_detect(label, "Annotation of"))
group_B03002$label <- gsub("!!", "_", group_B03002$label)
group_B03002$label <- gsub(":", "", group_B03002$label)
group_B03002 <- group_B03002 %>%  arrange(name)


vars <- c("NAME", "GEO_ID", "B03002_001E","B03002_002E",
                 "B03002_003E", "B03002_004E", "B03002_005E",
                 "B03002_006E", "B03002_007E","B03002_008E",
               "B03002_009E", "B03002_012E") 
#geos <- listCensusMetadata(name = "acs/acs5", vintage = 2019, type = "geographies")

hisp_race_puma <- getCensus(
  key = Sys.getenv("KEY"),
  name = "acs/acs5",
  vintage = 2019,
  vars = vars, 
  region = "public use microdata area:*", 
  regionin = "state:36")

# subset to NYC
hisp_race_puma <- hisp_race_puma[grep("NYC-", hisp_race_puma$NAME),]

# rename columns
names(hisp_race_puma)[5:14]<- c(group_B03002$label[c(1:9,12)])
hisp_race_puma <- hisp_race_puma %>% clean_names()