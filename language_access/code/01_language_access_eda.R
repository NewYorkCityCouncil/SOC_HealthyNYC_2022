## Load Libraries -----------------------------------------------

#' NOTE: The code below is intended to load all listed libraries. If you do not
#' have these libraries on your computer, the code will attempt to INSTALL them.
#' 
#' IF YOU DO NOT WANT TO INSTALL ANY OF THESE PACKAGES, DO NOT RUN THIS CODE.

list.of.packages <- c("tidyverse", "tidycensus",  "janitor", "sf", "leaflet", "leaflet.extras", 
                      "htmlwidgets", "RSocrata", "tidycensus", "jsonlite", 'censusapi',
                      "survey", "srvyr", 'scales')

# checks if packages has been previously installed
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# if not, packages are installed
if(length(new.packages)) install.packages(new.packages)

# packages are loaded
lapply(list.of.packages, require, character.only = TRUE)

# returns TRUE if package was loaded successfully


### Background ------------

#' Where there is a high density of population that speaks a language outside of 
#' the top 7 (law requires agency documents to be translated into the top 7 
#' languages used in the city - languages are based on census data
#' 
#' https://languagemap.nyc/Info/About
#' 





# Relevant Variables ------------------------------------------------------

# 213	B16001_003	Estimate!!Total:!!Spanish:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 215	B16001_005	Estimate!!Total:!!Spanish:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 216	B16001_006	Estimate!!Total:!!French (incl. Cajun):	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 218	B16001_008	Estimate!!Total:!!French (incl. Cajun):!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 219	B16001_009	Estimate!!Total:!!Haitian:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 221	B16001_011	Estimate!!Total:!!Haitian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 222	B16001_012	Estimate!!Total:!!Italian:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 224	B16001_014	Estimate!!Total:!!Italian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 225	B16001_015	Estimate!!Total:!!Portuguese:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 227	B16001_017	Estimate!!Total:!!Portuguese:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 228	B16001_018	Estimate!!Total:!!German:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 230	B16001_020	Estimate!!Total:!!German:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 231	B16001_021	Estimate!!Total:!!Yiddish, Pennsylvania Dutch or other West Germanic languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 233	B16001_023	Estimate!!Total:!!Yiddish, Pennsylvania Dutch or other West Germanic languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 234	B16001_024	Estimate!!Total:!!Greek:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 236	B16001_026	Estimate!!Total:!!Greek:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 237	B16001_027	Estimate!!Total:!!Russian:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 239	B16001_029	Estimate!!Total:!!Russian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 240	B16001_030	Estimate!!Total:!!Polish:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 242	B16001_032	Estimate!!Total:!!Polish:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 243	B16001_033	Estimate!!Total:!!Serbo-Croatian:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 245	B16001_035	Estimate!!Total:!!Serbo-Croatian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 246	B16001_036	Estimate!!Total:!!Ukrainian or other Slavic languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 248	B16001_038	Estimate!!Total:!!Ukrainian or other Slavic languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 249	B16001_039	Estimate!!Total:!!Armenian:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 251	B16001_041	Estimate!!Total:!!Armenian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 252	B16001_042	Estimate!!Total:!!Persian (incl. Farsi, Dari):	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 254	B16001_044	Estimate!!Total:!!Persian (incl. Farsi, Dari):!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 255	B16001_045	Estimate!!Total:!!Gujarati:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 257	B16001_047	Estimate!!Total:!!Gujarati:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 258	B16001_048	Estimate!!Total:!!Hindi:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 260	B16001_050	Estimate!!Total:!!Hindi:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 261	B16001_051	Estimate!!Total:!!Urdu:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 263	B16001_053	Estimate!!Total:!!Urdu:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 264	B16001_054	Estimate!!Total:!!Punjabi:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 266	B16001_056	Estimate!!Total:!!Punjabi:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 267	B16001_057	Estimate!!Total:!!Bengali:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 269	B16001_059	Estimate!!Total:!!Bengali:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 270	B16001_060	Estimate!!Total:!!Nepali, Marathi, or other Indic languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 272	B16001_062	Estimate!!Total:!!Nepali, Marathi, or other Indic languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 273	B16001_063	Estimate!!Total:!!Other Indo-European languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 275	B16001_065	Estimate!!Total:!!Other Indo-European languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 276	B16001_066	Estimate!!Total:!!Telugu:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 278	B16001_068	Estimate!!Total:!!Telugu:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 279	B16001_069	Estimate!!Total:!!Tamil:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 281	B16001_071	Estimate!!Total:!!Tamil:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 282	B16001_072	Estimate!!Total:!!Malayalam, Kannada, or other Dravidian languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 284	B16001_074	Estimate!!Total:!!Malayalam, Kannada, or other Dravidian languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 285	B16001_075	Estimate!!Total:!!Chinese (incl. Mandarin, Cantonese):	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 287	B16001_077	Estimate!!Total:!!Chinese (incl. Mandarin, Cantonese):!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 288	B16001_078	Estimate!!Total:!!Japanese:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 290	B16001_080	Estimate!!Total:!!Japanese:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 291	B16001_081	Estimate!!Total:!!Korean:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 293	B16001_083	Estimate!!Total:!!Korean:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 294	B16001_084	Estimate!!Total:!!Hmong:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 296	B16001_086	Estimate!!Total:!!Hmong:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 297	B16001_087	Estimate!!Total:!!Vietnamese:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 299	B16001_089	Estimate!!Total:!!Vietnamese:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 300	B16001_090	Estimate!!Total:!!Khmer:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 302	B16001_092	Estimate!!Total:!!Khmer:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 303	B16001_093	Estimate!!Total:!!Thai, Lao, or other Tai-Kadai languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 305	B16001_095	Estimate!!Total:!!Thai, Lao, or other Tai-Kadai languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 306	B16001_096	Estimate!!Total:!!Other languages of Asia:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 308	B16001_098	Estimate!!Total:!!Other languages of Asia:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 309	B16001_099	Estimate!!Total:!!Tagalog (incl. Filipino):	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 311	B16001_101	Estimate!!Total:!!Tagalog (incl. Filipino):!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 312	B16001_102	Estimate!!Total:!!Ilocano, Samoan, Hawaiian, or other Austronesian languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 314	B16001_104	Estimate!!Total:!!Ilocano, Samoan, Hawaiian, or other Austronesian languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 315	B16001_105	Estimate!!Total:!!Arabic:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 317	B16001_107	Estimate!!Total:!!Arabic:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 318	B16001_108	Estimate!!Total:!!Hebrew:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 320	B16001_110	Estimate!!Total:!!Hebrew:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 321	B16001_111	Estimate!!Total:!!Amharic, Somali, or other Afro-Asiatic languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 323	B16001_113	Estimate!!Total:!!Amharic, Somali, or other Afro-Asiatic languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 324	B16001_114	Estimate!!Total:!!Yoruba, Twi, Igbo, or other languages of Western Africa:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 326	B16001_116	Estimate!!Total:!!Yoruba, Twi, Igbo, or other languages of Western Africa:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 327	B16001_117	Estimate!!Total:!!Swahili or other languages of Central, Eastern, and Southern Africa:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 329	B16001_119	Estimate!!Total:!!Swahili or other languages of Central, Eastern, and Southern Africa:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 330	B16001_120	Estimate!!Total:!!Navajo:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 332	B16001_122	Estimate!!Total:!!Navajo:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 333	B16001_123	Estimate!!Total:!!Other Native languages of North America:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 335	B16001_125	Estimate!!Total:!!Other Native languages of North America:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 336	B16001_126	Estimate!!Total:!!Other and unspecified languages:	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 338	B16001_128	Estimate!!Total:!!Other and unspecified languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK E




# Less than Proficient English Variables ----------------------------------

# 215	B16001_005	Estimate!!Total:!!Spanish:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 218	B16001_008	Estimate!!Total:!!French (incl. Cajun):!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 221	B16001_011	Estimate!!Total:!!Haitian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 224	B16001_014	Estimate!!Total:!!Italian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 227	B16001_017	Estimate!!Total:!!Portuguese:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 230	B16001_020	Estimate!!Total:!!German:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 233	B16001_023	Estimate!!Total:!!Yiddish, Pennsylvania Dutch or other West Germanic languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 236	B16001_026	Estimate!!Total:!!Greek:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 239	B16001_029	Estimate!!Total:!!Russian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 242	B16001_032	Estimate!!Total:!!Polish:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 245	B16001_035	Estimate!!Total:!!Serbo-Croatian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 248	B16001_038	Estimate!!Total:!!Ukrainian or other Slavic languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 251	B16001_041	Estimate!!Total:!!Armenian:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 254	B16001_044	Estimate!!Total:!!Persian (incl. Farsi, Dari):!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 257	B16001_047	Estimate!!Total:!!Gujarati:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 260	B16001_050	Estimate!!Total:!!Hindi:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 263	B16001_053	Estimate!!Total:!!Urdu:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 266	B16001_056	Estimate!!Total:!!Punjabi:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 269	B16001_059	Estimate!!Total:!!Bengali:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 272	B16001_062	Estimate!!Total:!!Nepali, Marathi, or other Indic languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 275	B16001_065	Estimate!!Total:!!Other Indo-European languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 278	B16001_068	Estimate!!Total:!!Telugu:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 281	B16001_071	Estimate!!Total:!!Tamil:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 284	B16001_074	Estimate!!Total:!!Malayalam, Kannada, or other Dravidian languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 287	B16001_077	Estimate!!Total:!!Chinese (incl. Mandarin, Cantonese):!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 290	B16001_080	Estimate!!Total:!!Japanese:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 293	B16001_083	Estimate!!Total:!!Korean:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 296	B16001_086	Estimate!!Total:!!Hmong:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 299	B16001_089	Estimate!!Total:!!Vietnamese:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 302	B16001_092	Estimate!!Total:!!Khmer:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 305	B16001_095	Estimate!!Total:!!Thai, Lao, or other Tai-Kadai languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 308	B16001_098	Estimate!!Total:!!Other languages of Asia:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 311	B16001_101	Estimate!!Total:!!Tagalog (incl. Filipino):!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 314	B16001_104	Estimate!!Total:!!Ilocano, Samoan, Hawaiian, or other Austronesian languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 317	B16001_107	Estimate!!Total:!!Arabic:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 320	B16001_110	Estimate!!Total:!!Hebrew:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 323	B16001_113	Estimate!!Total:!!Amharic, Somali, or other Afro-Asiatic languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 326	B16001_116	Estimate!!Total:!!Yoruba, Twi, Igbo, or other languages of Western Africa:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 329	B16001_119	Estimate!!Total:!!Swahili or other languages of Central, Eastern, and Southern Africa:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 332	B16001_122	Estimate!!Total:!!Navajo:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 335	B16001_125	Estimate!!Total:!!Other Native languages of North America:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER
# 338	B16001_128	Estimate!!Total:!!Other and unspecified languages:!!Speak English less than "very well"	LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK E







#' DATA PROBLEM
#' Neither tidycensus nor censusapi have access to the data we need, but the acs
#' does in fact have this data. Therefore we are downloading the data from this
#' source: 
#' https://data.census.gov/cedsci/table?q=B16001%3A%20LANGUAGE%20SPOKEN%20AT%20HOME%20BY%20ABILITY%20TO%20SPEAK%20ENGLISH%20FOR%20THE%20POPULATION%205%20YEARS%20AND%20OVER&g=1600000US3651000&tid=ACSDT1Y2019.B16001


# clean df of just 'less than well' english proficiency
less_than_well <- read_csv('language_access/data/languages.csv') %>% 
  clean_names() %>% 
  filter(grepl('less than', language)) %>% 
  mutate(language = gsub(':Speak English less than "very well"', '', language)) %>% 
  rename(count_less_than_well = new_york_city_new_york_estimate,
         moe = new_york_city_new_york_margin_of_error) %>% 
  arrange(desc(count_less_than_well))

# clean df of all individuals who speak non-english languages natively
language_raw <- read_csv('language_access/data/languages.csv') %>% 
  clean_names() %>% 
  filter(!grepl('very well', language)) %>% 
  mutate(language = gsub(':', '', language)) %>% 
  rename(count_total = new_york_city_new_york_estimate,
         moe_total = new_york_city_new_york_margin_of_error) %>% 
  arrange(desc(count_total))



# join them
# remove unreliable


languages <- left_join(less_than_well, language_raw) %>% 
  filter(count_less_than_well > moe,
         count_total > moe_total)




#languages$not_proficient_rate <- languages$count_less_than_well/languages$count_total

languages$proficient <- languages$count_total - languages$count_less_than_well



lang_long <- languages %>% 
  select(-moe, -moe_total, -count_total) %>% 
  mutate('Not Proficient in English' = count_less_than_well) %>% 
  rename('Proficient in English' = proficient,
         Language = language) %>% 
  pivot_longer(cols = c(`Proficient in English`, `Not Proficient in English`),
               names_to = 'Proficiency',
               values_to = 'Count')

lang_long$Proficiency <- lang_long$Proficiency %>% 
  factor(levels= c("Proficient in English","Not Proficient in English"))


lang_long <- lang_long %>% 
  arrange(desc(count_less_than_well)) %>% 
  filter(Language != "Spanish")


lang_final <- lang_long[1:30,]


stacked_bar_fill_palette <- c('#23417d', '#a73226')


plot <- ggplot(lang_final, aes(x = reorder(Language, count_less_than_well), y = Count, fill = Proficiency)) +
  geom_bar(stat = 'identity') + 
  theme_minimal() +
  guides(x = guide_axis(n.dodge = 2)) +
  scale_fill_manual(values=stacked_bar_fill_palette) +
  scale_x_discrete(labels = c(
    "Chinese (incl. Mandarin, Cantonese)" = "Chinese (Mand, Cant)",                        
    "Russian" = "Russian",                                                     
    "Bengali" = "Bengali",                                                     
    "Yiddish, Pennsylvania Dutch or other West Germanic languages" = "Yiddish",
    "Haitian" = "Haitian",                                                     
    "Korean" = "Korean",                                                      
    "Arabic" = "Arabic",                                                      
    "Polish" = "Polish",                                                      
    "Yoruba, Twi, Igbo, or other languages of Western Africa" = "Western African langs.",     
    "Urdu" = "Urdu",                                                        
    "Italian" = "Italian",                                                     
    "French (incl. Cajun)" = "French (incl. Cajun)",                                        
    "Other lang. Asia" = "Other lang. Asia",                                     
    "Other Indo-European languages" = "Other Indo-European",                               
    "Tagalog (incl. Filipino)" = "Tagalog (incl. Filipino)"
  )) +
  scale_y_continuous(labels = comma) +
  labs(x = "Language Spoken at Home", y = "Total People", 
       title = "Non-Native English Speakers by Language and English Proficiency in NYC", 
       subtitle = "Excluding the almost 2 million Spanish speakers, of which over 800,000 are not proficient in English") +
  coord_flip()


plot



#' remove "top languages", considered by NYC to be:
#' Spanish, Chinese, Russian, Bengali, Haitian, Korean, Arabic, Urdu, French, Polish



top_languages <- c(languages$language[1:4], languages$language[6:11])

