# Setup
system("docker run -d --shm-size=2g -p 4445:4444 selenium/standalone-chrome",wait=T)
Sys.sleep(2)

library(RSelenium)
library(rvest)
library(httr)
library(dplyr)
library(sf)

remDr <- RSelenium::remoteDriver(remoteServerAddr = "localhost",
                                 port = 4445L,
                                 browserName = "chrome")

remDr$open()

# function to unzip shapefile -----------

unzip_sf <- function(zip_url) {
  temp <- tempfile()
  temp2 <- tempfile()
  #download the zip folder from the internet save to 'temp' 
  download.file(zip_url, temp)
  #unzip the contents in 'temp' and save unzipped content in 'temp2'
  unzip(zipfile = temp, exdir = temp2)
  #if returns "character(0), then .shp may be nested within the folder
  your_SHP_file <- ifelse(!identical(list.files(temp2, pattern = ".shp$",full.names=TRUE), character(0)), 
                          list.files(temp2, pattern = ".shp$",full.names=TRUE), 
                          list.files(list.files(temp2, full.names=TRUE), pattern = ".shp$", full.names = TRUE))
  unlist(temp)
  unlist(temp2)
  return(your_SHP_file)
}

# Get NTA names from shapefile
nta_names <- read_sf(unzip_sf("https://www1.nyc.gov/assets/planning/download/zip/data-maps/open-data/nynta2020_22a.zip")) %>%
  st_drop_geometry %>%
  pull(NTA2020)

# Go to popfactfinder.planning.nyc.gov site
URL <- "https://popfactfinder.planning.nyc.gov/explorer/ntas/"
URL_end <- "?acsTopics=%2Cecon-incomeAndBenefits&source=acs-current"


# For loop web crawl household median incomes
pop_tables <- list()

for(i in 1:length(nta_names)){
  temp_URL <- paste0(URL,nta_names[i],URL_end)
  
  remDr$navigate(temp_URL)
  Sys.sleep(3)
  remDr$findElement(using = 'xpath', value = '//*[@id="reveal-modal-container"]/div/div[2]/button')$clickElement()
  Sys.sleep(2)
  remDr$findElement(using = 'xpath', value = '//*[@id="ember33"]')$clickElement()
  Sys.sleep(2)
  
  temp_html <- rvest::read_html(remDr$getPageSource()[[1]])
  pop_tables[[i]] <- html_table(html_element(temp_html,xpath = '//*[@id="ember26"]/div[2]/div[2]/table'))
  print(i)
  Sys.sleep(2)
}

med_income <- data.frame(NTA2020 = nta_names,
                         med_income = lapply(pop_tables,function(x) x[12,2]) %>% unlist,
                         med_income_cv = lapply(pop_tables,function(x) x[12,4]) %>% unlist) %>%
  mutate_all(na_if,"")

write.csv(med_income,"../data/NTA2020_med_income.csv",row.names = F)
