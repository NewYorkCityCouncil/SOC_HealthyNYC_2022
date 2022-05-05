# Scrape EFAP pdf

library(pdftools)
library(tidyverse)

rm(list=ls())

url <- "https://www1.nyc.gov/assets/hra/downloads/pdf/services/efap/EFAP_ACTIVE.pdf"

text <- pdf_text(url) %>% read_lines() %>% str_squish()

date <- str_replace_all(str_extract(text[1], "[0-9]+/[0-9]+/[0-9]+"), "/", "_")
FP <- as.numeric(na.omit(unique(str_extract(str_extract(text, "FP = [0-9]+"), "[0-9]+")))[1])
SK <- as.numeric(na.omit(unique(str_extract(str_extract(text, "SK = [0-9]+"), "[0-9]+")))[1])
clean_text <- text[!str_detect(text, "EFAP OPEN PROGRAMS") & !str_detect(text, "EFRO#")
                   & !str_detect(text, "info by boro in alpha order") & !str_detect(text, "DISTZIP")
                   & !str_detect(text, "FP = ") & !str_detect(text, "SK = ") & str_detect(text, ".+")]

for (i in length(clean_text):1) {
  if(!str_detect(clean_text[i], "^8[0-9]{4} ")) {
    clean_text[i-1] <- paste(clean_text[i-1], clean_text[i])
  }
}

clean_text <- clean_text[str_detect(clean_text, "^8[0-9]{4} ")]

clean_text <- str_replace(clean_text, " EFAP MASTER LIST [[:print:]]+ DISTBORO", "")
  
df <- data.frame(matrix(,nrow=length(clean_text),ncol=8))

colnames(df) <- c(str_split(text[2], " ")[[1]][c(1:3,5:7)], str_split(text[3], " ")[[1]][1:2])

#test <- text[c(5,6,21,40,103)]

#cbind 
df[,1] <- str_extract(clean_text, "[0-9]{5}")
s1 <- str_replace(clean_text, "[0-9]{5} ", "")
df[,2] <- str_extract(s1, "[^ ]+")
s2 <- str_replace(s1, "[^ ]+ ", "")
df[,3] <- ifelse(str_detect(s2, "^[A-z ]+#")==TRUE, 
             str_extract(s2, "^[^0-9]+#[0-9]+"),
             str_extract(s2, "[^0-9\\(]+"))
s3 <- ifelse(str_detect(s2, "^[A-z ]+#")==TRUE, 
             str_replace(s2, "^[^0-9]+#[0-9]+", ""),
             str_replace(s2, "[^0-9\\(]+", ""))
df[,4] <- str_extract(str_replace(str_extract(s3, "[0-9\\-\\) ]{10,}"), "[\\) ]", ""), ".{12}")
s4 <- str_replace(s3, ".+[- ][0-9]{3}\\-[0-9]{4} ", "")
df[,5] <- ifelse(is.na(str_extract(s4, ".+[A-Z]{2} [0-9]{5}")), 
                 str_replace(str_extract(s4, ".+ [0-9]{5}"), ".{5}$", ""), 
                 str_replace(str_extract(s4, ".+[A-Z]{2} [0-9]{5}"), ".{8}$", ""))
df[,6] <- ifelse(is.na(str_extract(s4, ".+[A-Z]{2} [0-9]{5}")), 
                 NA,
                 str_extract(str_extract(str_extract(s4, ".+[A-Z]{2} [0-9]{5}"), ".{8}$"), "[A-Z]{2}"))
df[,7] <- ifelse(is.na(df[,6]), 
                 str_extract(s4, "[0-9]{5}"), 
                 str_extract(str_extract(s4, ".+[A-Z]{2} [0-9]{5}"), ".{5}$"))
df[,8] <- ifelse(is.na(df[,6]),
                 str_replace(s4, ".+ [0-9]{5} ", ""),
                 str_replace(s4, ".+[A-Z]{2} [0-9]{5} ", ""))

df[,3] <- ifelse(!is.na(str_extract(df[,2], "^[A-z]{3}")), 
                 paste(str_replace(df[,2], "[A-z]{3}", ""), df[,3]), 
                 df[,3])
df[,2] <- ifelse(!is.na(str_extract(df[,2], "^[A-z]{3}")), str_extract(df[,2], "^[A-z]{3}"), df[,2])

### Manual Changes
# 80175
df[df[,1]=="80175", "DISTBORO"] <- "BX"
#df[df[,1]=="80175", "DAYS"] <- "TUE 9AM-12PM"
#df[df[,1]=="80175", "DISTADD"] <- "2345 UNIVERSITY AVE (BASEMENT FORDHAM)"

# 85246
#df[df[,1]=="85246", "DISTBORO"] <- "QN"
df[df[,1]=="85246", "DAYS"] <- "SAT 9AM-5PM"
df[df[,1]=="85246", "DISTADD"] <- "133-36 ROOSEVELT AVENUE (BLAND HOUSES COMMUNITY CENTER)"

# 84540
#df[df[,1]=="84540", "DISTBORO"] <- "NY"
#df[df[,1]=="84540", "DAYS"] <- "FRI 9AM-1PM"
#df[df[,1]=="84540", "DISTADD"] <- "161 EAST 104TH STREET"
#df[df[,1]=="84540", "PROGRAM"] <- "JAN HUS PRESBYTERIAN CHURCH/HOMELESS OUTREACH AND ADVOCACY PROGRAM"

# 84349
#df[df[,1]=="84349", "DISTADD"] <- "148 POST AVE"

# 85521
#df[df[,1]=="85521", "DISTADD"] <- "47 WEST 34TH STREET"

# 85346
df[df[,1]=="85346", "DISTADD"] <- "2093 FULTON STREET"
df[df[,1]=="85346", "PROGRAM"] <- "BLESSED ASSURANCE CHURCH OF GOD BLESSED ASSURANCE CHARITY INC"

# 84137
df[df[,1]=="84137", "DISTADD"] <- "817 LIVONIA AVENUE"
df[df[,1]=="84137", "PROGRAM"] <- "EAST NEW YORK WESLEYAN CHURCH/NEW HOPE FAMILY WORSHIP CENTER"

#Dec 7, 2020
# 81120
df[df[,1]=="81120", "DISTADD"] <- "601 WEST 114TH STREET (BASEMENT OF BROADWAY PRESBYTERIAN CHURCH)"
# df[df[,1]=="81120", "DISTBORO"] <- "NY" 
df[df[,1]=="81120", "DAYS"] <- "FRI 12:30-2PM"

# 84157
df[df[,1]=="84157", "DISTADD"] <- "ADAM CLAYTON POWELL JR. BLVD @ 127TH NY STREET"
df[df[,1]=="84157", "DISTBORO"] <- "NY"
df[df[,1]=="84157", "DAYS"] <- "FRI 12-2PM"

# 83396
#df[df[,1]=="83397", "PROGRAM"] <- "CHRIST JESUS BAPTIST CHURCH"
#df[df[,1]=="83397", "PHONE"] <- "NONE"

# 81748
#df[df[,1]=="81748", "DISTADD"] <- "11 McKEEVER PLACE - 1ST FLOOR (BASEMENT LEVEL)"
df[df[,1]=="81748", "DISTBORO"] <- "BK"

#May 3, 2022
#85578
df[df[,1]=="85578", "PROGRAM"] <- "BROOKLYN MEAL SHARE"
df[df[,1]=="85578", "PHONE"] <- "NONE PROVIDED"

#80175
df[df[,1]=="80175", "DISTBORO"] <- "BX"

summary(is.na(df))

# df$DAYS <- ifelse(str_detect(df$DAYS, "EFAP PROGAMS"), 
#                   sub(" EFAP.*", "", df$DAYS),
#                   df$DAYS)

summary(is.na(df))

write.csv(df, paste0("food_access/data/EFAP_pdf_", date, ".csv"), row.names = FALSE)


