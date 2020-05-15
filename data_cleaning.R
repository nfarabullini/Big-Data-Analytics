library(readr)
library(data.table)
library(xlsx)
library(RYandexTranslate)
library(dplyr)

# this removes all variables, usefull if we rerun code to keep it clean
rm(list=ls())

######################
# IMPORTING DOG DATA #
######################

# source
# https://data.stadt-zuerich.ch/dataset/sid_stapo_hundebestand/resource/a05e2101-7997-4bb5-bed8-c5a61cfffdcf
dogs2020 <- data.table(read_csv("data_sources/20200306_hundehalter.csv"))

#removing unnececesary columns
dogs2020[, c("STADTKREIS","RASSE1_MISCHLING", "RASSE2", "RASSE2_MISCHLING"):=NULL]

#if a row has a NA entry in one of the cells, remove the entire row
dogs2020 <- na.omit(dogs2020)
# 0 rows ommited, still leave the code in place in case we change data basis.

#rename columns
setnames(dogs2020, old = c("HALTER_ID", "ALTER", "GESCHLECHT", "STADTQUARTIER", "RASSE1", "RASSENTYP", "GEBURTSJAHR_HUND", "GESCHLECHT_HUND", "HUNDEFARBE")
                  , new = c("OWNER_ID", "AGE", "SEX", "DISTRICT", "BREED", "BREEDTYPE", "YOB_DOG", "SEX_DOG", "COLOR_DOG"))


#couse stadtquartier is more granular district segemntation than stadtkreis, and not all datasets
# can be merged by stadtkreis we used it
# however it is hard to match on a map
# dogs dataset does not contain normal names. so we use wealth dataset to extract them
district_names <- data.table(read_csv("data_sources/wir100od1004.csv"))
district_names <- unique(district_names[, QuarSort, QuarLang])

#renaming
setnames(district_names, old = c("QuarSort", "QuarLang")
         , new = c("DISTRICT", "DISTRICT_NAME"))

dogs2020 <- merge(dogs2020,district_names , by = "DISTRICT", all.x = T)

#remove unused variables
rm(district_names)

####################
# IMPORTING WEALTH #
####################

# source
# https://data.stadt-zuerich.ch/dataset/fd_median_einkommen_quartier_od1003
wealth <- data.table(read_csv("data_sources/wir100od1004.csv"))

# we dont have date for 2020, the freshest data is on 2017
wealth <- wealth[SteuerJahr == 2017,]
#creating new column where to store average
wealth[, wealth50 := SteuerVermoegen_p50]
# replacing family values in this column with same devided by 2 for normalization
wealth$wealth50[wealth$SteuerTarifSort == 1] <- wealth$wealth50[wealth$SteuerTarifSort == 1]/2

#aggregating data for family status
#new table          old table   select mean of incomep50 (ignore NA), aggregate it by Quartal
wealth_merge <- wealth[,mean(wealth50, na.rm = T), by=QuarSort]

#leaving only quartals that have dogs in them
wealth_merge <- wealth_merge[wealth_merge$QuarSort %in% dogs2020$DISTRICT]

#renaming
setnames(wealth_merge, old = c("V1", "QuarSort")
         , new = c("wealth (T. CHF)", "DISTRICT"))

#merge income into dogs dataset
dogs2020 <- merge(dogs2020, wealth_merge, by = "DISTRICT", all.x = T)
# ATTENTION! FYI: not for all districts we have wealth values! 

#get rid of unused data
rm(wealth, wealth_merge)

####################
# IMPORTING INCOME #
####################

# source
# https://data.stadt-zuerich.ch/dataset/fd_median_vermoegen_quartier_od1004
income <- data.table(read_csv("data_sources/wir100od1003.csv"))

# we dont have date for 2020, the freshest data is on 2017
income <- income[SteuerJahr == 2017,]
#creating new column where to store average
income[, incomep50 := SteuerEInkommen_p50]
# replacing family values in this column with same devided by 2 for normalization
income$incomep50[income$SteuerTarifSort == 1] <- income$incomep50[income$SteuerTarifSort == 1]/2

#aggregating data for family status
#new table          old table   select mean of incomep50 (ignore NA), aggregate it by Quartal
income_merge <- income[,mean(incomep50, na.rm = T), by=QuarSort]

#leaving only quartals that have dogs in them
income_merge <- income_merge[income_merge$QuarSort %in% dogs2020$DISTRICT]

#renaming
setnames(income_merge, old = c("V1", "QuarSort")
         , new = c("income (T. CHF)", "DISTRICT"))

#merge income into dogs dataset
dogs2020 <- merge(dogs2020, income_merge, by = "DISTRICT", all.x = T)
# ATTENTION! FYI: not for all districts we have income values! 

#get rid of unused data
rm(income, income_merge)

#######################
# IMPORTING EDUCATION #
#######################

education <- data.table(read_csv("data_sources/bil101od1012 (2).csv")) 
### long to wide education reshape
education <-dcast(education, RaumSort ~ Bildungsstand, value.var = "AntBev")
#renaming for merge
setnames(education, old = c("RaumSort", "Obligatorische Schule", "Sekundarstufe II", "Tertiärstufe"), 
                    new = c("DISTRICT", "Basic_school %", "Gymnasium %", "University %"))

dogs2020 <- merge(dogs2020, education, by = "DISTRICT", all.x = T)

rm(education)

#######################
# IMPORTING HOME_TYPE #
#######################

home_type <- data.table(read_csv("data_sources/bau_best_geb_whg_bev_gebaeudeart_quartier_seit2008.csv")) 

# we dont have date for 2020, the freshest data is on 2019
home_type <- home_type[Jahr == 2019,]

home_type <- home_type[, sum(AnzGeb), by = list(QuarSort,GbdArtPubName)]

#renaming for merge
setnames(home_type, old = c("QuarSort", "GbdArtPubName", "V1"), 
         new = c("DISTRICT", "Hometype", "Number_homes"))

### long to wide hometype reshape
home_type <-dcast(home_type, DISTRICT ~ Hometype, value.var = "Number_homes")

# translate hometype
setnames(home_type, old = c("Produktions- und Lagergebäude", "Mehrfamilienhäuser", "Einfamilienhäuser", "Infrastrukturgebäude", "Kleingebäude", "Kommerzielle Gebäude", "Spezielle Wohngebäude"), 
         new = c("Factories and warehouses", "Apartments", "Family houses", "Infrastructure buildings", "Small buildings", "Commercial buildings", "Special accommodation"))

#removing unnececesary column
home_type[, Unbekannt:=NULL]

#merging
dogs2020 <- merge(dogs2020, home_type, by = "DISTRICT", all.x = T)

rm(home_type)




### package fix
translate = function (api_key, text = "", lang = "") 
{
  url = "https://translate.yandex.net/api/v1.5/tr.json/translate?"
  url = paste(url, "key=", api_key, sep = "")
  if (text != "") {
    url = paste(url, "&text=", text, sep = "")
  }
  if (lang != "") {
    url = paste(url, "&lang=", lang, sep = "")
  }
  url = gsub(pattern = " ", replacement = "%20", x = url)
  d = RCurl::getURL(url, ssl.verifyhost = 0L, ssl.verifypeer = 0L)
  d = jsonlite::fromJSON(d)
  d$code = NULL
  d
}

api_key <- "trnsl.1.1.20200515T134653Z.f9fb709ac3e94036.783aefa609692b463a79b5827d5c0e7f2d037a8c"

column_list <- c("BREED", "COLOR_DOG", "DISTRICT_NAME")

for (column_names in column_list) {
  unique_values <- unique(dogs2020[,get(column_names)])
for (unique_num in 1:length(unique_values)) {
  
  #debug
  print(column_names)
  print(paste(unique_num, " out of ", length(unique_values)))
  
  
  dogs2020[dogs2020[,get(column_names)] == unique_values[unique_num],
           eval(column_names) := translate(api_key,text=unique_values[unique_num],lang="de-en")$text]

}}

dogs2020[SEX == "w",SEX := "f"]
dogs2020[SEX_DOG == "w",SEX_DOG := "f"]

###################
# saving to excel #
###################

#write.xlsx(dogs2020, "dogs2020_merged.xlsx")

#######################
# Investigation - CB  #
#######################

# First let's investigate the top of the Question Tree - Who Has a Dog

# Relevant Columns w. Duplicates Removed
district_dog <- unique(subset(dogs2020, select=c("DISTRICT","OWNER_ID")))
# Count them - Nikki, Would love you to show me how to make your pretty map
district_dog_count <- aggregate(OWNER_ID ~ DISTRICT, data = district_dog, FUN = function(x){NROW(x)})

# Data Artifact - Dog w. District "8" Typographical Error? We Remove. Good example of Data Cleaning. 
district_dog_count <- district_dog_count[(district_dog_count$DISTRICT!=8), ]

# Usability
ddc <- district_dog_count

# Aggregate Back in to Higher Order Regions
# Okay, I'm sorry, there is probably a better way to do this, I am brand new to R
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('11', '12', '13', '14'), '1', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('21', '23', '24'), '2', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('31', '33', '34'), '3', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('41', '42', '44'), '4', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('51', '52'), '5', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('61', '63'), '6', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('71', '72', '73', '74'), '7', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('81', '82', '83'), '8', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('91', '92'), '9', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('101', '102'), '10', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('111', '115', '119'), '11', ddc$DISTRICT)
ddc$DISTRICT <- ifelse(ddc$DISTRICT %in% c('121', '122', '123'), '12', ddc$DISTRICT)

ddc <- aggregate(ddc$OWNER_ID, by=list(DISTRICT=ddc$DISTRICT), FUN=sum)

# To Be Continued, We are now still at the top of the question Tree
# There should be some discussion on a proper regression model as per Andris' original thoughts

######################################
# Owner Age - Dog Breed Relationship #
######################################

# Generate breeds table for totals
breeds <- table(breed=dogs2020$BREED, age=dogs2020$AGE)
breeds <- cbind(breeds, total = rowSums(breeds)) %>%
          as.data.frame()

# Use pie charts to visualise
par(mfrow = c(2,5))
pie(breeds$`11-20`, dogs2020$BREED)
pie(breeds$`21-30`, dogs2020$BREED)
pie(breeds$`31-40`, dogs2020$BREED)
pie(breeds$`41-50`, dogs2020$BREED)
pie(breeds$`51-60`, dogs2020$BREED)
pie(breeds$`61-70`, dogs2020$BREED)
pie(breeds$`71-80`, dogs2020$BREED)
pie(breeds$`81-90`, dogs2020$BREED)
pie(breeds$`91-100`, dogs2020$BREED)
pie(breeds$total, dogs2020$BREED)

# Delete generated table
rm(breeds)
