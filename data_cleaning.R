library(readr)
library(data.table)
library(xlsx)
library(RYandexTranslate)
library(dplyr)
library(stringr)

# this removes all variables, usefull if we rerun code to keep it clean
rm(list=ls())

######################
# IMPORTING DOG DATA #
######################

# source
# https://data.stadt-zuerich.ch/dataset/sid_stapo_hundebestand/resource/a05e2101-7997-4bb5-bed8-c5a61cfffdcf
dogs2020 <- data.table(read_csv("data_sources/20200306_hundehalter.csv"))

# Data Artifact - Dog w. District "8" Typographical Error? We Remove. Good example of Data Cleaning. 
dogs2020 <- dogs2020[(dogs2020$STADTQUARTIER!=8), ]

#removing unnececesary columns
dogs2020[, c("RASSE1_MISCHLING", "RASSE2", "RASSE2_MISCHLING"):=NULL]

#if a row has a NA entry in one of the cells, remove the entire row
dogs2020 <- na.omit(dogs2020)
# 0 rows ommited, still leave the code in place in case we change data basis.

#rename columns
setnames(dogs2020, old = c("HALTER_ID", "ALTER", "GESCHLECHT", "STADTQUARTIER", "STADTKREIS", "RASSE1", "RASSENTYP", "GEBURTSJAHR_HUND", "GESCHLECHT_HUND", "HUNDEFARBE")
                  , new = c("OWNER_ID", "AGE", "SEX", "DISTRICT", "DISTRICT_BIG", "BREED", "BREED_TYPE", "YOB_DOG", "SEX_DOG", "COLOR_DOG"))


#couse stadtquartier is more granular district segemntation than stadtkreis, and not all datasets
# can be merged by stadtkreis we used it
# however it is hard to match on a map
# dogs dataset does not contain normal names. so we use wealth dataset to extract them
district_names <- data.table(read_csv("data_sources/wir100od1004.csv"))
district_names <- unique(district_names[, QuarSort, QuarLang])

#renaming
setnames(district_names, old = c("QuarSort", "QuarLang")
         , new = c("DISTRICT", "DISTRICT_NAME"))

dogs2020 <- merge(dogs2020, district_names , by = "DISTRICT", all.x = T)

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
         , new = c("WEALTH_T_CHF", "DISTRICT"))

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
         , new = c("INCOME_T_CHF", "DISTRICT"))

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
                    new = c("DISTRICT", "BASIC_SCHOOL_PERCENTAGE", "GYMNASIUM_PERCENTAGE", "UNIVERSITY_PERCENTAGE"))

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
         new = c("FACTORIES_AND_WAREHOUSES", "APARTMENTS", "SINGLE_FAMILY_HOMES", "INFRASTRUCTURE_BUILDINGS", "SMALL_BUILDINGS", "COMMERCIAL_BUILDINGS", "SPECIAL_ACCOMODATION"))

#removing unnececesary column
home_type[, Unbekannt:=NULL]

#merging
dogs2020 <- merge(dogs2020, home_type, by = "DISTRICT", all.x = T)

rm(home_type)

########################
# IMPORTING POPULATION #
########################

# Import Population Figures
# https://www.stadt-zuerich.ch/prd/de/index/statistik/themen/bevoelkerung/bevoelkerungsentwicklung/kreise-und-quartiere.html#daten
pop_per_district <- data.table(read_csv("data_sources/2019-Table_1.csv"))

# Remove Irrelevant Rows
pop_per_district <- pop_per_district[8:nrow(pop_per_district)]

# Inject Column Name to Replace N/A
pop_per_district[1,1] <- "DISTRICT_NAME"

# Set Column Names
setnames(pop_per_district, as.character(pop_per_district[1, ]))

# Remove Duplicate Rows
pop_per_district <- pop_per_district[2:nrow(pop_per_district)]

# This Reg-Ex Matching Function Removes all Whitespace (bad data design)
pop_per_district[,2:5] <- data.table(apply(pop_per_district[,2:5],2,function(x)gsub('\\s+', '',x)))

# Join By District Name (Perfect Match, No N/A's)
dogs2020 <- merge(dogs2020, pop_per_district, by = "DISTRICT_NAME", all.x = T)

# Renaming
setnames(dogs2020, old = c("Total", "Schweizer/-innen", "Ausländer/-innen", "Anteil ausländische\nBevölkerung (%)")
         , new = c("TOTAL_POPULATION", "SWISS_POPULATION", "FOREIGN_POPULATION", "FOREIGN_POPULATION_PERCENTAGE"))

# Then we cast as a numeric to prepare for mathematical operations
dogs2020$TOTAL_POPULATION <- as.numeric(dogs2020$TOTAL_POPULATION)
dogs2020$SWISS_POPULATION   <- as.numeric(dogs2020$SWISS_POPULATION)
dogs2020$FOREIGN_POPULATION   <- as.numeric(dogs2020$FOREIGN_POPULATION  )
dogs2020$FOREIGN_POPULATION_PERCENTAGE   <- as.numeric(dogs2020$FOREIGN_POPULATION_PERCENTAGE)

#######################
#     TRANSLATION     #
#######################

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

column_list <- c("BREED", "COLOR_DOG")

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

save.image("dogs.Rdata")

###################
# saving to excel #
###################

#write.xlsx(dogs2020, "dogs2020_merged.xlsx")
