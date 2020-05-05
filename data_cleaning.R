library(readr)
library(data.table)
library(xlsx)

# source
# https://data.stadt-zuerich.ch/dataset/sid_stapo_hundebestand/resource/a05e2101-7997-4bb5-bed8-c5a61cfffdcf
dogs2020 <- data.table(read_csv("data_sources/20200306_hundehalter.csv"))

#removing unnececesary columns
dogs2020[, c("STADTQUARTIER","RASSE1_MISCHLING", "RASSE2", "RASSE2_MISCHLING"):=NULL]

#if a row has a NA entry in one of the cells, remove the entire row
dogs2020 <- na.omit(dogs2020)
# 0 rows ommited, still leave the code in place in case we change data basis.



income <- data.table(read_csv("wir100od1003.csv"))

income2015 <- income[SteuerJahr == 2015,]
#creating new column
income2015[, incomep50 := SteuerEInkommen_p50]
# replacing family values in this column with same devided by 2 for normalization
income2015$incomep50[income2015$SteuerTarifSort == 1] <- income2015$incomep50[income2015$SteuerTarifSort == 1]/2

#aggregating data for family status
#new table          old table   select mean of incomep50 (ignore NA), aggregate it by Quartal
income2015_merge <- income2015[,mean(incomep50, na.rm = T), by=QuarSort]

#leaving only quartals that have dogs in them
income2015_merge <- income2015_merge[income2015_merge$QuarSort %in% hunde2015$STADTQUARTIER]
setnames(income2015_merge, old = "V1", new = "income")
setnames(income2015_merge, old = "QuarSort", new = "STADTQUARTIER")


hunde2015_merged <- merge(hunde2015, income2015_merge, by = "STADTQUARTIER", all.x = T)


#### EDUCATION
education <- data.table(read_csv("bil101od1012 (2).csv")) 
### long to wide education reshape
education <-dcast(education, RaumSort ~ Bildungsstand, value.var = "AntBev")
#renaming for merge
setnames(education, old = "RaumSort", new = "STADTQUARTIER")


hunde2015_merged <- merge(hunde2015_merged, education, by = "STADTQUARTIER", all.x = T)


#### lang
lang <- data.table(read_csv("bev304od3041.csv")) 
### long to wide education reshape
lang <-dcast(lang, QuarSort ~ Sprache, value.var = "AnzBev")
#renaming for merge
setnames(lang, old = "QuarSort", new = "STADTQUARTIER")


hunde2015_merged <- merge(hunde2015_merged, lang, by = "STADTQUARTIER", all.x = T)


write.xlsx(hunde2015_merged, "hunde2015_merged.xlsx")
