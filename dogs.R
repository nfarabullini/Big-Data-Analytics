library(readr)
library(data.table)
library(xlsx)
hunde2015 <- data.table(read_csv("20151001hundehalter.csv"))
hunde2016 <- data.table(read_csv("20160307hundehalter.csv"))
hunde2017 <- data.table(read_csv("20170308hundehalter.csv"))
zuordnungstabellehunderassehundetyp <- data.table(read_csv("zuordnungstabellehunderassehundetyp.csv"))
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
