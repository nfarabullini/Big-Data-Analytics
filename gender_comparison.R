library(readr)
library(data.table)
library(xlsx)
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

library(ggplot2)
# gender-based stats
men <- 100*(nrow(dogs2020 %>% filter(dogs2020$SEX == "m"))/nrow(dogs2020))
women <- 100*(nrow(dogs2020 %>% filter(dogs2020$SEX == "w"))/nrow(dogs2020))
data.frame("Gender" = c("Male", "Female"), "Percentage %" = c(men, women))

# create histos for most popular breeds
women_owners <- owners_gender("w") 
men_owners <- owners_gender("m") 
owners_gender <- function(param) {
  owners <- dogs2020 %>% filter(dogs2020$SEX == param) 
  breeds <- unique(dogs2020$BREED)
  list_70 <- lapply(seq_len(length(breeds)), function (x) {
    length(which(owners$BREED == breeds[x]))
  }) 
  # more than 70 dogs per breed
  more_than_70 <- which(list_70 > 70)
  owners_70_unfiltered <- owners[more_than_70,]
  owners_70 <- owners %>% filter(owners$BREED %in% unique(owners_70_unfiltered$BREED))
  ggplot(owners_70, aes(owners_70$BREED)) + geom_bar()
}


