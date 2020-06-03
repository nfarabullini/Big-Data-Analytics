library(class)
library(psych)
library(e1071)
library(plyr)
library(data.table)
library(ggplot2)

rm(list=ls())

load('dogs.RData')

########################
# INVESTIGATION 1 - CB #
########################

# Here we are looking at properties at a district level 
# and trying to predict if properties of those districts
# determine percentage of dog ownership

########################################
# Investigation 1 - Subset & Agg. Data #
########################################

# First we will need to Subset the Data and Take aggregates to get District Level Statistics
# Relevant Columns w. Duplicates Removed
district_dog <- unique(subset(dogs2020, select=c("OWNER_ID", "DISTRICT_NAME", "WEALTH_T_CHF", "INCOME_T_CHF",
                                                 "BASIC_SCHOOL_PERCENTAGE", "GYMNASIUM_PERCENTAGE", "UNIVERSITY_PERCENTAGE",
                                                 "SINGLE_FAMILY_HOMES", "INFRASTRUCTURE_BUILDINGS", "SMALL_BUILDINGS", "COMMERCIAL_BUILDINGS",
                                                 "APARTMENTS", "FACTORIES_AND_WAREHOUSES", "SPECIAL_ACCOMODATION", "TOTAL_POPULATION",
                                                 "FOREIGN_POPULATION_PERCENTAGE")))

# Aggregation - Count total unique owners and preserve select district data
ddc <- aggregate(OWNER_ID ~ DISTRICT_NAME + WEALTH_T_CHF + INCOME_T_CHF + BASIC_SCHOOL_PERCENTAGE
                 + GYMNASIUM_PERCENTAGE + UNIVERSITY_PERCENTAGE + SINGLE_FAMILY_HOMES
                 + INFRASTRUCTURE_BUILDINGS + SMALL_BUILDINGS + COMMERCIAL_BUILDINGS
                 + APARTMENTS + FACTORIES_AND_WAREHOUSES + SPECIAL_ACCOMODATION + TOTAL_POPULATION + FOREIGN_POPULATION_PERCENTAGE, 
                 data = district_dog, FUN = function(x){NROW(x)})

# Rename Column(s) and Remove Redundant Data
colnames(ddc)[16] <- "TOTAL_UNIQUE_OWNERS"
rm(district_dog)

# Compute Difference
ddc$NON_DOG_OWNERS <- ddc$TOTAL_POPULATION - ddc$TOTAL_UNIQUE_OWNERS
ddc$PERCENT_DOG_OWNERS <- ddc$TOTAL_UNIQUE_OWNERS / ddc$TOTAL_POPULATION

# Compute Total Building Count for Percentages % of Total Buildings and Residential Buildings
ddc$TOTAL_BUILDINGS <- ddc$SINGLE_FAMILY_HOMES + ddc$INFRASTRUCTURE_BUILDINGS + ddc$SMALL_BUILDINGS
+ ddc$COMMERCIAL_BUILDINGS + ddc$APARTMENTS + ddc$FACTORIES_AND_WAREHOUSES + ddc$SPECIAL_ACCOMODATION
ddc$TOTAL_RESIDENCES <- ddc$SINGLE_FAMILY_HOMES + ddc$APARTMENTS + ddc$SPECIAL_ACCOMODATION

ddc$SINGLE_FAMILY_HOMES_PERCENTAGE <- ddc$SINGLE_FAMILY_HOMES / ddc$TOTAL_BUILDINGS
ddc$SINGLE_FAMILY_HOMES_RESIDENCE_PERCENTAGE <- ddc$SINGLE_FAMILY_HOMES / ddc$TOTAL_RESIDENCES

ddc$APARTMENTS_PERCENTAGE <- ddc$APARTMENTS / ddc$TOTAL_BUILDINGS
ddc$APARTMENTS_RESIDENCE_PERCENTAGE <- ddc$APARTMENTS / ddc$TOTAL_RESIDENCES

ddc$SPECIAL_ACCOMODATION_PERCENTAGE <- ddc$SPECIAL_ACCOMODATION / ddc$TOTAL_BUILDINGS
ddc$SPECIAL_ACCOMODATION_RESIDENCE_PERCENTAGE <- ddc$SPECIAL_ACCOMODATION / ddc$TOTAL_RESIDENCES

# Summary Statistics
describe(ddc)

#######################################
# Investigation 1 - Linear Model      #
#######################################

# Compute Scatter Plots for Preliminary Investigation of Independent Variables
scatter.smooth(x=ddc$TOTAL_POPULATION, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Population")
scatter.smooth(x=ddc$FOREIGN_POPULATION_PERCENTAGE, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Foreign Population %")

scatter.smooth(x=ddc$WEALTH_T_CHF, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Wealth")
scatter.smooth(x=ddc$INCOME_T_CHF, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Income")

scatter.smooth(x=ddc$SINGLE_FAMILY_HOMES_PERCENTAGE, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Single Fam. Home %")
scatter.smooth(x=ddc$SINGLE_FAMILY_HOMES_RESIDENCE_PERCENTAGE, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Single Fam. Home Res. %")

scatter.smooth(x=ddc$APARTMENTS_PERCENTAGE, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Apartments %")
scatter.smooth(x=ddc$APARTMENTS_RESIDENCE_PERCENTAGE, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Apartments Res. %")

scatter.smooth(x=ddc$SPECIAL_ACCOMODATION_PERCENTAGE, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Spec. Acc. %")
scatter.smooth(x=ddc$SPECIAL_ACCOMODATION_RESIDENCE_PERCENTAGE, y=ddc$PERCENT_DOG_OWNERS, main="Dog Ownership ~ Spec. Acc. Res. %")

# Based upon the plots we decide to investigate the quality of wealth and income on Dog Ownership %
cor(ddc$PERCENT_DOG_OWNERS, ddc$WEALTH_T_CHF)
cor(ddc$PERCENT_DOG_OWNERS, ddc$INCOME_T_CHF)

# Two Independent Regressions
linearModWealth <- lm(PERCENT_DOG_OWNERS ~ WEALTH_T_CHF, data=ddc)
linearModIncome <- lm(PERCENT_DOG_OWNERS ~ INCOME_T_CHF, data=ddc)
linearModCombi <- lm(PERCENT_DOG_OWNERS ~ WEALTH_T_CHF + INCOME_T_CHF, data=ddc)

##############################################
# Investigation 1 - Linear Model Summary     #
##############################################

# Only Income seems to have a reliable effect - Rsq: 0.4214, Independent Var. is Statistically Significant. 
# Each Additional 1000 CHF of Income Predicts an Additional 0.0269 PERCENT increase in Dog Ownership
summary(linearModIncome)

########################
# INVESTIGATION 2 - CB #
########################

# Now we will start again from the beginning 
# investigating the qualities of owners that 
# may determine breed selection

########################################
# Investigation 2 - Subset Data        #
########################################

# Subset our Data
dog_owner_chars <- subset(dogs2020, select=c("BREED", "BREED_TYPE", "YOB_DOG", "SEX_DOG", "COLOR_DOG", 
                                             "OWNER_ID", "AGE", "SEX", "WEALTH_T_CHF", "INCOME_T_CHF", 
                                             "DISTRICT_NAME"))

# Remove Outlier Breeds (<10 Entries)
dog_owner_chars <- ddply(dog_owner_chars, "BREED", function(d) {if(nrow(d)>9) d else NULL})

#############################################
# Investigation 2 - Naive Bayes Model       #
#############################################
setDT(dog_owner_chars)
dog_owner_chars <- dog_owner_chars[1:100, c("BREED", "AGE", "SEX")]

nB <- naiveBayes(BREED ~ ., data=dog_owner_chars, laplace = 0, na.action = na.pass)

# convert nB into a data frame
nB_df_age <- as.data.frame(nB$tables$`dog_owner_chars$AGE`)
# select the breed
breed_name <- "Beagle"
breed <- which(nB_df_age$Y == breed_name)
# create data frame for breed values
d_age <- data.frame(x = nB_df_age[breed,]$dog_owner_chars.AGE, y = nB_df_age[breed,]$Freq)
# create plot
ggplot(d_age, aes(x = x, y = y, group = 1)) + geom_point() + geom_line() + 
  labs(x = "Age group", y = "Frequency", title = paste(breed_name, "frequency per age group"))

# similar approach for sex
nB_df_sex <- as.data.frame(nB$tables$`dog_owner_chars$SEX`)
# select sex
breed_name <- "Beagle"
breed <- which(nB_df_sex$Y == breed_name)
# create data frame for sex values
# geom_bar() in ggplot2 takes all of the values that one wants to plot in the bar plot and automatically calculates teh frquency. 
# Here the frequency is already given. 
# A workaround is to create a data frame that replicates the frequency value with respect the number given for frequency, such that the right amount of values is output
rep_freq_f <- cbind("x" = rep(nB_df_sex[breed,]$Freq[1], nB_df_sex[breed,]$Freq[1]*100),
                    "y" = rep("f", nB_df_sex[breed,]$Freq[1]*100))
rep_freq_m <- cbind("x" = rep(nB_df_sex[breed,]$Freq[2], nB_df_sex[breed,]$Freq[2]*100),
                    "y" = rep("m", nB_df_sex[breed,]$Freq[2]*100))
freq <- as.data.frame(rbind(rep_freq_f, rep_freq_m))
# create plot
ggplot(freq, aes(x = y)) + geom_bar() + 
  labs(x = "Age group", y = "Frequency", title = paste(breed_name, "frequency per sex")) 


# general breed plot 
ggplot(d_age, aes(x = x.dog_owner_chars.AGE, y = y.Freq)) + geom_point()

df_m_11_20 <- data.frame(AGE="11-20", SEX="m")
df_f_11_20 <- data.frame(AGE="11-20", SEX="f")



predict_m_11_20 <- predict(nB, dog_owner_chars, type = "raw")
predict_f_11_20 <- predict(nB, df_f_11_20, type="raw")

