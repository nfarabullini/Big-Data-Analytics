library(class)
library(psych)
library(e1071)

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

dog_owner_chars <- subset(dogs2020, select=c("BREED", "BREED_TYPE", "YOB_DOG", "SEX_DOG", "COLOR_DOG", 
                                             "OWNER_ID", "AGE", "SEX", "WEALTH_T_CHF", "INCOME_T_CHF", 
                                             "DISTRICT_NAME"))

#############################################
# Investigation 2 - Naive Bayes Model       #
#############################################

nB <- naiveBayes(dog_owner_chars$BREED ~ dog_owner_chars$AGE + dog_owner_chars$SEX, data=dog_owner_chars, laplace = 0, na.action = na.pass)

owner_chars <- subset(dogs2020, select=c("AGE", "SEX"))
predict(nB, owner_chars, type = c("class", "raw"), threshold = 0.001, eps = 0)

