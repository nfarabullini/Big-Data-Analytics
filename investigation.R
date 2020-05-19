load('dogs.RData')

#######################
# Investigation - CB  #
#######################

# First let's investigate the top of the Question Tree - Who Has a Dog

# Relevant Columns w. Duplicates Removed
district_dog <- unique(subset(dogs2020, select=c("OWNER_ID", "DISTRICT_NAME", "TOTAL_POPULATION")))

# Aggregation - DDC District Dog Count
ddc <- aggregate(OWNER_ID ~ DISTRICT_NAME + TOTAL_POPULATION, data = district_dog, FUN = function(x){NROW(x)})
rm(district_dog)

# Rename Column(s)
colnames(ddc)[3] <- "TOTAL_UNIQUE_OWNERS"

# Compute Difference
ddc$NON_DOG_OWNERS <- ddc$TOTAL_POPULATION - ddc$TOTAL_UNIQUE_OWNERS
ddc$PERCENT_DOG_OWNERS <- ddc$TOTAL_UNIQUE_OWNERS / ddc$TOTAL_POPULATION
