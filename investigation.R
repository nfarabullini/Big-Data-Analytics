load('dogs.RData')

library(class)
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


# k nearest neighbours template

#https://www.edureka.co/blog/knn-algorithm-in-r/
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/factor
# for labeling for using text in model.

#data separation
dat.d <- sample(1:nrow(loan.subset.n),size=nrow(loan.subset.n)*0.7,replace = FALSE) #random selection of 70% data.

train.loan <- loan.subset[dat.d,] # 70% training data
test.loan <- loan.subset[-dat.d,] # remaining 30% test data


#aprox k value
k <-  sqrt(nrow(dogs2020))

knn.test <- knn(train=train.loan, test=test.loan, cl=train.loan_labels, k=26)
