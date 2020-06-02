library(class)
library(stringr)
library(data.table)

# this removes all variables, usefull if we rerun code to keep it clean
rm(list=ls())

load('dogs.RData')

#####################
# MODEL PREPARATION #
#####################

# color recode to make bigger chunks
# doublecolored dogs will be recoded as singlecolored with first color taken as main.
a <- str_split(dogs2020$COLOR_DOG, "/")
new_colors <- a[[1]][1]
rm(i)
for (i in 2:length(a)) {
  new_colors <- append(new_colors, a[[i]][1])
}
dogs2020$COLOR_DOG <- new_colors

# refactoring text attributes to numeric, so that knn can work with it.
dogs2020$DISTRICT_NAME <- as.numeric(as.factor(dogs2020$DISTRICT_NAME))
dogs2020$AGE <- as.numeric(as.factor(dogs2020$AGE))
dogs2020$SEX <- as.numeric(as.factor(dogs2020$SEX))
dogs2020$SEX_DOG <- as.numeric(as.factor(dogs2020$SEX_DOG))
dogs2020$COLOR_DOG <- as.numeric(as.factor(dogs2020$COLOR_DOG))
dogs2020$YOB_DOG <- as.numeric(as.factor(dogs2020$YOB_DOG))

#saving labels in order to see later what numbers represent what breedtypes
breedtype_labels <- as.factor(dogs2020$BREED_TYPE)
dogs2020$BREED_TYPE <- as.numeric(as.factor(dogs2020$BREED_TYPE))

#saving labels in order to see later what numbers represent what breeds
breed_labels <- as.factor(dogs2020$BREED)
dogs2020$BREED <- as.numeric(as.factor(dogs2020$BREED))

######################################################
# lets try naive approach and predict breed directly #
######################################################

# data separation, so that we have test and training group
dat.d <- sample(1:nrow(dogs2020),size=nrow(dogs2020)*0.7,replace = FALSE) #random selection of 70% data.

# now we tried differrent input settings in an attempt to find optimal one

# train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "YOB_DOG", "AGE", "SEX", "SEX_DOG", "COLOR_DOG")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "YOB_DOG", "AGE", "SEX", "SEX_DOG", "COLOR_DOG")] # remaining 30% test data

# train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "YOB_DOG", "AGE", "SEX", "SEX_DOG")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "YOB_DOG", "AGE", "SEX", "SEX_DOG")] # remaining 30% test data

# train.dogs <- dogs2020[dat.d, c("COLOR_DOG")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("COLOR_DOG")] # remaining 30% test data

# this is the only setting where we are not "cheating" by using dog associated attributes
train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "AGE", "SEX")] # 70% training data
test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "AGE", "SEX")] # remaining 30% test data

train.dogs_labels <- dogs2020[dat.d,BREED]
test.dogs_labels <-dogs2020[-dat.d,BREED]

# train.dogs_labels <- dogs2020[dat.d,group]
# test.dogs_labels <-dogs2020[-dat.d,group]


#aprox k value
k_value <-  sqrt(nrow(dogs2020))

knn.test <- knn(train=train.dogs, test=test.dogs, cl=train.dogs_labels, k=k_value)

ACC.test <- 100 * sum(test.dogs_labels == knn.test)/NROW(test.dogs_labels)

table(knn.test ,test.dogs_labels)

ACC.test

# accuracy of direct prediction is 9,22%

#################################################################
# lets try separating dataset and predict breed type at first,  #
# smaller chunks may lead to more accuracy                      #
#################################################################

# this is the only setting where we are not "cheating" by using dog associated attributes
train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "AGE", "SEX")] # 70% training data
test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "AGE", "SEX")] # remaining 30% test data

train.dogs_labels <- dogs2020[dat.d,BREED_TYPE]
test.dogs_labels <-dogs2020[-dat.d,BREED_TYPE]

#aprox k value
k_value <-  sqrt(nrow(dogs2020))

knn.test <- knn(train=train.dogs, test=test.dogs, cl=train.dogs_labels, k=k_value)

ACC.test <- 100 * sum(test.dogs_labels == knn.test)/NROW(test.dogs_labels)

table(knn.test ,test.dogs_labels)

ACC.test

#this yields 62.96% accuracy in preed type prediction
# now we want to predict exact breed 

# we save predicted breedtype, by applying algorythm to whole dataset. (this can open potential for overfitting)
train.dogs$PREDICTED_BREED_TYPE <- knn(train=train.dogs, test=train.dogs, cl=train.dogs_labels, k=k_value)
test.dogs$PREDICTED_BREED_TYPE <- knn(train=train.dogs[,c("DISTRICT_NAME", "AGE", "SEX")], test=test.dogs, cl=train.dogs_labels, k=k_value)

# and try knn on breed directly now
rm(i)
results <- 0
for (i in 1:length(unique(test.dogs_labels)))
{
  if (any(train.dogs$PREDICTED_BREED_TYPE == i) == F) {
    next()
  }
  
  train.dogs_labels <- dogs2020[dat.d,BREED]
  test.dogs_labels <- dogs2020[-dat.d,BREED]
  
  knn.test <- knn(train=train.dogs[train.dogs$PREDICTED_BREED_TYPE == i], 
                  test=test.dogs[test.dogs$PREDICTED_BREED_TYPE == i], 
                  cl=train.dogs_labels[train.dogs$PREDICTED_BREED_TYPE == i], k=k_value)
  
  ACC.test <- 100 *
    sum(test.dogs_labels[test.dogs$PREDICTED_BREED_TYPE == i] == knn.test)/
    NROW(test.dogs_labels[test.dogs$PREDICTED_BREED_TYPE == i])
  
  table(knn.test ,test.dogs_labels[test.dogs$PREDICTED_BREED_TYPE == i])
  
  results[i] <- ACC.test
  
}

# doesnt really help.
# weighted accuracy is (2305 * 9.32754880694143/100 + 6.38297872340426/100 * 47) / 2352
(length(test.dogs_labels[test.dogs$PREDICTED_BREED_TYPE == 1]) * results[1]/100 
  + length(test.dogs_labels[test.dogs$PREDICTED_BREED_TYPE == 3]) * results[3]/100 ) / 
  (length(test.dogs_labels[test.dogs$PREDICTED_BREED_TYPE == 1]) + length(test.dogs_labels[test.dogs$PREDICTED_BREED_TYPE == 3]))
# = 0.09268707, which is not substantially better than approach without breed type grouping.


# reasonable would be to throw away every breed that comes less than 100 times.
# cause we clearly see that because we dont have more attributes on dog owner levels 
# and more gradual data, we are becomming here just inference of distribution of dogs among the
# attributes we use.
# so to improve accuracy it makes sense to reduce the number of possible "answers"


# in order to test it, we have to calculate how many people own which breeds
# count how many people own every breed.
breed_filter <- data.table(aggregate(OWNER_ID ~ BREED, data = dogs2020, FUN = function(x){NROW(x)}))

#renaming
setnames(breed_filter, old = c("OWNER_ID")
         , new = c("NUMBER_OWNERS"))

# sorting by number of owners
breed_filter <- breed_filter[order(-rank(NUMBER_OWNERS),)]
breed_filter <- breed_filter[breed_filter[, NUMBER_OWNERS > 100]]

# leaving in dataset only desired breeds
dogs2020 <- dogs2020[BREED %in% breed_filter[, BREED]]

# 3685 out of 7839 rows are out. (4154 left)
# 15 breeds out of 313 breeds left

# now we rerun exact copy of code in the first try

dat.d <- sample(1:nrow(dogs2020),size=nrow(dogs2020)*0.7,replace = FALSE) #random selection of 70% data.

# this is the only setting where we are not "cheating" by using dog associated attributes
train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "AGE", "SEX")] # 70% training data
test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "AGE", "SEX")] # remaining 30% test data

train.dogs_labels <- dogs2020[dat.d,BREED]
test.dogs_labels <-dogs2020[-dat.d,BREED]

#aprox k value
k_value <-  sqrt(nrow(dogs2020))

knn.test <- knn(train=train.dogs, test=test.dogs, cl=train.dogs_labels, k=k_value)

ACC.test <- 100 * sum(test.dogs_labels == knn.test)/NROW(test.dogs_labels)

table(knn.test ,test.dogs_labels)

ACC.test

# accuracy of direct prediction is 17,64%
# result is better, but still follows the distribution. and is not significantly improven

# now in an attempt to get more accurate results we split it into chunks, so that breeds
# that come up more often, dont get over prefferenced by knn. (same idea as with breed_type but
# chunking is based on the number of owners of particular breed)

# calculating values for grouping
owners_sum <- sum(breed_filter$NUMBER_OWNERS)
number_groups <- 6
groupsize <- round(owners_sum / number_groups)

# adding grouping variable
breed_filter[,group:=0]

# in any situation first value is group 1
breed_filter$group[1] <- 1

# assigning groups
for (i in 2:nrow(breed_filter)) {
  if (sum(breed_filter[group == breed_filter$group[i-1], NUMBER_OWNERS]) > groupsize){
    breed_filter$group[i] <- breed_filter$group[i-1]+1
  } else {
    breed_filter$group[i] <- breed_filter$group[i-1]
  }
}

# transfer grouping to original dataset
dogs2020 <- merge(dogs2020, breed_filter[,c("BREED", "group")], by = "BREED", all.x = T)

# rewrite train and test datsets.
# this is the only setting where we are not "cheating" by using dog associated attributes
train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "AGE", "SEX", "group")] # 70% training data
test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "AGE", "SEX","group")] # remaining 30% test data

train.dogs_labels <- dogs2020[dat.d,BREED]
test.dogs_labels <-dogs2020[-dat.d,BREED]



# here we once more execute code very similar to the above part with breed_type

rm(i)
results <- 0
for (i in 1:length(unique(breed_filter$group)))
{
  if (any(train.dogs$group == i) == F) {
    next()
  }
  
  train.dogs_labels <- dogs2020[dat.d,BREED]
  test.dogs_labels <- dogs2020[-dat.d,BREED]
  
  knn.test <- knn(train=train.dogs[train.dogs$group == i], 
                  test=test.dogs[test.dogs$group == i], 
                  cl=train.dogs_labels[train.dogs$group == i], k=k_value)
  
  ACC.test <- 100 *
    sum(test.dogs_labels[test.dogs$group == i] == knn.test)/
    NROW(test.dogs_labels[test.dogs$group == i])
  
  table(knn.test ,test.dogs_labels[test.dogs$group == i])
  
  results[i] <- ACC.test
  
}

# doesnt really help.
# weighted accuracy is
(length(test.dogs_labels[test.dogs$group == 1]) * results[1]/100 
  + length(test.dogs_labels[test.dogs$group == 2]) * results[2]/100
  + length(test.dogs_labels[test.dogs$group == 3]) * results[3]/100
  + length(test.dogs_labels[test.dogs$group == 4]) * results[4]/100
  + length(test.dogs_labels[test.dogs$group == 5]) * results[5]/100) / 
    (length(test.dogs_labels[test.dogs$group == 1]) 
  + length(test.dogs_labels[test.dogs$group == 2])
  + length(test.dogs_labels[test.dogs$group == 3])
  + length(test.dogs_labels[test.dogs$group == 4])
  + length(test.dogs_labels[test.dogs$group == 5]))
# = 0.4667201, which is substantially better than 9% we got in the beginning.
# this is the best result we could get. However, the knn bascally still follows the distribution
# of data. and because of that, it would be right to follow the occams razors principle and
# use more simlpe naive bayes approach, that is made to use the data distributuion for
# following predictions
