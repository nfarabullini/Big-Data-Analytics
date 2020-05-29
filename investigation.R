library(class)

# this removes all variables, usefull if we rerun code to keep it clean
rm(list=ls())

load('dogs.RData')

# color recode
a <- str_split(dogs2020$COLOR_DOG, "/")
new_colors <- a[[1]][1]
for (i in 2:length(a)) {
  new_colors <- append(new_colors, a[[i]][1])
}
dogs2020$COLOR_DOG <- new_colors

# prep
dogs2020$DISTRICT_NAME <- as.numeric(as.factor(dogs2020$DISTRICT_NAME))
dogs2020$AGE <- as.numeric(as.factor(dogs2020$AGE))
dogs2020$SEX <- as.numeric(as.factor(dogs2020$SEX))
dogs2020$SEX_DOG <- as.numeric(as.factor(dogs2020$SEX_DOG))
dogs2020$COLOR_DOG <- as.numeric(as.factor(dogs2020$COLOR_DOG))
dogs2020$YOB_DOG <- as.numeric(as.factor(dogs2020$YOB_DOG))

breedtype_labels <- as.factor(dogs2020$BREEDTYPE)
dogs2020$BREEDTYPE <- as.numeric(as.factor(dogs2020$BREEDTYPE))

breed_labels <- as.factor(dogs2020$BREED)
dogs2020$BREED <- as.numeric(as.factor(dogs2020$BREED))



############################################################
# testing if grouping is possible with reasonable accuracy #
############################################################

# count how many people own every breed.
breed_filter <- data.table(aggregate(OWNER_ID ~ BREED, data = dogs2020, FUN = function(x){NROW(x)}))

#renaming
setnames(breed_filter, old = c("OWNER_ID")
         , new = c("NUMBER_OWNERS"))

# sorting by number of owners
breed_filter <- breed_filter[order(-rank(NUMBER_OWNERS),)]

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


#data separation
dat.d <- sample(1:nrow(dogs2020),size=nrow(dogs2020)*0.7,replace = FALSE) #random selection of 70% data.

train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "YOB_DOG", "AGE", "SEX", "SEX_DOG", "COLOR_DOG")] # 70% training data
test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "YOB_DOG", "AGE", "SEX", "SEX_DOG", "COLOR_DOG")] # remaining 30% test data

# train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "YOB_DOG", "AGE", "SEX", "SEX_DOG")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "YOB_DOG", "AGE", "SEX", "SEX_DOG")] # remaining 30% test data

# train.dogs <- dogs2020[dat.d, c("COLOR_DOG")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("COLOR_DOG")] # remaining 30% test data

# train.dogs <- dogs2020[dat.d, c("DISTRICT_NAME", "AGE", "SEX")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("DISTRICT_NAME", "AGE", "SEX")] # remaining 30% test data

# train.dogs_labels <- dogs2020[dat.d,BREEDTYPE]
# test.dogs_labels <-dogs2020[-dat.d,BREEDTYPE]

# train.dogs_labels <- dogs2020[dat.d,BREED]
# test.dogs_labels <-dogs2020[-dat.d,BREED]

train.dogs_labels <- dogs2020[dat.d,group]
test.dogs_labels <-dogs2020[-dat.d,group]


#aprox k value
k_value <-  sqrt(nrow(dogs2020))

knn.test <- knn(train=train.dogs, test=test.dogs, cl=train.dogs_labels, k=k_value)

ACC.test <- 100 * sum(test.dogs_labels == knn.test)/NROW(test.dogs_labels)

table(knn.test ,test.dogs_labels)

ACC.test

# nope grouping is not possible

# what about direct prediction?
train.dogs_labels <- dogs2020[dat.d,BREED]
test.dogs_labels <-dogs2020[-dat.d,BREED]


knn.test <- knn(train=train.dogs, test=test.dogs, cl=train.dogs_labels, k=k_value)

ACC.test <- 100 * sum(test.dogs_labels == knn.test)/NROW(test.dogs_labels)

table(knn.test ,test.dogs_labels)

ACC.test

# nope too...

# reasonable would be to throw away every breed that comes less than 100 times.

# code

# result is better, but still follows the distribution.