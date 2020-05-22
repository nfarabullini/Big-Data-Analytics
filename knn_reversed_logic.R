# this removes all variables, usefull if we rerun code to keep it clean
rm(list=ls())

load('dogs.RData')

library(class)
#######################
# Investigation - CB  #
#######################

# breed_filter <- data.table(aggregate(OWNER_ID ~ BREED, data = dogs2020, FUN = function(x){NROW(x)}))
# breed_filter <- breed_filter[OWNER_ID > 100]
# breed_filter <- breed_filter[OWNER_ID < 500, BREED]
# dogs2020 <- dogs2020[BREED %in% breed_filter]

#dogs2020 <- dogs2020[BREEDTYPE == "K"]
#dogs2020 <- dogs2020[BREEDTYPE == "I"]

# k nearest neighbours template

# color recode
# a <- str_split(dogs2020$COLOR_DOG, "/")
# new_colors <- a[[1]][1]
# for (i in 2:length(a)) {
#   new_colors <- append(new_colors, a[[i]][1])
# }
# dogs2020$COLOR_DOG <- new_colors

# prep
district_name_labels <- as.factor(dogs2020$DISTRICT_NAME)
dogs2020$DISTRICT_NAME <- as.numeric(as.factor(dogs2020$DISTRICT_NAME))

age_labels <- as.factor(dogs2020$AGE)
dogs2020$AGE <- as.numeric(as.factor(dogs2020$AGE))

sex_labels <- as.factor(dogs2020$SEX)
dogs2020$SEX <- as.numeric(as.factor(dogs2020$SEX))

dogs2020$SEX_DOG <- as.numeric(as.factor(dogs2020$SEX_DOG))
dogs2020$COLOR_DOG <- as.numeric(as.factor(dogs2020$COLOR_DOG))
dogs2020$YOB_DOG <- as.numeric(as.factor(dogs2020$YOB_DOG))

#breedtype_labels <- as.factor(dogs2020$BREEDTYPE)
dogs2020$BREEDTYPE <- as.numeric(as.factor(dogs2020$BREEDTYPE))

#breed_labels <- as.factor(dogs2020$BREED)
dogs2020$BREED <- as.numeric(as.factor(dogs2020$BREED))





#https://www.edureka.co/blog/knn-algorithm-in-r/
#https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/factor
# for labeling for using text in model.

#data separation
dat.d <- sample(1:nrow(dogs2020),size=nrow(dogs2020)*0.7,replace = FALSE) #random selection of 70% data.

train.dogs <- dogs2020[dat.d, c("BREEDTYPE", "YOB_DOG", "AGE", "SEX", "SEX_DOG", "COLOR_DOG")] # 70% training data
test.dogs <- dogs2020[-dat.d,c("BREEDTYPE", "YOB_DOG", "AGE", "SEX", "SEX_DOG", "COLOR_DOG")] # remaining 30% test data

# train.dogs <- dogs2020[dat.d, c("BREEDTYPE", "YOB_DOG", "AGE", "SEX", "SEX_DOG")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("BREEDTYPE", "YOB_DOG", "AGE", "SEX", "SEX_DOG")] # remaining 30% test data

# train.dogs <- dogs2020[dat.d, c("COLOR_DOG")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("COLOR_DOG")] # remaining 30% test data

# train.dogs <- dogs2020[dat.d, c("BREEDTYPE", "AGE", "SEX")] # 70% training data
# test.dogs <- dogs2020[-dat.d,c("BREEDTYPE", "AGE", "SEX")] # remaining 30% test data

train.dogs_labels <- dogs2020[dat.d,DISTRICT_NAME]
test.dogs_labels <-dogs2020[-dat.d,DISTRICT_NAME]

# train.dogs_labels <- dogs2020[dat.d,BREED]
# test.dogs_labels <-dogs2020[-dat.d,BREED]

#aprox k value
k_value <-  sqrt(nrow(dogs2020))

knn.test <- knn(train=train.dogs, test=test.dogs, cl=train.dogs_labels, k=k_value)

ACC.test <- 100 * sum(test.dogs_labels == knn.test)/NROW(test.dogs_labels)

table(knn.test ,test.dogs_labels)

ACC.test