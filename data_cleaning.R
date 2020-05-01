library(dplyr)

dogs <- read.csv("/home/mirai_user/Downloads/20151001hundehalter.csv")

# remove useless columns
drops <- c("RASSE1_MISCHLING", "RASSE2", "STADTQUARTIER")
drops_pos <- unlist(lapply(drops, function (x) {
  grep(x, names(dogs))
}))

dogs <- dogs[-drops_pos] 

#if a row has a NA entry in one of the cells, remove the entire row
dogs <- na.omit(dogs)

# subset dogs based on owner's age group
age_owners <- as.list(unique(dogs$ALTER))

age_groups <- lapply(seq_len(length(age_owners)), function (x) {
  which(dogs$ALTER == age_owners[[x]])
})

# extrapolate dogs info only for a specific age group
dogs <- dogs[age_groups[[10]],]
