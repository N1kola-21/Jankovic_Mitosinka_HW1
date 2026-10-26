
#identifying our dataset
our_numbers <- c(55049603, 98396204)
dataset_number <- sum(our_numbers) %% 60
if(dataset_number == 0) dataset_number <- 60
dataset_number

#Task 1
loaded_objects <- load("dataset_07.RData")
houses <- get(loaded_objects)
dim(houses)
nrow(houses)
ncol(houses)

#names and types
names(houses)
sapply(houses, class)
str(houses, max.level = 1)

#structure of variables that arent simple
houses$special_features[1:5]
table(lengths(houses$special_features))
houses$properties[1:5]
table(lengths(houses$properties))

#any values missing?
colSums(is.na(houses))
#houses[is.na(houses$size)] error undefined columns, i want rows so need to add the comma
houses[is.na(houses$size), ]
houses[is.na(houses$floor), ]
houses[is.na(houses$orientation), ]

#possible values of categorical variables
table(houses$rooms, useNA = "ifany") #useNA = "ifany" adds NA values in column as separate category
table(houses$flat_type, useNA = "ifany")
table(houses$orientation, useNA = "ifany")
table(unlist(houses$special_features))
table(unlist(houses$properties))

#summary of numerical variables
summary(houses[, c("price", "floor", "number_of_floors")])
sum(houses$floor > houses$number_of_floors, na.rm = TRUE)

#individual observations
head(houses)
tail(houses)
set.seed(7)
houses[sample(nrow(houses), 10), ]

#Task 2
table(gsub("[0-9.]", "", houses$size), useNA = "ifany")





