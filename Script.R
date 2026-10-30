setwd("./Jankovic_Mitosinka_HW1")

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
table(houses$rooms)
sum(is.na(houses$rooms))
table(houses$flat_type)
sum(is.na(houses$flat_type))
table(houses$orientation)
sum(is.na(houses$orientation))
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
#2.1 Apartment size
#Finding which variable to use doesn't require a command. We just search after typing houses$
table(houses$size)
houses$size_m2 <- as.numeric(gsub(" m2", "", houses$size))
#This code failed because in the table not all values have a space between the number and m2 (NOT THE ACTUAL REASON. KEPT TO SHOW THOUGHT PROCESS)
#So first we clean them sepparately then convert to numeric
clean_size <- gsub(" m2", "", houses$size)
clean_size <- gsub("m2", "", clean_size)

houses$size_m2 <- as.numeric(clean_size)
#We still get NAs. We will inspect the data to see what is going on
houses$size[is.na(houses$size_m2) & !is.na(houses$size)]
head(houses$size_m2)
head(clean_size)

#Actually every single value gets converted to NA. This is probably due to superscript
clean_size <- gsub(" m²", "", houses$size)
clean_size <- gsub("m²", "", clean_size)

houses$size_m2 <- as.numeric(clean_size)
#We did not get a warning now so the conversion did not generate new NAs
summary(houses$size_m2)
class(houses$size_m2)

#2.2 Rooms
head(houses$rooms)
houses$number_of_rooms <- as.numeric(substr(houses$rooms, 1, 1))

#Verify
summary(houses$number_of_rooms)
table(houses$number_of_rooms, houses$rooms)
class(houses$number_of_rooms)

#2.3
head(houses$price)
houses$price_per_m2 <- houses$price / houses$size_m2
summary(houses$price_per_m2)

#2.4
head(houses$location)
cordinations_list <- strsplit(houses$location, split = ", ")
head(cordinations_list)

#Now we can create latitude and longitude
houses$latitude <- as.numeric(sapply(cordinations_list, function(x) x[1]))
houses$longitude <- as.numeric(sapply(cordinations_list, function(x) x[2]))

summary(houses$latitude)
summary(houses$longitude)

#Part 3
library(tidyverse)

#3.1
head(houses$flat_type)

flat_type_summary <- houses |>
  group_by(flat_type) |>
  summarise(
    number_of_listings = n(),
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    mean_size = mean(size_m2, na.rm = TRUE),
    mean_price_per_m2 = mean(price_per_m2, na.rm = TRUE)
  ) |>
  arrange(desc(mean_price_per_m2))

flat_type_summary

#3.2
head(houses$orientation)

orientetion_summary <- houses |>
  filter(!is.na(number_of_rooms), !is.na(orientation)) |>
  group_by(number_of_rooms, orientation) |>
  summarise(
    mean_price_per_m2 = mean(price_per_m2, na.rm = TRUE),
    number_of_observations = n()
  ) |>
  filter(number_of_observations >= 5) |>
  arrange(desc(mean_price_per_m2))

orientetion_summary

#This order does not make it easy to interpret so we ordered it differently
orientetion_summary |>
  arrange(number_of_rooms, desc(mean_price_per_m2))

#3.3 Floors

houses$floor
head(houses$number_of_floors)

#Looking at the output of houses$floors we will assume that 0 means ground floor

floor_type_summary <- houses |>
  filter(!is.na(floor), !is.na(number_of_floors)) |>
  mutate(
    floor_type = case_when(
      floor == 0 ~ "ground floor",
      floor == number_of_floors ~ "top floor",
      floor > 0 & floor < number_of_floors ~ "intermediate floor"
    )
  ) |>
  group_by(floor_type) |>
  summarise(
    mean_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    mean_price_per_m2 = mean(price_per_m2, na.rm = TRUE),
    number_of_observations = n()
  )

floor_type_summary |>
  arrange(floor_type)

#Part 4

#Structure of the list columns
houses$properties[1:5]
houses$special_features[1:5]

#Doesnt work, flat 4 has washching machine, dishwasher but returns false
houses$properties[1:5] == "dishwasher"
as.character(houses$properties[4])

#This returns only one value, not for each flat
"dishwasher" %in% houses$properties


#The function
has_characteristic <- function(list_column, characteristic) {
  vapply(list_column, function(x) characteristic %in% x, logical(1))
}


houses$has_garage     <- has_characteristic(houses$special_features, "garage")
houses$has_balcony    <- has_characteristic(houses$special_features, "balcony")
houses$has_dishwasher <- has_characteristic(houses$properties, "dishwasher")


#Check for comparison
houses$special_features[1:6]
houses[1:6, c("has_garage", "has_balcony")]
houses$properties[1:5]
houses$has_dishwasher[1:5]


table(unlist(houses$special_features))
sum(houses$has_garage)
sum(houses$has_balcony)
table(unlist(houses$properties))
sum(houses$has_dishwasher)

