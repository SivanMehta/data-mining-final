source('./analysis/load-data.R')
library(dplyr)
library(protoclust)

# Throw out 20 variables that are useless and would just slow down dist()
on.a.diet <- train[, !(colnames(train) %in% c(
  "QUARTER", # can derive from month
  "ORIGIN_AIRPORT_SEQ_ID",
  "ORIGIN_AIRPORT_MARKET_ID",
  "ORIGIN_CITY_NAME",
  "ORIGIN_STATE_ABR",
  "ORIGIN_STATE_FIPS",
  "ORIGIN_STATE_NM",
  "ORIGIN_STATE_WAC",
  "DEST_AIRPORT_SEQ_ID",
  "DEST_AIRPORT_MARKET_ID",
  "DEST_CITY_NAME",
  "DEST_STATE_ABR",
  "DEST_STATE_FIPS",
  "DEST_STATE_NM",
  "DEST_STATE_WAC",
  "DEST_STATE_NEW",
  "DEP_DELAY_NEW",
  "DEP_DELAY",
  "DEP_DELAY_GROUPS",
  "DEP_TIME_BLK",
  "ARR_DELAY_NEW",
  "ARR_DELAY",
  "ARR_DELAY_GROUPS",
  "ARR_TIME_BLK",
  "CANCELLATION_CODE",
  "FLIGHTS",
  "DISTANCE_GROUP"
))]

# replace NAs in _DELAY variables with 0
on.a.diet[is.na(on.a.diet$CARRIER_DELAY),]$CARRIER_DELAY <- 0
on.a.diet[is.na(on.a.diet$WEATHER_DELAY),]$WEATHER_DELAY <- 0
on.a.diet[is.na(on.a.diet$NAS_DELAY),]$NAS_DELAY <- 0
on.a.diet[is.na(on.a.diet$SECURITY_DELAY),]$SECURITY_DELAY <- 0
on.a.diet[is.na(on.a.diet$LATE_AIRCRAFT_DELAY),]$LATE_AIRCRAFT_DELAY <- 0

sample.data <- sample_n(on.a.diet, nrow(on.a.diet) * .1)
flights.dist <- dist(sample.data)

#tree.sin <- hclust(flights.dist, method = "single")
#tree.avg <- hclust(flights.dist, method = "average")
tree.com <- hclust(flights.dist, method = "complete")
tree.proto <- protoclust(flights.dist)

#plot(tree.sin)
#plot(tree.avg)
plot(tree.com)
plot(tree.proto)

# Complete linkage and prototype linkage seem to have
# most clear groups, so we'll compare the results of these
# in predicting DEP_DEL15

assignments.com <- cutree(tree.com, 2)
xtabs(~assignments.com + sample.data$DEP_DEL15)

# Generally, these are doing pretty poorly, as they largely 
# reflect the proportions in the original data. Let's look
# at prototype clustering

assignments.proto <- protocut(tree.proto, 2)
xtabs(~assignments.proto$cl + sample.data$DEP_DEL15)

table(sample.data$DEP_DEL15)

# If we use cluster assignments as classifications, what is
# our missclassifcation rate?

predictions.com <- assignments.com - 1
right.com <- length(which(predictions.com == sample.data$DEP_DEL15)) / nrow(sample.data)
misclass.com <- 1 - right.com

predictions.proto <- assignments.proto$cl - 1
right.proto <- length(which(predictions.proto == sample.data$DEP_DEL15)) / nrow(sample.data)
misclass.proto <- 1 - right.proto
