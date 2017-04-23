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

assignments.2.com <- cutree(tree.com, 2)
assignments.4.com <- cutree(tree.com, 4)
assignments.8.com <- cutree(tree.com, 8)
assignments.16.com <- cutree(tree.com, 16)
xtabs(~assignments.2.com + sample.data$DEP_DEL15)
xtabs(~assignments.4.com + sample.data$DEP_DEL15)
xtabs(~assignments.8.com + sample.data$DEP_DEL15)
xtabs(~assignments.16.com + sample.data$DEP_DEL15)
table(sample.data$DEP_DEL15)

# Generally, these are doing pretty poorly, as they largely 
# reflect the proportions in the original data. Let's look
# at prototype clustering

assignments.2.proto <- protocut(tree.proto, 2)
assignments.4.proto <- protocut(tree.proto, 4)
assignments.8.proto <- protocut(tree.proto, 8)
assignments.16.proto <- protocut(tree.proto, 16)
xtabs(~assignments.2.proto$cl + sample.data$DEP_DEL15)
xtabs(~assignments.4.proto$cl + sample.data$DEP_DEL15)
xtabs(~assignments.8.proto$cl + sample.data$DEP_DEL15)
xtabs(~assignments.16.proto$cl + sample.data$DEP_DEL15)
table(sample.data$DEP_DEL15)
