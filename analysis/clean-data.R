# 36-462: Data Mining Final
# 04/16/17
# R script for cleaning airline data
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

# call `source("clean-data.R")` to use

library(dplyr)

# some variables are redundant, so we drop them
drops_all <- c("ORIGIN_AIRPORT_ID", "ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_CITY_MARKET_ID", "ORIGIN_WAC",
           "ORIGIN_STATE_WAC", "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID", "DEST_CITY_MARKET_ID",
           "DEST_WAC", "DEST_STATE_WAC", "YEAR", "ORIGIN_STATE_NM", "DEST_STATE_NM",
           "ORIGIN_CITY_NAME", "DEST_CITY_NAME", "CARRIER", "ORIGIN_STATE_FIPS", "DEST_STATE_FIPS",
           "FLIGHTS", "ARR_DELAY")

keep.2016 <- c("DEP_DEL15", "WEATHER_DELAY", "DIVERTED",
              "NAS_DELAY", "ORIGIN", "DEST", "ARR_DEL15",
              "MONTH", "DAY_OF_MONTH", "CRS_DEP_TIME", "FL_DATE",
              "DISTANCE", "DAY_OF_WEEK", "DISTANCE_GROUP", "QUARTER", 
              "CRS_ELAPSED_TIME", "CRS_ARR_TIME")

replace <- c("CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY",
             "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME",
             "TOTAL_ADD_GTIME", "LONGEST_ADD_GTIME")

# cleans the 2015 flight data that is used to build the model
clean_data_train <- function() {
  clean <- read.csv("data/flights2015.csv")

  for (c in replace) {
    clean[,c] <- car::recode(clean[,c], "c(NA)=0")
  }
  clean <- clean[, !(names(clean) %in% drops_all)]

  return(clean)
}

# cleans the 2016 flight data that we use to test our model
clean_data_vis <- function() {
  clean <- read.csv("data/flights2016_visible.csv")

  for (c in replace) {
    clean[,c] <- car::recode(clean[,c], "c(NA)=0")
  }

  clean <- clean[, !(names(clean) %in% drops_all)]

  return(clean)
}

clean_data_2016 <- function() {
  clean <- read.csv("data/flights2016.csv")

  clean <- clean[, names(clean) %in% keep.2016]

}

# separate arrivals and departures, because we are only interesting in predicting delays on departures
# by default this is done for the 2015 data
separate <- function(dat = NA) {
  if (is.na(dat)) {dat = clean_data_train()}
  depart_ind <- which(dat$ORIGIN == "PIT")
  departures <- dat[depart_ind,]
  arrivals <- dat[-depart_ind,]

  departures$ORIGIN <- NULL
  departures$ORIGIN_STATE_ABR <- NULL
  departures$X <- NULL

  arrivals$DEST <- NULL
  arrivals$DEST_STATE_ABR <- NULL
  arrivals$X <- NULL

  return(c(departures = departures, arrivals = arrivals))
}

# loads all cleaned data into session
train <- clean_data_train()
vis <- clean_data_vis()
data.2016 <- clean_data_2016()


clear_NA <- function(df) {
  df$is.guess <- ifelse(is.na(df$DIVERTED), 1, 0)

  for (c in colnames(df)) {
    df[,c] <- car::recode(df[,c], "c(NA)=0")
  }
  return(df)
}

train <- clear_NA(train)
vis <- clear_NA(vis)
data.2016 <- clear_NA(data.2016)

# add a column of the time a flight arrives in PIT or leaves from PIT
pit_time <- function(dat) {
  if (dat[which(names(dat) == "ORIGIN")] == "PIT") {
    return(as.integer(dat[[which(names(dat) == "CRS_DEP_TIME")]]))
  } else if (dat[which(names(dat) == "DEST")] == "PIT") {
    return(as.integer(dat[[which(names(dat) == "CRS_ARR_TIME")]]))
  }
}

train$CRS_PIT_TIME <- apply(train, 1, pit_time)
vis$CRS_PIT_TIME <- apply(vis, 1, pit_time)
data.2016$CRS_PIT_TIME <- apply(data.2016, 1, pit_time)

