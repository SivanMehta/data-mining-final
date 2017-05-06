# 36-462: Data Mining Final
# 04/16/17
# R script for cleaning airline data 
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

# call `source("clean-data.R")` to use

library(car)

# for model building, there are some variables which are blocked out in the prediction set
# so we drop them
drop_guess <- c("DEP_TIME", "DEP_DELAY",
                "DEP_DELAY_NEW", "DEP_DEL15", "DEP_DELAY_GROUP", "DEP_TIME_BLK", "TAXI_OUT", "WHEELS_OFF",
                "WHEELS_ON", "TAXI_IN", "ARR_TIME", "ARR_DELAY", "ARR_DELAY_NEW", "ARR_DEL15", "ARR_DELAY_GROUP",
                "ARR_TIME_BLK", "CANCELLED", "CANCELLATION_CODE", "DIVERTED", "ACTUAL_ELAPSED_TIME", "AIR_TIME",
                "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME",
                "TOTAL_ADD_GTIME", "LONGEST_ADD_GTIME")

# some variables are redundant, so we drop them
drops_all <- c("ORIGIN_AIRPORT_ID", "ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_CITY_MARKET_ID", "ORIGIN_WAC",
           "ORIGIN_STATE_WAC", "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID", "DEST_CITY_MARKET_ID",
           "DEST_WAC", "DEST_STATE_WAC", "YEAR", "ORIGIN_STATE_NM", "DEST_STATE_NM",
           "ORIGIN_CITY_NAME", "DEST_CITY_NAME", "CARRIER", "ORIGIN_STATE_FIPS", "DEST_STATE_FIPS",
           "FLIGHTS", "ARR_DELAY")

replace <- c("CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", 
             "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME",
             "TOTAL_ADD_GTIME", "LONGEST_ADD_GTIME")

# cleans the 2015 flight data that is used to build the model
clean_data_train <- function() {
  clean <- read.csv("data/flights2015.csv")

  for (c in replace) {
    clean[,c] <- recode(clean[,c], "c(NA)=0")
  }
  clean <- clean[, !(names(clean) %in% drops_all)]
  
  return(clean)
}

# cleans the 2016 flight data that we use to test our model
clean_data_vis <- function() {
  clean <- read.csv("data/flights2016_visible.csv")
  
  for (c in replace) {
    clean[,c] <- recode(clean[,c], "c(NA)=0")
  }
  
  clean <- clean[, !(names(clean) %in% drops_all)]

  return(clean)
}

# cleans the 2016 flight data that we predict on
clean_data_guess <- function() {
  clean <- read.csv("data/flights2016_guess.csv")
  
  clean <- clean[, !(names(clean) %in% drops_all)]
  clean <- clean[, !(names(clean) %in% drop_guess)]
  
  return(clean)
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
guess <- clean_data_guess()


# add a column of the time a flight arrives in PIT or leaves from PIT
pit_time <- function(dat) {
  if (dat[10] == "PIT") {
    return(dat[14])
  } else if (dat[12] == "PIT") {
    return(dat[25])
  }
}

train$CRS_PIT_TIME <- apply(train, 1, pit_time)
vis$CRS_PIT_TIME <- apply(vis, 1, pit_time)
guess$CRS_PIT_TIME <- apply(guess, 1, pit_time)

clear_NA <- function(dat) {
  for (c in colnames(dat)) {
    dat[,c] <- recode(dat[,c], "c(NA)=0")
  }
  return(dat)
}

train <- clear_NA(train)
vis <- clear_NA(vis)
guess <- clear_NA(guess)
