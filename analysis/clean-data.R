# 36-462: Data Mining Final
# 04/16/17
# R script for cleaning airline data 
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

library(car)

clean_data_u <- function(path) {
  raw <- read.csv(path)
  clean <- raw
  # pick which aliases to keep
  # drop full names of states for origin and destination
  clean$ORIGIN_STATE_NM <- NULL
  clean$DEST_STATE_NM <- NULL
  # drop full names of city for origin and destination 
  clean$ORIGIN_CITY_NAME <- NULL
  clean$DEST_CITY_NAME <- NULL
  
  # the BOT suggests using UNIQUE_CARRIER_ID for analysis instead of CARRIER
  clean$CARRIER <- NULL
  
  # dropping TAIL_NUM because there are 2770 levels and it is not explained by the BOT website
  clean$TAIL_NUM <- NULL
  
  # STATE_FIPS is a numerical coding for state and thus redundant
  clean$ORIGIN_STATE_FIPS <- NULL
  clean$DEST_STATE_FIPS <- NULL
  
  # Making an AM = 1/PM = 0 
  # Based on the scheduled departure and arrival times, not the actual
  clean$DEP_AM_PM <- ifelse(clean$CRS_DEP_TIME < 1200, "1", "0")
  clean$ARR_AM_PM <- ifelse(clean$CRS_ARR_TIME < 1200, "1", "0")
  
  # FLIGHTS is the number of flights but because every row is a single flight... 
  clean$FLIGHTS <- NULL
  
  # because we are only concerned with delayed flights, use ARR_DELAY_NEW instead of ARR_DELAY
  clean$ARR_DELAY <- NULL
  
  # write.csv(clean, paste(substr(path, 1, nchar(path)-4), "clean_u.csv", sep = "_"))
  return(clean)
}

# for model building, there are some variables which are blocked out in the prediction set
# so we drop them
drop_guess <- c("DEP_TIME", "DEP_DELAY",
                "DEP_DELAY_NEW", "DEP_DEL15", "DEP_DELAY_GROUP", "DEP_TIME_BLK", "TAXI_OUT", "WHEELS_OFF",
                "WHEELS_ON", "TAXI_IN", "ARR_TIME", "ARR_DELAY", "ARR_DELAY_NEW", "ARR_DEL15", "ARR_DELAY_GROUP",
                "ARR_TIME_BLK", "CANCELLED", "CANCELLATION_CODE", "DIVERTED", "ACTUAL_ELAPSED_TIME", "AIR_TIME",
                "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME",
                "TOTAL_ADD_GTIME", "LONGEST_ADD_GTIME")

drops_all <- c("ORIGIN_AIRPORT_ID", "ORIGIN_AIRPORT_SEQ_ID", "ORIGIN_CITY_MARKET_ID", "ORIGIN_WAC",
           "ORIGIN_STATE_WAC", "DEST_AIRPORT_ID", "DEST_AIRPORT_SEQ_ID", "DEST_CITY_MARKET_ID",
           "DEST_WAC", "DEST_STATE_WAC", "YEAR")

replace <- c("CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", 
             "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "FIRST_DEP_TIME",
             "TOTAL_ADD_GTIME", "LONGEST_ADD_GTIME")

# path = a path leading to a csv cleaned by clean_data_u()
clean_data_s <- function(path) {
  clean_u <- read.csv(path)
  if (grepl("guess", path)) {
    clean_u <- clean_u[, !(names(clean_u) %in% drop_guess)]
  }
  clean_s <- clean_u[, !(names(clean_u) %in% drops_all)]
  for (c in replace) {
    clean_s[,c] <- recode(clean_s[,c], "c(NA)=0")
  }
  
  clean_s$X <- NULL
  
  write.csv(clean_s, paste(substr(path, 1, nchar(path)-6), "s.csv", sep = "_"))
}

# clean data for training, testing, and predicting files for unsupervised analysis
clean_data_u("data/flights2015.csv")
clean_data_u("data/flights2016_visible.csv")
clean_data_u("data/flights2016_guess.csv")

# clean data for training, testing, and predicting files for supervised analysis
clean_data_s("data/flights2015_clean_u.csv")
clean_data_s("data/flights2016_visible_clean_u.csv")
clean_data_s("data/flights2016_guess_clean_u.csv")

# separate arrivals and departures, because we are only interesting in predicting delays on departures
separate <- function(path) {
  dat <- read.csv(path)
  depart_ind <- which(dat$ORIGIN == "PIT")
  departures <- dat[depart_ind,]
  arrivals <- dat[-depart_ind,]
  
  departures$ORIGIN <- NULL
  departures$ORIGIN_STATE_ABR <- NULL
  departures$X <- NULL
  
  arrivals$DEST <- NULL
  arrivals$DEST_STATE_ABR <- NULL
  arrivals$X <- NULL
  
  write.csv(departures, paste(substr(path, 1, nchar(path)-12), "dep.csv", sep = "_"))
  write.csv(arrivals, paste(substr(path, 1, nchar(path)-12), "arr.csv", sep = "_"))
}

separate("data/flights2015_clean_s.csv")
separate("data/flights2016_visible_clean_s.csv")
separate("data/flights2016_guess_clean_s.csv")
