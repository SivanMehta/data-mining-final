
# National Air System Delay# This is the number of NAS delays there have been on each day so far for each flight
#### We may want to change this to within the last hour, or within the last X many flights

# Change NA values to 0

source("./features/ordered-data-set.R")
library(lubridate)

with.nas.delay <- function(df) {
  df <- orderDataSet(df)
  df$NAS_DELAY_IND <- ifelse((df$NAS_DELAY > 0) & (df$DEP_DEL15) > 0, 1, 0)
  delays = rep(-1, nrow(df))
  df$DAY_OF_YEAR <- yday(df$FL_DATE)
  days = c(1:365)
  day = 1
  numFlights = 0
  numNASDelay = 0
  for (i in 1:nrow(df)){ #for each flight, count the number of delays that day so far
    if (df$DAY_OF_YEAR[i] != day){
      day = day + 1
      numNASDelay = 0
      numFlights = 0
    }
    if (numFlights == 0) {
      delays[i] <- 0
    } else {
      delays[i] <- numNASDelay / numFlights
    }
    
    # was this flight actually NAS delayed?
    if (df$NAS_DELAY_IND[i] == 1) {
      numNASDelay = numNASDelay + 1
    }
    numFlights = numFlights + 1
  }
  
  df$nas.delay.today = delays
  return(df)
}