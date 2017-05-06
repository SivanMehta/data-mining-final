source("analysis/clean-data.R")
source("./features/ordered-data-set.R")
library(lubridate)

# calculate the number of flights that have been delayed that day, 
# up until that time for visible data (2015 and 2016_visible)
with.dep.arr.delay <- function(df) {
  df <- orderDataSet(df)
  
  df$dep.delay.ind <- ifelse((df$ORIGIN == "PIT") & (df$DEP_DEL15) > 0, 1, 0)
  df$arr.delay.ind <- ifelse((df$DEST == "PIT") & (df$ARR_DEL15) > 0, 1, 0)
  
  df$dep.delay.ind <- ifelse(is.na(df$dep.delay.ind), 0, df$dep.delay.ind)
  df$arr.delay.ind <- ifelse(is.na(df$arr.delay.ind), 0, df$arr.delay.ind)
  
  delays.dep = rep(-1, nrow(df))
  delays.arr = rep(-1, nrow(df))
  df$DAY_OF_YEAR <- yday(df$FL_DATE)
  days = c(1:365)
  day = 1
  numFlights = 0
  
  numDEPDelay = 0
  numARRDelay = 0
  for (i in 1:nrow(df)){ #for each flight, count the number of delays.dep that day so far
    if (df$DAY_OF_YEAR[i] != day){
      day = day + 1
      numDEPDelay = 0
      numARRDelay = 0
      numFlights = 0
    }
    if (numFlights == 0) {
      delays.dep[i] <- 0
      delays.arr[i] <- 0
    } else {
      delays.dep[i] <- numDEPDelay / numFlights
      delays.arr[i] <- numARRDelay / numFlights
    }
    
    # was this flight actually delayed?
    if (df$dep.delay.ind[i] == 1) {
      numDEPDelay = numDEPDelay + 1
    }
    if (df$arr.delay.ind[i] == 1) {
      numARRDelay = numARRDelay + 1
    }
    numFlights = numFlights + 1
  }
  
  df$dep.delay.today = delays.dep
  df$arr.delay.today = delays.arr
  return(df)
}