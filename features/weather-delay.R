source("./analysis/clean-data.R")
source("./features/ordered-data-set.R")
library(lubridate)

with.weather.delay <- function(df){
  df <- orderDataSet(df)
  df$WEATHER_DELAY_IND <- ifelse((df$WEATHER_DELAY > 0) & (df$DEP_DEL15 > 0), 1, 0)
  delays = rep(-1, nrow(df))
  df$DAY_OF_YEAR <- yday(df$FL_DATE)
  days = c(1:365)
  day = 1
  numFlights = 0
  numWeatherDelay = 0
  for (i in 1:nrow(df)){ #for each flight, count the number of delays that day so far
    if (df$DAY_OF_YEAR[i] != day){
      day = day + 1
      numWeatherDelay = 0
      numFlights = 0
    }
    if (numFlights == 0) {
      delays[i] <- 0
    } else {
      delays[i] <- numWeatherDelay / numFlights
    }
    
    # was this flight actually weather delayed?
    if (df$WEATHER_DELAY_IND[i] == 1) {
      numWeatherDelay = numWeatherDelay + 1
    }
    numFlights = numFlights + 1
  }
  
  df$weather.delay.today = delays
  return(df)
}