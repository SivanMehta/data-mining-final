source("./analysis/clean-data.R")
train <- read.csv("./data/flights2015.csv")

# We may want to change this to within the last hour instead of the whole day

# Works only with ordered data set

# Change NA values to 0
ordered$WEATHER_DELAY[is.na(ordered$WEATHER_DELAY)] <- 0
ordered$WEATHER_DELAY_IND <- ifelse((ordered$WEATHER_DELAY > 0) & (ordered$DEP_DEL15 > 0), 1, 0)

with.weather.delay <- function(df){
  delays = rep(-1, nrow(df))
  days = c(1:max(df$DAY_OF_YEAR))
  day = 1
  numflights = 0
  numWeatherDelay = 0
  for (i in 1:nrow(df)){ #for each flight, count the ratio of the NAS delays that day so far
    if (df$DAY_OF_YEAR[i] != day){
      day = day + 1
      numWeatherDelay = 0
      numflights = 0
    }
    if (numflights == 0){
      delays[i] = 0
    }
    else{
      delays[i] = numWeatherDelay/numflights #ratio of flights that have been delayed so far due to NAS?
    }
    if (df$WEATHER_DELAY_IND[i] == 1){ #was this flight NAS delayed?
      numWeatherDelay = numWeatherDelay + 1
    }
    numflights = numflights + 1
  }
  df$weather.delay.ratio = delays
  return(df)
}

tom = with.weather.delay(ordered[c(1:500),])
