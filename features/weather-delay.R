source("./analysis/clean-data.R")
train <- read.csv("./data/flights2015.csv")

# We may want to change this to within the last hour instead of the whole day

# Works only with ordered data set

# Change NA values to 0
ordered$WEATHER_DELAY[is.na(ordered$WEATHER_DELAY)] <- 0
ordered$WEATHER_DELAY_IND <- ifelse(ordered$WEATHER_DELAY > 0, 1, 0)

with.weather.delay <- function(df){
  delays = rep(-1, nrow(df))
  days = c(1:max(df$DAY_OF_YEAR))
  day = 1
  numWeatherDelay = 0
  for (i in 1:nrow(df)){ #for each flight, count the number of delays that day so far
    if (df$DAY_OF_YEAR[i] != day){
      day = day + 1
      numWeatherDelay = 0
    }
    delays[i] = numWeatherDelay #on this day how many flights have been delayed so far?
    if (df$WEATHER_DELAY_IND[i] == 1){ #was this flight actually weather delayed?
      numWeatherDelay = numWeatherDelay + 1
    }
  }
  df$weather.delay.today = delays
  return(df)
}

tom = with.weather.delay(ordered)
