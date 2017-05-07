source("./features/general-delay.R")

weather.indicator <- function(df) {
  return( ifelse(df$WEATHER_DELAY > 0 & (df$DEP_DEL15 > 0), 1, 0) )
}

NAS.indicator <- function(df) {
  return( ifelse((df$NAS_DELAY > 0) & (df$DEP_DEL15) > 0, 1, 0) )
}

dep.indicator <- function(df) {
  return( ifelse((df$ORIGIN == "PIT") & (df$DEP_DEL15) > 0, 1, 0) )
}

arr.indicator <- function(df) {
  return( ifelse((df$DEST == "PIT") & (df$ARR_DEL15) > 0, 1, 0) )
}

add.features <- function(df) {
  # weather delays
  df <- add.delay(df, weather.indicator, "weather.delay.ratio")

  #nas delays
  df <- add.delay(df, NAS.indicator, "NAS.delay.ratio")

  # departure delay ratio
  #df <- add.delay(df, dep.indicator, "dep.delay.ratio")

  # arrival delay ratio
  #df <- add.delay(df, arr.indicator, "arr.delay.ratio")

  derivative <- rep(0, nrow(df))
  for (i in 5:nrow(df)) {
    derv_avg <- 0
    for (j in 1:4) {
      derv_avg = derv_avg + (df$weather.delay.ratio[i] - df$weather.delay.ratio[(i-j)])
    }
    derivative[i] <- derv_avg / 4
  }
  df$derivative <- derivative
  
  return(df)
}

# example usage
# source("./analysis/clean-data.R")
# example <- train[1:5000, ]
#
# example <- add.features(example)
#
# plot(example$arr.delay.ratio)
