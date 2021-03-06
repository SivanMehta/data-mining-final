source("./features/ordered-data-set.R")
library(lubridate)

add.delay <- function(df, indicator.fn, col.name) {
  df <- order.data.set(df)
  ind.name <- paste(col.name, ".ind", sep = "")
  df[,ind.name] <- indicator.fn(df)
  delays = rep(-1, nrow(df))
  df$DAY_OF_YEAR <- yday(df$FL_DATE)
  days = c(1:365)
  day = 1
  numFlights = sum(df$DAY_OF_YEAR == 1) # 0
  numVarDelay = 0
  # for each flight, count the number of delays that day so far
  for (i in 2:nrow(df)){ # for (i in 1:nrow(df)){
    if (df$DAY_OF_YEAR[i] != day) {
      day = day + 1
      numVarDelay = 0
      numFlights = sum(df$DAY_OF_YEAR == day)  # 0
    }
    if (numFlights == 0) {
      delays[i] <- 0
    } else {
      delays[i] <- numVarDelay / numFlights
    }

    # was this flight actually weather delayed?
    if (df[i, ind.name] == 1) {
      numVarDelay = numVarDelay + 1
    }
    # numFlights = numFlights + 1
  }

  df[,col.name] = delays
  return(df)
}

add.plane.flights <- function(df) {
  # create lookup table
  flights.that.day <- df %>%
    group_by(FL_DATE, TAIL_NUM) %>%
    summarise(count = n())

  lookup.flight.count <- function(obs) {
    return(
      filter(flights.that.day,
           FL_DATE == obs["FL_DATE"],
           TAIL_NUM == obs["TAIL_NUM"]
           )[1,]$count
    )
  }

  df$additional.flights <- apply(df, 1, lookup.flight.count)

  return(df)
}
