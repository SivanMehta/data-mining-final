library(dplyr)

# Returns a dataset ordered chronologically by scheduled departure time
# (can easily change to actual departure time if needed)
order.data.set <- function(df) {
  df$index <- 1:nrow(df)
  result <- arrange(df, MONTH, DAY_OF_MONTH, CRS_DEP_TIME)
  return(result)
}

reinstate.order <- function(df) {
  result <- arrange(df, index)
  return(result)
}
