library(dplyr)

# Returns a dataset ordered chronologically by scheduled departure time
# (can easily change to actual departure time if needed)
order.data.set <- function(df) {
  df$index <- 1:nrow(df)
  result <- arrange(df, MONTH, DAY_OF_MONTH, CRS_DEP_TIME)
  return(result)
}

<<<<<<< HEAD
# Returns a dataset ordered chronologically by scheduled departure time (can easily 
# change to actual departure time if needed)
orderDataSet <- function(df){
  df$DAY_OF_YEAR = getDays(df$MONTH, df$DAY_OF_MONTH)
  day = 1
  data.at.day1 = df[which(df$DAY_OF_YEAR == day),] # data frame of only day 1, unordered
  chrono.by.day1 = data.at.day1[order(data.at.day1$CRS_PIT_TIME),] # data frame of only day in time order
  newdf = chrono.by.day1
  days = c(2:365)
  for (day in days){
    data.at.day = df[which(df$DAY_OF_YEAR == day),] # data frame of only day i, unordered
    chrono.by.day = data.at.day[order(data.at.day$CRS_DEP_TIME),] # data frame of only day in time order
    newdf = rbind(newdf,chrono.by.day)
  }
  return(newdf)
=======
reinstate.order <- function(df) {
  result <- arrange(df, index)
  return(result)
>>>>>>> master
}
