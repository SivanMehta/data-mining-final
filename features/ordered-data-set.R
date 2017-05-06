
# Returns a dataset ordered chronolically by scheduled departure time (can easily change to actual
# departure time in needed)

orderDataSet <- function(df){
  day = 1
  data.at.day1 = df[which(df$DAY_OF_YEAR == day),] # data frame of only day 1, unordered
  chrono.by.day1 = data.at.day1[order(data.at.day1$CRS_DEP_TIME),] # data frame of only day in time order
  newdf = chrono.by.day1
  days = c(2:365)
  for (day in days){
    data.at.day = df[which(df$DAY_OF_YEAR == day),] # data frame of only day i, unordered
    chrono.by.day = data.at.day[order(data.at.day$CRS_DEP_TIME),] # data frame of only day in time order
    newdf = rbind(newdf,chrono.by.day)
  }
  return(newdf)
}
