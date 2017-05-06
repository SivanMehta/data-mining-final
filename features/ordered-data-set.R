
# Returns the day number given months and days
# Note: This is for a non-leap year
getDays <- function(months, days){
  monthDays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
  cumdays <- rep(0, length(days))
  for (i in 1:length(months)){
    if (months[i] == 1) {cumdays[i] <- days[i]}
    else{
      cumdays[i] <- sum(monthDays[1:(months[i]-1)])+days[i]
    }
  }
  return(cumdays)
}

# Returns a dataset ordered chronologically by scheduled departure time (can easily 
# change to actual departure time if needed)
orderDataSet <- function(df){
  df$DAY_OF_YEAR = getDays(df$MONTH, df$DAY_OF_MONTH)
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
