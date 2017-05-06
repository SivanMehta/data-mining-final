# 36-462: Data Mining Final
# 05/05/17
# R script for performing time series analysis
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

library(dplyr)
library(ggplot2)

source("analysis/clean-data.R")

###################### Moving average analysis ###################### 

#  time_series:  A vector containing the time series values
#  ww:  The window width for the moving average
#  tt:  The point at which we want the moving average (leading up to that point)
moving_average <- function(tt, time_series, ww) {
  #  Throw an error if the window width is too big
  if (ww > length(time_series))  
    stop("Window width is greater than length of time series")
  
  #  If the window width is greater than the time point, return NA
  if (tt < ww)  return(NA)
  
  return(mean(time_series[(tt-ww+1):tt]))
}

#  time_series:  A vector containing the time series values
#  ww:  The window width for the moving average
get_moving_averages <- function(time_series, ww) {
  #  Throw an error if the window width is too big
  if (ww > length(time_series))  
    stop("Window width is greater than length of time series")
  
  all_avgs <- seq(1, length(time_series))
  for (start in 1:length(time_series)) {
    all_avgs[start] <- moving_average(start, time_series, ww)
  }
  return(all_avgs)
}

by_date <- group_by(train, FL_DATE)
delays_per_day <- summarize(by_date,
                            N_FLIGHTS = n(),
                            DEP_DELAY_TOT = sum(DEP_DEL15, na.rm = TRUE),
                            ARR_DELAY_TOT = sum(ARR_DEL15, na.rm = TRUE))


delays_per_day$FL_DATE <- as.Date(delays_per_day$FL_DATE)

# two week departures delays moving averages
dep_moving_average_14 <- get_moving_averages(delays_per_day$DEP_DELAY_TOT, 14)
delays_per_day$DEP_AVG_14 <- dep_moving_average_14

ggplot(delays_per_day, aes(x = FL_DATE, y = DEP_DELAY_TOT)) + 
  scale_x_date() + 
  geom_line(color = "firebrick") + 
  geom_point(color = "goldenrod") + 
  labs(x = "Date of Trip", y = "Number of Trips", title = "Trips Taken per Day Over Time") +
  geom_line(aes(x = FL_DATE, y = DEP_AVG_14), color = "blue") 

# two week arrivals delays moving averages
arr_moving_average_14 <- get_moving_averages(delays_per_day$ARR_DELAY_TOT, 14)
delays_per_day$ARR_AVG_14 <- arr_moving_average_14

ggplot(delays_per_day, aes(x = FL_DATE, y = ARR_DELAY_TOT)) + 
  scale_x_date() + 
  geom_line(color = "firebrick") + 
  geom_point(color = "goldenrod") + 
  labs(x = "Date of Trip", y = "Number of Trips", title = "Trips Taken per Day Over Time") +
  geom_line(aes(x = FL_DATE, y = ARR_AVG_14), color = "blue") 


###################### Lag analysis ###################### 

nn <- nrow(delays_per_day)
lag <- 1
lag1 <- delays_per_day$DEP_DELAY_TOT[1:(nn-lag)]
current <- delays_per_day$DEP_DELAY_TOT[(lag+1):nn]
lag_df <- data.frame(current, lag1)

ggplot(lag_df) + geom_line(aes(x = lag1, y = current)) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  labs(x = "Past", y = "Present/Future", title = "Lag by 1 Plot")

lag1 <- delays_per_day$ARR_DELAY_TOT[1:(nn-lag)]
current <- delays_per_day$ARR_DELAY_TOT[(lag+1):nn]
lag_df <- data.frame(current, lag1)

ggplot(lag_df) + geom_line(aes(x = lag1, y = current)) + 
  geom_abline(intercept = 0, slope = 1, color = "red") + 
  labs(x = "Past", y = "Present/Future", title = "Lag by 1 Plot")


###################### Autocorrelation analysis ###################### 

my_acf <- function(time_series) { 
  acf_df <- data.frame(seq(1:26), acf(time_series, plot =FALSE)$acf)
  colnames(acf_df) <- c("lag", "acf")
  acf_plot <- ggplot(acf_df, aes(x = lag, y = acf)) + 
    labs(x = "Lag", y = "ACF", title = "Auto-Correlation Plot") + geom_hline(yintercept = 0) +
    geom_hline(yintercept = 1.96/sqrt(nrow(acf_df)), color = "blue", linetype = "dashed") +
    geom_hline(yintercept = -1.96/sqrt(nrow(acf_df)), color = "blue", linetype = "dashed")
  for (lag in 1:nrow(acf_df)) {
    acf_plot <- acf_plot + geom_segment(aes(xend = lag, yend = 0))
  } 
  acf_plot + scale_y_continuous(limits = c(-1, 1))
} 

my_acf(delays_per_day$DEP_DELAY_TOT)
my_acf(delays_per_day$ARR_DELAY_TOT)
