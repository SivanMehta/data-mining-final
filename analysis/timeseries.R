# 36-462: Data Mining Final
# 05/05/17
# R script for performing time series analysis
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

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

###################### Lag analysis ###################### 

plot.log.analysis <- function() {
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
}

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

#my_acf(delays_per_day$DEP_DELAY_TOT)
#my_acf(delays_per_day$ARR_DELAY_TOT)
