
# National Air System Delay
# This is the number of NAS delays there have been on each day so far for each flight
#### We may want to change this to within the last hour, or within the last X many flights

# Change NA values to 0
ordered$NAS_DELAY[is.na(ordered$NAS_DELAY)] <- 0
ordered$NAS_DELAY_IND <- ifelse((ordered$NAS_DELAY > 0) & (ordered$DEP_DEL15) > 0, 1, 0)

with.nas.delay <- function(df){
  delays = rep(-1, nrow(df))
  days = c(1:max(df$DAY_OF_YEAR))
  day = 1
  numNASDelay = 0
  for (i in 1:nrow(df)){ #for each flight, count the number of delays that day so far
    if (df$DAY_OF_YEAR[i] != day){
      day = day + 1
      numNASDelay = 0
    }
    delays[i] = numNASDelay #on this day how many flights have been delayed so far due to NAS?
    if (df$NAS_DELAY_IND[i] == 1){ #was this flight NAS delayed?
      numNASDelay = numNASDelay + 1
    }
  }
  df$NAS.delay.today = delays
  return(df)
}