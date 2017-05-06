# 36-462: Data Mining Final
# 04/16/17
# R script for generating features
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

source("analysis/clean-data.R")

# calculate the number of flights that have been delayed that day, up until that time
# for visible data (2015 and 2016_visible)
delay_ratio_vis <- function(dat) {
  ncol <- length(levels(dat$UNIQUE_CARRIER)) + 2
  features <- data.frame(matrix(0, nrow = nrow(dat), ncol = ncol))
  features <- cbind(features, dat$FL_DATE)
  features <- cbind(features, dat$CRS_PIT_TIME)
  carriers <- levels(dat$UNIQUE_CARRIER)
  dat$DEP_DEL15 <- recode(dat$DEP_DEL15, "c(NA)=0")
  dat$ARR_DEL15 <- recode(dat$ARR_DEL15, "c(NA)=0")
  
  dates <- levels(dat$FL_DATE)
  for (date in dates) {
    day_ind <- which(dat$FL_DATE == date)
    day_ind <- day_ind[order(dat$CRS_PIT_TIME[day_ind])]
    day <- dat[day_ind,]
    
    for (i in 1:length(day_ind)) {
      day_dat <- day[1:i,]
      # ie, how is PIT performing at any given time
      dep_inds <- which(day_dat$ORIGIN == "PIT")
      if (length(dep_inds) == 0) {
        dep_delay <- 0
      } else {
        dep_delay <- sum(day_dat$DEP_DEL15[dep_inds]) / length(dep_inds) # leaving PIT
      }
      features[day_ind[i], 1] <- dep_delay
      
      arr_inds <- which(day_dat$DEST == "PIT")
      if (length(arr_inds) == 0) {
        arr_delay <- 0
      } else {
        arr_delay <- sum(day_dat$ARR_DEL15[arr_inds]) / length(arr_inds) # arriving to PIT 
      }
      features[day_ind[i], 2] <- arr_delay
      
      # how is a carrier performing at any given time
      for (c in 1:length(carriers)) {
        carrier <- carriers[c]
        car_inds <- day_dat[which(day_dat$UNIQUE_CARRIER == carrier),]
        if (nrow(car_inds) == 0) {
          car_delay <- 0
          } else {
            car_delay <- sum(car_inds$DEP_DEL15 | car_inds$ARR_DEL15) / nrow(car_inds)
          }
        features[day_ind[i], (2+c)] <- car_delay
      }
    }
  }
  colnames(features) <- c(c("DEP_DELAY_RATIO", "ARR_DELAY_RATIO"), 
                          paste(carriers, "DELAY_RATIO", sep = "_"))
  return(features) 
}

try0 <- delay_ratio_vis(vis)
