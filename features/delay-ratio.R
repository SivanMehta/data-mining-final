# 36-462: Data Mining Final
# 04/16/17
# R script for generating features
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

train <- read.csv("data/flights2015_clean_s.csv")
test <- read.csv("data/flights2016_visible_clean_s.csv")
guess <- read.csv("data/flights2016_guess_clean_s.csv")

# calculate the number of flights that have been delayed that day, 
# up until that time for visible data (2015 and 2016_visible)
delay_ratio_vis <- function(dat) {
  dat$DELAY_RATIO_DEP <- rep(0, nrow(dat))
  dates <- levels(dat$FL_DATE)
  for (date in dates) {
    day_dat <- dat[which(dat$FL_DATE == date),]
    day_dat <- day_dat[order(day_dat$CRS_DEP_TIME),]
    for (i in 1:nrow(day_dat)) {
      PIT_delay <- sum(day_dat$DEP_DEL15[day_dat$ORIGIN == "PIT"])
      day_dat$DELAY_RATIO[i] <- PIT_delay / i
    }
  }
  return(dat)
}

source("analysis/clean-data.R")
