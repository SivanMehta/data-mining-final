# 36-462: Data Mining Final
# 05/05/17
# R script for appending visible features to guessing data
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

source("analysis/clean-data.R")
source("features/add-features.R")

train <- add.features(train)
vis <- add.features(vis)

library(dplyr)

by_date <- group_by(vis, FL_DATE)
last_vis <- summarize(by_date, 
                      last.time = max(CRS_PIT_TIME),
                      dep.delay.ratio = last(dep.delay.ratio),
                      arr.delay.ratio = last(arr.delay.ratio),
                      weather.delay.ratio = last(weather.delay.ratio),
                      NAS.delay.ratio = last(NAS.delay.ratio))

guess <- merge(guess, last_vis, by = "FL_DATE")
  