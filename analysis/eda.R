source("./analysis/load-data.R")

# replace NAs in _DELAY variables with 0
train[is.na(train$CARRIER_DELAY),]$CARRIER_DELAY <- 0
train[is.na(train$WEATHER_DELAY),]$WEATHER_DELAY <- 0
train[is.na(train$NAS_DELAY),]$NAS_DELAY <- 0
train[is.na(train$SECURITY_DELAY),]$SECURITY_DELAY <- 0
train[is.na(train$LATE_AIRCRAFT_DELAY),]$LATE_AIRCRAFT_DELAY <- 0

late <- train[which(train$DEP_DELAY > 0),]
on.time <- train[which(train$DEP_DELAY <= 0),]

base.rate <- nrow(late) / (nrow(late) + nrow(on.time))

library(ggplot2)

snapshot.train <- train[which(train$DEP_DELAY < 150),]

# overall look at delays
ggplot(snapshot.train) +
  aes(alpha = .5) + 
  geom_histogram(aes(x = DEP_DELAY), binwidth = 1, fill = "red") +
  geom_histogram(aes(x = ARR_DELAY), binwidth = 1, fill = "blue") + 
  scale_x_continuous(limits = c(-50, 75)) +
  ggtitle("Delays by Departure or Arrival")
  
# Is one carrier more delayed than the others?
ggplot(snapshot.train) +
  aes(alpha = .5) + 
  geom_density(aes(x = DEP_DELAY, color = CARRIER), binwidth = 1) +
  scale_x_continuous(limits = c(-20, 50)) +
  ggtitle("Departure Delays by Carrier")
