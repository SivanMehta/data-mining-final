source("./analysis/load-data.R")
library(ggplot2)
library(dplyr)

late <- train[which(train$DEP_DEL15 > 0),]
on.time <- train[which(train$DEP_DEL15 == 0),]

base.rate <- nrow(late) / (nrow(late) + nrow(on.time))

sample.data <- na.omit(sample_n(train, nrow(train) * .1))

# Is one carrier more delayed than the others?
ggplot(train) +
  aes(alpha = .5) + 
  geom_density(aes(x = DEP_DELAY, color = UNIQUE_CARRIER)) +
  scale_x_continuous(limits = c(-20, 50)) +
  ggtitle("Departure Delays by Carrier")

# What about a particular airport?
ggplot(train) +
  aes(alpha = .5) + 
  geom_density(aes(x = DEP_DELAY, color = as.factor(ORIGIN_WAC))) +
  scale_x_continuous(limits = c(-20, 50)) +
  ggtitle("Departure Delays by Origin Airport")