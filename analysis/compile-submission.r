source("./analysis/clean-data.R")
source("./features/add-features.R")

data.2016 <- add.features(data.2016)

# based on our analysis, this is the model with the smallest test error
fit.lm <- lm(DEP_DEL15 ~ weather.delay.ratio, data = data.2016)

guesses.2016 <- data.2016[which(data.2016$is.guess == 1), ]

predictions <- ifelse(predict(fit.lm, newdata = guesses.2016) > .5, 1, 0)

delay.guesses <- predictions
performance.guess <- 0
team.name <- "Naive Dazed and Confused"

save(list=c("delay.guesses","performance.guess","team.name"), file="stat462final.RData")
