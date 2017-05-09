source("./features/add-features.R")
train <- add.features(train)
vis <- add.features(vis)
data.2016 <- add.features(data.2016)
source("./analysis/Linear.R")
guesses.2016 <- data.2016[which(data.2016$is.guess == 1), ]

# compare results from linear model, random forest, base rate, smoothing spline
# logistic regression, complete linkage and prototype clustering, svms, naive bayes
# and additive models with training error, test error, and number of delays predicted in guess set

models <- c("base rate", "linear model", "random forest", "complete linkage clustering", 
            "prototype clustering", "svm", "naive bayes", "additive models")
model.table <- as.data.frame(models)
model.table$training.error <- 1
model.table$test.error <- NA
model.table$guess.delayed <- NA

to.percent <- function(error) {
  paste(round(error * 100, 2), "%", sep = "")
}

# base rate
model.table[1, 2] <- to.percent(length(which(train$DEP_DEL15 > 0)) / nrow(train))
model.table[1, 3] <- to.percent(length(which(vis$DEP_DEL15 > 0)) / nrow(vis))
model.table[1, 4] <- "NA"

# linear model
model.table[2, 2] <- to.percent(y.hat.2[1])
model.table[2, 3] <- to.percent(y.hat.2[2])
y.hat.lm <- predict(fit.lm.2, newdata = guesses.2016)
predictions.y.hat <- length(which(y.hat.lm > .5))
model.table[2, 4] <- predictions.y.hat

rm(y.hat.lm)
rm(predictions.y.hat)

# random forest
# (copied from someone else's session because it takes really long to run)
model.table[3, 2] <- to.percent(0.1699)
model.table[3, 3] <- to.percent(0.0916)

# clustering
model.table[4, 2] <- to.percent(misclass.com)
model.table[5, 2] <- to.percent(misclass.proto)
