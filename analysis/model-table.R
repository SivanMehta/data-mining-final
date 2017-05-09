# compare results from linear model, random forest, base rate, smoothing spline
# logistic regression, complete linkage and prototype clustering, svms, naive bayes
# and additive models with training error, test error, and number of delays predicted in guess set

models <- c("base rate", "linear model", "random forest", "smooth.spline", "logistic lasso",
            "complete linkage clustering", "prototype clustering", "svm", "naive bayes", "additive models")
model.table <- as.data.frame(models)
model.table$training.error <- 1
model.table$test.error <- 1
model.table$guess.delayed <- 1

to.percent <- function(error) {
  paste(round(error * 100, 2), "%", sep = "")
}

# base rate
model.table[1, 2] <- to.percent(length(which(train$DEP_DEL15 > 0)) / nrow(train))
model.table[1, 3] <- to.percent(length(which(vis$DEP_DEL15 > 0)) / nrow(vis))
model.table[1, 4] <- "NA"