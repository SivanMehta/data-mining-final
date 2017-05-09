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
model.table$test.error <- ""
model.table$guess.delayed <- ""

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
model.table[4, 2] <- to.percent(1 - misclass.com)
model.table[5, 2] <- to.percent(1 - misclass.proto)

# svms (values saved from a previous session)
# select.names <- c("weather.delay.ratio", "CRS_PIT_TIME")
# train.df <- data.frame(x = train[,select.names], y = train$DEP_DEL15)
# vis.df <- data.frame(x = vis[,select.names])

#svm.linear <- tune(svm, y ~ ., data = train_df[1:2000,], scale = TRUE, kernel = "linear", ranges = list(cost = c(1,10,100)))

# train.pred <- predict(svm_linear$best.model, newdata = train.df)
# train.pred <- ifelse(train.pred > 0.5, 1, 0)
# misclass.train <- length(which(train$DEP_DEL15 == train.pred)) / nrow(train)
model.table[6, 2] <- to.percent(.1646)
# vis.pred <- predict(svm.linear$best.model, newdata = vis.df)
# vis.pred <- ifelse(vis.pred > 0.5, 1, 0)
# misclass.test <- length(which(vis$DEP_DEL15 == vis.pred)) / nrow(vis)
model.table[6, 3] <- to.percent(.0894)
