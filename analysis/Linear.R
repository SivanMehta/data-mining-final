linear.models <- function(tr, te, fit) {
  predsTrain = fitted(fit)
  predsTrain = ifelse(predsTrain > 0.5, 1, 0)
  errTrain_lm = mean(predsTrain != train$DEP_DEL15)

  predslm = predict(fit, newdata = vis)
  predslm = ifelse(predslm > 0.5, 1, 0)
  errTest_lm = mean(predslm != vis$DEP_DEL15)
  return(c(errTrain_lm, errTest_lm))
}

# Considering the variable importance from the random forest, can we
# fit a linear model with just those? and do well

source("./analysis/clean-data.R")
source("./features/add-features.R")

train <- add.features(train)
vis <- add.features(vis)

fit.lm.1 <- lm(DEP_DEL15 ~ weather.delay.ratio.ind +
                           index + DAY_OF_YEAR +
                           arr.delay.ratio + DAY_OF_YEAR, data = train)
y.hat.1 <- linear.models(train, vis, fit.lm.1)

fit.lm.2 <- lm(DEP_DEL15 ~ weather.delay.ratio.ind, data = train)
y.hat.2 <- linear.models(train, vis, fit.lm.2)
y.hat.2[2]

fit.lm.3 <- lm(DEP_DEL15 ~ weather.delay.ratio + index, data = train)
y.hat.3 <- linear.models(train, vis, fit.lm.3)
y.hat.3[2]
