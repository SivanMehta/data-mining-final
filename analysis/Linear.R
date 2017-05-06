linear.models <- function(tr, te, fit) {
  predsTrain = fitted(fit)
  predsTrain = ifelse(predsTrain > .5, 1, 0)
  errTrain_lm = mean(predsTrain != train$DEP_DEL15)
<<<<<<< HEAD
  
  predslm = predict(fit, newdata = vis, type = "response")
  predslm = ifelse(predslm > 0.5, 1, 0)
=======

  predslm = predict(fit, newdata = vis)
  predslm = ifelse(predslm > .5, 1, 0)
>>>>>>> 29c8e2e5483ed19243ab2234b44269e813726f59
  errTest_lm = mean(predslm != vis$DEP_DEL15)
  return(c(errTrain_lm, errTest_lm))
}

# Considering the variable importance from the random forest, can we
# fit a linear model with just those? and do well

train <- add.features(train)
vis <- add.features(vis)

<<<<<<< HEAD
fit_lm2 = lm(DEP_DEL15 ~ weather.delay.ratio.ind + DAY_OF_YEAR, data = train)
errors2 = linear.models(train, vis, fit_lm2)
=======
# fit.lm.1 <- lm(DEP_DEL15 ~ weather.delay.ratio.ind +
                            #index + DAY_OF_YEAR +
#                            arr.delay.ratio + DAY_OF_YEAR, data = train)
# y.hat.1 <- linear.models(train, vis, fit.lm.1)
>>>>>>> 29c8e2e5483ed19243ab2234b44269e813726f59

fit.lm.2 <- lm(DEP_DEL15 ~ weather.delay.ratio.ind, data = train)
y.hat.2 <- linear.models(train, vis, fit.lm.2)
y.hat.2[2]

# fit.lm.3 <- lm(DEP_DELAY ~ weather.delay.ratio.ind, data = train)
# y.hat.3 <- linear.models(train, vis, fit.lm.3)
# y.hat.2[2]
