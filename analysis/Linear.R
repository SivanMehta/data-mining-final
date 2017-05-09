linear.models <- function(tr, te, fit) {
  predsTrain = fitted(fit)
  predsTrain = ifelse(predsTrain > .5, 1, 0)
  errTrain_lm = mean(predsTrain != train$DEP_DEL15)

  predslm = predict(fit, newdata = vis)
  predslm = ifelse(predslm > .5, 1, 0)
  errTest_lm = mean(predslm != vis$DEP_DEL15)
  return(c(errTrain_lm, errTest_lm))
}

# Considering the variable importance from the random forest, can we
# fit a linear model with just those? and do well

train <- add.features(train)
vis <- add.features(vis)

# fit.lm.1 <- lm(DEP_DEL15 ~ weather.delay.ratio.ind +
                            #index + DAY_OF_YEAR +
#                            arr.delay.ratio + DAY_OF_YEAR, data = train)
# y.hat.1 <- linear.models(train, vis, fit.lm.1)

fit.lm.2 <- lm(DEP_DEL15 ~ weather.delay.ratio.ind, data = train)
y.hat.2 <- linear.models(train, vis, fit.lm.2)

# fit.lm.3 <- lm(DEP_DELAY ~ weather.delay.ratio.ind, data = train)
# y.hat.3 <- linear.models(train, vis, fit.lm.3)
# y.hat.2[2]


# fit.lm.4 = lm(DEP_DEL15 ~ derivative, data = train)
# error.4 = linear.models(train, vim, fit.lm.4)
# 
# ## GLM
# fit_glm = glm(DEP_DEL15 ~ derivative + weather.delay.ratio + dep.delay.ratio, data = train, family='binomial')
# guess_test = predict(fit_glm, newdata=vis, type='response')
# table_50 = table(guess_test>0.5, vis$DEP_DEL15)
# misclass_rate_glm = (table_50[2] + table_50[3])/(table_50[1] + table_50[2] + table_50[3] + table_50[4])

