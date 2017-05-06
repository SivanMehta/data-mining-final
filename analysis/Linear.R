
linear.models <- function(train, test, fit){
  predsTrain = fitted(fit)
  predsTrain = ifelse(predsTrain > 0.5, 1, 0)
  errTrain_lm = mean(predsTrain != train$DEP_DEL15)
  
  predslm = predict(fit, newdata = vis)
  predslm = ifelse(predslm > 0.5, 1, 0)
  errTest_lm = mean(predslm != vis$DEP_DEL15)
  
  return(c(errTrain_lm, errTest_lm))
}

all.lm = lm(DEP_DEL15 ~ index + arr.delay.ratio + DAY_OF_YEAR + 
            dep.delay.ratio + NAS.delay.ratio + MONTH, data = train)
errors.all = linear.models(train, vis, all.lm)
  
#Only dep.delay.ratio.ind
fit_lm = lm(DEP_DEL15 ~ weather.delay.ratio, data = train)
errors1 = linear.models(train, vis, fit_lm)

fit_lm2 = lm(DEP_DEL15 ~ index + arr.delay.ratio, data = train)
errors2 = linear.models(train, vis, fit_lm2)

fit_lm3 = lm(DEP_DEL15 ~ index + arr.delay.ratio + DAY_OF_YEAR, 
             data = train)
errors3 = linear.models(train, vis, fit_lm3)

fit_lm3 = lm(DEP_DEL15 ~ weather.delay.ratio.ind + arr.delay.ratio.ind + 
               arr.delay.ratio, data = train)
errors3 = linear.models(train, vis, fit_lm3)
