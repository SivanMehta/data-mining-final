
#GAM

library(mgcv)

fit.gam = gam(DEP_DEL15 ~ index + s(arr.delay.ratio) + s(dep.delay.ratio) + s(weather.delay.ratio), 
                data = train, family = "binomial")
preds.train.gam = predict(fit.gam, newdata = train, type = "response")
preds.train.gam = ifelse(preds.train.gam < .5, 0, 1)
preds.test.gam = predict(fit.gam, newdata = vis, type = "response")
preds.test.gam = ifelse(preds.test.gam < .5, 0, 1)
err.train.gam = mean(preds.train.gam != train$DEP_DEL15)
err.test.gam = mean(preds.test.gam != vis$DEP_DEL15)


fit_glm = glm(DEP_DEL15 ~ dep.delay.ratio + weather.delay.ratio + arr.delay.ratio + NAS.delay.ratio, 
              data = train, family='binomial')

guess_test = predict(fit_glm, newdata=vis, type='response')
table_50 = table(guess_test>0.5, vis$DEP_DEL15)
misclass_rate_glm = (table_50[2] + table_50[3])/(table_50[1] + table_50[2] + table_50[3] + table_50[4])
table_50




