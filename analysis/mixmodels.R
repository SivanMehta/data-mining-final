# 36-462: Data Mining Final
# 05/05/17
# R script for svm 
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

library(poLCA)

source("analysis/clean-data.R")
source("features/add-features.R")

train <- add.features(train)
vis <- add.features(vis)

n <- c("dep.delay.ratio.ind", "arr.delay.ratio.ind", 
       "weather.delay.ratio.ind", "NAS.delay.ratio.ind")

train_lcm <- train[,n] + 1
lcm <- poLCA(cbind(dep.delay.ratio.ind, arr.delay.ratio.ind, 
                   weather.delay.ratio.ind, NAS.delay.ratio.ind) ~ 1, 
             train_lcm, nclass = 2, verbose = FALSE)

library(glmnet)

fit <- glm(DEP_DEL15 ~ NAS.delay.ratio, data = train, family = "binomial")
mean(train$DEP_DEL15 != ifelse(fit$fitted.values > 0.9, 1, 0))
mean(train$DEP_DEL15 != 0)
summary(fit$fitted.values)


fit1 <- lm(DEP_DELAY ~ weather.delay.ratio, data = train)
fitted_val <- ifelse(fitted(fit1) >= 50, 1, 0)
mean(train$DEP_DEL15 != fitted_val)

fitted_val = fitted(fit1)
fitted_val = fitted_val[which(train$DEP_DEL15 == 1 & fitted_val > 7.2)]
summary(fitted_val)
table(train$DEP_DEL15[which(round(fitted_val, 3) == 7.137)])
