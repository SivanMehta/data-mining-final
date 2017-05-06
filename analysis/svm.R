# 36-462: Data Mining Final
# 05/05/17
# R script for svm 
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

library(e1071)

source("analysis/clean-data.R")
source("features/add-features.R")

train <- add.features(train)
vis <- add.features(vis)


n <- c("dep.delay.ratio", "arr.delay.ratio", "weather.delay.ratio", "NAS.delay.ratio",
       "DAY_OF_YEAR", "CRS_DEP_TIME")
train_df <- data.frame(x = train[,n], y = train$DEP_DEL15)

svm_linear <- tune(svm, y ~ ., data = train_df[1:2000,], scale = TRUE, kernel = "linear",
                   ranges = list(cost = c(1,10,100)))
vis_df <- data.frame(x = vis[,n])
vis_pred <- predict(svm_linear$best.model, newdata = vis_df)
vis_pred <- ifelse(vis_pred > 0.5, 1, 0)
mean(vis_pred != vis$DEP_DEL15)

svm_radial <- tune(svm, y ~ ., data = train_df, scale = TRUE, kernel = "radial",
                   ranges = list(cost = c(1,10,100)), gamma = c(0.1, 1, 10))
vis_pred <- predict(svm_radial$best.model, newdata = vis_df)
vis_pred <- ifelse(vis_pred > 0.5, 1, 0)
mean(vis_pred != vis$DEP_DEL15)