# 36-462: Data Mining Final
# 05/05/17
# R script for svm 
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

library(e1071)

source("analysis/clean-data.R")
source("features/add-features.R")

train <- add.features(train)
vis <- add.features(vis)


select.names <- c("weather.delay.ratio", "CRS_PIT_TIME")
train_df <- data.frame(x = train[,select.names], y = train$DEP_DEL15)
vis_df <- data.frame(x = vis[,select.names])

svm_linear <- tune(svm, y ~ ., data = train_df[1:2000,], scale = TRUE, kernel = "linear",
                   ranges = list(cost = c(1,10,100)))

vis_pred <- predict(svm_linear$best.model, newdata = vis_df)
vis_pred <- ifelse(vis_pred > 0.5, 1, 0)
mean(vis_pred != vis$DEP_DEL15)



svm_radial <- tune(svm, y ~ ., data = samp, scale = TRUE, kernel = "radial",
                   ranges = list(cost = c(1,10,100)), gamma = c(0.1, 1, 10))

samp <- train_df[sample(1:nrow(train_df), 2000, replace = FALSE),]
svm_radial <- svm(y ~ ., data = samp, scale = TRUE, kernel = "radial")
vis_pred <- predict(svm_radial$best.model, newdata = vis_df)
vis_pred <- ifelse(vis_pred > 0.5, 1, 0)
mean(vis_pred != vis$DEP_DEL15)

derivative <- rep(0, nrow(train))
for (i in 5:nrow(train)) {
  derv_avg <- 0
  for (j in 1:4) {
    derv_avg = derv_avg + (train$weather.delay.ratio[i] - train$weather.delay.ratio[(i-j)])
  }
  derivative[i] <- derv_avg / 4
  
}
