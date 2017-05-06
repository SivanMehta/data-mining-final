# 36-462: Data Mining Final
# 05/05/17
# R script for running tensor flow modeling
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

library(neuralnet)

source("analysis/clean-data.R")
source("features/add-features.R")

train <- add.features(train)
vis <- add.features(vis)

n <- c("dep.delay.ratio", "arr.delay.ratio", "weather.delay.ratio", "NAS.delay.ratio")
f <- as.formula(paste("DEP_DEL15 ~", paste(n, collapse = " + ")))
nn <- neuralnet(f, data = train, hidden = c(2), linear.output = FALSE)

plot(nn)

train_fit <- compute(nn, train[,n])
train_fit <- ifelse(train_fit$net.result[,1] > 0.5, 1, 0)
train_err <- mean(train_fit != train$DEP_DEL15)

nn_pred <- compute(nn, vis[,n])
vis_fit <- ifelse(nn_pred$net.result[,1] > 0.5, 1, 0)
vis_err <- mean(train_fit != vis$DEP_DEL15)
