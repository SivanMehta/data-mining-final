# 36-462: Data Mining Final
# 05/05/17
# R script for running tensor flow modeling
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

library(neuralnet)

source("analysis/clean-data.R")
source("features/add-features.R")

n <- c("dep.delay.ratio", "arr.delay.ratio", "weather.delay.ratio", "NAS.delay.ratio")
f <- as.formula(paste("DEP_DEL15 ~", paste(n, collapse = " + ")))
nn <- neuralnet(f, data = train, hidden = c(2), linear.output = FALSE)

set.seed(500)
library(MASS)
data <- Boston

index <- sample(1:nrow(data),round(0.75*nrow(data)))
train <- data[index,]
test <- data[-index,]
lm.fit <- glm(medv~., data=train)
summary(lm.fit)
pr.lm <- predict(lm.fit,test)
MSE.lm <- sum((pr.lm - test$medv)^2)/nrow(test)

maxs <- apply(data, 2, max) 
mins <- apply(data, 2, min)

scaled <- as.data.frame(scale(data, center = mins, scale = maxs - mins))

train_ <- scaled[index,]
test_ <- scaled[-index,]

n <- names(train_)
f <- as.formula(paste("medv ~", paste(n[!n %in% "medv"], collapse = " + ")))
nn <- neuralnet(f,data=train_,hidden=c(5,3),linear.output=T)