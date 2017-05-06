
# Models
source("./analysis/clean-data.R")
source("./features/add-features.R")
example <- train[1:5000, ]
 
train = add.features(train)
vis = add.features(vis)

takeout = c("FL_DATE", "DEP_TIME", "WHEELS_OFF", "WHEELS_ON", "TAXI_OUT", "TAXI_IN", "TAIL_NUM", "AIRLINE_ID", 
            "FL_NUM", "DEP_DELAY", "DEP_DELAY_GROUP", "DEP_DELAY_NEW", "CANCELLED", "CANCELLATION_CODE", 
            "ACTUAL_ELAPSED_TIME", "AIR_TIME", "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "ARR_TIME", 
            "ARR_DELAY_NEW", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "ARR_DEL15",
            "ARR_DELAY_GROUP", "DIVERTED", "UNIQUE_CARRIER", "dep.delay.ratio.ind", "arr.delay.ratio.ind",
            "NAS.delay.ratio.ind")
train = train[, !(names(train) %in% takeout)]
vis = vis[, !(names(vis) %in% takeout)]


# Random Forest 
library(randomForest)
rf = randomForest(as.factor(DEP_DEL15)~., data = train, importance=TRUE)
errTrain.rf = mean(train$DEP_DEL15 != rf$predicted) # .1725 Not good

testPreds = predict(rf, newdata = vis, type = "response")
errTest.vis = mean(vis$DEP_DEL15 != testPreds)

# Variable importance plot
varImpPlot(rf, main = "Variable importance plots")

for (i in 1:ncol(train.rf)){
  print(get.levels(train.rf[,i]))
}


rf.vis = randomForest(as.factor(DEP_DEL15)~.,data = vis.rf, importance=TRUE)

testPreds = predict(rf.vis, newdata = vis.rf, type = "response")
errTrain.vis = mean(vis.rf$DEP_DEL15 != rf.vis$predicted)

errTest.vis = mean(vis.rf$DEP_DEL15 != rf$predicted)

importantVars = importantVars[order[importantVars[,3]]]




