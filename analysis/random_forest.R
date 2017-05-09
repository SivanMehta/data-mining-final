
library(randomForest)

# Models
source("./analysis/clean-data.R")
source("./features/add-features.R")
 
train = add.features(train)
vis = add.features(vis)

train$day = yday(train$FL_DATE)
vis$day = yday(vis$FL_DATE)

takeout = c("FL_DATE", "DEP_TIME", "WHEELS_OFF", "WHEELS_ON", "TAXI_OUT", "TAXI_IN", "TAIL_NUM", "AIRLINE_ID", 
            "FL_NUM", "DEP_DELAY", "DEP_DELAY_GROUP", "DEP_DELAY_NEW", "CANCELLED", "CANCELLATION_CODE", 
            "ACTUAL_ELAPSED_TIME", "AIR_TIME", "CARRIER_DELAY", "WEATHER_DELAY", "NAS_DELAY", "ARR_TIME", 
            "ARR_DELAY_NEW", "SECURITY_DELAY", "LATE_AIRCRAFT_DELAY", "ARR_DEL15",
            "ARR_DELAY_GROUP", "DIVERTED", "UNIQUE_CARRIER", "dep.delay.ratio.ind", "arr.delay.ratio.ind",
            "NAS.delay.ratio.ind", "weather.delay.ratio.ind", "ORIGIN", "DEST","TOTAL_ADD_GTIME", 
            "FIRST_DEP_TIME", "LONGEST_ADD_GTIME", "index", "DAY_OF_YEAR")
train = train[, !(names(train) %in% takeout)]
vis = vis[, !(names(vis) %in% takeout)]


# Random Forest 
rf = randomForest(as.factor(DEP_DEL15)~ day + dep.delay.ratio + CRS_ELAPSED_TIME + MONTH + 
                    NAS.delay.ratio + weather.delay.ratio + DAY_OF_MONTH + 
                    DISTANCE + CRS_PIT_TIME + CRS_DEP_TIME + QUARTER +
                    DAY_OF_WEEK + DISTANCE_GROUP, data = train, importance=TRUE)
errTrain.rf = mean(train$DEP_DEL15 != rf$predicted) 

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

#Try 2
rf = randomForest(DEP_DEL15 ~ dep.delay.ratio + weather.delay.ratio + arr.delay.ratio + NAS.delay.ratio, 
                  data = train, importance = TRUE)
rf.predicted = ifelse(rf$predicted > 0.5, 1, 0)
errTrain.rf = mean(train$DEP_DEL15 != rf.predicted) 
rf.errTrain = predict(rf, newdata = train, type = "response")


# With indicator Vars
rf.ind = randomForest(DEP_DEL15 ~ arr.delay.ratio.ind + arr.delay.ratio.ind + NAS.delay.ratio.ind,
                      data = train, importance = TRUE)
varImpPlot(rf.ind, main = "Variable importance plots")

rf.predicted = ifelse(rf.ind$predicted > 0.5, 1, 0)
errTrain.rf = mean(train$DEP_DEL15 != rf.predicted) 
rf.errTrain = predict(rf.ind, newdata = train, type = "response")

testPreds = predict(rf.ind, newdata = vis, type = "response")
testPreds = ifelse(testPreds > 0.5, 1, 0)
errTest = mean(vis$DEP_DEL15 != testPreds)

errTest.vis = mean(vis.rf$DEP_DEL15 != rf$predicted)

testPreds = predict(rf.ind, newdata = guess, type = "response")
testPreds = ifelse(testPreds > 0.5, 1, 0)
