source("./analysis/clean-data.R")
source("./features/add-features.R")
library(glmnet)
library(e1071)
vis <- add.features(vis)

# glm
model.glm <- glm(DEP_DEL15 ~ dep.delay.ratio + arr.delay.ratio, 
                 data = vis, 
                 family = 'binomial')
y.hat <- predict(model.glm, newdata = vis, type = 'response')
table(y.hat > .5, vis$DEP_DEL15)
# this doesn't work

model.nb <- naiveBayes(DEP_DEL15 ~ dep.delay.ratio + arr.delay.ratio, 
                 data = vis, 
                 family = 'binomial')
y.hat <- predict(model.nb, newdata = vis, type = 'response')
table(y.hat > .5, vis$DEP_DEL15)