source('./analysis/load-data.R')
library(dplyr)
library(protoclust)

sample.data <- sample_n(test, nrow(test) * .1)
flights.dist <- dist(sample.data)

#tree.sin <- hclust(flights.dist, method = "single")
#tree.avg <- hclust(flights.dist, method = "average")
tree.com <- hclust(flights.dist, method = "complete")
tree.proto <- protoclust(flights.dist)

#plot(tree.sin)
#plot(tree.avg)
plot(tree.com)
plot(tree.proto)

# Complete linkage and prototype linkage seem to have
# most clear groups, so we'll compare the results of these
# in predicting DEP_DEL15

assignments.com <- cutree(tree.com, 2)
xtabs(~assignments.com + sample.data$DEP_DEL15)

# Generally, these are doing pretty poorly, as they largely 
# reflect the proportions in the original data. Let's look
# at prototype clustering

assignments.proto <- protocut(tree.proto, 2)
xtabs(~assignments.proto$cl + sample.data$DEP_DEL15)

table(sample.data$DEP_DEL15)

# If we use cluster assignments as classifications, what is
# our missclassifcation rate?

predictions.com <- assignments.com - 1
right.com <- length(which(predictions.com == sample.data$DEP_DEL15)) / nrow(sample.data)
misclass.com <- right.com

predictions.proto <- assignments.proto$cl - 1
right.proto <- length(which(predictions.proto == sample.data$DEP_DEL15)) / nrow(sample.data)
misclass.proto <- right.proto
