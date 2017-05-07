# 36-462: Data Mining Final
# 05/05/17
# R script for svm
# Data documentation: https://www.transtats.bts.gov/DL_SelectFields.asp?Table_ID=236

library(poLCA)

# INPUTS
# x = a dataframe
# model = an object fit by poLCA
# offset = a number to recode a binary variable
# log = if the probabilities should be on the logarithmic scale
# OUTPUTS
# returns
dmultbinarymix <- function(x, model, offset=1, log=FALSE) {
  x <- x-offset # this recodes so that it's actually binary
  # remakes the probabilities matrices from the poLCA object
  prob.matrix <- sapply(model$probs, function(mat) { mat[,2] })
  # just in case any matrices are empty
  if (is.null(dim(prob.matrix))) {
    prob.matrix <- array(prob.matrix, dim=c(1,length(prob.matrix)))
  }

  class.probs <- model$P
  # this function returns the product of the binomial probabilities for each variable
  # and weights that product by the probability of being in the conditioned class
  class.cond.prob <- function(x,c) {
    class.probs[c]*prod((prob.matrix[c,]^x)*((1-prob.matrix[c,])^(1-x)))
  }
  # this function sums the weighted probabilities for each class
  one.point.prob <- function(x) {
    summands <- sapply(1:length(class.probs), class.cond.prob, x=x)
    return(sum(summands))
  }
  probs <- apply(x, 1, one.point.prob)
  if (log) {
    return(log(probs))
  } else {
    return(probs)
  }
}


n <- c("NAS.delay.ratio.ind", "weather.delay.ratio.ind")
train_lcm <- train[,n] + 1
vis_lcm <- vis[,n] + 1

lcm <- poLCA(cbind(NAS.delay.ratio.ind, weather.delay.ratio.ind) ~ 1,
             train_lcm, nclass = 2, verbose = FALSE)

train_pred <- ifelse(dmultbinarymix(train_lcm, lcm) > 0.5, 0, 1)
vis_pred <- ifelse(dmultbinarymix(vis_lcm, lcm) > 0.5, 0, 1)

train_err <- mean(train_pred != train$DEP_DEL15)
vis_err <- mean(vis_pred != vis$DEP_DEL15)
