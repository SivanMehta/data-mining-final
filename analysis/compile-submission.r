source("./analysis/clean-data.R")
source("./features/add-features.R")

data.2016 <- add.features(data.2016)
train <- add.features(train)
vis <- add.features(vis)

source("./analysis/mixmodels.R")

guesses.2016 <- data.2016[which(data.2016$is.guess == 1), ]

n <- c("arr.delay.ratio.ind", "dep.delay.ratio.ind")
vis_lcm <- vis[,n] + 1
train_lcm <- train[,n] + 1
test_lcm <- guesses.2016[,n] + 1

lcm <- poLCA(cbind(arr.delay.ratio.ind, dep.delay.ratio.ind) ~ 1,
             train_lcm, nclass = 2, verbose = FALSE)

predictions <- ifelse(dmultbinarymix(test_lcm, lcm) > 0.5, 0, 1)

delay.guesses <- predictions
performance.guess <- y.hat.2[2]
team.name <- "http://easyscienceforkids.com/all-about-the-empty-pot-a-chinese-folk-story-about-honesty/"

save(list=c("delay.guesses","performance.guess","team.name"), file="stat462final.RData")

check_submission_format = function(x){
  load(x)
  print(sprintf("Your team name is %s", team.name))
  print(sprintf("Your submission has %d guesses and is supposed to have 871", length(delay.guesses)))
  print(sprintf("You guessed %d delayed flights and %d non-delayed flights",sum(delay.guesses==1), sum(delay.guesses==0)))
  print(sprintf("You expect a misclassification error of %f", performance.guess))
}


check_submission_format("stat462final.RData")
