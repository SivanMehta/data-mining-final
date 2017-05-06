source("./analysis/clean-data.R")
source("./features/add-features.R")
source("./analysis/Linear.R")

data.2016 <- add.features(data.2016)

# based on our analysis, this is the model with the smallest test error
fit.lm <- lm(DEP_DEL15 ~ weather.delay.ratio, data = data.2016)

guesses.2016 <- data.2016[which(data.2016$is.guess == 1), ]

predictions <- ifelse(predict(fit.lm, newdata = guesses.2016) > .5, 1, 0)

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