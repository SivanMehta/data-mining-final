source("./analysis/clean-data.R")
source("./features/add-features.R")

guesses <- add.features(data.2016)

guesses.2016 <- data.2016[which(data.2016$is.guess == 1), ]

delay.guesses <- rep(0, nrow(flights2016))
performance.guess <- 0
team.name <- "TBD"

save(list=c("delay.guesses","performance.guess","team.name"), file="stat462final.RData")
