flights2016 <- read.csv("./data/flights2016.csv")

delay.guesses <- rep(0, nrow(flights2016))
performance.guess <- 0
team.name <- "TBD"

save(list=c("delay.guesses","performance.guess","team.name"), file="stat462final.RData")
