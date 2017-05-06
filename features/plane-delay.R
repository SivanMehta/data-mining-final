source("./analysis/clean-data.R")
train <- read.csv("./data/flights2015.csv")

add.plane.delays <- function(df) {
  # count number of times a plane was delayed in 2015
  planes <- names(table(df$TAIL_NUM))
  delayed <- df[which(df$DEP_DEL15 == 1), ]
  
  delays <- rep(-1, length(planes))
  df$plane.delay <- 0
  for(i in 2:length(planes)) {
    plane <- planes[i]
    
    plane.delays <- length(which(delayed$TAIL_NUM == plane))
    
    df$plane.delay[df$TAIL_NUM == plane] <- plane.delays
  }
  
  return(df)
}

train <- add.plane.delays(train)

plane.delays.2015 <- function(df) {
  planes <- names(table(df$TAIL_NUM))
  delayed <- df[which(df$DEP_DEL15 == 1), ]
  
  delays <- rep(-1, length(planes))
  df$plane.delay <- 0
  for(i in 2:length(planes)) {
    plane <- planes[i]
    
    plane.delays <- train[which(train$TAIL_NUM == plane),]$plane.delay[1]
    if(is.na(plane.delays)) {
      plane.delays <- 0
    }
    
    df$plane.delay[df$TAIL_NUM == plane] <- plane.delays
  }
  
  return(df)
}

