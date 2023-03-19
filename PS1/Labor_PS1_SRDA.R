setwd("/Users/changjay/Desktop/Labor_Economics/ECON-7069/PS1")
getwd()

rm(list=ls()) # remove variables
mydata <- read.csv("psfd_rr2020_v202208.csv", header = TRUE)

library(ggplot2)

# age
mydata$a02a <- 109 - mydata$a02a

# working rate
mydata$w03[mydata$w03 == 2] <- 1
mydata$w03[mydata$w03 == 3] <- 0

mean_work <- tapply(mydata$w03, mydata$a02a, mean)
mydata$mean_work <- mean_work[match(mydata$a02a, names(mean_work))]

# Plot
ggplot(mydata, aes(x = mean_work, y = a02a)) + 
  geom_point()