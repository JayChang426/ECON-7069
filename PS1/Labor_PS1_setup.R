setwd("/Users/changjay/Desktop/Labor_Economics/PS1")
getwd()

rm(list=ls()) # remove variables

# Define a function to add two numbers
my_sum <- function(x, y) {
  z <- x + y
  return(z)
}

# Initiate the debugger on the function
debug(my_sum)

# Call the function with some input values
my_sum(2, 3)
