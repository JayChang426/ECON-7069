library(stats) 
library(tidyverse)

q14<-function(param){ 
  
  sig1 <- param[1]
  sig2 <- param[2]
  
  ep1=rnorm(2,0,sig1^2) # N = 2 
  ep2=rnorm(2,0,sig2^2) 
  y=ep1+ep2
  
  log(sig1^2+sig2^2) + log(2*pi) + (1 / (2*(sig1^2+sig2^2))) * sum(y^2)
  # "Maximum Likelihood Estimation" is to maximize the value of likelihood function.
  # since ‘optim‘ minimizes the function, we have to minimize the negative of the log-likelihood to get the maximization.
}

param<-c(4, 1) #initial values
q14.result<-optim(param, q14, method="L-BFGS-B", hessian=TRUE) 
q14.result$par