rm(list=ls())

library(data.table)
library(MASS)
library(dplyr)

set.seed(2758)

#set parameters
mu0 = 0
mu1 = 0
sigma0 = 1
sigma1 = 1
sigma01 = 0.5
beta1 = 0.1
beta2 = 0.05

sigma.nu <- sqrt(sigma0^2 + sigma1^2 - 2*sigma01) 
rho <- sigma01/(sigma0*sigma1)

#generate multivariate normal
sigma <- rbind(c(sigma0^2,sigma01),c(sigma01,sigma1^2)) 
mu <- c(0,0) 
q4df <- as.data.frame(mvrnorm(n=10000,mu=mu,Sigma=sigma)) 
q4df <- data.table(q4df)
colnames(q4df) <- c("ep0", "ep1")
X1 <- sample(seq(0, 40, 1) ,10000,replace=TRUE)
X2 <- sample(c(0, 6, 9, 12, 16, 18, 24), 10000, replace=TRUE) 
q4df <- cbind(q4df, X1, X2)
q4df <- q4df%>%
  mutate(w0 = mu0 + beta1*X1 + ep0,
         w1 = mu1 + beta1*X1 + beta2*X2 + ep1,
         I = if_else(w1 - w0 > 0, 1, 0), 
         prop_formula = 1 - pnorm((mu0 - mu1 - beta2*X2)/sigma.nu, 0, 1))
#the last line: estimate propensity score using the formula

#estimate propensity score using logit
logit <- glm(I ~ X2,data = q4df, 
             family = binomial(link="logit"))
q4df$prop_logit = predict(logit, type="response")

cor(q4df$prop_formula, q4df$prop_logit) #correlation = 0.9996493

#IPW
q4df<-q4df%>%
  mutate(w.formula = if_else(I == 1, 1/prop_formula , 1/(1-prop_formula)),
         w.logit = if_else(I==1, 1/prop_logit, 1/(1-prop_logit)),
         wage = if_else(I==1, w1, w0)) 
ipw <- lm(wage~I, weights=w.formula, data=q4df)
summary(ipw)
ipw$coefficients 

ipw.logit <- lm(wage~I, weights=w.logit, data=q4df) 
summary(ipw.logit)
ipw.logit$coefficients

#regress w on I
q9 <- lm(wage~I, data=q4df) 
summary(q9) 
q9$coefficients

#add covariates
q10<-lm(wage~I+X2,data=q4df) 
summary(q10)
q10$coefficients



