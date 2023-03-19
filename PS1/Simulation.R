library(data.table)
library(MASS)

set.seed(2758)

# assign
mu0 <- 0
mu1 <- 0
sd0 <- 1
sd1 <- 1
rho <- -0.50
c= 1

# statistic
s01 = rho*sd0*sd1 # covariance between epsilon_0 and epsilon_1
sdv = (sd0**2+sd1**2-2*s01)**(1/2) # standard deviation of v (epsilon_0 - epsilon_1)

# stimulation, bivariate normal distribution
wages<- as.data.table(mvrnorm(n=10000000,mu=c(0, 0),
                              Sigma=matrix(c(sd0**2, s01, s01, sd1**2), ncol=2)))
colnames(wages) <- c("e0","e1")
wages[ ,c("w0","w1") := list(e0 + mu0, e1 + mu1)] # [ , c("w0","w1")]: include all rows and specific columns
wages[ ,I := ifelse(w1 > (w0 + c),1,0) ] # ifelse: indicator variable

#can't be seen
Ex_w0I=wages[, .(Ex=mean(w0)), by =I][I== 1][[1,2]]
#can be seen
Ex_w1I=wages[, .(Ex=mean(w1)), by =I][I== 1][[1,2]]

#population
z=(mu0-mu1+c)/sdv
IM_ratio=dnorm(z)/(1-pnorm(z))

E_w0I=mu0+(sd0*sd1/sdv)*(rho-(sd0/sd1))*IM_ratio
E_w1I=mu1+(sd0*sd1/sdv)*((sd1/sd0)-rho)*IM_ratio

#E_e0I=(sd0*sd1/sdv)*(rho-(sd0/sd1))*IM_ratio
#E_e1I=(sd0*sd1/sdv)*((sd1/sd0)-rho)*IM_ratio
