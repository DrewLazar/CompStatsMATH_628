
##################### MLE of  multiparameters in a Normal distribution ###############

x=c(1.77, -0.23, 2.76, 3.80, 3.47, 56.75, -1.34, 4.24, -2.44, 3.29, 3.71, -2.44, 4.53, -0.07, -1.05, -13.87, -2.53,-1.75, 0.27,43.21)

# l calculates the loglikelihood (objective function) as funtion of mu and sigma

l = function(th){ # Note th is a vector of parameters mu and sigma: note my sigma is sigma^2
  mu = th[1]
  sigma = th[2]
  
  sum1 = 0
  for (i in 1:20){
    sum1 = sum1 +(x[i] -mu)^2
  }
  -length(x)*log(2*pi) -(1/2)*length(x)*log(sigma)- (1/2)*(1/sigma)*sum1 # return this value which is the log-likelihood  
}

# Calculate the gradient vector c(dl/d mu, dl/d sigma)
# First order derivative of l with respect to mu and sigma

l.prime = function(th){
  mu = th[1]
  sigma = th[2]
  sum2 = 0
  sum3 = 0
  for (i in 1:20){
  sum2 = sum2 + (x[i]-mu)
  sum3 = sum3 + (x[i]-mu)^2
} 
dldmu = (1/sigma)*sum2
dldsigma = sum3/(2*sigma^2) -length(x)/(2*sigma)
c(dldmu,dldsigma) # return the gradient vector
}

# Calculate the hessian matrix matrix(dlsq/d musq, dlsq/d mu sigma,dlsq/d mu sigma, dlsq/d sigmasq)
# Second order derivatives 

l2.prime = function(th){
  mu = th[1]
  sigma = th[2]
  sum4 = 0
  sum5 = 0
  for (i in 1:20){
    sum4 = sum4 + (x[i]-mu)
    sum5 = sum5 + (x[i]-mu)^2
  } 
  dldmu2 = -length(x)/sigma
  dlmusig = -sum4/sigma^2
  dldsigma2 = -sum5/(sigma^2) + length(x)/(2*sigma^2)
  A = matrix(c(dldmu2,dlmusig, dlmusig, dldsigma2), nrow=2, ncol=2, byrow=T) # return the gradient vector
  A
}

# Caculate sample mean and sample standard deviation
# Use these as inital values for mu and sigma in the optim function



mu0 = mean(x)
sigma0 = (sd(x))^2

theta = c(mu0, sigma0)

l(theta)
l.prime(theta)
l2.prime(theta)

itr = 100
library(MASS)

for(i in 1:itr){theta = theta - l.prime(theta) %*% ginv(l2.prime(theta))}



# Use R function optim() from library{stats} to maximize l(mu,sigma)

res <- optim(c(mu0,sigma0), l, l.prime, method = "BFGS",control=list(fnscale=-1))

