# MLE for Zero and two inflated Poisson model for Swidish fertility data
# Feb 20, 2017 MATH 628:  constrained optimization example
# June 6, 2013
# Revised October 11, 2013
# ztipfit.R
# Reference: R packages : Optim, Optimx 
#########################################################################################

# Calculate maximum likelihood estimators of lambda and pi
# Define counts (f) and no of births(y) as global variables 

f <<- c(114, 205, 466, 242, 85, 35, 16, 4, 1, 0, 1, 0, 1)
y <<- c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
fnot <<- sum(f)-f[1]-f[3]

# function to optimize : log likelihood function
ln <- function(th){
  p1 <- th[1]
  p2 <- th[2]
  lambda <- th[3]
  q <- 1-p1-p2
  f[1]*(log(p1+q*exp(-lambda)))+f[3]*(log(p2+q*.5*lambda^2*exp(-lambda)))+log(q)*fnot - lambda*fnot + log(lambda)*(sum(f*y) - 2*f[3])
}
gr = function(th){
  p1 <- th[1]
  p2 <- th[2]
  la <- th[3]
  q <- 1-p1-p2
  
  dervp1 = f[1]*(1-exp(-la))/(p1+q*exp(-la)) -f[3]*.5*la^2*exp(-la)/(p2+q*.5*la^2*exp(-la)) - fnot/q
  dervp2 = -f[1]*exp(-la)/(p1+q*exp(-la)) +f[3]*(1-.5*la^2*exp(-la))/(p2+q*.5*la^2*exp(-la)) - fnot/q
  dervla =-f[1]*q*exp(-la)/(p1+q*exp(-la))+ f[3]*.5*q*la*exp(-la)*(2-la)/(p2+q*.5*la^2*exp(-la)) - fnot +(1/la)*(sum(f*y) -2*f[3])  
  
  c(dervp1,dervp2,dervla)
}

# Calculate the mle using optim function

result = optim(c(.01,.01,2), ln, gr, method = "L-BFGS-B", lower = c(.001, .001, .1), upper = c(.29, .7, 25), control=list(fnscale=-1), hessian = TRUE) 

#mle

mle = result$par
q = 1-sum(mle[1:2])

# Information matrix and estimated dispersion matrix of the mle

infmat = (-1)*result$hessian

dismat = solve(infmat)
dismat
