# Example of Gibbs Algorithm

 Nsim=5000 #initial values
 n=15
 a=3
 b=7
 
 # Array function in R
 
 y = array(0, dim=c(5,1))
 y
 
 X=Th=array(0,dim=c(Nsim,1)) #init arrays
 
 Th[1]=rbeta(1,a,b) #init chains
 X[1]=rbinom(1,n,Th[1])
 
 #sampling loop
 for (i in 2:Nsim){ 
   X[i]=rbinom(1,n,Th[i-1])
   Th[i]=rbeta(1,a+X[i],n-X[i]+b)
 }
 
 plot(X)
 plot(Th)
 
 hist(Th, probability = T, xlab="theta", main="")
 curve(dbeta(x, 3, 7),col="darkblue",  lwd=2, add=TRUE)
 
 mu_th = a/(a+b) # theoritical mean of a beta distribution
 mu_th 
 sigsq_th = a*b/((a+b)^2*(a+b+a)) # theoritical variance of a beta distribution
 sigsq_th
 
 mean(Th) # sample mean of Gibbs chain for theta
 var(Th)  # sample variance of Gibbs chain for theta
 
 hist(X, probability = T,xlab="X", main="")
 
 # A simple function to evaluate the beta-binomial density
 dbb <- function(x, N, u, v) {
   choose(N,x)*beta(x+u, N-x+v)/beta(u,v)
 }
 dbb(3, 15, 3,7)
 
 mu_X = n*mu_th # theoritical mean of a beta-binomial distribution
 mu_X
 rho=1/(a+b+1)
 
 sigsq_X = mu_X*(1-mu_th)*(1+(n-1)*rho) # theoritical variance of a beta-binomial distribution
 sigsq_X
 
 mean(X) # sample mean of Gibbs chain for theta
 var(X)  # sample variance of Gibbs chain for theta
 
 
 
 #curve(dbb(x,15, 3, 7),col="darkgreen", lwd=2, add=TRUE)
 
 
 