# Use Metropolis-Hastings algorithm to generate samples from 
# a Beta(2.7, 6.3) using a uniform proposal density

a=2.7; b=6.3; 
 Nsim=5000
 X=rep(runif(1),Nsim) # initialize the chain
 for (i in 2:Nsim){
   Y=runif(1)
  rho=dbeta(Y,a,b)/dbeta(X[i-1],a,b)
   X[i]=X[i-1] + (Y-X[i-1])*(runif(1)<rho)
 }
 

 
 # Check the properties of Beta distirbution with MCMC samples
 mu = a/(a+b)
 mu
 sigsq = a*b/((a+b)^2 * (a+b+1))
 sigsq
 
 mean(X)
 var(X)
 
 # Histograms of the MCMC sample values
 par(mfrow=c(2,1))
 
 hist(X, probability = T, xlab="x-variable", main="")
 curve(dbeta(x, 2.7, 6.3),col="darkblue",  lwd=2, add=TRUE)
 
 hist(rbeta(5000, 2.7, 6.3), probability = T,xlab="x-variable", main="")
 
 
 curve(dbeta(x, 2.7, 6.3),col="darkgreen", lwd=2, add=TRUE)
 
 