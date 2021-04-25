#Plot our function
x <- seq(0,6,0.05)
y <- rep(0,length(x))
plot(x, log(x)/(1+x),
     main="Problem 2.1.1",
     ylab="log(x)/(1+x)",
     type="l",
     col="blue")
#Plot (the numerator) of first derivative
plot(x,(1+x)-x*log(x),
     type="l",
     col="red")
lines(x,y, col="blue")
#Bisection Method
#Define our function 
func <- function(x) {
  (1+x)-x*log(x)
}
#Define our bisection function 
bisection <- function(f, a, b, n = 1000, tol1 = 1e-5, tol2 = 1e-5) {
  # Make sure that we have captured a root in [a,b]
  if (f(a)*f(b)>0) {
    stop('f(a)*f(b)>0')
  } 
  for (i in 1:n) {
    c <- (a + b) / 2 # Calculate midpoint
#Check our tolerance conditions 
   if ((b - a) / 2 < tol1 || abs(f(c))  < tol2  ) {
    mylist<-list(c,i)
     return(mylist)
    }
    # If another iteration is required, 
    # check the signs of the function at the points c and a and reassign
    # a or b accordingly as the midpoint to be used in the next iteration.
   if(f(a)*f(c)<=0){b<-c
   }else{a<-c}
  }
  # If the max number of iterations is reached and no root has been found, 
  # return message and end function.
  print('Too many iterations')
}