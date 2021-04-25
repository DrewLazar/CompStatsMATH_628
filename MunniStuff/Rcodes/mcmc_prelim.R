# Convergence of transition probabilites 
# Simple demonstration of stationary probabilities

P = matrix(c(.5,.5,1,0), nrow=2, ncol = 2, byrow = T)
P
library(expm)

# Calculate limiting probabilites by 

P%^%5 # after 5 steps 

P%^%100 # after 100 steps 

# Generate initial probilities from any distribution 
# After a large number of steps the probabilities become stationary
mu = matrix(c(.534161,.465839, .986845, .013155), nrow = 2, ncol=2, byrow = T)
mu

pi = mu%*%P
pi
pi_10 = mu%*%P%^%10
pi_10

pi_100 = mu%*%P%^%100
pi_100

# Finding stationary distribution from a transition probability matrix

solve(matrix(c(-.4, .1, .2, .1, -.3, .2, 1,1,1), nrow = 3, byrow = T), c(0,0,1))

# Multiply the tpm by itself over and over again until convergence

P1 = matrix(c(.6, .1, .3, .1, .7, .2, .2, .2, .6), nrow = 3, byrow = T)

P1%^%25
P1%^%100
