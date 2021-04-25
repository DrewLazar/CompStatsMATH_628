# Direct and indirect Monte Carlo methods 

# direct methods implements direct simulation to draw samples 
x=rexp(100,2)
mc_h = mean(sqrt(2*x))
mc_h

# Idirect monte carlo methods

# Example 1 : Important sampling 

# Marginal posterior is a t kernel
# Let g(theta) be a t with 2 d.f

# simulate theta from t with 2 df

N = 1000
thetas = rt(N, df=2)

theta_hat = mean(thetas)
theta_hat # monte carlo estimate of theta

thetas2 = thetas^2

thetas2_hat = mean(thetas2)

var_theta_hat = thetas2_hat - theta_hat^2
var_theta_hat

# Example 2: importance sampling with nontrivial weight function

# L(theta) ~ t_15
# pi(theta) ~ uniform(0,1)
# importance function g(theta) ~ t_2

N = 1000
thetas = rt(N, df=2)

# Compute the importance sampling weights

w.theta = dt(thetas, df=15)* 1/dt(thetas, df=2)

# Estimate posterios expectation

num = sum(thetas*w.theta)/N
den = sum(w.theta)/N

# MC estimator of posterios expectation by importance sampling method
post.mean.theta = num/den
post.mean.theta

# Example 3: Rejection sampling

# Assume L(theta)* pi(theta) ~ t_15
# envelop function is t_2
# M is 1.5
# U ~ uniform(0,1)

N = 1000
M = 1.5
thetas = rt(N, df=2)

# draw uniform random values
U = runif(N)

# acceptance condition
test = (U*M*dt(thetas, df=2) < dt(thetas, df=15))

# See how many are accepted
table(test)

# Keeping only acceptable thetas

keep.thetas = thetas[test]

# Posterior estimator of theta

post.mean.theta.rej = mean(keep.thetas)

post.mean.theta.rej

# Example 4: Weighted Bootstrap

# compute weights as in importance sampling

# compute normalized weights

qi = w.theta/sum(w.theta)

# Resample 500 thetastar 

m = 500

theta.stars = sample(thetas, size=m, replace=T, prob=qi)

# Compute posterior mean : MC estimate of theta by weighted bootstrap

post.mean.wtboot = mean(theta.stars)

post.mean.wtboot
