library(LearnBayes)

######### 4.1
sleep_times = c(9, 8.5, 7, 8.5, 6, 12.5, 6, 9, 8.5, 7.5, 8, 6, 9, 8, 7, 10, 9, 7.5, 5, 6.5)
d = mycontour(normchi2post, c(6, 10, 1, 12), sleep_times, xlab="mean",ylab="variance")
S = sum((sleep_times - mean(sleep_times))^2)
n = length(sleep_times)
sigma2 = S/rchisq(1000, n - 1)
mu = rnorm(1000, mean = mean(sleep_times), sd = sqrt(sigma2)/sqrt(n))
points(mu, sigma2)

mu_cred_int = quantile(mu, c(0.05, 0.95))
std_cred_int = sqrt(quantile(sigma2, c(0.05, 0.95)))

p75 = mu + 0.674*sqrt(sigma2)
mean(p75)
sd(p75)



####### 4.4

vals = c(10,11,12,11,9)
d = mycontour(normchi2post, c(6, 15, 0.1, 15), vals, xlab="mean",ylab="variance")
S = sum((vals - mean(vals))^2)
n = length(vals)
sigma2 = S/rchisq(1000, n - 1)
mu = rnorm(1000, mean = mean(vals), sd = sqrt(sigma2)/sqrt(n))
points(mu, sigma2)
# c
n_grid_points = 11
mu_list = seq(7,14, length.out = n_grid_points)
mu_grid = matrix(rep(mu_list, n_grid_points), c(n_grid_points,n_grid_points))
var_list = seq(0.01, 15, length.out = n_grid_points)
var_grid = t(matrix(rep(var_list, n_grid_points), c(n_grid_points,n_grid_points)))


tot_likelihood = 1
for (i in 1:length(vals)){
  i_val = vals[i]
  i_likelihood = pnorm(i_val+0.5, mu_grid, sqrt(var_grid)) - pnorm(i_val-0.5, mu_grid, sqrt(var_grid))
  tot_likelihood = tot_likelihood * i_likelihood
}
posterior = tot_likelihood / var_grid
library(plot3D)
surf3D(mu_grid, var_grid, posterior)

# generate data points with metropolis hasting mcmc

posterior_func = function(mu, variance){
  tot_likelihood = 1
  for (i in 1:length(vals)){
    i_val = vals[i]
    i_likelihood = pnorm(i_val+0.5, mu, sqrt(variance)) - pnorm(i_val-0.5, mu, sqrt(variance))
    tot_likelihood = tot_likelihood * i_likelihood
  }
  posterior = tot_likelihood / variance
}

nbr_samples = 10000
gen_sample = t(matrix(rep(c(11,2),nbr_samples), c(2, nbr_samples)))

for (i in 2:nbr_samples){
  prev_mu = gen_sample[i-1, 1]
  prev_var = gen_sample[i-1, 2]
  prop_mu = rnorm(1, prev_mu, 0.5)
  prop_var = abs(rnorm(1, prev_var, 0.5))
  
  accept = posterior_func(prop_mu, prop_var)/posterior_func(prev_mu, prev_var)
  u = runif(1)
  if (u < accept){
    gen_sample[i,1] = prop_mu
    gen_sample[i,2] = prop_var
  } else {
    gen_sample[i,1] = prev_mu
    gen_sample[i,2] = prev_var
  }
}
plot(gen_sample[,1], gen_sample[,2])


#### 4.7
# a
vals = c(12.2,.9,.8,5.3,2,1.2,1.2,1,.3,1.8,3.1,2.8)
gamma.sampling.post=function(theta,y){
  sum(dgamma(y,shape=theta[1],scale=theta[2],log=TRUE))
}
mycontour(gamma.sampling.post, c(0, 4, 0.0001, 30), vals, xlab="alpha",ylab="lambda")

data_sim = simcontour(gamma.sampling.post, c(0, 4, 0.0001, 30), vals, 1000)
x_sim = data_sim$x
y_sim = data_sim$y
points(x_sim, y_sim)

muu = x_sim*y_sim
quantile(muu, c(0.05, 0.95))

# b
gamma.sampling.post_rate=function(theta,y){
  sum(dgamma(y,shape=theta[1],rate=theta[2],log=TRUE)) - 2*log(theta[2])
}
mycontour(gamma.sampling.post_rate, c(0, 4, 0.001, 2), vals, xlab="alpha",ylab="beta")
data_sim = simcontour(gamma.sampling.post_rate, c(0, 4, 0.001, 1), vals, 1000)
x_sim = data_sim$x
y_sim = data_sim$y
points(x_sim, y_sim)

muu = x_sim/y_sim
quantile(muu, c(0.05, 0.95))

## c
gamma.sampling.post_mu=function(theta,y){
  sum(dgamma(y,shape=theta[1],scale=theta[2]/theta[1],log=TRUE)) + log((theta[1] + theta[2])/theta[1]^2)
}
mycontour(gamma.sampling.post_mu, c(0.0001, 4, 0.0001, 15), vals, xlab="alpha",ylab="mu")
data_sim = simcontour(gamma.sampling.post_mu, c(0.0001, 4, 0.0001, 15), vals, 1000)
x_sim = data_sim$x
y_sim = data_sim$y
points(x_sim, y_sim)

muu = y_sim
quantile(muu, c(0.05, 0.95))





