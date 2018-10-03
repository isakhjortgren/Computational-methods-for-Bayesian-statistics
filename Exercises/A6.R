### 6.2
post_fnc = function(theta){
  exp(5*theta)*exp(-2*theta^2)/(1+exp(theta))^5
}

N = 10000
s = sqrt(0.193)
samples = rep(0.5, N)
for(i in 2:N){
  proposal = rnorm(1, samples[i-1] , 2*s)
  accept_ratio = post_fnc(proposal) / post_fnc(samples[i-1])
  u = runif(1)
  if(u < accept_ratio){
    samples[i] = proposal
  } else {
    samples[i] = samples[i-1]
  }
}
mean(samples)
sd(samples)
sum(samples>0)/N


## 6.4
library(LearnBayes)
library(MASS)
data = c(36,13,23,6,20,12,23,93,98,91,89,100,90,95,90,87)
theta = c(90,2.5)
post_obj = laplace(cauchyerrorpost, theta, data)

N = 10000
s = sqrt(0.193)
samples = t(matrix(rep(theta, N), c(2,N)))
var_matrix = matrix(c(0.02, 0, 0, 0.01), c(2,2))
for(i in 2:N){
  proposal = mvrnorm(1, samples[i-1,], var_matrix)
  accept_ratio = cauchyerrorpost(proposal, data) / cauchyerrorpost(samples[i-1,], data)
  u = runif(1)
  if(u < accept_ratio){
    samples[i,] = proposal
  } else {
    samples[i,] = samples[i-1,]
  }
}
mean_mu = mean(samples[,1])
mean_logS = mean(samples[,2])
