### 2.11
n_samples = 1000
sample_size = 25
prob = 0.2
X = rbinom(n_samples, sample_size, prob)
hist(X)
possible_x_vals = seq(0, sample_size)
real_dist = n_samples*dbinom(x = possible_x_vals, size=sample_size, prob=prob)
lines(possible_x_vals, real_dist)





#### 2.22
# b
Nsim=10^4
mu = 0
sigma = 1
a = 1
nbr_sims = 0
X=rep(0,Nsim)
for (i in 1:Nsim){
  z=rnorm(1,mean=mu,sd=sigma)
  while(z<a){ 
    z=rnorm(1,mean=mu,sd=sigma) 
    nbr_sims = nbr_sims + 1
  }
  X[i]=z
}

acceptance_ratio = nbr_sims / Nsim
acceptance_ratio_theo = 1/pnorm(-a)

# c
x_range = seq(-0,5,length.out = 1000)
trunc_pdf = dnorm(x_range)/pnorm(-a)
trunc_pdf[x_range < a] = 0
M=7
mu_bar = 0
MPDF = M*dnorm(x_range, mean=mu_bar)
plot(x_range, MPDF, 'l')
lines(x_range, trunc_pdf)
 
# d

dTruncNorm <- function(x, a){
  density = dnorm(x)
  density[x < a] = 0
  density = density / pnorm(-a)
  return(density)
}

AcceptRejectExp <- function(nbr_samples, M, a, alpha){
  X = rep(0, nbr_samples)
  nbr_trials = 0
  for (i in 1:nbr_samples){
    prop_found = FALSE
    while (!prop_found){
      nbr_trials = nbr_trials + 1
      prop_x = a + rexp(1, rate=alpha)
      u = runif(1)
      if (u*M*dexp(prop_x-a, rate=alpha) < dTruncNorm(prop_x, a)) {
        X[i] = prop_x
        prop_found = TRUE
      }
    }
  }
  print(nbr_trials/nbr_samples)
  return(X)
}

a = 2
alpha = a
M = 1.2
X = AcceptRejectExp(1000, M, a, alpha)
hist(X)
plot(x_range, M*dexp(x_range-a, alpha), 'l', col='red')
lines(x_range, dTruncNorm(x_range, a), 'l')


### 2.18

x = seq(-5,5,length.out = 1000)
UnknownDensity <- function(x){
  exp(-x^2/2)*(sin(6*x)^2 + 3*cos(x)^2*sin(4*x)^2+1)
}
M = 15
g =M* exp(-x^2/2)/sqrt(2*pi)
plot(x,g, 'l')
f = UnknownDensity(x)
lines(x,f, col='red')
# find best M
min_M_found = FALSE
dM = 0.001
while(!min_M_found){
  M = M -dM
  g = M* exp(-x^2/2)/ sqrt(2*pi)
  min_M_found = sum(f>g) > 0
}
M = M + dM


AcceptReject <- function(nbr_samples, M){
  X = rep(0, nbr_samples)
  nbr_trials = 0
  for (i in 1:nbr_samples){
    prop_found = FALSE
    while (!prop_found){
      nbr_trials = nbr_trials + 1
      prop_x = rnorm(1)
      u = runif(1)
      if (u*M*dnorm(prop_x) < UnknownDensity(prop_x)) {
        X[i] = prop_x
        prop_found = TRUE
      }
    }
  }
  print(nbr_trials/nbr_samples)
  return(X)
}


X = AcceptReject(2500, M)
hist(X, breaks = 50, freq=FALSE)
lines(x, 1.8392*f/M)


