# A5.1 b
post_fnc = function(theta){
  exp(5*theta)*exp(-2*theta^2)/(1+exp(theta))^5
}

theta_arr = seq(-2,3,length.out = 10001)
plot(theta_arr, post_fnc(theta_arr), 'l')
M = 0.0715
proposal = M*dnorm(theta_arr, 0.478, 0.5)
lines(theta_arr, proposal, col='red')
sum(post_fnc(theta_arr) > proposal)

N=100000
samples = rep(0,N)
for (i in 1:N){
  accept = FALSE
  while(!accept){
    proposal = rnorm(1, 0.478, 0.5)
    u = runif(1)
    accept = u*M*dnorm(proposal, 0.478, 0.5) < post_fnc(proposal)
  }
  samples[i] = proposal
}

sum(samples>0)/N

# c
m = 100000
proposals = rnorm(m, 0.478, 0.5)
weights = post_fnc(proposals) / dnorm(proposals, 0.478, 0.5)
p = weights / sum(weights)
N = 10000
samples = sample(proposals, size=m, replace = TRUE, prob=p)
sum(samples>0)/m




#################################  
# A5.4

log_post = function(beta0, beta1, y){
  i = 1:length(y)
  sum( y*(beta0 + beta1*i) - exp(beta0 + beta1*i) )
}
y = c(15,11,14,17,5,11,10,4,8,10,7,9,11,3,6,1,1,4)





