###### 3.9:3
dtheta = 0.1
theta = seq(-2, 12, by=dtheta)
data = c(0, 10, 9, 8, 11, 3, 3, 8, 8, 11)
likelihood = 1
for (i in 1:length(data)){
  y_i = data[i]
  likelihood = likelihood * 1/(1+(y_i-theta)^2)
}
likelihood = likelihood /(sum(likelihood) * dtheta)

plot(theta, likelihood, 'l')
# sharp peak around 8-9, it seems like the low measurments is ignored
post_mean = sum(likelihood*theta)*dtheta
post_var = sum(likelihood*(theta-post_mean)^2)*dtheta
post_std = sqrt(post_var)


###### 3.9:3
B = 200
N_arr = seq(1,B)
post = 1/N_arr^5
post[1:99] = 0
post = post/sum(post)
plot(N_arr, post,'l')
post_mean = sum(N_arr*post)
post_var = sum(post*(N_arr-post_mean)^2)
post_std = sqrt(post_var)
prob_greater_than_150 = sum(post[150:200])


###### 3.9:4
N = 1000
P1_sample = rbeta(N, 100, 100)
p1_quantile = quantile(P1_sample, c(0.05, 0.95))
P2_sample = 0.9*rbeta(N, 500, 500) + 0.1*rbeta(N, 1, 1)
p2_quantile = quantile(P2_sample, c(0.05, 0.95))

#b 
#P1:
n = 100
s = 30
f = 100-s
P1_post_sample = rbeta(N, 100+s, 100+f)
p1_post_quantile = quantile(P1_post_sample, c(0.05, 0.95))

#P2:

probs = c(.9, .1)
beta.par1 = c(500,500)
beta.par2 = c(1,1)
betapar = rbind(beta.par1, beta.par2)
data = c(s, f)
post = binomial.beta.mix(probs, betapar, data)
P2_post_sample = post$probs[1]*rbeta(N, post$betapar[1,1], post$betapar[1,2]) + post$probs[2]*rbeta(N, post$betapar[2,1], post$betapar[2,2])
p2_post_quantile = quantile(P2_post_sample, c(0.05, 0.95))
