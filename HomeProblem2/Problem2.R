death.notices = seq(0,9)
death.count = c(162, 267, 271,185, 111, 61, 27, 8, 3, 1)
death.total_counts = sum(death.count)

lambda = sum(death.count * death.notices) / sum(death.count)

death.expected = sum(death.count)*dpois(death.notices, lambda)

plot(death.notices, death.expected, col='red', ylab = 'counts', xlab='notices')
points(death.notices, death.count)


plot(death.notices, log10(death.expected), col='red', ylab = 'log10(counts)', xlab='notices')
points(death.notices, log10(death.count))


## sample from dist with gibbs sampling


p = 0.5 # initial guess
y = c(162,267,271,185,111,61,27,8,3,1)
z=round(p*y)
lambda1 = 2.15
lambda2 = 2.15
N = 100000
values = t(matrix(rep(c(z,p,lambda1, lambda2),N),c(13,N)))

for (i in 2:N){
  #propose new p
  p = rbeta(1, 1+sum(z), 1+sum(y-z))
  #propose new lambda1
  j=0:9
  lambda1 = rgamma(1, 1+sum(j*z), 1+sum(z))
  #prop new lambda2
  lambda2 = rgamma(1, 1+sum(j*(y-z)), 1+sum(y-z))
  #prop new z0,z1,...,z9
  q1 = lambda1^j*exp(-lambda1)*p
  q2 = lambda2^j*exp(-lambda2)*(1-p)
  q = q1/(q1+q2)
  z = rbinom(10,y,q)
  
  values[i,1:10] = z
  values[i,11] = p
  values[i,12] = lambda1
  values[i,13] = lambda2
}
p_mean = mean(values[,11])
lambda1_mean = mean(values[,12])
lambda2_mean = mean(values[,13])

hist(values[,11],xlab = 'p')
hist(values[,12],xlab = 'lambda 1')
hist(values[,13],xlab = 'lambda 2')


# 2.g
nbr_from_l1_dist = rbinom(1, sum(y), p_mean)
nbr_from_l2_dist = sum(y)-nbr_from_l1_dist
sample_1 = rpois(nbr_from_l1_dist, lambda1_mean)
sample_2 = rpois(nbr_from_l2_dist, lambda2_mean)
total_new_sample = c(sample_1, sample_2)
ns_counts = as.vector(table(total_new_sample))
plot(0:9, ns_counts, col='red')
points(0:9, y)

plot(0:9, log10(ns_counts), col='red', ylab = 'log10(counts)', xlab='notices')
points(0:9, log10(y))
