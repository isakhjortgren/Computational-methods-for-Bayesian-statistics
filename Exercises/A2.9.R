# A2.9:1
library(LearnBayes)
p = seq(0, 1, length=9)
prior = c(0.001, 0.001, 0.95, 0.008, 0.008, 0.008, 0.008, 0.008, 0.008)
data = c(6,4)
post = pdisc(p, prior, data)
post[3]  # prob of no esp

#####################
# A2.9:4
p = seq(0,1,length=11)
prior_joe = c(0, 0.5, 0.2, 0.2, 0.05, 0.05, 0, 0, 0, 0, 0)
mean_joe = sum(p*prior_joe)
var_joe = sum(prior_joe*(p-mean_joe)^2)
std_joe = sqrt(var_joe)

alpha = 3
beta = 12
mean_sam = alpha/(alpha+beta)
var_sam = alpha*beta/((alpha+beta)^2*(alpha+beta+1))
std_sam = sqrt(var_sam)

#a) similar mean and variance

s = seq(0,12)
post_joe = pdiscp(p, prior_joe, 12, s)
plot(s, post_joe,  'l')
post_sam = pbetap(c(alpha, beta), 12, s)
lines(s, post_sam)
#####################