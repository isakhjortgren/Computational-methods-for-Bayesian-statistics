p = 0.5 # initial guess
y = c(162,267,271,185,111,61,27,8,3,1)
z=round(p*y)
lambda1 = 1
lambda2 = 3

N = 1000
j=0:9

values = t(matrix(rep(c(p, lambda1, lambda2), N), c(3,N)))
for (i in 2:N){
  q1 = lambda1^j*exp(-lambda1)*p
  q2 = lambda2^j*exp(-lambda2)*(1-p)
  q = q1/(q1+q2)
  p_new = sum(y*q)/sum(y)
  lambda1_new = sum(y*q*j)/(1+sum(y*q))
  lambda2_new = sum(y*(1-q)*j)/(1+sum(y*(1-q)))
  values[i, 1] = p_new
  values[i, 2] = lambda1_new
  values[i, 3] = lambda2_new
  p = p_new
  lambda1 = lambda1_new
  lambda2 = lambda2_new
}
p
lambda1
lambda2


## density plots
lambda_hat = 2.1569
one_model_exp = sum(y)*dpois(j, lambda_hat)
plot(j, log10(one_model_exp), ylab = 'log10(counts)', xlab = 'notices')

EM_two_model = sum(y)*(p*dpois(j, lambda1) + (1-p)*dpois(j, lambda2))
points(j, log10(EM_two_model), col='red')

points(j, log10(y), col='green')
legend('topright',c('1 parameter model', '2 parameter model', 
                    'observed counts'), 
       pch=c(1,1,1), col = c('black', 'red', 'green'))




