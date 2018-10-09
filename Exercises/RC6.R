# 6.7 b)

truncDBeta = function(x, alpha, beta, c, d){
  densReal = dbeta(x, alpha, beta)
  densReal[x<c] = 0
  densReal[x>d] = 0
  return(densReal)
}
x = seq(0,1, length.out=1001)
c = 0.5
d = 0.7
densReal = truncDBeta(x, 2.7, 6.3, c, d)
plot(x, densReal,'l')

N = 100000
sample1 = rep(0.6, N)
nbr_accept = 0
for (i in 2:N){
  #prop_x = rbeta(1, 2, 6)
  prop_x = runif(1, c, d)
  u = runif(1)
  accept_ratio = truncDBeta(prop_x, 2.7, 6.3, c, d) / truncDBeta(sample1[i-1], 2.7, 6.3, c, d)
  if (accept_ratio > u){
    nbr_accept = nbr_accept + 1
    sample1[i] = prop_x
  } else {
    sample1[i] = sample1[i-1]
  }
}
nbr_accept/N

