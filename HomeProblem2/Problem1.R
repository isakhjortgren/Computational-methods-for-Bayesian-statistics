power <- function(v){
  p = rep(0, length(v))
  p[4<v] = cos(v[4<v]*pi/11 + 7*pi/11) + 1
  p[15<v] = 7/4 + v[15<v]/30 - (v[15<v]^2)/900
  p[25<v] = 0
  return(p)
}
v = seq(0,30,length.out = 1001)
p = power(v)
plot(v,p, 'l')
lines(v, 10*dgamma(v, shape=alpha, rate=beta))
plot(v, dgamma(v, shape=alpha, rate=beta) * p, 'l')
lines(v, dgamma(v-4, 4, 0.35))
# a
alpha = 1.08
beta = 1/10.5
prob_p_greater_0 = pgamma(25, shape=alpha, rate=beta)- pgamma(4, shape=alpha, rate=beta)

# b
N = 100000
v_sample = rgamma(N, shape=alpha, rate=beta)
p_sample = power(v_sample)
mean_p_sample =mean(p_sample)
std_p_sample = sd(p_sample)
approx_95_conf_int = mean_p_sample + 1.96*std_p_sample/sqrt(N)*c(-1,1)
mean_p_sample
approx_95_conf_int
hist(p_sample, breaks=40)

# c
# indicator f=gamma(4,0.35)
# integral ~= mean(  p*v/f  )
indicator_sample = runif(N, 4, 25)
p_indi = power(indicator_sample)
f_indi = dgamma(indicator_sample, shape=alpha, rate=beta)
g_indi = dunif(indicator_sample, 4, 25)
power_sample = p_indi * f_indi / g_indi
imp_sample_mean = mean(power_sample)
imp_sample_std = sd(power_sample)
imp_sample_approx_95_conf_int = imp_sample_mean + 1.96*imp_sample_std/sqrt(N)*c(-1,1)
imp_sample_mean
imp_sample_approx_95_conf_int

# var = E(x^2)-E(x)^2
power_sample_pow2 = p_indi^2 * f_indi / g_indi
mean_power_sample_pow2 = mean(power_sample_pow2)
imp_sample_variance = mean_power_sample_pow2 - imp_sample_mean^2
  
  
  
  