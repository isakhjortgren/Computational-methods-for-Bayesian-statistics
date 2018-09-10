

MixtureDistribution <- function(theta_values, expectations, variances, weights){
  nbr_mixture_dists = length(expectations)
  mixture_density = rep(0, length(theta_values))
  
  for (i in 1:nbr_mixture_dists){
    i_mu = expectations[i]
    i_var = variances[i]
    i_weight = weights[i]
    mixture_density = mixture_density + i_weight * dnorm(theta_values, i_mu, i_var)
  }
  return(mixture_density)
}

mu_vals = c(1.5157, 1.52, 1.52)
var_vals = c(0.001, 0.0015, 0.01)
weights = c(0.32, 0.58, 0.1)
theta = seq(1.51, 1.53, length=100)
md_arr = MixtureDistribution(theta, mu_vals, var_vals, weights)

plot(theta, md_arr, 'l')
