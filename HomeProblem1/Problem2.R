MixtureDistribution <- function(theta_values, expectations, variances, 
                                weights){
  nbr_mixture_dists = length(expectations)
  mixture_density = 0
  stds = sqrt(variances)
  for (i in 1:nbr_mixture_dists){
    i_mu = expectations[i]
    i_std = stds[i]
    i_weight = weights[i]
    mixture_density = mixture_density + i_weight * dnorm(theta_values, 
                                                         i_mu, i_std)
  }
  return(mixture_density)
}

mu_vals = c(1.5157, 1.52, 1.52)
var_vals = c(0.001, 0.0015, 0.01)^2
stds = sqrt(var_vals)
weights = c(0.32, 0.58, 0.1)
theta = seq(1.500, 1.54, length=1000)
md_arr = MixtureDistribution(theta, mu_vals, var_vals, weights)  # prior

plot(theta, md_arr, 'l', xlab='theta', ylab = 'probability density')

##### b prior predictive #####
x_arr = theta
dtheta = theta[2]-theta[1]
prior_pred = rep(0, length(x_arr))
std_likelihood = 0.001
for (i in 1:length(x_arr)){
  x_i = x_arr[i]
  i_likelihood =  dnorm(x_i, theta, std_likelihood)
  prior_pred[i] = dtheta*sum(i_likelihood*md_arr)
}
lines(x_arr, prior_pred, col='red')


prior_pred_analytical = MixtureDistribution(x_arr, expectations = mu_vals, variances = i_std^2 + std_likelihood^2)
lines(x_arr, prior_pred_analytical, lty=1, 'l', xlab='theta', ylab = 'prior predictive probability density')


#### c posterior given x = 1.52131 ######
x = 1.52131
posterior = md_arr * dnorm(x, theta, std_likelihood)
posterior = posterior/(sum(posterior)*dtheta)
plot(theta, posterior, 'l', lty=2)
lines( x_arr, prior_pred, 'l', col='red')
lines(theta, md_arr, 'l', col='green')

sum(posterior*dtheta)

i_start = which(posterior==max(posterior))
i_diff = 0
acc_post_prob = 0
while (acc_post_prob < 0.95){
  i_diff = i_diff + 1
  acc_post_prob = dtheta*sum(posterior[(i_start-i_diff):(i_start+i_diff)])
}
cred_interval = c(x_arr[i_start-i_diff], x_arr[i_start+i_diff])  

#plot analythical expr:
post_analytical = 0
prior_pred_x = 0
list_std_upd = rep(0, 3)
list_mu_upd = rep(0, 3)
list_weight_upd = rep(0, 3)
for (i in 1:length(mu_vals)){
  i_mu = mu_vals[i]
  i_std = sqrt(var_vals[i])
  i_weight = weights[i]
  i_std_upd = (1/i_std^2 + 1/std_likelihood^2)^(-1/2)
  list_std_upd[i] = i_std_upd
  i_mu_upd = i_std_upd^2 * (i_mu/i_std^2 + x/std_likelihood^2)
  list_mu_upd[i] = i_mu_upd
  
  prior_pred_x_i = i_weight*dnorm(x, i_mu, sqrt(i_std^2 + std_likelihood^2))
  list_weight_upd[i] = prior_pred_x_i
  prior_pred_x = prior_pred_x + prior_pred_x_i
  #post_analytical = post_analytical +  prior_pred_x_i * dnorm(theta, i_mu_upd, i_std_upd)
}
list_weight_upd = list_weight_upd/prior_pred_x
#post_analytical = post_analytical/prior_pred_x
post_analytical = MixtureDistribution(theta, list_mu_upd, list_std_upd^2, list_weight_upd)
lines(theta, post_analytical, 'l', ylab='Posterior density funtion')



#### d posterior predictive


post_pred = rep(0, length(x_arr))
for (i in 1:length(x_arr)){
  x_new = x_arr[i]
  post_pred[i] = dtheta * sum(dnorm(x_new, theta, std_likelihood)*posterior)
}
lines(x_arr, post_pred, lty=2)

# post_pred_analythical
post_pred_analytical = 0
prior_pred_x = 0
gamma_p_i_list = c(1:3)
for (i in 1:length(mu_vals)){
  i_mu = mu_vals[i]
  i_std = sqrt(var_vals[i])
  i_weight = weights[i]
  i_std_upd = (1/i_std^2 + 1/std_likelihood^2)^(-1/2)
  i_mu_upd = i_std_upd^2 * (i_mu/i_std^2 + x/std_likelihood^2)
  
  gamma_p_i = i_weight * dnorm(x, i_mu, sqrt(i_std^2 + std_likelihood^2))
  gamma_p_i_list[i] = gamma_p_i
  prior_pred_x = prior_pred_x + i_weight * dnorm(x, i_mu, sqrt(i_std^2 + std_likelihood^2))
  
  post_pred_analytical = post_pred_analytical + gamma_p_i * dnorm(x_arr, i_mu_upd, sqrt(i_std_upd^2 + std_likelihood^2))
}
post_pred_analytical = post_pred_analytical/prior_pred_x

plot(x_arr, post_pred_analytical, 'l', lty = 1, col='blue', xlab='x new', ylab='posterior predictive density')

##### e likelihood ratio

LR = post_pred_analytical/prior_pred_analytical
plot(x_arr, LR,'l',xlab='x c', ylab='Likelihood ratio')

#### f 
# if the LR is large it is more probable that the new sample is from the same piece of glass than if it would have been from
# an independent source.







