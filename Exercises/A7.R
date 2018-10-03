# A7.1

log_post = function(theta1, theta2, lambda){
  n = length(lambda)
  first_part = theta1 + theta2 -2*(exp(theta1) + exp(theta2))
  sec_part = n*theta2*exp(theta1) - n*lgamma(exp(theta1))
  sum_part1 = (exp(theta1) -1) * sum(log(lambda))
  sum_part2 = - exp(theta2)*sum(lambda)
  
  return(first_part + sec_part + sum_part1 + sum_part2)
}
