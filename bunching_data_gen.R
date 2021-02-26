
bunching_data_gen = function(bunch_point, n_bunch, variance_bunch) {
  # DESCRIPTION:
  # generates an income distribution with fixed parameters and
  # then generates a bunching income distribution about the
  # bunching point with n, mean, and variance selected by the user
  
  # ARGUMENTS (fixed):
  # n: number of incomes drawn along smooth distribution
  # mean: mean of smooth distribution
  # variance: variance of smooth distribution
  # max_bunch: maximum window for bunching
  
  # ARGUMENTS (user-selected):
  # bunch_point: location of bunch point along x axis
  # n_bunch: number of bunching incomes drawn
  # variance_bunch: variance of bunching incomes
  
  # fixed parameters
  n = 100000
  mean = 10
  variance = 0.2
  max_bunch = 2
  
  # log-normal income draws
  x = as.data.frame(rnorm(n, mean, variance))
  colnames(x) = "x"
  x = exp(x)
  x = x/1000
  x = as.data.frame(x[x<80])
  
  # normal bunching draws
  x_bunch = as.data.frame(rnorm(n_bunch, bunch_point, variance_bunch))
  
  # append bunching draws to log-normal draws
  colnames(x_bunch) = "x"
  colnames(x) = "x"
  x_set = rbind(as.data.frame(x), as.data.frame(x_bunch))
  
  return(x_set)
  
}

