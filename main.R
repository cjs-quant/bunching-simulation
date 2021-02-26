
rm(list = ls())

# call libraries
library(data.table)

# source helper functions
source("/Users/ChristopherSimard/Desktop/Economics/Simulations/bunching_simulation/bunching_data_gen.R")
source("/Users/ChristopherSimard/Desktop/Economics/Simulations/bunching_simulation/bunching_bins.R")
source("/Users/ChristopherSimard/Desktop/Economics/Simulations/bunching_simulation/bunching_optimal_bw.R")
source("/Users/ChristopherSimard/Desktop/Economics/Simulations/bunching_simulation/bunching_estimation.R")

# bunching parameters
bunch_point = 20
n_bunch = 500
variance_bunch = 0.2
t_below = 0.20
t_above = 0.25

# bin parameters
n_bins = 100
max_bunch = 2

# draw income distribution with bunching
x_set = bunching_data_gen(bunch_point, n_bunch, variance_bunch)

# plot as density line w/o bunching
plot(density(x_set$x), col="red")
  abline(v = bunch_point, col="black", lwd=1)

# create bunching regression data
data = bunching_bins(max_bunch, n_bins)
y = data$y
x = data$x

# compute optimal bunching window
results = bunching_optimal_bw(y, x, n_bins)
bin_mid = results$bin_mid
bw_l = results$bw_l
bw_r = results$bw_r
mu = results$mu
y_hat_all = results$y_hat_all
ci_low_all = results$ci_low_all
ci_high_all = results$ci_high_all

# plot bunching data
plot(x$x_1[-c((bin_mid-bw_l):(bin_mid+bw_r))], y$y[-c((bin_mid-bw_l):(bin_mid+bw_r))], pch=16, xlim=range(x$x_1), ylim = range(y$y))
  points(x$x_1[c((bin_mid-bw_l):(bin_mid+bw_r))], y$y[c((bin_mid-bw_l):(bin_mid+bw_r))], pch=16, col="red")
  lines(mu$mu, y_hat_all)
  lines(mu$mu, ci_low_all, col="gray")
  lines(mu$mu, ci_high_all, col="gray")
  abline(v = bunch_point, col="red")
  abline(v = max(x$x_1[c((bin_mid-bw_l):(bin_mid+bw_r))]), lty=2, col="black")
  abline(v = min(x$x_1[c((bin_mid-bw_l):(bin_mid+bw_r))]), lty=2, col="black")

# compute bunching estimator
estimates = bunching_estimation(y, x, bin_mid, bw_l, bw_r, bunch_point)
bf = estimates$bf
ef = estimates$ef

