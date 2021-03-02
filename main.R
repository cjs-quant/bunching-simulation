
rm(list = ls())

# source helper functions
source("/Users/ChristopherSimard/Desktop/Economics/Simulations/bunching_simulation/bunching_data_gen.R")
source("/Users/ChristopherSimard/Desktop/Economics/Simulations/bunching_simulation/bunching_bins.R")
source("/Users/ChristopherSimard/Desktop/Economics/Simulations/bunching_simulation/bunching_optimal_bw.R")
source("/Users/ChristopherSimard/Desktop/Economics/Simulations/bunching_simulation/bunching_estimation.R")

# bunching parameters
bunch_point = 20
n_bunch = 1000
variance_bunch = 0.15
t_below = 0.20
t_above = 0.25
n_bins = 150
max_bunch = 2

# draw income distribution with bunching
x_set = bunching_data_gen(bunch_point, n_bunch, variance_bunch)

# plot income density
ggplot(x_set) + 
  geom_density(mapping=aes(x), fill="#69b3a2", color="#e9ecef", alpha=0.8) + 
  xlab("Income (in 000s)") + ylab("Density") + 
  geom_vline(xintercept = bunch_point)

# create bunching regression data
data = bunching_bins(x_set, n_bins)
y = data$y
x = data$x
temp=cbind(y,x)

# compute optimal bunching window
results = bunching_optimal_bw(y, x, n_bins)
bin_mid = results$bin_mid
bw_l = results$bw_l
bw_r = results$bw_r
mu = results$mu
y_hat_all = results$y_hat_all
ci_low_all = results$ci_low_all
ci_high_all = results$ci_high_all

ggplot() + 
  geom_point(mapping=aes(temp$x_1, temp$y))

# plot bunching data
plot(x$x_1[-c((bin_mid-bw_l):(bin_mid+bw_r))], y$y[-c((bin_mid-bw_l):(bin_mid+bw_r))], pch=16, xlim=range(x$x_1), ylim = range(y$y))
    points(x$x_1[c((bin_mid-bw_l):(bin_mid+bw_r))], y$y[c((bin_mid-bw_l):(bin_mid+bw_r))], pch=16, col=ifelse(((bw_l == 0) & (bw_r == 0)), "black", "red"))
  lines(mu$mu, y_hat_all)
  lines(mu$mu, ci_low_all, col="gray")
  lines(mu$mu, ci_high_all, col="gray")
  abline(v = bunch_point, col="red")
  if (bw_r > 0) {abline(v = max(x$x_1[c((bin_mid-bw_l):(bin_mid+bw_r))]), lty=2, col="black")}
  if (bw_l > 0) {abline(v = min(x$x_1[c((bin_mid-bw_l):(bin_mid+bw_r))]), lty=2, col="black")}

# compute bunching estimator
estimates = bunching_estimation(y, x, bin_mid, bw_l, bw_r, bunch_point)
bf = estimates$bf
ef = estimates$ef

