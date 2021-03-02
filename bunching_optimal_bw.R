
bunching_optimal_bw = function(y, x, bunch_point, n_bins) {
  # DESCRIPTION:
  # takes bunching data and computes optimal bunching window 
  # using algorithm described in Bosch, Dekker, and Strohmaier (2020)
  
  # ARGUMENTS
  # y: number of bunchers
  # x: bunching bins
  # bunch_point: the point at which people bunch
  # n_bins: number of bunching bins
  
  # fixed parameters
  max_bunch = 2
  
  # loop over bin combinations
  n_below = floor((n_bins)/10) # number of bins below
  n_above = ceiling((n_bins)/10) # number of bins above
  bin_mid = ceiling(n_bins/2)
  run = 1
  
  # initialize bw distributions
  results = as.data.frame(matrix(0, nrow=(n_above+1)*(n_below+1), ncol=4))
  colnames(results) = c("left_bw", "right_bw", "left_bw_opt", "right_bw_opt")
  
  # loop over bin combinations
  for (i in 0:n_below) {
    for (j in 0:n_above) {
      # subset data
      y_temp = data.matrix(y[-c((bin_mid-i):(bin_mid+j)),])
      x_temp = data.matrix(x[-c((bin_mid-i):(bin_mid+j)),])
      
      # run regression
      beta = solve(t(x_temp) %*% x_temp) %*% t(x_temp) %*% y_temp
      
      # predicted y-values
      y_hat = x_temp %*% beta
      
      # se of estimates
      r = y_temp - y_hat
      se = as.numeric(sqrt((t(r) %*% r) / (nrow(x_temp) - ncol(x_temp))))
      
      # confidence intervals (on regression support)
      ci_low = y_hat + se * qt(0.05, nrow(x_temp) - ncol(x_temp))
      ci_high = y_hat - se * qt(0.05, nrow(x_temp) - ncol(x_temp))
      
      # excess observations per bin
      excess_obs = as.data.frame(y_temp - ci_high)
      
      # find bunch point
      excess_obs$row = as.numeric(rownames(excess_obs))
      excess_obs$diff = append(NaN, diff(excess_obs$row))
      max = max(na.omit(excess_obs$diff))
      
      # bw selection setup
      above_start = excess_obs$row[excess_obs$diff == max][2]
      below_start = above_start - max
      count_l = 0
      count_r = 0
      
      # bw_l selection
      a = 0
      k = 0
      while (a == 0) {
        if (excess_obs$V1[excess_obs$row == below_start - k] > 0) {
          count_l = count_l + 1
          k = k + 1
        } else {
          a = 1
        }
      }
      
      # bw_r selection
      a = 0
      k = 0
      while (a == 0) {
        if (excess_obs$V1[excess_obs$row == above_start + k] > 0) {
          count_r = count_r + 1
          k = k + 1
        } else {
          a = 1
        }
      }
      
      # store results
      results$left_bw[run] = i
      results$right_bw[run] = j
      results$left_bw_opt[run] = count_l
      results$right_bw_opt[run] = count_r
      
      # iterate
      run = run+1
      
    }
  }
  
  # replace optimal right bw with NA if false positive (no bunching)
  results = results[with(results, order(left_bw, right_bw)), ]
  for (i in 1:(nrow(results)-1)) {
    if ((results$right_bw_opt[i] == 0) & (results$left_bw[i] == results$left_bw[i+1])) {results$right_bw_opt[i+1] = 0}
  }
  
  # replace optimal left bw with NA if false positive (no bunching)
  results = results[with(results, order(right_bw, left_bw)), ]
  for (i in 1:(nrow(results)-1)) {
    if ((results$left_bw_opt[i] == 0) & (results$right_bw[i] == results$right_bw[i+1])) {results$left_bw[i+1] = 0}
  }
  
  # compute bws
  results$left_bw_opt[results$left_bw_opt == 0] = NA
  results$right_bw_opt[results$right_bw_opt == 0] = NA
  results$left_bw_opt = results$left_bw_opt + results$left_bw
  results$right_bw_opt = results$right_bw_opt + results$right_bw
  
  # optimal bw_l
  if (length(na.omit(results$left_bw_opt)) < 11) {
    bw_l = 0
  } else {bw_l = median(na.omit(results$left_bw_opt))}
  
  # optimal bw_r
  if (length(na.omit(results$right_bw_opt)) < 11) {
    bw_r = 0
  } else {bw_r = median(na.omit(results$right_bw_opt))}
  
  # compute optimal beta
  y_temp = data.matrix(y[-c((bin_mid-bw_l):(bin_mid+bw_r)),])
  x_temp = data.matrix(x[-c((bin_mid-bw_l):(bin_mid+bw_r)),])
  beta = solve(t(x_temp) %*% x_temp) %*% t(x_temp) %*% y_temp
  
  # confidence intervals (on full support)
  mu = seq(bunch_point - max_bunch, bunch_point + max_bunch, by = 0.1)
  mu = cbind(1, mu, mu^2)
  y_hat_all = data.matrix(mu) %*% beta
  ci_low_all = y_hat_all + se * qt(0.025, nrow(x_temp) - ncol(x_temp))
  ci_high_all = y_hat_all - se * qt(0.025, nrow(x_temp) - ncol(x_temp))
  
  # coerce to data frame
  mu = as.data.frame(mu)
  results = list("bin_mid"=bin_mid, "bw_l"=bw_l, "bw_r"=bw_r, "mu"=mu, "y_hat_all"=y_hat_all, "ci_low_all"=ci_low_all, "ci_high_all"=ci_high_all)
  
  return(results)
  
}

