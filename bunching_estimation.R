
bunching_estimation = function(y, x, bin_mid, bw_l, bw_r, bunch_point) {
  # DESCRIPTION: 
  # computes bunching estimator and income elasticity 
  # from Chetty (2009) given optimal bandwidths (bw_l, bw_r) 
  # from bunching_optimal_bw.R
  
  # ARGUMENTS:
  # y: counts per bin for regression
  # x: bin mids for regression
  # bin_mid: row index of omitted bin
  # bw_l: number of omitted bins to the left of bin_mid
  # bw_r: number of omitted bins to the right of bin_mid
  # bunch_point: income level of kink
  
  # dummy variables for bunching estimation
  dummies = as.data.frame(matrix(0, nrow(x), (bin_mid+bw_r)-(bin_mid-bw_l) + 1))
  colnames(dummies) = paste("g", (bin_mid-bw_l):(bin_mid+bw_r), sep = "_")
  for (i in 0:(length(colnames(dummies))-1)) {
    dummies[bin_mid-bw_l+i,i+1] = 1
  }
  x = cbind(x, dummies)
  
  # run bunching regression
  x_temp = data.matrix(x)
  y_temp = data.matrix(y)
  beta = solve(t(x_temp) %*% x_temp) %*% t(x_temp) %*% y_temp
  
  # excess bunchers around kink
  bn = sum(beta[4:nrow(beta),])
  
  # counterfactual bunchers around kink
  x_within = x[c((bin_mid-bw_l):(bin_mid+bw_r)),]
  C_within = data.matrix(x_within[,1:3]) %*% data.matrix(beta[1:3,])
  C = sum(C_within)
  
  # bunching estimator
  bf = bn/(C - bn)
  
  # income elasticity
  ef = bf/(bunch_point*log((1 - t_above)/(1 - t_below)))
  estimates = cbind(bf, ef)
  colnames(estimates) = c("Excess Bunching", "Income Elasticity")
  
  return(estimates)
  
}

