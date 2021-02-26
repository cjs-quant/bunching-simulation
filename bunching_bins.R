
bunching_bins = function(max_bunch, n_bins) {
  # DESCRIPTION:
  # generates n bins along support containing the bunch point 
  # defined by the user, computes the number of observations 
  # within each bin, and coerces into a regressible format
  
  # ARGUMENTS:
  # max_bunch: the maximum window for bunching
  # n_bins: the number of bins within bunching window
  
  # compute bins within max bunch range
  x_set = as.data.frame(x_set$x[bunch_point+max_bunch>x_set$x & bunch_point-max_bunch<x_set$x])
  colnames(x_set) = "x"
  hist = hist(x_set$x, seq(min(x_set$x),max(x_set$x),l=(n_bins+1)), plot=FALSE)
  
  # counts of x per bin
  data = as.data.frame(hist$counts)
  data$x = hist$mids
  colnames(data) = c("y", "x")
  rm(x_set)
  
  # creates data for regression
  order = 2
  for (i in 1:order) {
    data[paste("x",toString(i),sep ="_")] = data$x^(i)
  }
  
  # creates datasets for regression
  y = as.data.frame(data$y)
  colnames(y) = "y"
  data$y = NULL
  data$x = NULL
  x = as.data.frame(cbind(1, data))
  data = list("y"=y, "x"=x)
  
  return(data)
  
}

