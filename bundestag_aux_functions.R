# function to compute weights

f_weights <- function(timepoints, time, bw) {
  
  #normalize time to [0,1]
  time_n <- timepoints - timepoints[1]
  time_n <- time_n / max(time_n)
  
  # calculate weights
  weights <- dnorm(time_n, time, bw)
  
  return(list('weights'=weights, 'time'=time_n))
  
}

# computing a weighted correlation matrix

f_wcor <- function(X, w) {
  
  wm <- apply(X, 2, function(x) sum(x*w) / sum(w))
  
  corm<- matrix(NA, ncol(X), ncol(X))
  for(i in 1:ncol(X)) {
    for(j in 1:i) {
      corm[i,j] <- sum(((X[,i]-wm[i]) * (X[,j]-wm[j]))*w) / sum(w)
    }
  }
  
  corm <- cov2cor(corm)
  corm[is.na(corm)] <- 0
  corm <- corm + t(corm)
  diag(corm) <- 1
  
  return(corm)
  
}
