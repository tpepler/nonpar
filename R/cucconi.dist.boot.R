cucconi.dist.boot <- function(x, y, reps = 10000){
  
  # Computes the distribution of the Cucconi test statistic using bootstrap sampling
    
  m <- length(x)
  n <- length(y)
  x.s <- (x - mean(x)) / sd(x) # standardise the x-values
  y.s <- (y - mean(y)) / sd(y) # standardise the y-values

  bootFunc <- function(){
    xboot <- x.s[sample(1:m, size = m, replace = TRUE)]
    yboot <- y.s[sample(1:n, size = n, replace = TRUE)]
    return(cucconi.teststat(x = xboot, y = yboot, m = m, n = n))
  }
  bootvals <- replicate(reps, expr = bootFunc())
  
#  bootvals <- rep(NA,times=reps)
#  for(r in 1:reps){
#    xboot<-x.s[sample(1:m,size=m,replace=TRUE)]
#    yboot<-y.s[sample(1:n,size=n,replace=TRUE)]
#    bootvals[r]<-cucconi.teststat(x=xboot,y=yboot, m=m, n=n)
#  }
  return(bootvals)
}