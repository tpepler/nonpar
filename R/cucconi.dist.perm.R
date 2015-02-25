cucconi.dist.perm <- function(x, y, reps = 1000){

  # Computes the distribution of the Cucconi test statistic using random permutations
    
  m <- length(x)
  n <- length(y)
  N <- m + n
  alldata <- c(x, y)

  bootFunc <- function(){
    permdata <- alldata[sample(1:N, size = N, replace = FALSE)]
    xperm <- permdata[1:m]
    yperm <- permdata[(m + 1):N]
    return(cucconi.teststat(x = xperm, y = yperm, m = m, n = n))    
  }
  permvals <- replicate(reps, expr = bootFunc())
  
#  permvals<-rep(NA,times=reps)
#  for(r in 1:reps){
#    permdata<-alldata[sample(1:N,size=N,replace=FALSE)]
#    xperm<-permdata[1:m]
#    yperm<-permdata[(m+1):N]
#    permvals[r]<-cucconi.teststat(x=xperm,y=yperm, m=m, n=n)
#  }
  return(permvals)
}