cucconi.test<-function(x, y){
# Implementation of the Cucconi test for the two-sample location-scale problem
# A permutation distribution of the test statistic (C) under the null hypothesis
# is used to calculate the p-value
# Reference: Marozzi (2013), p. 1302-1303
  
  m<-length(x)
  n<-length(y)
  N<-m+n
  S<-rank(c(x,y))[(m+1):N]
  denom<-sqrt(m*n*(N+1)*(2*N+1)*(8*N+11)/5)
  U<-(6*sum(S^2)-n*(N+1)*(2*N+1))/denom
  V<-(6*sum((N+1-S)^2)-n*(N+1)*(2*N+1))/denom
  rho<-(2*(N^2-4))/((2*N+1)*(8*N+11))-1
  C<-(U^2+V^2-2*rho*U*V)/(2*(1-rho^2))
  #h0dist<-cucconi.dist.boot(x=x, y=y)
  h0dist<-cucconi.dist.perm(x=x, y=y)
  p.value<-length(h0dist[h0dist>=C])/length(h0dist)
  return(list(C=C,p.value=p.value))
}

cucconi.dist.boot<-function(x, y, reps=10000){
# Computes the distribution of the Cucconi test statistic using bootstrap sampling

  cucconi.teststat<-function(x, y, m, n){
    N<-m+n
    S<-rank(c(x,y))[(m+1):N]
    denom<-sqrt(m*n*(N+1)*(2*N+1)*(8*N+11)/5)
    U<-(6*sum(S^2)-n*(N+1)*(2*N+1))/denom
    V<-(6*sum((N+1-S)^2)-n*(N+1)*(2*N+1))/denom
    rho<-(2*(N^2-4))/((2*N+1)*(8*N+11))-1
    C<-(U^2+V^2-2*rho*U*V)/(2*(1-rho^2))
    return(C)
  }
  
  m<-length(x)
  n<-length(y)
  x.s<-(x-mean(x))/sd(x) # standardise the x-values
  y.s<-(y-mean(y))/sd(y) # standardise the y-values
  bootvals<-rep(NA,times=reps)
  for(r in 1:reps){
    xboot<-x.s[sample(1:m,size=m,replace=TRUE)]
    yboot<-y.s[sample(1:n,size=n,replace=TRUE)]
    bootvals[r]<-cucconi.teststat(x=xboot,y=yboot, m=m, n=n)
  }
  return(bootvals)
}

cucconi.dist.perm<-function(x, y, reps=1000){
  # Computes the distribution of the Cucconi test statistic using random permutations
  
  cucconi.teststat<-function(x, y, m, n){
    N<-m+n
    S<-rank(c(x,y))[(m+1):N]
    denom<-sqrt(m*n*(N+1)*(2*N+1)*(8*N+11)/5)
    U<-(6*sum(S^2)-n*(N+1)*(2*N+1))/denom
    V<-(6*sum((N+1-S)^2)-n*(N+1)*(2*N+1))/denom
    rho<-(2*(N^2-4))/((2*N+1)*(8*N+11))-1
    C<-(U^2+V^2-2*rho*U*V)/(2*(1-rho^2))
    return(C)
  }
  
  m<-length(x)
  n<-length(y)
  N<-m+n
  alldata<-c(x,y)
  permvals<-rep(NA,times=reps)
  for(r in 1:reps){
    permdata<-alldata[sample(1:N,size=N,replace=FALSE)]
    xperm<-permdata[1:m]
    yperm<-permdata[(m+1):N]
    permvals[r]<-cucconi.teststat(x=xperm,y=yperm, m=m, n=n)
  }
  return(permvals)
}
