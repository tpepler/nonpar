cucconi.test <- function(x, y, method = c("permutation", "bootstrap")){

  # Implementation of the Cucconi test for the two-sample location-scale problem
  # A permutation/bootstrap distribution of the test statistic (C) under the
  # null hypothesis is used to calculate the p-value.
  # Reference: Marozzi (2013), p. 1302-1303

  m <- length(x)
  n <- length(y)
  C <- cucconi.teststat(x = x, y = y, m = m, n = n)
  
  if(method[1] == "permutation"){
    h0dist <- cucconi.dist.perm(x = x, y = y)
  }
  
  if(method[1] == "bootstrap"){
    h0dist <- cucconi.dist.boot(x = x, y = y)
  }
  
  p.value <- length(h0dist[h0dist >= C]) / length(h0dist)
  
  cat("\nCucconi two-sample location-scale test\n")
  cat("\nNull hypothesis: The locations and scales of the two population distributions are equal.\n")
  cat("Alternative hypothesis: The locations and/or scales of the two population distributions differ.\n")
  cat(paste("\nC = ", round(C, 3), ", p-value = ", round(p.value, 4), "\n\n", sep=""))
  
  return(list(C = C,
              method = method[1],
              p.value = p.value))
}