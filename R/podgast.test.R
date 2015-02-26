podgast.test <- function(x, y){
  # Podgor-Gastwirth two-sample location-scale test as described in Marozzi (2013) p.1301
  # The p-value is the asymptotic p-value from an F-distribution with 2 and (N-3) degrees of freedom
  
  m <- length(x)
  n <- length(y)
  N <- m + n
  Ivec <- c(rep(1, times = m), rep(0, times = n))
  Svec <- rank(c(x, y))
  Svec2 <- Svec^2
  Smat <- as.matrix(cbind(Int = rep(1, times = N), Svec, Svec2))
  bvec <- solve(t(Smat) %*% Smat) %*% (t(Smat) %*% Ivec)
  numer <- (t(bvec) %*% t(Smat) %*% Ivec - m^2 / N) / 2
  denom <- (m - t(bvec) %*% t(Smat) %*% Ivec) / (N - 3)
  PGstat <- numer / denom
  p.value <- pf(PGstat, df1 = 2, df2 = (N - 3), lower.tail = FALSE)
  
  cat("\nPodgor-Gastwirth two-sample location-scale test\n")
  cat("\nNull hypothesis: The locations and scales of the two population distributions are equal.\n")
  cat("Alternative hypothesis: The locations and/or scales of the two population distributions differ.\n")
  cat(paste("\nStatistic = ", round(PGstat, 3), ", p-value = ", round(p.value, 4), "\n\n", sep=""))
  
  return(list(statistic = PGstat,
              p.value = p.value))
}