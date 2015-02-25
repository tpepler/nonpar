boot.t.test <- function(x, y, reps = 1000, mu = 0, alternative = c("two.sided", "less", "greater"))
{
  # Bootstrap t-test as described in Efron and Tibshirani (1993), (Algorithm 16.2, p224)
  
  nx <- length(x)
  ny <- length(y)
  t.obs <- (mean(x) - mean(y) - mu) / sqrt(var(x) / nx + var(y) / ny)
  comb.mean <- mean(c(x, y))
  x.c <- x - mean(x) + comb.mean
  y.c <- y - mean(y) + comb.mean
  t.boot <- rep(NA, times = reps)
  
  bootFunc <- function(){
    bootx <- x.c[sample(1:nx, size = nx, replace = TRUE)]
    booty <- y.c[sample(1:ny, size = ny, replace = TRUE)]
    return((mean(bootx) - mean(booty) - mu) / sqrt(var(bootx) / nx + var(booty) / ny))
  }
  
  t.boot <- replicate(reps, expr = bootFunc())
  
  if(alternative[1] == "two.sided"){
    pval <- length(t.boot[abs(t.boot) >= abs(t.obs)]) / reps
    h1phrase <- "not equal to"
  }
  
  if(alternative[1] == "less"){
    pval <- length(t.boot[t.boot <= t.obs]) / reps
    h1phrase <- "less than"
  }
  
  if(alternative[1] == "greater"){
    pval <- length(t.boot[t.boot >= t.obs]) / reps
    h1phrase <- "greater than"
  }
  
  cat("\nBootstrap Two Sample t-test\n")
  cat(paste("\nt = ", round(t.obs, 3), ", p-value = ", round(pval, 4), "\n", sep=""))
  cat(paste("Alternative hypothesis: true difference in means is ", h1phrase, " ", mu, "\n\n", sep=""))
  
  return(list(mu0 = mu,
              statistic = t.obs,
              alternative = alternative[1],
              p.value = pval))
}
