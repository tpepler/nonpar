#********************************************************************
### BOOTSTRAP T-TEST FUNCTION

boot.t.test <- function(x, y, reps = 1000, alternative = c("two.sided", "less", "greater"))
{
  # Bootstrap t-test as described in Efron and Tibshirani (1993), (Algorithm 16.2, p224)
  
  nx <- length(x)
  ny <- length(y)
  t.obs <- (mean(x) - mean(y)) / sqrt(var(x) / nx + var(y) / ny)
  comb.mean <- mean(c(x, y))
  x.c <- x - mean(x) + comb.mean
  y.c <- y - mean(y) + comb.mean
  t.boot <- rep(NA, times = reps)
  
  bootFunc <- function(){
    bootx <- x.c[sample(1:nx, size = nx, replace = TRUE)]
    booty <- y.c[sample(1:ny, size = ny, replace = TRUE)]
    return((mean(bootx) - mean(booty)) / sqrt(var(bootx) / nx + var(booty) / ny))
  }
  
  t.boot <- replicate(reps, expr = bootFunc())
  
#  for(r in 1:reps){
#    bootx <- x.c[sample(1:nx, size = nx, replace = TRUE)]
#    booty <- y.c[sample(1:ny, size = ny, replace = TRUE)]
#    t.boot[r] <- (mean(bootx) - mean(booty)) / sqrt(var(bootx) / nx + var(booty) / ny)
#  }

if(alternative[1] == "two.sided"){pval <- length(t.boot[abs(t.boot) >= abs(t.obs)]) / reps}
#else{pval <- length(t.boot[t.boot > t.obs]) / reps}
if(alternative[1] == "less"){pval <- length(t.boot[t.boot <= t.obs]) / reps}
if(alternative[1] == "greater"){pval <- length(t.boot[t.boot >= t.obs]) / reps}
return(list(statistic = t.obs,
            alternative = alternative,
            p.value = pval))
}

#********************************************************************