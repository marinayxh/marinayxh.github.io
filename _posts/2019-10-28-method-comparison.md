---
title: "Comparison: Coverage Probability of Various Methods"
date: 2019-10-28
permalink: /method-comp/
excerpt: "Simulation Study"
---


## Introduction


Coverage probability is an important operating characteristic of methods for constructing interval estimates, particularly confidence intervals. Ideally, a 95% confidence interval will capture the population parameter of interest in 95% of samples. One can also calculate 80% or 90% confidence intervals. In general, an X% confidence interval should capture the population parameter of interest in X% of samples.




## Not all methods are equally good


Here we will perform a 2×4×2 factorial simulation study to compare the coverage probability of various methods of calculating 90% confidence intervals. The three factors in the experiment are

1. True, underlying distribution
* standard normal
* gamma(shape = 1.4, scale = 3)

2. Model
* method of moments with normal
* method of moments with gamma
* kernel density estimation
* bootstrap

3. Parameter of interest
* sample min (1st order statistic)
* median



Other settings in the experiment that will not change are:

* Sample size, N=201
* Outside the loop estimation




## Brief Definition


#### Method of Moments

The method of moments is a technique for estimating the parameters of a statistical model. It works by finding values of the parameters that result in a match between the sample moments and the population moments (as implied by the model). This methodology can be traced back to Pearson (1894) who used it to fit a simple mixture model. It is sometimes regarded as a poor cousin of maximum likelihood estimation since the latter has superior theoretical properties in many settings. Nonetheless, the method of moments and generalizations thereof continue to be of use in practice for certain (challenging) types of estimation problem because of their conceptual and computational simplicity.

More details: https://newonlinecourses.science.psu.edu/stat414/node/193/



#### Kernel Density Estimation

In statistics, kernel density estimation (KDE) is a non-parametric way to estimate the probability density function of a random variable. Kernel density estimation is a fundamental data smoothing problem where inferences about the population are made, based on a finite data sample.

More details: https://mathisonian.github.io/kde/



#### Bootstrap

In statistics, bootstrapping is any test or metric that relies on random sampling with replacement. Bootstrapping allows assigning measures of accuracy (defined in terms of bias, variance, confidence intervals, prediction error or some other such measure) to sample estimates. This technique allows estimation of the sampling distribution of almost any statistic using random sampling methods. Generally, it falls in the broader class of resampling methods.

Bootstrapping is the practice of estimating properties of an estimator (such as its variance) by measuring those properties when sampling from an approximating distribution. One standard choice for an approximating distribution is the empirical distribution function of the observed data. In the case where a set of observations can be assumed to be from an independent and identically distributed population, this can be implemented by constructing a number of resamples with replacement, of the observed dataset (and of equal size to the observed dataset).

It may also be used for constructing hypothesis tests. It is often used as an alternative to statistical inference based on the assumption of a parametric model when that assumption is in doubt, or where parametric inference is impossible or requires complicated formulas for the calculation of standard errors.

More details: https://en.wikipedia.org/wiki/Bootstrapping_(statistics)




## Simulation


First, we create a function that can be used to generate data with normal distribution or gamma distribution.

```r
generate_data <- function(N, dist, sh, sc) {
  if(dist == "norm") {
    return(rnorm(N) + 4)
  } else if(dist == "gamma") {
    return(rgamma(N, shape = sh, scale = sc))
  }
}
```

Then, we create a function that can be used to return the 95% confidence interval from the approximated sampling distribution. We have four estimation methods here, which are method of moments with normal distribution, method of moments with gamma distribution, kernel density estimation and bootstrap estimation.

```r
estimate.ci <- function(data, mod, R = 5000, par.int, smoo = 0.3) {
  N <- length(data)
  sum.measure <- get(par.int)
  if (mod=="MMnorm"){ 
    mm.mean <- mean(data)
    mm.sd <- sd(data)
    sim.data <- rnorm(length(data),mm.mean,mm.sd)
    
    sim.data <- array(rnorm(N*R,mm.mean,mm.sd),dim=c(N,R))
    samp.dist <- apply(sim.data,2,sum.measure)
  } else if(mod == "MMgamma") {
    mm.shape <- mean(data)^2/var(data)
    mm.scale <- var(data)/mean(data)
    
    sim.data <- array(rgamma(length(data)*R, shape = mm.shape, scale = mm.scale), dim = c(N, R))
    samp.dist <- apply(sim.data, 2, FUN = sum.measure)
    
  } else if(mod == "KDE") { 
    ecdfstar <- function(t, data, smooth = smoo) {
      outer(t, data, function(a, b) {pnorm(a, b, smooth)}) %>% rowMeans
      }
    
    tbl <- data.frame(
      x = seq(min(data) - 2*sd(data), max(data) + 2*sd(data), by = 0.01)
    )
    tbl$p <- ecdfstar(tbl$x, data, smoo)
    tbl <- tbl[!duplicated(tbl$p),]
    
    qkde <- function(ps, tbl){
      rows <- cut(ps, tbl$p, labels = FALSE)
      tbl[rows, "x"]
      }
    
    U <- runif(N * R)
    sim.data <- array(qkde(U, tbl), dim = c(N, R))
    samp.dist <- apply(sim.data, 2, FUN = sum.measure)
  } else if(mod == "Boot") {
    sim.data <- array(sample(data, N * R, replace = TRUE), dim = c(N, R))
    samp.dist <- apply(sim.data, 2, FUN = sum.measure)
    
  }
  return(quantile(samp.dist, c(0.05, 0.95)))
}
```

Next, we create a function to estimate whether the parameter is captured by that range or not. 

```r
capture_par <- function(ci, true.par) {
  1 * (ci[1] < true.par & true.par < ci[2])
}
```

Now, we can calculate the coverage probabilities in each condition.

```r
library(dplyr)
N <- 201
shape.set <- 1.4
scale.set <- 3
true.norm.med <- qnorm(0.5)
true.norm.min <- mean(apply(array(rnorm(N*10000), dim = c(N, 10000)), 2, min))
true.gamma.med <- qgamma(0.5, shape = shape.set, scale = scale.set) 
true.gamma.min <- mean(apply(array(rgamma(N*10000, shape = shape.set, scale = scale.set), dim = c(N, 10000)), 2, min))
  
simsettings <- expand.grid(dist = c("norm", "gamma"), model = c("MMnorm", "MMgamma", "KDE", "Boot"), par.int = c("median", "min"), cov.prob =NA, stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
for (k in 1:nrow(simsettings)){
  dist1 <- simsettings[k, 1]
  model1 <- simsettings[k, 2]
  par.int1 <- simsettings[k, 3]
  
  if (dist1 == "norm" & par.int1 == "median") {
    true.par1 = true.norm.med + 4
  } else if (dist1 == "gamma" & par.int1 == "median") {
    true.par1 = true.gamma.med
  } else if (dist1 == "norm" & par.int1 == "min") {
    true.par1 = true.norm.min + 4
  } else if (dist1 == "gamma" & par.int1 == "min") {
    true.par1 = true.gamma.min
  }
  cover <- NA
  
  for (sims in 1:1000) {
    cover[sims] <- generate_data(N, dist1, sh = shape.set, sc = scale.set) %>% estimate.ci(mod = model1, par.int = par.int1, R = 5000) %>% capture_par(true.par = true.par1)
    simsettings[k ,4] <- mean(cover)
  }
}

simsettings

```
![](/images/method1.PNG)




## Conclusion


From the table that we generate above, we can see that MMnorm with normal distribution or MMgamma with gamma distribution have relative higher coverage probability. When KDE combines with normal distribution, the coverage probability is relative higher than when KDE combines with gamma distribution. For bootstrap, when capturing median, the coverage probability is relative higher than when capturing min. Except the combination of MMnorm with gamma distribution that capturing median, overall, the coverage probability is relative higher in any estimation method when we are capturing median instead of min.

From our simulation study, we can know that coverage probability is affected by many factors, such as the estimation methods, the parameters, and the population distributions. We should be confident about choosing the best estimation method when we face different situations.