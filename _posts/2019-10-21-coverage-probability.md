---
title: "Coverage Probability Calculation"
date: 2019-10-21
permalink: /coverage-prob/
excerpt: "Simulation Study"
---


## Background Introduction


Coverage probability is an important operating characteristic of methods for constructing interval estimates, particularly confidence intervals. The coverage probability of a technique for calculating a confidence interval is the proportion of the time that the interval contains the true value of interest, which is the median here. Moreover, The width of a confidence interval is related to its coverage probability – wider confidence intervals have higher coverage probabilities, narrower confidence intervals have lower coverage probabilities.

You might wonder why this is necessary. Isn't the coverage probability always (1-α) = 0.95? No, that is only true when the population is normally distributed (which is never true in practice) or the sample sizes are large enough that you can invoke the Central Limit Theorem. Simulation enables you to estimate the coverage probability for small samples when the population is not normal. You can simulate from skewed or heavy-tailed distributions to see how skewness and kurtosis affect the coverage probability.

**Definition:**

For the purposes of this deliverable, define the 95% confidence interval of the median to be the middle 95% of sampling distribution of the median. Similarly, the 95% confidence interval of the mean, standard deviation, etc. is the middle 95% of the respective sampling distribution.

**Definition:**

For the purposes of this deliverable, define the coverage probability as the long run proportion of intervals that capture the population parameter of interest. Conceptually, one can calculate the coverage probability with the following steps

**Steps**

* Generate a sample of size N from a known distribution.

* Construct a confidence interval.

* Determine if the confidence captures the population parameter.

* Repeat steps (1) - (3) many times. Estimate the coverage probability as the proportion of samples for which the confidence interval captured the population parameter.

Ideally, a 95% confidence interval will capture the population parameter of interest in 95% of samples.




## The Goal


Perform a simulation to calculate the coverage probability of the 95% confidence interval of the median when computed from F̂Xmle. You will write a blog post to explain coverage probability and to explain your simulation.




## Steps to Find the Coverage Probability


First, we load the required packages.

```r
library(dplyr)
```

Then, here, we have a sample of size N (201) from a known distribution (normal distribution). We set the parameters to be mean = 0, and sd = 1. Then we have the two functions: one is for generating data by using rnorm and another one is for comparing interval to the true value, which will return 1 for true (capture) and 0 for false (no capture).

```r
set.seed(43589)
N <- 201
pop.mean = 0
pop.sd = 1
(true.parameters <- c(N,mean = pop.mean, sd = pop.sd))



generate_data <- function(parameters){
  rnorm(parameters[1],parameters[2],parameters[3])
}

capture_median <- function(ci){
  1*(ci[1]<0 & 0<ci[2])     # convert from boolean to numeric
}

```
![](/images/cov1.PNG)

* How to use MLE to estimate the distribution:

Maximum likelihood estimation is a method that determines values for the parameters of a model. The parameter values are found such that they maximize the likelihood that the process described by the model produced the data that were actually observed.
Since the distribution that we used here is Gaussian distribution, which has two parameters, the mean and the standard deviation. Different values of these parameters result in different curves. We want to know which curve was most likely responsible for creating the data points that we observed? Maximum likelihood estimation is a method that will find the values of μ and σ that result in the curve that best fits the data.

```r
est.mle <- function(data){
  mle.mean <- mean(data)
  mle.sd <- sqrt(((length(data)-1)/length(data))*var(data))
  
  return(c(length(data),mle.mean,mle.sd)) # list of parameters 
}

true.parameters %>% generate_data %>% est.mle

```
![](/images/cov2.PNG)

Now we have our function to find the 95% confidence interval of the median to be the middle 95% of sampling distribution of the median, and we set the process of generating the median to be 5000 and store the values. Then we use the quantile function for finding the confidence interval for our new sample medians.

```r
boot.meds.ci <- function(parameters) {
  R <- 5000
  sample.meds <- NA
  for(i in 1:R) {
    sample.meds[i] <- parameters %>% generate_data() %>% median
    #smaple.meds[i] <- rnorm(paramters[1], paramters[2], paramters[3]) %>% median
  }
  quantile(sample.meds, c(0.025, 0.975))
}
```

Here, we estimate the coverage probability as the proportion of samples for which the confidence interval captured the population parameter. We count the number of capture and then find the coverage probability. The simulation time is set to be 5000, and we have the coverage probability of 0.99.

```r
capture <- NA
for(i in 1:5000){
  capture[i]<-true.parameters %>% generate_data %>% est.mle %>% boot.meds.ci %>% capture_median
}
mean(capture)
```
![](/images/cov3.PNG)
