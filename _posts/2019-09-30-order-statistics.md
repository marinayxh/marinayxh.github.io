---
title: "Order Statistics Distribution Function"
date: 2019-09-30
permalink: /order-statistics-sim/
excerpt: "Simulation Study"
---


## Introduction


The median is an important quantity in data analysis. It represents the middle value of the data distribution. Estimates of the median, however, have a degree of uncertainty because (a) the estimates are calculated from a finite sample and (b) the data distribution of the underlying data is generally unknown. One important roles of a data scientist is to quantify and to communicate the degree of uncertainty in his or her data analysis.




## Questions

#### Question 1

Begin with the median from a sample of N=200 from the standard normal distribution. Write an R function that is the density function for the median in this sample. Note that the 100th order statistic is approximately the median and use the order statistic formula discussed in class. Generate a plot of the function.

*The graph shows that there is a maximum density at 0.*

```r
dorder <- function(x){
  100*
    choose(200, 100)*
    pnorm(x)^(100-1)*
    (1-pnorm(x))^(200-100)*
    dnorm(x)
}

curve(dorder,-1,1)
```
![](/images/order1.PNG)



#### Question 2

Write an R function that is the probability function for the median in this sample. Use the order statistic formula discussed in class. Generate a plot of the function.

*The graph shows that there is a maximum slope when at 0.0. Since porder is the probability function for the median, therefore the graph shows the maximum probability of median being 0.*

```r
porder <- function(x){
  pbinom(99, 200, pnorm(x，0，1), lower.tail = FALSE)
}

curve(porder, -1, 1)
```
![](/images/order2.PNG)



#### Question 3

Write an R function that is the quantile function for the median in this sample. (You have several options for how to write this function.) Generate a plot of the function.

*If the median is morely to be 0.0, then here it does split the distribution in half.*  

```r
qorder <- function(p){
  out <- p
  for(i in seq_along(p)){
    out[i] <- uniroot(function(x){porder(x) - p[i]}, interval = c(-100,100))$root
  }
  out
}

curve(qorder, 0.01, 0.99)
```
![](/images/order3.PNG)



#### Question 4

Simulate the sampling distribution for the median as you did in the previous deliverable. Create a plot of the empirical CDF (ECDF). Overlay the plot of the ECDF with a plot of the CDF.

*The ECDF and CDF match with each other pretty well, so the ordered statistic approach can be regarded as accurate.*

```r
N <- 200
M <- 5000
out <- array(rnorm(M*N), c(M,N))
meds <- apply(out,1,median)
median_ecfd <- ecdf(meds)

plot(median_ecfd, xlab = "Medians", main = "",col = "blue",lwd =3)
curve(porder, add = TRUE, col = "red", lwd = 3)
legend("topleft", c("ECDF","CDF"), lwd = 3, col = c("blue","red"), bty = "n")
```
![](/images/order4.PNG)



#### Question 5

Using the simulated sampling distribution from the previous question, create a histogram (on the density scale). Overlay the histogram with a plot of the density function.

*The graph, again, shows that the maximum density lays when median is about 0.0.*

```r
hist(meds, xlab = "Medians", main = "", freq = FALSE,col = "blue")
curve(dorder, add = TRUE, col = "red", lwd = 3)
legend("topleft", c("epdf","pdf"), lwd = 3, col = c("blue","red"), bty = "n")
box()
```
![](/images/order5.PNG)



#### Question 6

Generate a QQ plot for the simulated data of the median relative to the known sampling distribution of the median. Does the simulated data agree with the theoretical sampling distribution?

*If sample and theoretical quantiles come from the same distribution, then the plotted points will fall along the line y=x, approximately, which is shown in this graph.*

```r
p <- ppoints(200)
x <- qorder(p)
y <- quantile(meds, probs = p)

plot(x,y, asp = 1, xlab = "Theoretical quantile", ylab = "Sample quantile")
abline(0,1)
```
![](/images/order6.PNG)



#### Question 7

Modify the dorder, porder, and qorder functions so that the functions take a new parameter k (for the kth order statistic) so that the functions will work for any order statistic and not just the median.

```r
dorder <- function(x, k){
  k*
    choose(200, k)*
    pnorm(x,0,1)^(k-1)*
    (1-pnorm(x,0,1))^(200-k)*
    dnorm(x,0,1)
}

porder <- function(x, k){
  pbinom(k-1, 200, pnorm(x,0,1), lower.tail = FALSE)
}

qorder <- function(p, k){
  out <- p
  for(i in seq_along(p)){
    out[i] <- uniroot(function(x){porder(x, k) - p[i]}, c(-100,100))$root
  }
  out
}
```



#### Question 8

Generate the QQ plot for simulated data from the sampling distribution of the sample max and the theoretical largest order statistic distribution.

*If sample and theoretical quantiles come from the same distribution, then the plotted points will fall along the line y=x, approximately, which is shown in this graph, with a new parameter k (for the kth order statistic)*

```r
N <- 200
M <- 5000
out <- array(rnorm(M*N), c(M,N))
maxs <- apply(out,1,max)

p <- ppoints(200)
x <- qorder(p, 200)
y <- quantile(maxs, probs = p)

plot(x,y, asp = 1, xlab = "Theoretical quantile", ylab = "Sample quantile")
abline(0,1)

```
![](/images/order7.PNG)



#### Question 9

Modify the dorder, porder, and qorder functions so that the functions take new parameters dist and ... so that the functions will work for any continuous distribution that has d and p functions defined in R.

```r
dorder <- function(x, k, n, dist = "norm", ...){
  
  pf <- get(paste0("p", dist))
  df <- get(paste0("d", dist))
  k*
    choose(n, k)*
    pf(x, ...)^(k-1)*
    (1-pf(x, ...))^(n-k)*
    df(x, ...)
}

porder <- function(x, k, n, dist = "norm", ...){
  pf <- get(paste0("p", dist))

  pbinom(k-1, n, pf(x, ...), lower.tail = FALSE)
}

qorder <- function(p, k, n, dist = "norm", ...){
  out <- p
  for(i in seq_along(p)){
    out[i] <- uniroot(function(x){porder(x, k, n, dist, ...) - p[i]}, c(-100,100))$root
  }
  out
}
```



#### Question 10

Use the newly modified functions to plot the probability and density functions for the sample min (N=200).

*From the graph, we can see that there is a rapid increasing in probability when min is approaching -2.*

```r
curve(porder(x, 1, 200), -5,0, ylab = "Probability", xlab = "min", lwd = 3)
```
![](/images/order8.PNG)

```r
curve(dorder(x, 1, 200), -5,0, ylab = "Density", xlab = "min", lwd = 3)
```
![](/images/order9.PNG)

*So, overall, the sample distributions fitted the theoretical values well based on all the above analysis.*