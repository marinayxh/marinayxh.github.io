---
title: "Monte Carlo Simulation Error"
date: 2019-09-02
permalink: /monte-carlo-simulation-error/
excerpt: "Simulation Study"
---


## SIMULATION


* Large-scale computer-based simulations is a method of producing quantitative and predictive information. It is better that simulation results can come with a high confidence level.
* For doing simulation, we could generate randoms from unknown distribution and then repeat the simulation multiple times. Results will not be the same for each time.
* For repeating the simulation process, each time the outputs are not the same. Simulations involve their attendant errors, and these error models can be used to improve the prediction.
* Some of the reasons for simulation errors: inaccurate input data, inaccurate physics models, and limited accuracy of the solutions of the governing equations.
* There are two types of error. Absolute Error refers to the mathematical difference between the estimated probability (P`) and the actual probability (P). Relative Error divides the absolute error by the actual probability.


#### ABSOLUTE ERROR
* Absolute error is the difference between the measured value and "true" value.
* This can be caused by your scale not measuring the exact amount you are trying to measure.
* Absolute error = |P` - P|

#### RELATIVE ERROR
* When used as a measure of precision, it is the ratio of the absolute error of a measurement to the measurement being taken. 
* It can describe accuracy on how a measurement is compared to the true value.
* Relative error =  |P` - P| / P




## Example


Here we perform a 14 X 5 factorial experiment simulation that estimates the error for each combination of replicate number (2^2, 2^3, …, 2^15) and probability (0.01, 0.05, 0.10, 0.25, 0.50). Let p hat denote the probability estimated from simulation, and let p denote the true underlying probability.

The main idea is to create a matrix that can store all the mean number of the absolute error/ relative error from each sample size, and then we can make a plot for these mean numbers.


### Simulation error in Log2 scale
* Notice that top one graph is for absolute error, and the bottom one is for relative error.
* Besides, different lines in different colors which representing certain value of probability: yellow one is for 0.50, purple one is for 0.25, green one is for 0.10, blue one is for 0.05 and red one is for 0.01.
```r
n <- rep(NA,14)
for(i in 1:14){
  n[i] <-2^(i+1)
}
M <- matrix(NA,14,5)
p <- c(0.01, 0.05, 0.10, 0.25, 0.50)
for(x in 1:length(p)){
  for(y in 1:length(n)){
    TestSample <- rep(NA,100000)
    for(z in 1:100000){
      TestSample[z] <- abs(rbinom(1, n[y],p[x])/n[y]-p[x])
    }
    M[y,x]<- mean(TestSample)
  }
M
}
```

```r
plot(M[,1], xlim = c(0,14),ylim = c(0,0.2),xaxt="n",xlab = "N(log2 scale)",ylab="Absolute Error")
lname <- p
xname <- n
axis(1, at=1:14,las=2,lab=xname)
lines(M[,1],col="red",type = "b")
lines(M[,2],col="blue",type = "b")
lines(M[,3],col="green",type = "b")
lines(M[,4],col="purple",type = "b")
lines(M[,5],col="orange",type = "b")
```
![](/images/monte1.PNG)

*Again, from the top of the graph to its bottom: the lines are made based on different probability, and they are following the order of 0.50, 0.25, 0.10, 0.05, and 0.01. We can see that absolute error is deeply decreasing when the replicate number (in log2 scale) getting larger. We also have the highest relative error laying on p=0.50.*

```r
n <- rep(NA,14)
for(i in 1:14){
  n[i] <-2^(i+1)
}
MR <- matrix(NA,14,5)
p <- c(0.01, 0.05, 0.10, 0.25, 0.50)
for(x in 1:length(p)){
  for(y in 1:length(n)){
    TestSample <- rep(NA,100000)
    for(z in 1:100000){
      TestSample[z] <- abs(rbinom(1, n[y],p[x])/n[y]-p[x])/p[x]
    }
    MR[y,x]<- mean(TestSample)
  }
  
}
```

```r
plot(MR[,1], xlim = c(0,14),ylim = c(0,2.0),xaxt="n",xlab = "N(log2 scale)",ylab="Relative Error")
lname <- p
xname <- n
axis(1, at=1:14,las=2,lab=xname)

lines(MR[,1],col="red",type = "b")
lines(MR[,2],col="blue",type = "b")
lines(MR[,3],col="green",type = "b")
lines(MR[,4],col="purple",type = "b")
lines(MR[,5],col="orange",type = "b")
```
![](/images/monte2.PNG)

*Again, from the top of the graph to its bottom: the lines are made based on different probability, and they are following the order of 0.50, 0.25, 0.10, 0.05, and 0.01. We can see that relative error is deeply decreasing when the replicate number (in log2 scale) getting larger. We also have the highest relative error laying on p=0.01. Moreover, the overall value range for relative error is larger than for absolute error.*




### Simulation error with y-axis in Log10 scale
Then, we try to generate the results with the y-axis on the log10 scale.
```r
n <- rep(NA,14)
for(i in 1:14){
  n[i] <-2^(i+1)
}
M10 <- matrix(NA,14,5)
p <- c(0.01, 0.05, 0.10, 0.25, 0.50)
for(x in 1:length(p)){
  for(y in 1:length(n)){
    TestSample <- rep(NA,100000)
    for(z in 1:100000){
      TestSample[z] <- abs(rbinom(1, n[y],p[x])/n[y]-p[x])
    }
    M10[y,x]<- log10(mean(TestSample))
  }
  
}

plot(M10[,1], xlim = c(0,14),ylim = c(-3.5,0.5),xaxt="n",xlab = "N(log2 scale)",ylab="Absolute Error")
lname <- p
xname <- n
axis(1, at=1:14,las=2,lab=xname)
lines(M10[,1],col="red",type = "b")
lines(M10[,2],col="blue",type = "b")
lines(M10[,3],col="green",type = "b")
lines(M10[,4],col="purple",type = "b")
lines(M10[,5],col="orange",type = "b")
```
![](/images/monte3.PNG)

```{r}
n <- rep(NA,14)
for(i in 1:14){
  n[i] <-2^(i+1)
}
M10_R <- matrix(NA,14,5)
p <- c(0.01, 0.05, 0.10, 0.25, 0.50)
for(x in 1:length(p)){
  for(y in 1:length(n)){
    TestSample <- rep(NA,100000)
    for(z in 1:100000){
      TestSample[z] <- abs(rbinom(1, n[y],p[x])/n[y]-p[x])/p[x]
    }
    M10_R[y,x]<- log10(mean(TestSample))
  }
  
}

plot(M10_R[,1], xlim = c(0,14),ylim = c(-3.5,0.5),xaxt="n",xlab = "N(log2 scale)",ylab="Relative Error")
lname <- p
xname <- n
axis(1, at=1:14,las=2,lab=xname)
lines(M10_R[,1],col="red",type = "b")
lines(M10_R[,2],col="blue",type = "b")
lines(M10_R[,3],col="green",type = "b")
lines(M10_R[,4],col="purple",type = "b")
lines(M10_R[,5],col="orange",type = "b")
```
![](/images/monte4.PNG)




## Resources


For more information:

* There are two main reasons to use logarithmic scales in charts and graphs. The first is to respond to skewness towards large values; the data values are spread out better with the logarithmic scale. The second is to show percent change or multiplicative factors.*

Check more info on https://www.forbes.com/sites/naomirobbins/2012/01/19/when-should-i-use-logarithmic-scales-in-my-charts-and-graphs/#344eb6715e67

