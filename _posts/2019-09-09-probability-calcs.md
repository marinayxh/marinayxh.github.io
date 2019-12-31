---
title: "Baseball Game Example: Probability Calculation, Part.I"
date: 2019-09-09
permalink: /probability-calculation/
excerpt: "Simulation Study"
---


## Introduction


The world series is a best-of-7 match-up between the champions of the American and National Leagues of Major League Baseball. We suppose that the Braves and the Twins are teams competing in the World Series.

What we know (in any given game):
        
        
        Probability that the Braves win is PB 
            
        Probability that the Twins win is PT = 1−PB.
    



## Questions and Solutions


    1. What is the probability that the Braves win the World Series given that PB = 0.55?

We know that the best situation for a team to win "best-of-seven" is to win the first four games. Other than that, first, we assume that Braves will have their last win at the fifth game, which means they will need to win any three of the previous four games (in other words, they will lose one of the previous four games). If we continue in this way and assume the last win at either the sixth or seventh game, then Braves will need to lose either two or three of the previous games. Then, we could sum the probabilities of these four potential situations, and we could get the answer for this question.

Here, we introduce a term called "negative binomial distribution". It is a discrete probability distribution of the number of successes in a sequence of independent and identically distributed Bernoulli trials before a specified (non-random) number of failures (denoted r) occurs. It can directly give us the probability of having three, two or one game lose before winning the fourth games (no matter how many games can be played in total).

From the calculation, we could know that the probability for Braves to win a best-of-7 is 0.608 when given the prob of Braves to win is 0.55.

```r
pnbinom(3,4,0.55)

```
![](/images/prob1.PNG)
    
    
    
    2. What is the probability that the Braves win the World Series given that PB = x?

We now try to find the probability that Braves win the World Series given PB=x; x could be a number from 0.5 to 1.0, and here we set the interval to be 0.01 and continue using negative binomial.

From the graph, we can see a positive correlation between the probability of the Braves winning a head-to-head matchup and probability of winning world series, which means that once Braves has higher probability of winning one game, then it will have higher probability of winning the World Series, which makes sense.

```r
c_seq_win <- rep(NA,51)
for (j in 1:51 ) {
  p_x <- seq(0.5,1.0,by=0.01)
  c_seq_win[j] <- pnbinom(3,4,p_x[j])
}
dt<-data.frame(c_seq_win,c_dt_x<-seq(0.5,1,0.01))
dt%>%
  ggplot(aes(x=c_dt_x,y=c_seq_win))+
           labs(title = "Probability of winning the World Series",x="Probability of the Braves winning a head-to-head matchup",y="Pr(Win World Series)") + 
  geom_point()
```
![](/images/prob2.PNG)
    
    
    
    3. Suppose one could change the World Series to be best-of-9 or some other best-of-X series. What is the shortest series length so that P(Braves win World Series|PB=.55)≥0.8?

Because of the property of this game type, X could not be an even number. We have tried X to be from 1 to 99, and we use negative binomial as well. 

According to this new vector that containing all the probability of Braves winning the World Series (given PB=0.55) under different series length, we can see that when they have seventy-one games (best-of-71), Braves could win the world series with a probability equal or greater than 0.8 (specifically, it is 0.8017017).

```r

c_win_series <- rep(NA,99)
for (i in seq(1,99,2)) {
  c_win_series[i]<-pnbinom(floor(i/2),ceiling(i/2),0.55)
}
c_win_series[71]

```
![](/images/prob3.PNG)



    4. We now want to find the shortest series length so that P(Braves win World Series|PB=x)≥0.8? This will be a figure with PB on the x-axis and series length is the y-axis.

We are creating a new matrix here for this question. The column represents series length, and the row represents PB. It is pointless for PB equals to 0.5, so we start with PB=0.51 and set the interval to be 0.01. From the new matrix we could see that when PB is greater than 0.55, the chance for braves to win World Series could get to equal or greater than 0.8. When PB is greater than 0.73, the chance for braves to win World Series could get to equal or greater than 0.8 regardless of the series length.

We kind of get similar idea from the graph; the dash line represents 0.8. We could see that when PB gets to certain point, the probability of Braves to win World Series is always greater than 0.8. 

```r

c_win_series_greater <- matrix(NA,50,49)
for (k in 1:49) {
  for (h in 1:50) {
    i <- seq(3,99,2)
    p_x <- seq(0.51,1.0,0.01)
    c_win_series_greater[h,k]<-pnbinom(floor(i[k]/2),ceiling(i[k]/2),p_x[h])
  }
}

matplot(c_win_series_greater,type="l",xaxt="n",yaxt="n",xlab = "PB",ylab="Series Length")
abline(h=0.8,col="black",lty=2, lwd=3)
lname_series <- seq(3,99,2)
xname_series <- seq(0.51,1.0,0.01)
axis(1, at=1:50,las=1,lab=xname_series)
#axis(2, at=1:49,las=1,lab=lname_series)
```
![](/images/prob4.PNG)



    5. We now calculate P(PB=0.55|Braves win World Series in 7 games) under the assumption that either PB=0.55 or PB=0.45.

Here, we introduce a new term called "Bayes rule". It describes the probability of an event, based on prior knowledge of conditions that might be related to the event. 

For example, if we were trying to provide the probability that a patient has cancer, we would initially say it is whatever percent of the population has cancer. However, given additional evidence such as the fact that this person is a smoker, we can update our probability, since the probability of having cancer is higher given that the person is a smoker. 

Bayes rule allows us to utilize prior knowledge to improve our probability estimations.

The formula for Bayes rule is:
              
              P(A|B) = P(B|A)P(A)/P(B)

In our case:

              P(PB=0.55|Braves win World Series in 7 games) <- 
              (P(Braves win World Series in 7 games|PB=0.55)*P(PB=0.55))
              /P(Braves win World Series in 7 games)

By calculating, we could get P(PB=0.55|Braves win World Series in 7 games) under the assumption that PB=0.55 or PB = 0.45 is 0.55, in the case that we consider the probability of PB = 0.55 and the probability of PB = 0.45 are the same.

```r
P <- (dnbinom(3,4,0.55)*(1/2))/((dnbinom(3,4,0.55)+dnbinom(3,4,0.45))/2)
P
```
![](/images/prob5.PNG)




# Reference


https://en.wikipedia.org/wiki/Bayes%27_theorem

https://towardsdatascience.com/what-is-bayes-rule-bb6598d8a2fd