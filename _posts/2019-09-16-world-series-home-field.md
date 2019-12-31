---
title: "Baseball Game Example: Probability Calculation, Part.II"
date: 2019-09-16
permalink: /world-series/
excerpt: "Simulation Study"
---


## Background Introduction


The home field advantage is the edge which a team may have when playing a game at its home stadium. For example, it is the edge the Braves may have over the Yankees when the head-to-head match-up is in Atlanta. It is the advantage the Yankees may have when the head-to-head match-up is in New York.

The World Series is a first-to-4-wins match-up between the champions of the American and National Leagues of Major League Baseball. In this assignment, you are going to use simulation and analytic methods to compare the probability of winning the World Series with and without home field advantage.




## Setup


Suppose that the Braves and the Yankees are teams competing in the World Series. We have two possible schedules for each game of the series. (NYC = New York City, ATL = Atlanta)

Let PB be the probability that the Braves win a single head-to-head match-up with the Yankees, under the assumption that home field advantage doesn’t exist. Let PBH denote the probability that the Braves win a single head-to-head match-up with the Yankees as the home team (H for home). Let PBA denote the probability that the Braves win a single head-to-head match-up with the away team (A for away).


Game Location  |   No advantage   |    Advantage
---------------|------------------|-----------------------
ATL	           |   PB	            |    PBH = PB*1.1
NYC	           |   PB	            |    PBA = 1−(1−PB)*1.1




## Questions


_1. Compute analytically the probability that the Braves win the world series when the sequence of game locations is {NYC, NYC, ATL, ATL, ATL, NYC, NYC}. Calculate the probability with and without home field advantage when PB=0.55. What is the difference in probabilities?_


We have p as a new column standing for probability with home field advantage, and we have p_no_a as a new column standing for probability without home field advantage. Compared to the other one, we can see that when there is no home field advantage, the winning probability stays the same when the last win hits on the same game. From the overall outcome, since 0.608 is greater than 0.604, we could see that the winning probability is slightly higher when there is no home field advantage for this specific sequence of game locations. The difference between prob is about 0.004.

Here is the link for the all-possible-world-series-outcomes csv file:

[link](https://drive.google.com/file/d/1prNMAbOzpaxJk2w0Zg35VWDljLgXt4RC/view?usp=sharing)

```r
# Get all the possible outcomes
# For the 
require(dplyr)
require(data.table)
library(ggplot2)
library(ie2misc)
library(readr)
apo <- fread("D:/Fall 2019/Prob 5620/Probability-and-Inference-Portfolio-Yue-Marina/04-world-series-home-field/all-possible-world-series-outcomes.csv")


# Home Field indicator
set.seed(12345)
hfi <- c(0,0,1,1,1,0,0)

# P_B, with home field advantage
pb <- 0.55
advantage_multiplier <- 1.1 # when there is home advantage
pbh <- pb*advantage_multiplier
pba <- 1 - (1 - pb)*advantage_multiplier

# Calculate the probability of each possible outcome
apo[, p := NA_real_] # Initialize new column in apo to store prob
for(i in 1:nrow(apo)){
  prob_game <- rep(1, 7)
  for(j in 1:7){
    p_win <- ifelse(hfi[j], pbh, pba)
    prob_game[j] <- case_when(
        apo[i,j,with=FALSE] == "W" ~ p_win
      , apo[i,j,with=FALSE] == "L" ~ 1 - p_win
      , TRUE ~ 1
    )
  }
  apo[i, p := prod(prob_game)] # Data.table syntax
}

# Sanity check: does sum(p) == 1?
# apo[, sum(p)] # This is data.table notation

# Probability of overall World Series outcomes

# when there is home field advantage
overall_win=apo[overall_outcome=="W"]
prob_with_adv=sum(overall_win$p)
prob_with_adv
```
![](/images/world1.PNG)

```r
# when there is no home field advantage
prob_without_adv=pnbinom(3,4,0.55)
prob_without_adv
```
![](/images/world2.PNG)

```r
prob_without_adv -  prob_with_adv
```
![](/images/world3.PNG)



_2. Calculate the same probabilities as the previous question by simulation._


Now, we simulate the process that we have done for the first question in order to mimic the reality. For each simulation, we randomly generate the results for 7 games and record as either "W" or "L". If Braves win four games or more, then they win the world series. After the repetition, we see that the same probability for the situation when there is home field advantage is about 0.60219, and the same probability for the situation when there is no home field advantage is about 0.60674. The difference is about 0.005. It is actually smaller than our probability in difference that from the first question. According to the simulation, there is no obvious difference between these two conditions.

```r
# When there is home field advantage
# hfi <- c(0,0,1,1,1,0,0) 
#{NYC, NYC, ATL, ATL, ATL, NYC, NYC}

# P_B, with home field advantage
# pb <- 0.55
# advantage_multiplier <- 1.1 # Set = 1 for no advantage
# pbh <- 0.55*advantage_multiplier
# pba <- 1 - (1 - 0.55)*advantage_multiplier
set.seed(12345)
hfi <- c(0, 0, 1, 1, 1, 0, 0)
advantage_multiplier <- 1.1 
pbh <- 0.55 * advantage_multiplier
pba <- 1 - (1 - 0.55) * advantage_multiplier

win_lose <- rep(NA, 100000)
for (j in 1:100000) {
  result <- rep(NA, 7)
for (i in 1:7) {
  p_win <- ifelse(hfi[i], pbh, pba)
  result[i] = rbinom(1, 1, p_win)
  if (sum(result, na.rm = TRUE) == 4) {
    win_lose[j] = 'W'
    break
  }
  else if (length(result[which(result == 0)]) == 4) {
    win_lose[j] = "L"
    break
  }
  next
  }
}
prob_simu_with_adv=mean(win_lose == 'W')
prob_simu_with_adv
```
![](/images/world4.PNG)

```r
# when there is no home field advantage
set.seed(12345)
advantage_multiplier <- 1
pbh <- 0.55 * advantage_multiplier
pba <- 1 - (1 - 0.55) * advantage_multiplier

#simulation for one game
win_lose <- rep(NA, 100000)
for (j in 1:100000) {
  result <- rep(NA, 7)
  for (i in 1:7) {
    p_win <- ifelse(hfi[i], pbh, pba)
    result[i] = rbinom(1, 1, p_win)
    if (sum(result, na.rm = TRUE) == 4) {
      win_lose[j] = 'W'
      break
  }
    else if (length(result[which(result == 0)]) == 4) {
      win_lose[j] = "L"
      break
    }
    next
  }
}
prob_simu_without_adv=mean(win_lose == 'W')
prob_simu_without_adv
```
![](/images/world5.PNG)



_3. What is the absolute and relative error for your simulation in the previous question?_


Now, we try to test the simulation errors that both on absolute and relative. According to what we know from the blog of "Monte Carlo Simulation Error" [link](https://github.com/marinayxh/Probability-and-Inference-Portfolio-Yue-Marina-/tree/master/02-monte-carlo-error), we could easily get these two types of errors. The absolute error is about 0.002 and the relative error here is about 0.003 when there is home field advantage. The absolute error is about 0.0015 and the relative error here is about 0.0025 when there is no home field advantage. 

```r
# Absolute error (with home field advantage)
abs_error_with_adv = abs(prob_simu_with_adv-prob_with_adv)
abs_error_with_adv
```
![](/images/world6.PNG)

```r
# Relative error (with home field advantage)
rel_error_with_adv=abs(prob_simu_with_adv-prob_with_adv)/prob_with_adv
rel_error_with_adv
```
![](/images/world7.PNG)

```r
# Absolute error (without home field advantage)
abs_error_without_adv=abs(prob_simu_without_adv-prob_without_adv)
abs_error_without_adv
```
![](/images/world8.PNG)

```r
# Relative error (without home field advantage)
rel_error_without_adv=abs(prob_simu_without_adv-prob_without_adv)/prob_without_adv
rel_error_without_adv
```
![](/images/world9.PNG)