# Final-Project
---
title: "Math 152: Statistical Theory - Homework 5"
author: "Jordan Leonard"
date: "Due: Friday, September 25, 2020, midnight PDT"
output: pdf_document
---


```{r warning=FALSE, comment=FALSE, message=FALSE, echo = FALSE}
knitr::opts_chunk$set(message=FALSE, warning=FALSE, fig.height=3, fig.width=5, 
                      fig.align = "center")
library(tidyverse)
library(knitr)
```


### Assignment

#### 1:  PodQ
Describe one thing you learned from someone in your pod this week (it could be: content, logistical help, background material, R information, etc.)  1-3 sentences.

*This week my pod assisted me with some of the Log and Partial Derivative equations when it came to solving for the MLE estimates.*


#### 2: 7.6.5

Suppose that $X_1, \ldots , X_n$ form a random sample from the uniform distribution on the interval [a, b], where both endpoints a and b are unknown. Find the MLE of the mean of the distribution.

#### 3: 7.6.6

Suppose that $X_1, \ldots , X_n$  form a random sample from a normal distribution for which both the mean and the variance are unknown. Find the MLE of the 0.95 quantile of the distribution, that is, of the point $\theta$ such that $P(X < \theta) = 0.95.$

#### 4: 7.6.8

Suppose that  $X_1, \ldots , X_n$ form a random sample from a gamma distribution for which the pdf is given by $$f(x | \alpha) = \frac{1}{\Gamma(\alpha)} x^{\alpha - 1}e^{-x} \ \ \ x > 0$$
(Eq. (7.6.2)) . Find the MLE of the ratio $\Gamma'(\alpha) / \Gamma(\alpha)$.  

#### 5: 7.6.9

Suppose that $X_1, \ldots , X_n$  form a random sample from
a gamma distribution for which both parameters $\alpha$ and $\beta$
are unknown. Find the MLE of $\alpha / \beta$.  Hint:  use the Gamma distribution pdf from the back of the book.  Also, for this particular problem, only one derivative is necessary.

#### 6. 7.6.21

Prove that the method of moments estimators of the
mean and variance of a normal distribution are also the
MLEs.


#### 7: 7.6.23

Suppose that $X_1, \ldots , X_n$ form a random sample from the beta distribution with parameters $\alpha$ and $\beta$. Let $\theta = (\alpha, \beta)$ be the vector parameter.  
a. Find the method of moments estimator for $\theta$.  
b. Show that the method of moments estimator is not the MLE. (Note: you do not need to actually solve for the MLE.)  


```{r, echo=FALSE}
include_graphics("HW5_Q2-4.pdf")
include_graphics("HW5_Q5-7.pdf")
```

#### 8: R - Tanks

Consider the tank problem encountered in class.  Your task at hand is to provide the best possible estimator for the true number of tanks.   Consider the R code below that analyzes two estimators ($2\overline{X}$ and the maximum of the sample).    You should **provide an argument (in words but using the evidence collected here) for your choice of estimator** using the following information:

* a comparison of at least 5 estimators (of your choice!)
* consideration of sample bias, sample variance, sample median, sample mean, sample MSE
* run the entire analysis twice.  Once with a sample size of n=5, once with a sample size of n=100

The MOM estimate which is calculated by $(2\overline{X})-1$ and the $2\overline{X}$ gave the most accurate representation of the total number of tanks for the samples of size 5 and 100. This makes sense since the mean does skew due to outliers in the sample so it is possible to capture the true count of tanks within the sample despite any small outliers.

Hint on the R code:  create new estimators one by one by adding them on to the two which I wrote.

1. make sure your new estimator has a place holder
2. use the `c()` function inside the `for()` loop to keep the computed estimator for each sample of tanks
3. `estimate` should now hold all the different estimates
4. ... same with `method` and finally `all.estimates`.
5. the rest of the code should just run.

```{r fig.width=7, fig.height=6}
# Keep the population size at 447
npop = 447
nsamp = 100  # change this to 100 for the second part of the analysis
reps = 10000

xbar2 = c()  # placeholder for your repeated sample statistics
sampmax = c()

## Own estimators (5)

med2 = c() # 2*median
xbar2_1 = c()  # 2*mean - 1 (MOM)
range2 = c() # 2*range (IQR)
xbar_2sd = c() # mean + 2*sd
confidence95_plus = c() # 95 confidence interval +
confidence95_minus = c() # 95 confidence interval -


for (i in 1:reps){
  mysample =  sample(1:npop,nsamp,replace=F)  # sample some tanks from the population
	xbar2 = c(xbar2, 2*mean(mysample))
  sampmax = c(sampmax, max(mysample))
  med2 = c(med2, 2*median(mysample))
  xbar2_1 = c(xbar2_1, 2*mean(mysample) - 1)
  range2 = c(range2, 2*(max(mysample) - min(mysample)))
  xbar_2sd = c(xbar_2sd, mean(mysample) + 2*sd(mysample))
  confidence95_plus = c(confidence95_plus, (mean(mysample) + 1.96*(sd(mysample)/sqrt((length(mysample))))))
  confidence95_minus = c(confidence95_minus, (mean(mysample) - 1.96*(sd(mysample)/sqrt((length(mysample))))))
}


estimate <- c(xbar2, sampmax, 
              med2, xbar2_1, range2, xbar_2sd, confidence95_plus, confidence95_minus)
method <- c(rep("2xbars", reps), rep("samplemax", reps), 
            rep("2median", reps), rep("2mean-1(MOM)", reps), rep("2range(IQR)", reps), rep("mean+2SD", reps), rep("95conf+", reps), rep("95conf-", reps))
all.estimates <- data.frame(estimate, method)


# below is some syntax with which you may be unfamiliar.  
# if you don't have the packages, you may need to run, for example:
# install.packages("tidyverse")

# also, if you create the statistics as above, all of the code below should 
# work nicely without you doing anything to it.

# please ask if you have question!


all.estimates %>%
  group_by(method) %>%
  summarize(mean = mean(estimate), median = median(estimate), bias = mean(estimate) - npop, 
            var = var(estimate), mse = (mean(estimate) - npop)^2 + var(estimate))
  
ggplot(all.estimates, aes(x = estimate)) + geom_histogram() +
  geom_vline(xintercept = npop) + facet_wrap(~method)


```
