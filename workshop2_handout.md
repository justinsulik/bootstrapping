# Workshop 2
September 18, 2017  





# Bootstrapping 2

Recap:

- What is a confidence interval?

- How do you bootstrap one?

- Why bootstrap?

## Outline

- The R `boot` package
- Difference in means
- Regression parameters & other statistics
- Why care about confidence intervals? "The new statistics"
- A couple of confusable, related methods

## Outcomes

- Bootstrap the difference between means (between- and within-subjects)
- Write a custom function to bootstrap
- Find different kinds of CIs
- Provide CIs for a regression coefficient

# The R boot package


```r
library(tidyverse)
library(boot)
library(lme4)
```


`boot(data,statistic=myFunction,R=10000)`

The boot function handles the bootstrap resampling for you. You just need to write a (simple) function telling it what you want bootstrapped

The function you pass to `statistic` in `boot()` needs 2 things:

- data

- a sampling index

## Sampling index


```r
data <- c(3, 6, 7, 1, 9)

data[1]
```

```
## [1] 3
```

```r
data[2]
```

```
## [1] 6
```

```r
data[c(1,2)]
```

```
## [1] 3 6
```


```r
data[c(5,4,3,2,1)]
```

```
## [1] 9 1 7 6 3
```

```r
i <- sample(1:5,5,replace=TRUE)
i
```

```
## [1] 5 3 3 3 5
```

```r
data[i]
```

```
## [1] 9 7 7 7 9
```

## Statistic

The `boot` function generates the sampling index `i`. You just need to pass it to your custom function


```r
myFun <- function(data,i){
  bootData <- data[i] #Resample
  M <- mean(bootData) #Take the man
  return(M) #Return it to the boot function
}
```

More simply:


```r
myFun <- function(data,i){
  return(mean(data[i]))
}
```


```r
out <- boot(data,myFun,1000)
out
```

```
## 
## ORDINARY NONPARAMETRIC BOOTSTRAP
## 
## 
## Call:
## boot(data = data, statistic = myFun, R = 1000)
## 
## 
## Bootstrap Statistics :
##     original  bias    std. error
## t1*      5.2 -0.0082    1.228675
```

Have a look at this `boot` object, e.g. 

`names(out)`

`out$t`

mean: 5.1918



```r
data %>% 
  data.frame(x=.) %>% 
  ggplot(aes(x=x)) + 
  geom_dotplot() + 
  geom_density(data=out$t %>% data.frame(x=.))
```

![](workshop2_handout_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

# Confidence intervals

## Confidence intervals 


```r
boot.ci(out) #default: conf=0.95
```

```
## Warning in boot.ci(out): bootstrap variances needed for studentized
## intervals
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = out)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   ( 2.800,  7.616 )   ( 3.000,  7.800 )  
## 
## Level     Percentile            BCa          
## 95%   ( 2.600,  7.400 )   ( 2.599,  7.400 )  
## Calculations and Intervals on Original Scale
```

## What's the difference?

- Some of these just use the bootstrap distribution, 
- Others add in additional info (like error or distributional info)

## Simple

- Percentile: just look at quantiles of bootstrap distribution
- Basic: uses s.e. from `out`

## Quite fancy

- Normal/studentized include additional info about distribution (normal/t) 
- Studentized also needs estimated variance of the boot statistic

## Studentized 

Your boot function has to calculate the bootstrap variance


```r
myFun2 <- function(data, i) {
  boot.sample <- data[i]
  m <- mean(boot.sample)
  n <- length(boot.sample)
  sem <- sd(boot.sample)/sqrt(n)
  c(m, sem^2) 
}
out <- boot(data,myFun2,1000)
boot.ci(out)
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = out)
## 
## Intervals : 
## Level      Normal              Basic             Studentized     
## 95%   ( 2.630,  7.786 )   ( 2.800,  7.800 )   (-1.797,  8.940 )  
## 
## Level     Percentile            BCa          
## 95%   ( 2.6,  7.6 )   ( 2.4,  7.6 )  
## Calculations and Intervals on Original Scale
```

## Very fancy

- BCa - bias corrected & accelerated
- two extra parameters:
    - $z_0$ for skewness
    - $a$ for acceleration, or change in variance
    - If $z_0 = a = 0$, same as percentile
    - If bootstrap histogram normal, same as basic
- But **you** don't have to calculate them!
- Let the computer do the work

## When to use?

- Use BCa wherever possible (powerful and general)
- Percentile: not good with skew data
- Studentized: needs extra formula for error

![](carpenter.jpg){ width=50px }

## Confused?

Take home message about different methods

- Very simple: just use bootstrap data
- Fancier: include extra info for error/bias/etc.

- For large N/normal data sets, doesn't matter
- For smaller N/skew sets, does matter
- BCa is fancy, 
    - BUT doesn't rely on you doing anything else
- No hard and fast rules
- If you get very different results, you should start wondering 'why?' rather than looking for smallest interval

# Let's try (1)

Create different random data. Try with 'mean', 'median'


```r
#Normal
data1 <- rnorm(15,10,2)
data2 <- rnorm(6,10,10)
data3 <- rnorm(200,10,7)

#Skew
data4 <- rbeta(10, 8, 2)
data5 <- rbeta(500, 8, 2)
```

## Let's try (2)

Use the `cd4` dataset that comes with the `boot` package

Provide 95% bootstrapped CIs for the correlation between 'baseline' and 'oneyear'

(Hint: start with `cor()` and work out how to extract the relevant value from the output table)


```r
bootR <- function(data,i){
  bootData <- data[i,]
  r <- cor(bootData)[1,2]
  return(r)
}

out <- boot(cd4,bootR,1000)
boot.ci(out)
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = out)
## 
## Intervals : 
## Level      Normal              Basic         
## 95%   ( 0.5484,  0.9130 )   ( 0.5758,  0.9477 )  
## 
## Level     Percentile            BCa          
## 95%   ( 0.4986,  0.8705 )   ( 0.5051,  0.8732 )  
## Calculations and Intervals on Original Scale
```

(see [http://www.jstor.org/stable/2246110](Diciccio & Efron, 1984, http://www.jstor.org/stable/2246110))

# Difference in means

## Between-subject design

Simulate some data

(Don't need equal samples!)

(Just using them so I can look at within-subject design)


```r
a = rnorm(100,0,3)
b = rnorm(100,1,2) 

data <- data.frame(groups=c(rep('a',length(a)), rep('b',length(b))),
                   val=append(a,b))
```

### Check we understand the data

`a` and `b` are from different distributions


```r
data %>% 
  ggplot(aes(x=val,color=groups)) + 
  geom_density()
```

![](workshop2_handout_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

### Ok, so how to bootstrap?

Bootstrapping means doesn't always tell us much


```r
getCIs <- function(data){
  bootout <- boot(data,myFun,1000)
  CIs <- boot.ci(bootout, type='bca')
  lower <- CIs$'bca'[4]
  upper <- CIs$'bca'[5]
  return(sprintf("[%.3f, %.3f]", lower, upper))
}

getCIs(a)
```

```
## [1] "[-0.768, 0.303]"
```

```r
getCIs(b)
```

```
## [1] "[0.639, 1.545]"
```

Between-subjects: boot function should

- resample from each group

- take means of each

- return difference in means



```r
diffMeans <- function(dat, i) {
    groupDiffs <- aggregate(val ~ groups, data=dat, subset=i, FUN=mean)
    -diff(groupDiffs$val)
}

out <- boot(data, statistic=diffMeans, strata=data$groups, 1000)
boot.ci(out, type=c("bca"))
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = out, type = c("bca"))
## 
## Intervals : 
## Level       BCa          
## 95%   (-2.053, -0.707 )  
## Calculations and Intervals on Original Scale
```

### Compare with t-test


```r
t.test(a,b, paired=FALSE)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  a and b
## t = -3.7994, df = 189.23, p-value = 0.0001954
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -2.0693484 -0.6549358
## sample estimates:
##  mean of x  mean of y 
## -0.2371858  1.1249563
```

## Within-subjects design 


```r
c = a+b
data = data.frame(a=a,b=b,c=c)

data %>% 
  ggplot(aes(x=a,y=c)) + 
  geom_point() + 
  stat_smooth(method=lm)
```

![](workshop2_handout_files/figure-html/unnamed-chunk-17-1.png)<!-- -->

### Check we understand the data


```r
data %>% 
  gather(group,val,c(a,c)) %>% 
  ggplot(aes(x=val,color=group)) + 
  geom_density()
```

![](workshop2_handout_files/figure-html/unnamed-chunk-18-1.png)<!-- -->


```r
getCIs(a)
```

```
## [1] "[-0.772, 0.257]"
```

```r
getCIs(c)
```

```
## [1] "[0.133, 1.648]"
```

- Should we just try the same thing between `a` and `c` as we just did between `a` and `b`?

- Nope, sampling separately loses the correlation

- We just want to know if the difference between `a` and `c` is likely to include 0

- Bootstrap `b`!


```r
getCIs(b)
```

```
## [1] "[0.667, 1.589]"
```

```r
t.test(c,a,paired=TRUE)
```

```
## 
## 	Paired t-test
## 
## data:  c and a
## t = 5.0095, df = 99, p-value = 2.386e-06
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  0.6793699 1.5705426
## sample estimates:
## mean of the differences 
##                1.124956
```

# Regression parameters

Want anything more complicated? Model it, THEN bootstrap

There's really not much to say. Just write a new function.


```r
bootLM <- function(data,i){
  bootData <- data[i,] # Because we're sampling rows from dataframe
  bootMod <- update(mod, data=bootData)
  return(coef(bootMod))
}

mod <- lm(c~a,data)
out <- boot(data,bootLM,1000)
boot.ci(out,index=2,type='bca') #because `a` is the 2nd row in the model summary
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = out, type = "bca", index = 2)
## 
## Intervals : 
## Level       BCa          
## 95%   ( 0.929,  1.247 )  
## Calculations and Intervals on Original Scale
```


```r
mod.lmer <- lmer(Sepal.Width ~ Petal.Width + (1 + Petal.Width|Species), data = iris)
fixef(mod.lmer)
```

```
## (Intercept) Petal.Width 
##   2.1406698   0.7820251
```


```r
mySumm <- function(mod) { #No need for an index here!
  fixef(mod)[[2]] #For 2nd row in summary - fixef(mod)
}

bootMer(mod.lmer, mySumm, nsim = 500) %>% boot.ci(type='basic') 
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 500 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = ., type = "basic")
## 
## Intervals : 
## Level      Basic         
## 95%   ( 0.4209,  1.1385 )  
## Calculations and Intervals on Original Scale
```

```r
confint(mod.lmer, parm='Petal.Width', method='boot', nsim=500, boot.type='basic')
```

```
##                2.5 %   97.5 %
## Petal.Width 0.421604 1.094898
```

# Other stats 

Correlations, $R^2$s, whatever you need...


```r
bootLM <- function(data,i){
  bootData <- data[i,] # Because we're sampling rows from dataframe
  bootMod <- update(mod, data=bootData)
  return(summary(bootMod)$r.squared)
}

out <- boot(data,bootLM,1000)
boot.ci(out,type='bca')
```

```
## BOOTSTRAP CONFIDENCE INTERVAL CALCULATIONS
## Based on 1000 bootstrap replicates
## 
## CALL : 
## boot.ci(boot.out = out, type = "bca")
## 
## Intervals : 
## Level       BCa          
## 95%   ( 0.5272,  0.7338 )  
## Calculations and Intervals on Original Scale
```

## A couple of reminders

- data[i] for vectors, data[i,] for dataframes
- stratified bootstrap for between-groups comparison
    - long dataframe
- update(model,data=data[,i]) for models
- Use BCa (but not available for lmers)

# Why care about confidence intervals? 


## "The new statistics"

- p-value: probability of finding observed (or more extreme) results when null hypothesis is true

- CI: if the experiment were repeated endlessly, 95% of the CIs would contain the true mean

- [video](https://www.youtube.com/watch?v=wb0rnZBlcRg)

- [APS article](http://journals.sagepub.com/doi/abs/10.1177/0956797613504966)

- Psychological Science (APS): shift from NHST to effect sizes, confidence intervals, etc.

LMER p-values from worst to best [according to lme4 designers](http://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#p-values-mcmc-and-parametric-bootstrap):

- Wald Z-tests

- For balanced, nested LMMs where degrees of freedom can be computed according to classical rules: Wald t-tests

- Likelihood ratio test (anova) 

- Markov chain Monte Carlo (MCMC) or parametric bootstrap confidence intervals


## Reporting

Simplest: M=23.3, 95% CI [17.2, 43.7]

Simple: M=23.3, bootstrapped 95% CI [17.2, 43.7]

Fancy: M=23.3, 95% CI [17.2, 43.7] (Bias-corrected & accelerated bootstrap, DiCiccio & Efron, 1996)

You can include a p-value if you want, but just reporting a CI that doesn't include 0 is just as good

# A couple of confusable, related methods

## Parametric vs non-parametric 

Non-parametric bootstrap: just resample with replacement (what we've been doing)

Parametric bootstrap: estimate distribution, and use those parameters to generate samples

`x.star <- rnorm(length(x), mean = mean(x), sd = sd(x))`

    - Can have more power
    
    - But choice of model can be arbitrary
    
## Bootstrap vs. random permutation test

Bootstrap 

   - just resample with replacement (what we've been doing)

Permutation test 

   - pool data 
   - divide pooled data into all possible permutations 

Random permutation test 

   - divide pooled data into random permutations

## Bootstrap vs. random permutation test


```r
a <- rnorm(10, 2, 3)
b <- rnorm(10, 0, 2)

pool <- append(a,b)

permutationMeans <- numeric(1000) 
for(i in 1:1000){
   permutationSample <- sample(pool, length(pool))
   mean1 <- mean(permutationSample[1:5])
   mean2 <- mean(permutationSample[6:10])
   permutationMeans[i] <- mean2-mean1
}
```


```r
permutationMeans %>% 
  data.frame(x=.) %>% 
  ggplot(aes(x=x)) + 
  geom_density() + 
  geom_vline(xintercept=mean(a)-mean(b), color='red')
```

![](workshop2_handout_files/figure-html/unnamed-chunk-26-1.png)<!-- -->

By pooling data 

- Behaving as if samples drawn from same population

- Seeing how extreme observed value is

- So more like null hypothesis-testing

# When can't you use it?

- The whole point is that it's general and robust
- Still, you can't use it for:
    - Extreme values (min/max)
    - Statistics highly influenced by tails
- It can't save you from low N
- It's for estimating confidence around statistics
- For fancy grouping/nesting, model then bootstrap

# Outcomes (revisited)

- Bootstrap the difference between means (between- and within-subjects)
- Write a custom function to bootstrap
- Find different kinds of CIs
- Provide CIs for a regression coefficient
