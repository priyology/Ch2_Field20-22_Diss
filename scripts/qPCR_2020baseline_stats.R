########## ~ M. GIGAS 2020 Baseline qPCR Stats ==============

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

########## 2020 Cohort OsHV-1 Data ==============
OsHV1 <- read_csv("data/qPCR/qPCR_runs.csv")
glimpse(OsHV1)
summary(OsHV1)
tail(OsHV1)
View(OsHV1)

#### now remove NAs from data sheet
colSums(is.na(OsHV1)) #No relevant NAs
OsHV1 <- OsHV1 %>% 
  filter(Pool_Numb != "145")

### Year as character
OsHV1$Year <- as.character(OsHV1$Year)
is.character(OsHV1$Year)

### Sampling_Period as character
OsHV1$Sampling_Period <- as.character(OsHV1$Sampling_Period)
is.character(OsHV1$Sampling_Period)

### Cohort as character
OsHV1$Cohort <- as.character(OsHV1$Cohort)
is.character(OsHV1$Cohort)

#### *** 2020 Cohort Only *** ===== 

Baseline <- OsHV1 %>%
  filter(Year == "2020")

View(Baseline)

### Sampling_Period as factor
Baseline$Sampling_Period <-  as.factor(Baseline$Sampling_Period)
is.factor(Baseline$Sampling_Period)

#### *** 2020 v 2022 Cohort OsHV-1 by Year *** ===== 
Yr3Comp <- OsHV1 %>%
  filter(Cohort == "2020")

View(Yr3Comp)


##########################################
#### OsHV-1 Quantity 2020 Juveniles =====
#########################################

#### Model selection ====
#### m_null_2020: log_transform ~ 1  =====

m_null_2020 <- glm(log_transform ~ 1, family = gaussian(link = "identity"), data = Baseline)
summary(m_null_2020)

# Call:
#glm(formula = log_transform ~ 1, family = gaussian(link = "identity"), 
#    data = Baseline)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5686  -0.7761  -0.4364   0.1037   6.6887  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.7761     0.2013   3.856  0.00027 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 2.633915)
#
#Null deviance: 168.57  on 64  degrees of freedom
#Residual deviance: 168.57  on 64  degrees of freedom
#AIC: 250.4
#
#Number of Fisher Scoring iterations: 2

#### m1: log_transform ~ Site  =====

m1 <- glm(log_transform ~ Site, family = gaussian(link = "identity"), data = Baseline)
summary(m1)

# Call:
#glm(formula = log_transform ~ Site, family = gaussian(link = "identity"), 
#    data = Baseline)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0286  -0.7122   0.0153   0.3634   5.8785  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.5863     0.2448   6.480 1.59e-08 ***
#  SiteSouth    -1.6457     0.3489  -4.717 1.37e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.977334)
#
#Null deviance: 168.57  on 64  degrees of freedom
#Residual deviance: 124.57  on 63  degrees of freedom
#AIC: 232.74
#
#Number of Fisher Scoring iterations: 2


#### m2: log_transform ~ Sampling_Period  =====

m2 <- glm(log_transform ~ Sampling_Period, family = gaussian(link = "identity"), data = Baseline)
summary(m2)

#Call:
#  glm(formula = log_transform ~ Sampling_Period, family = gaussian(link = "identity"), 
#      data = Baseline)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7938  -0.9019  -0.5224   0.1441   6.4426  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        1.0222     0.2813   3.634 0.000563 ***
#  Sampling_Period4  -0.4998     0.4009  -1.247 0.217112    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 2.611297)
#
#Null deviance: 168.57  on 64  degrees of freedom
#Residual deviance: 164.51  on 63  degrees of freedom
#AIC: 250.82
#
#Number of Fisher Scoring iterations: 2


#### m3: log_transform ~ Site + (1|Bag)  =====

m3 <- lmer(log_transform ~ Site + (1|Bag), data = Baseline)
summary(m3)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log_transform ~ Site + (1 | Bag)
#Data: Baseline
#
#REML criterion at convergence: 214
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.2089 -0.4807  0.0532  0.3150  3.7379 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Bag      (Intercept) 0.8508   0.9224  
#Residual             1.3152   1.1468  
#Number of obs: 65, groups:  Bag, 8
#
#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)  
#(Intercept)   1.6136     0.5026  5.9855   3.210   0.0184 *
#  SiteSouth    -1.6729     0.7116  6.0133  -2.351   0.0569 .
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr)
#SiteSouth -0.706


#### m4: log_transform ~ Site + Sampling_Period  =====

m4 <- glm(log_transform ~ Site + Sampling_Period, family = gaussian(link = "identity"), data = Baseline)
summary(m4)

# Call:
#glm(formula = log_transform ~ Site + Sampling_Period, family = gaussian(link = "identity"), 
#    data = Baseline)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8166  -0.6967  -0.1672   0.2969   5.6482  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)        1.8166     0.2955   6.148 6.26e-08 ***
#  SiteSouth         -1.6385     0.3465  -4.729 1.35e-05 ***
#  Sampling_Period4  -0.4750     0.3465  -1.371    0.175    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.950116)
#
#Null deviance: 168.57  on 64  degrees of freedom
#Residual deviance: 120.91  on 62  degrees of freedom
#AIC: 232.8
#
#Number of Fisher Scoring iterations: 2

#### m5: log_transform ~ Site + Sampling_Period + (1|Bag)  =====

m5 <- lmer(log_transform ~ Site + Sampling_Period + (1|Bag), data = Baseline)
summary(m5)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log_transform ~ Site + Sampling_Period + (1 | Bag)
#Data: Baseline
#
#REML criterion at convergence: 211.5
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.0416 -0.2723 -0.0255  0.2778  3.5771 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Bag      (Intercept) 0.8672   0.9312  
#Residual             1.2641   1.1243  
#Number of obs: 65, groups:  Bag, 8
#
#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)   
#(Intercept)        1.8581     0.5230  6.8764   3.552  0.00959 **
#  SiteSouth         -1.6661     0.7152  6.0137  -2.330  0.05858 . 
#Sampling_Period4  -0.5026     0.2791 56.0341  -1.801  0.07713 . 
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) SitSth
#SiteSouth   -0.681       
#Smplng_Prd4 -0.259 -0.005

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5)
BIC(m_null, m1, m2, m3, m4, m5)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m3), resid(m3))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m3))
qqline(resid(m3))

#### Density Plot of Residuals ===============
plot(density(resid(m3)))


#######################################
#### OsHV-1 Quantity 2020 - 2022 =====
#######################################


#### m_null_Yr3Comp: log_transform ~ 1  =====

m_null_Yr3Comp <- glm(log_transform ~ 1, family = gaussian(link = "identity"), data = Yr3Comp)
summary(m_null_Yr3Comp)

#Call:
#glm(formula = log_transform ~ 1, family = gaussian(link = "identity"), 
#    data = Yr3Comp)
#
#Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
#-1.4276  -0.6352  -0.3470   0.1812   6.8296  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   0.6352     0.1482   4.285 4.49e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 2.043622)
#
#Null deviance: 188.01  on 92  degrees of freedom
#Residual deviance: 188.01  on 92  degrees of freedom
#AIC: 333.39
#
#Number of Fisher Scoring iterations: 2

#### m6: log_transform ~ Year/Sampling_Period =====

m6 <- glm(log_transform ~ Year/Sampling_Period, family = gaussian(link = "identity"), data = Yr3Comp)
summary(m6)

#Call:
#  glm(formula = log_transform ~ Year/Sampling_Period, family = gaussian(link = "identity"), 
#      data = Yr3Comp)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7938  -0.7226  -0.3598   0.1335   6.4426  
#
#Coefficients: (8 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 1.0222     0.2473   4.133 8.08e-05 ***
#  Year2021                   -0.7122     0.3812  -1.868    0.065 .  
#Year2022                   -0.7267     0.7522  -0.966    0.337    
#Year2020:Sampling_Period4  -0.4998     0.3525  -1.418    0.160    
#Year2021:Sampling_Period4       NA         NA      NA       NA    
#Year2022:Sampling_Period4       NA         NA      NA       NA    
#Year2020:Sampling_Period5       NA         NA      NA       NA    
#Year2021:Sampling_Period5       NA         NA      NA       NA    
#Year2022:Sampling_Period5       NA         NA      NA       NA    
#Year2020:Sampling_Period6       NA         NA      NA       NA    
#Year2021:Sampling_Period6       NA         NA      NA       NA    
#Year2022:Sampling_Period6       NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 2.018703)
#
#Null deviance: 188.01  on 92  degrees of freedom
#Residual deviance: 179.66  on 89  degrees of freedom
#AIC: 335.16
#
#Number of Fisher Scoring iterations: 2

#### m7: log_transform ~ Site  =====
m7 <- glm(log_transform ~ Site, family = gaussian(link = "identity"), data = Yr3Comp)
summary(m7)

#Call:
#  glm(formula = log_transform ~ Site, family = gaussian(link = "identity"), 
#      data = Yr3Comp)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7176  -0.6459   0.0240   0.3263   6.1896  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   1.2753     0.1807   7.058 3.25e-10 ***
#  SiteSouth    -1.3529     0.2627  -5.150 1.50e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.599784)
#
#Null deviance: 188.01  on 92  degrees of freedom
#Residual deviance: 145.58  on 91  degrees of freedom
#AIC: 311.6
#
#Number of Fisher Scoring iterations: 2

#### m8: log_transform ~ Year/Sampling_Period + Site  =====
m8 <- glm(log_transform ~ Year/Sampling_Period + Site, family = gaussian(link = "identity"), data = Yr3Comp)
summary(m8)

#Call:
#glm(formula = log_transform ~ Year/Sampling_Period + Site, family = gaussian(link = "identity"), 
#    data = Yr3Comp)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7165  -0.6231  -0.1152   0.3293   5.7483  
#
#Coefficients: (8 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 1.7165     0.2495   6.879 8.42e-10 ***
#  Year2021                   -0.6905     0.3311  -2.085   0.0400 *  
#  Year2022                   -1.4209     0.6657  -2.135   0.0356 *  
#  SiteSouth                  -1.4319     0.2617  -5.472 4.14e-07 ***
#  Year2020:Sampling_Period4  -0.4781     0.3062  -1.561   0.1220    
#Year2021:Sampling_Period4       NA         NA      NA       NA    
#Year2022:Sampling_Period4       NA         NA      NA       NA    
#Year2020:Sampling_Period5       NA         NA      NA       NA    
#Year2021:Sampling_Period5       NA         NA      NA       NA    
#Year2022:Sampling_Period5       NA         NA      NA       NA    
#Year2020:Sampling_Period6       NA         NA      NA       NA    
#Year2021:Sampling_Period6       NA         NA      NA       NA    
#Year2022:Sampling_Period6       NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.523377)
#
#Null deviance: 188.01  on 92  degrees of freedom
#Residual deviance: 134.06  on 88  degrees of freedom
#AIC: 309.93
#
#Number of Fisher Scoring iterations: 2

#### m9: log_transform ~ Year + Site + + (1|Bag)  =====
m9 <- glm(log_transform ~ Year/Sampling_Period + Site + (1|Bag), family = gaussian(link = "identity"), data = Yr3Comp)
summary(m9)

# Call:
#glm(formula = log_transform ~ Year/Sampling_Period + Site + (1 | 
#                                                               Bag), family = gaussian(link = "identity"), data = Yr3Comp)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7165  -0.6231  -0.1152   0.3293   5.7483  
#
#Coefficients: (9 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 1.7165     0.2495   6.879 8.42e-10 ***
#  Year2021                   -0.6905     0.3311  -2.085   0.0400 *  
#  Year2022                   -1.4209     0.6657  -2.135   0.0356 *  
#  SiteSouth                  -1.4319     0.2617  -5.472 4.14e-07 ***
#  1 | BagTRUE                     NA         NA      NA       NA    
#Year2020:Sampling_Period4  -0.4781     0.3062  -1.561   0.1220    
#Year2021:Sampling_Period4       NA         NA      NA       NA    
#Year2022:Sampling_Period4       NA         NA      NA       NA    
#Year2020:Sampling_Period5       NA         NA      NA       NA    
#Year2021:Sampling_Period5       NA         NA      NA       NA    
#Year2022:Sampling_Period5       NA         NA      NA       NA    
#Year2020:Sampling_Period6       NA         NA      NA       NA    
#Year2021:Sampling_Period6       NA         NA      NA       NA    
#Year2022:Sampling_Period6       NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.523377)
#
#Null deviance: 188.01  on 92  degrees of freedom
#Residual deviance: 134.06  on 88  degrees of freedom
#AIC: 309.93
#
#Number of Fisher Scoring iterations: 2

#### AIC/BIC Scores ===============
AIC(m_null_Yr3Comp, m6, m7, m8, m9)
BIC(m_null_Yr3Comp, m6, m7, m8, m9)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m10), resid(m10))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m10))
qqline(resid(m10))

#### Density Plot of Residuals ===============
plot(density(resid(m10)))


###########################
#### OsHV-1 Infection =====
###########################

#### Model selection ====
#### m_null_binom: OsHV1 ~ 1  =====

m_null_binom <- glm(OsHV1 ~ 1, family = binomial(link = "logit"), data = Baseline)
summary(m_null_binom)

# > summary(m_null_binom)
#Call:
#  glm(formula = OsHV1 ~ 1, family = binomial(link = "logit"), data = Baseline)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6559  -0.6559  -0.6559  -0.6559   1.8123  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.4271     0.2625  -5.437 5.41e-08 ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 91.387  on 92  degrees of freedom
#Residual deviance: 91.387  on 92  degrees of freedom
#AIC: 93.387
#
#Number of Fisher Scoring iterations: 4

#### m7: OsHV1 ~ Site  =====

m7 <- glm(OsHV1 ~ Site, family = binomial(link = "logit"), data = Baseline)
summary(m7)

#Call:
#  glm(formula = OsHV1 ~ Site, family = binomial(link = "logit"), 
#      data = Baseline)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-0.95690  -0.95690  -0.00008  -0.00008   1.41524  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)  
#(Intercept)   -0.5436     0.2963  -1.834   0.0666 .
#SiteSouth    -19.0225  1621.2285  -0.012   0.9906  
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 91.387  on 92  degrees of freedom
#Residual deviance: 64.438  on 91  degrees of freedom
#AIC: 68.438
#
#Number of Fisher Scoring iterations: 18

#### m8: OsHV1 ~ Year  =====

m8 <- glm(OsHV1 ~ Year, family = binomial(link = "logit"), data = Baseline)
summary(m8)

# Call:
#glm(formula = OsHV1 ~ Year, family = binomial(link = "logit"), 
#    data = Baseline)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6965  -0.6965  -0.6965  -0.6039   1.8930  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   -1.2928     0.3017  -4.285 1.83e-05 ***
#  Year2021      -0.3167     0.6253  -0.506    0.613    
#Year2022     -15.2733  1199.7724  -0.013    0.990    
#---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 91.387  on 92  degrees of freedom
#Residual deviance: 89.357  on 90  degrees of freedom
#AIC: 95.357
#
#Number of Fisher Scoring iterations: 15

#### m9: OsHV1 ~ Site + Year  =====

m9 <- glm(OsHV1 ~ Site + Year, family = binomial(link = "logit"), data = Baseline)
summary(m9)

# Call:
#glm(formula = OsHV1 ~ Site + Year, family = binomial(link = "logit"), 
#    data = Baseline)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.05078  -0.90052  -0.00008  -0.00007   1.48230  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)   -0.3054     0.3522  -0.867    0.386
#SiteSouth    -19.1642  1617.3754  -0.012    0.991
#Year2021      -0.3878     0.7064  -0.549    0.583
#Year2022     -19.2607  5377.0065  -0.004    0.997
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 91.387  on 92  degrees of freedom
#Residual deviance: 60.264  on 89  degrees of freedom
#AIC: 68.264
#
#Number of Fisher Scoring iterations: 18
