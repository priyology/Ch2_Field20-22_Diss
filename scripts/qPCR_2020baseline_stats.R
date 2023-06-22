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

### Year as character
OsHV1$Year <- as.character(OsHV1$Year)
is.character(OsHV1$Year)

### Sampling_Period as character
OsHV1$Sampling_Period <- as.character(OsHV1$Sampling_Period)
is.character(OsHV1$Sampling_Period)

### Cohort as character
OsHV1$Cohort <- as.character(OsHV1$Cohort)
is.character(OsHV1$Cohort)

#### *** 2020 v 2022 Cohort OsHV-1 by Year *** ===== 
Baseline <- OsHV1 %>%
  filter(Cohort == "2020")

View(Baseline)

#### Model selection ====
#### m_null: log_transform ~ 1  =====

m_null <- glm(log_transform ~ 1, family = gaussian(link = "identity"), data = Baseline)
summary(m_null)

# Call:
#  glm(formula = log_transform ~ 1, family = gaussian(link = "identity"), 
#      data = Baseline)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.4276  -0.6352  -0.3470   0.1812   6.8296  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.6352     0.1482   4.285 4.49e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 2.043622)
#
# Null deviance: 188.01  on 92  degrees of freedom
# Residual deviance: 188.01  on 92  degrees of freedom
# AIC: 333.39
#
# Number of Fisher Scoring iterations: 2

#### m1: log_transform ~ Site  =====

m1 <- glm(log_transform ~ Site, family = gaussian(link = "identity"), data = Baseline)
summary(m1)

# Call:
# glm(formula = log_transform ~ Site, family = gaussian(link = "identity"), 
#    data = Baseline)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.7176  -0.6459   0.0240   0.3263   6.1896  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.2753     0.1807   7.058 3.25e-10 ***
#  SiteSouth    -1.3529     0.2627  -5.150 1.50e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.599784)
#
# Null deviance: 188.01  on 92  degrees of freedom
# Residual deviance: 145.58  on 91  degrees of freedom
# AIC: 311.6
#
# Number of Fisher Scoring iterations: 2

#### m2: log_transform ~ Year  =====

m2 <- glm(log_transform ~ Year, family = gaussian(link = "identity"), data = Baseline)
summary(m2)

# Call:
#  glm(formula = log_transform ~ Year, family = gaussian(link = "identity"), 
#      data = Baseline)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.5686  -0.7652  -0.3234   0.1037   6.6887  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   0.7761     0.1772   4.380  3.2e-05 ***
#  Year2021     -0.4661     0.3413  -1.366    0.175    
# Year2022     -0.4806     0.7360  -0.653    0.515    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 2.041371)
#
# Null deviance: 188.01  on 92  degrees of freedom
# Residual deviance: 183.72  on 90  degrees of freedom
# AIC: 335.24
#
# Number of Fisher Scoring iterations: 2

#### m3: log_transform ~ Site + Year  =====

m3 <- glm(log_transform ~ Site + Year, family = gaussian(link = "identity"), data = Baseline)
summary(m3)

# Call:
# glm(formula = log_transform ~ Site + Year, family = gaussian(link = "identity"), 
#    data = Baseline)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -1.9260  -0.6590  -0.0465   0.4048   5.9811  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.4837     0.2017   7.356 8.91e-11 ***
#  SiteSouth    -1.4372     0.2638  -5.448 4.48e-07 ***
#  Year2021     -0.4550     0.2972  -1.531   0.1293    
# Year2022     -1.1882     0.6540  -1.817   0.0726 .  
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 1.547985)
#
# Null deviance: 188.01  on 92  degrees of freedom
# Residual deviance: 137.77  on 89  degrees of freedom
# AIC: 310.47
#
# Number of Fisher Scoring iterations: 2

#### m4: log_transform ~ Site + Year + (1|Bag)  =====

m4 <- lmer(log_transform ~ Site + Year + (1|Bag), data = Baseline)
summary(m4)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method [lmerModLmerTest
#]
#Formula: log_transform ~ Site + Year + (1 | Bag)
#Data: Baseline
#
#REML criterion at convergence: 290
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.3104 -0.4812 -0.0216  0.3271  4.4853 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Bag      (Intercept) 0.4748   0.689   
#Residual             1.1893   1.091   
#Number of obs: 93, groups:  Bag, 9
#
#Fixed effects:
#  Estimate Std. Error      df t value Pr(>|t|)   
#(Intercept)   1.5030     0.3873  6.4012   3.881   0.0072 **
#  SiteSouth    -1.4535     0.5393  6.0196  -2.695   0.0357 * 
#  Year2021     -0.4662     0.2605 83.0275  -1.790   0.0772 . 
# Year2022     -1.2075     0.9602  9.6275  -1.257   0.2382   
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# Correlation of Fixed Effects:
#  (Intr) SitSth Yr2021
# SiteSouth -0.694              
#Year2021  -0.180 -0.003       
#Year2022  -0.403  0.280  0.072

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4)
BIC(m_null, m1, m2, m3, m4)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m4), resid(m4))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m4))
qqline(resid(m4))

#### Density Plot of Residuals ===============
plot(density(resid(m4)))
