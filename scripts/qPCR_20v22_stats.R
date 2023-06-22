########## M. gigas 2020 v. 2022 qPCR Stats ==============

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
colSums(is.na(OsHV1)) #No NAs

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
OsHV1_20v22 <-OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort)

View(OsHV1_20v22)

#### Model selection ====
#### m_null_noLog: Copies_per_mgTissue ~ 1  =====

m_null_noLog <- glm(Copies_per_mgTissue ~ 1, family = gaussian(link = "identity"), data = OsHV1_20v22)
summary(m_null_noLog)

#Call:
#  glm(formula = Copies_per_mgTissue ~ 1, family = gaussian(link = "identity"), 
#      data = OsHV1_20v22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-942359  -942357  -942322  -754387  6062135  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)   942360     867586   1.086    0.313
#
#(Dispersion parameter for gaussian family taken to be 6.021644e+12)
#
#Null deviance: 4.2152e+13  on 7  degrees of freedom
#Residual deviance: 4.2152e+13  on 7  degrees of freedom
#AIC: 261.05
#
#Number of Fisher Scoring iterations: 2

#### m2: Copies_per_mgTissue ~ Cohort  =====

m2 <- glm(Copies_per_mgTissue ~ Cohort, family = Gamma(link = "identity"), data = OsHV1_20v22)
summary(m2)

# Call:
#glm(formula = Copies_per_mgTissue ~ Cohort, family = Gamma(link = "identity"), 
#    data = OsHV1_20v22)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-4.3160  -1.7416  -1.2271  -0.0042   1.6755  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) 4.464e+00  3.628e+00   1.231    0.265
#Cohort2022  1.885e+06  1.532e+06   1.231    0.265
#
#(Dispersion parameter for Gamma family taken to be 2.641545)
#
#Null deviance: 125.742  on 7  degrees of freedom
#Residual deviance:  33.207  on 6  degrees of freedom
#AIC: 138.93
#
#Number of Fisher Scoring iterations: 3

#### AIC/BIC Scores ===============
AIC(m_null_noLog, m2)
BIC(m_null_noLog, m2)


### Model selection -- log_transformed ====
#### m_null: log_transform ~ 1  =====

m_null <- glm(log_transform ~ 1, family = gaussian(link = "identity"), data = OsHV1_20v22)
summary(m_null)

# Call:
# glm(formula = log_transform ~ 1, family = gaussian(link = "identity"), 
#    data = OsHV1_20v22)
#
# Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
# -2.908  -2.249  -1.093   2.623   4.284  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)  
# (Intercept)    2.561      1.002   2.555   0.0378 *
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 8.038216)
#
# Null deviance: 56.268  on 7  degrees of freedom
# Residual deviance: 56.268  on 7  degrees of freedom
# AIC: 42.308
#
# Number of Fisher Scoring iterations: 2

#### m_null: log_transform ~ 1  =====

m1 <- glm(log_transform ~ Cohort, family = gaussian(link = "identity"), data = OsHV1_20v22)
summary(m1)

# Call:
#glm(formula = log_transform ~ Cohort, family = gaussian(link = "identity"), 
#    data = OsHV1_20v22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.0307  -0.4124   0.1717   0.8128   2.0186  
#
#Coefficients:
# Estimate Std. Error t value Pr(>|t|)   
#(Intercept)   0.2955     0.7959   0.371  0.72316   
#Cohort2022    4.5312     1.1256   4.026  0.00692 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 2.534016)
#
#Null deviance: 56.268  on 7  degrees of freedom
#Residual deviance: 15.204  on 6  degrees of freedom
#AIC: 33.84
#
#Number of Fisher Scoring iterations: 2

#### AIC/BIC Scores ===============
AIC(m_null, m1)
BIC(m_null, m1)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m1), resid(m1))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m1))
qqline(resid(m1))

#### Density Plot of Residuals ===============
plot(density(resid(m1)))
