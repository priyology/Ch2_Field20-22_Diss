########## M. gigas 2020 qPCR Stats ==============

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

### Year_Sampled as character
OsHV1$Year_Sampled <- as.character(OsHV1$Year_Sampled)
is.character(OsHV1$Year_Sampled)

### Sampling_Period as character
OsHV1$Sampling_Period <- as.character(OsHV1$Sampling_Period)
is.character(OsHV1$Sampling_Period)

### Cohort as character
OsHV1$Cohort <- as.character(OsHV1$Cohort)
is.character(OsHV1$Cohort)

#### *** 2020 Cohort Stats by Year *** ===== 
OsHV1_2020 <- OsHV1 %>%
  filter(Cohort == "2020",
         Year_Sampled == "2020")

View(OsHV1_2020)

#### m_null_2020: OsHV1 ~ 1 ===============
m_null_2020 <- glm(OsHV1 ~ 1, family = binomial(link = "logit"), data = OsHV1_2020)
summary(m_null_2020)

# Call:
#glm(formula = OsHV1 ~ 1, family = binomial(link = "logit"), data = OsHV1_2020)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6965  -0.6965  -0.6965  -0.6965   1.7523  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -1.2928     0.3017  -4.285 1.83e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 106.87  on 125  degrees of freedom
#Residual deviance: 106.87  on 125  degrees of freedom
#AIC: 108.87
#
#Number of Fisher Scoring iterations: 4

#### m1: OsHV1 ~ Site ===============
m1 <- glm(OsHV1 ~ Site, family = binomial(link = "logit"), data = OsHV1_2020)
summary(m1)

#Call:
#  glm(formula = OsHV1 ~ Site, family = binomial(link = "logit"), 
#      data = OsHV1_2020)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.05078  -1.05078  -0.00008  -0.00008   1.30954  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)   -0.3054     0.3522  -0.867    0.386
#SiteSouth    -19.2607  1901.0589  -0.010    0.992
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 67.731  on 64  degrees of freedom
#Residual deviance: 44.987  on 63  degrees of freedom
#AIC: 48.987
#
#Number of Fisher Scoring iterations: 18


#### *** 2020 v 2022 Cohort Stats by Year *** =====
OsHV1_2020v2022 <- OsHV1 %>%
  filter(Sample_Date == "27-Aug-22")

OsHV1_2020v2022

#### m_null_20_22: OsHV1 ~ 1 ===============
m_null_20_22 <- glm(OsHV1 ~ 1, family = binomial(link = "logit"), data = OsHV1_2020v2022)
summary(m_null_20_22)

#Call:
#  glm(formula = OsHV1 ~ 1, family = binomial(link = "logit"), data = OsHV1_2020v2022)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-1.177  -1.177   0.000   1.177   1.177  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept) 2.355e-16  7.071e-01       0        1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 11.09  on 7  degrees of freedom
#Residual deviance: 11.09  on 7  degrees of freedom
#AIC: 13.09
#
#Number of Fisher Scoring iterations: 2

#### m2: OsHV1 ~ Cohort ===============
#m_null_20_22 <- glm(OsHV1 ~ Cohort, family = binomial(link = "logit"), data = OsHV1_2020v2022)
#summary(m_null_20_22)
#
# Call:
#glm(formula = OsHV1 ~ Cohort, family = binomial(link = "logit"), 
#    data = OsHV1_2020v2022)
#
#Deviance Residuals: 
#  Min          1Q      Median          3Q         Max  
#-6.547e-06  -6.547e-06   0.000e+00   6.547e-06   6.547e-06  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)
#(Intercept)   -24.57   65505.35   0.000        1
#Cohort2022     49.13   92638.58   0.001        1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 1.1090e+01  on 7  degrees of freedom
#Residual deviance: 3.4294e-10  on 6  degrees of freedom
#AIC: 4

#Number of Fisher Scoring iterations: 23
