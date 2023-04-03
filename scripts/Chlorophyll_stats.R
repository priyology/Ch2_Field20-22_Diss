#### ~ 2021/2022 STATS ~ =====

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(pbkrtest) ## to use with emmeans
library(ggeffects) ## another model plotting option

#### *** 2021: Benthic Chlorophyll *** ====
### load data sheet
ChlA <- read_csv("data/Chlorophyll/Chla_All.csv")
glimpse(ChlA)
summary(ChlA)
tail(ChlA)
View(ChlA)

## clean data
#### now remove NAs from data sheet
colSums(is.na(ChlA)) ## Date: 4 None

### order Site
ChlA$Site <-factor(ChlA$Site, c("North", "Middle", "South"))

#### GAUSSIAN Model selection ====
#### m_null_gauss: Chl_a ~ 1  =====

m_null_gauss <- glm(Chl_a ~ 1, family = gaussian(link = "identity"), data = ChlA)
summary(m_null_gauss)

#Call:
#glm(formula = Chl_a ~ 1, family = gaussian(link = "identity"), 
#    data = ChlA)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-4.218  -1.656  -0.474   1.299  12.334  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.2179     0.1539   27.41   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 6.393907)
#
#Null deviance: 1720  on 269  degrees of freedom
#Residual deviance: 1720  on 269  degrees of freedom
#AIC: 1270.2
#
#Number of Fisher Scoring iterations: 2


#### m1: Chl_a ~ Type  =====

m1 <- glm(Chl_a ~ Type, family = gaussian(link = "identity"), data = ChlA)
summary(m1)

#Call:
#  glm(formula = Chl_a ~ Type, family = gaussian(link = "identity"), 
#      data = ChlA)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-4.1827  -1.6211  -0.4388   1.2789  12.3692  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    4.1827     0.1723  24.275   <2e-16 ***
#  TypeSeawater   0.1760     0.3853   0.457    0.648    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 6.41277)
#
#Null deviance: 1720.0  on 269  degrees of freedom
#Residual deviance: 1718.6  on 268  degrees of freedom
#AIC: 1272
#
#Number of Fisher Scoring iterations: 2

#### m2: Chl_a ~ Year  =====

m2 <- glm(Chl_a ~ Year, family = gaussian(link = "identity"), data = ChlA)
summary(m2)

#Call:
#glm(formula = Chl_a ~ Year, family = gaussian(link = "identity"), 
#    data = ChlA)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-4.074  -1.709  -0.527   1.173  12.208  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) -541.6914   623.8334  -0.868    0.386
#Year           0.2700     0.3086   0.875    0.382
#
#(Dispersion parameter for gaussian family taken to be 6.399479)
#
#Null deviance: 1720.0  on 269  degrees of freedom
#Residual deviance: 1715.1  on 268  degrees of freedom
#AIC: 1271.4
#
#Number of Fisher Scoring iterations: 2

#### m3: Chl_a ~ Site  =====

m3 <- glm(Chl_a ~ Site, family = gaussian(link = "identity"), data = ChlA)
summary(m3)

# Call:
#glm(formula = Chl_a ~ Site, family = gaussian(link = "identity"), 
#    data = ChlA)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.7211  -1.4012  -0.4575   1.1604  11.4514  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.4398     0.2457  18.070  < 2e-16 ***
#  SiteMiddle    0.6606     0.3484   1.896    0.059 .  
#SiteSouth    -1.4622     0.3588  -4.075 6.07e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 5.674944)
#
#Null deviance: 1720.0  on 269  degrees of freedom
#Residual deviance: 1515.2  on 267  degrees of freedom
#AIC: 1239.9
#
#Number of Fisher Scoring iterations: 2



#### m4: Chl_a ~ Site + (1|Year:Sampling_Period) =====

m4 <- lmer(Chl_a ~ Site + (1|Year:Sampling_Period), data = ChlA)
summary(m4)

# Linear mixed model fit by REML. t-tests use Satterthwaite's  method
#[lmerModLmerTest]
#Formula: Chl_a ~ Site + (1 | Year:Sampling_Period)
#Data: ChlA
#
#REML criterion at convergence: 1205.7
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.8003 -0.6072 -0.0966  0.3570  4.8890 
#
#Random effects:
#  Groups               Name        Variance Std.Dev.
#Year:Sampling_Period (Intercept) 1.143    1.069   
#Residual                         4.736    2.176   
#Number of obs: 270, groups:  Year:Sampling_Period, 11
#
#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)   4.4453     0.3929  15.3559  11.313 7.50e-09 ***
#  SiteMiddle    0.6680     0.3183 256.5573   2.098   0.0369 *  
#  SiteSouth    -1.6706     0.3312 258.8175  -5.044 8.58e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) StMddl
#SiteMiddle -0.403       
#SiteSouth  -0.388  0.478

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m4), resid(m4))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m4))
qqline(resid(m4))

#### Density Plot of Residuals ===============
plot(density(resid(m4)))

#### GAMMA Model selection ====

ChlA_gamma <- ChlA %>% 
  filter(Chl_a != 0)

#### m_null_gamma: Chl_a ~ 1  =====

m_null_gamma <- glm(Chl_a ~ 1, family = Gamma(link = "identity"), data = ChlA_gamma)
summary(m_null_gamma)

# Call:
#glm(formula = Chl_a ~ 1, family = Gamma(link = "identity"), data = ChlA_gamma)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.3193  -0.4668  -0.1240   0.2730   1.7524  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.2493     0.1534    27.7   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.3493136)
#
#Null deviance: 82.026  on 267  degrees of freedom
#Residual deviance: 82.026  on 267  degrees of freedom
#AIC: 1154.3
#
#Number of Fisher Scoring iterations: 3

#### m5: Chl_a ~ Type  =====

m5 <- glm(Chl_a ~ Type, family = Gamma(link = "identity"), data = ChlA_gamma)
summary(m5)

# Call:
#glm(formula = Chl_a ~ Type, family = Gamma(link = "identity"), 
#   data = ChlA_gamma)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.3153  -0.4613  -0.1178   0.2688   1.7632  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    4.2218     0.1715  24.622   <2e-16 ***
#  TypeSeawater   0.1369     0.3919   0.349    0.727    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.3530063)
#
#Null deviance: 82.026  on 267  degrees of freedom
#Residual deviance: 81.982  on 266  degrees of freedom
#AIC: 1156.2
#
#Number of Fisher Scoring iterations: 3

#### m6: Chl_a ~ Year  =====

m6 <- glm(Chl_a ~ Year, family = Gamma(link = "identity"), data = ChlA_gamma)
summary(m6)

#Call:
#  glm(formula = Chl_a ~ Year, family = Gamma(link = "identity"), 
#      data = ChlA_gamma)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.3031  -0.4547  -0.1451   0.2522   1.7162  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept) -408.8314   623.1445  -0.656    0.512
#Year           0.2043     0.3083   0.663    0.508
#
#(Dispersion parameter for Gamma family taken to be 0.3529392)
#
#Null deviance: 82.026  on 267  degrees of freedom
#Residual deviance: 81.872  on 266  degrees of freedom
#AIC: 1155.8
#
#Number of Fisher Scoring iterations: 3

#### m7: Chl_a ~ Site  =====

m7 <- glm(Chl_a ~ Site, family = Gamma(link = "identity"), data = ChlA_gamma)
summary(m7)

# Call:
#glm(formula = Chl_a ~ Site, family = Gamma(link = "identity"), 
#    data = ChlA_gamma)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1062  -0.3929  -0.1169   0.2487   1.6021  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.4398     0.2483  17.884  < 2e-16 ***
#  SiteMiddle    0.6606     0.3793   1.742   0.0827 .  
#SiteSouth    -1.3887     0.3089  -4.496 1.04e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.2939085)
#
#Null deviance: 82.026  on 267  degrees of freedom
#Residual deviance: 70.563  on 265  degrees of freedom
#AIC: 1116.1
#
#Number of Fisher Scoring iterations: 3


#### m8: Chl_a ~ Site + (1|Year:Sampling_Period) =====

m8 <- glmer(Chl_a ~ Site + (1|Year:Sampling_Period), family = Gamma(link = "identity"), data = ChlA_gamma)
summary(m8)

# Generalized linear mixed model fit by maximum likelihood (Laplace
#Approximation) [glmerMod]
#Family: Gamma  ( identity )
#Formula: Chl_a ~ Site + (1 | Year:Sampling_Period)
#Data: ChlA_gamma
#
#AIC      BIC   logLik deviance df.resid 
#1076.1   1094.0   -533.0   1066.1      263 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.5962 -0.6371 -0.1423  0.3934  4.4457 
#
#Random effects:
#  Groups               Name        Variance Std.Dev.
#Year:Sampling_Period (Intercept) 0.5111   0.7149  
#Residual                         0.2377   0.4876  
#Number of obs: 268, groups:  Year:Sampling_Period, 11
#
#Fixed effects:
#  Estimate Std. Error t value Pr(>|z|)    
#(Intercept)   4.4727     0.4058  11.023  < 2e-16 ***
#  SiteMiddle    0.5361     0.2839   1.888    0.059 .  
#SiteSouth    -1.4225     0.2625  -5.420 5.97e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) StMddl
#SiteMiddle -0.311       
#SiteSouth  -0.380  0.485

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m8), resid(m8))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m8))
qqline(resid(m8))

#### Density Plot of Residuals ===============
plot(density(resid(m8)))


