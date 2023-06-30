########## ~ M. GIGAS 2021 + 2022 qPCR Stats ==============

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

########## 2021 C. sikamea OsHV-1 Data ==============
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

#### *** C. sikeamea dataset *** ===== 
Mgigas <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None")

Mgigas$Site <- as.factor(Mgigas$Site)
is.factor(Mgigas$Site)

Mgigas$Site <- factor(Mgigas$Site, levels = c("North", "Middle", "South"))

View(Mgigas)

##########################
#### OsHV-1 Quantity =====
##########################

#### m_null: log_transform ~ 1  =====

m_null <- glm(log_transform ~ 1, family = gaussian(link = "identity"), data = Mgigas)
summary(m_null)

# Call:
#glm(formula = log_transform ~ 1, family = gaussian(link = "identity"), 
#    data = Mgigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.6917  -1.3926  -0.4644   0.8617   6.7355  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.6917     0.1119   24.05   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 4.95833)
#
#Null deviance: 1958.5  on 395  degrees of freedom
#Residual deviance: 1958.5  on 395  degrees of freedom
#AIC: 1760.8
#
#Number of Fisher Scoring iterations: 2

#### m1: log_transform ~ SH_TempLev  =====

m1 <- glm(log_transform ~ SH_TempLev, family = gaussian(link = "identity"), data = Mgigas)
summary(m1)

# Call:
# glm(formula = log_transform ~ SH_TempLev, family = gaussian(link = "identity"), 
#    data = Mgigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.7033  -1.4032  -0.4529   0.8687   6.7240  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)    2.70325    0.15845  17.061   <2e-16 ***
#  SH_TempLevLow -0.02302    0.22408  -0.103    0.918    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 4.970782)
#
#Null deviance: 1958.5  on 395  degrees of freedom
#Residual deviance: 1958.5  on 394  degrees of freedom
#AIC: 1762.8
#
#Number of Fisher Scoring iterations: 2

#### m2: log_transform ~ SH_Tide =====

m2 <- glm(log_transform ~ SH_Tide, family = gaussian(link = "identity"), data = Mgigas)
summary(m2)

#Call:
#glm(formula = log_transform ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = Mgigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.9474  -1.3332  -0.4347   0.8565   6.4798  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.9474     0.1388  21.233  < 2e-16 ***
#  SH_TideTide  -0.7032     0.2302  -3.055  0.00241 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 4.85592)
#
#Null deviance: 1958.5  on 395  degrees of freedom
#Residual deviance: 1913.2  on 394  degrees of freedom
#AIC: 1753.6
#
#Number of Fisher Scoring iterations: 2

#### m3: log_transform ~ Site =====

m3 <- glm(log_transform ~ Site, family = gaussian(link = "identity"), data = Mgigas)
summary(m3)

#Call:
#  glm(formula = log_transform ~ Site, family = gaussian(link = "identity"), 
#      data = Mgigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.5144  -1.5279  -0.1228   0.6462   5.9129  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.6456     0.1857  14.245  < 2e-16 ***
#  SiteMiddle    0.8688     0.2627   3.308  0.00103 ** 
#  SiteSouth    -0.7303     0.2627  -2.780  0.00569 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 4.553047)
#
#Null deviance: 1958.5  on 395  degrees of freedom
#Residual deviance: 1789.3  on 393  degrees of freedom
#AIC: 1729
#
#Number of Fisher Scoring iterations: 2


#### m4: log_transform ~ (Year/Sampling_Period) =====

m4 <- glm(log_transform ~ (Year/Sampling_Period), family = gaussian(link = "identity"), data = Mgigas)
summary(m4)

# Call:
#glm(formula = log_transform ~ (Year/Sampling_Period), family = gaussian(link = "identity"), 
#    data = Mgigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.6191  -1.6125  -0.2312   0.7086   5.8082  
#
#Coefficients: (2 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 3.6191     0.2819  12.839  < 2e-16 ***
#  Year2022                   -0.2848     0.3305  -0.862   0.3895    
#Year2021:Sampling_Period4  -0.6904     0.3986  -1.732   0.0841 .  
#Year2022:Sampling_Period4  -1.7218     0.2441  -7.053 7.95e-12 ***
#  Year2021:Sampling_Period5       NA         NA      NA       NA    
#Year2022:Sampling_Period5       NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 4.290545)
#
#Null deviance: 1958.5  on 395  degrees of freedom
#Residual deviance: 1681.9  on 392  degrees of freedom
#AIC: 1706.5

Number of Fisher Scoring iterations: 2

#### m5: log_transform ~ SH_Tide + Site =====
m5 <- glm(log_transform ~ SH_Tide + Site, family = gaussian(link = "identity"), data = Mgigas)
summary(m5)

#Call:
#glm(formula = log_transform ~ SH_Tide + Site, family = gaussian(link = "identity"), 
#    data = Mgigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.7701  -1.4678  -0.0452   0.7929   5.6572  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.9013     0.2003  14.484  < 2e-16 ***
#  SH_TideTide  -0.7032     0.2203  -3.191 0.001531 ** 
#  SiteMiddle    0.8688     0.2596   3.346 0.000898 ***
#  SiteSouth    -0.7303     0.2596  -2.813 0.005160 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 4.44908)
#
#Null deviance: 1958.5  on 395  degrees of freedom
#Residual deviance: 1744.0  on 392  degrees of freedom
#AIC: 1720.9
#
#Number of Fisher Scoring iterations: 2


#### m6: log_transform ~ SH_Tide + Site + SH_Tide*Site =====
m6 <- glm(log_transform ~ SH_Tide + Site + SH_Tide*Site, family = gaussian(link = "identity"), data = Mgigas)
summary(m6)

#Call:
#glm(formula = log_transform ~ SH_Tide + Site + SH_Tide * Site, 
#    family = gaussian(link = "identity"), data = Mgigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.7191  -1.5793  -0.0333   0.7112   5.7082  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)              3.0524     0.2302  13.260  < 2e-16 ***
#  SH_TideTide             -1.1187     0.3817  -2.931  0.00358 ** 
#  SiteMiddle               0.6667     0.3255   2.048  0.04122 *  
#  SiteSouth               -0.9815     0.3255  -3.015  0.00274 ** 
#  SH_TideTide:SiteMiddle   0.5558     0.5398   1.030  0.30388    
#SH_TideTide:SiteSouth    0.6909     0.5398   1.280  0.20140    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 4.450895)
#
#Null deviance: 1958.5  on 395  degrees of freedom
#Residual deviance: 1735.8  on 390  degrees of freedom
#AIC: 1723
#
#Number of Fisher Scoring iterations: 2

#### m7: log_transform ~ SH_Tide + Site + (Year/Sampling_Period) =====
m7 <- glm(log_transform ~ SH_Tide + Site + (Year/Sampling_Period), family = gaussian(link = "identity"), data = Mgigas)
summary(m7)

# Call:
#glm(formula = log_transform ~ SH_Tide + Site + (Year/Sampling_Period), 
#    family = gaussian(link = "identity"), data = Mgigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-4.4417  -1.2865  -0.2307   0.9873   5.6413  
#
#Coefficients: (2 not defined because of singularities)
#Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                 3.5729     0.3012  11.861  < 2e-16 ***
#  SH_TideTide                -0.4583     0.2312  -1.982 0.048177 *  
#  SiteMiddle                  0.8688     0.2415   3.597 0.000363 ***
#  SiteSouth                  -0.7303     0.2415  -3.024 0.002662 ** 
#  Year2022                   -0.0556     0.3338  -0.167 0.867777    
#Year2021:Sampling_Period4  -0.6904     0.3776  -1.828 0.068256 .  
#Year2022:Sampling_Period4  -1.7218     0.2312  -7.446 6.25e-13 ***
#  Year2021:Sampling_Period5       NA         NA      NA       NA    
#Year2022:Sampling_Period5       NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 3.849811)
#
#Null deviance: 1958.5  on 395  degrees of freedom
#Residual deviance: 1497.6  on 389  degrees of freedom
#AIC: 1666.6
#
#Number of Fisher Scoring iterations: 2

#### m8: log_transform ~ SH_Tide + Site + (Year/Sampling_Period) + (1|Bag) =====
m8 <- lmer(log_transform ~ SH_Tide + Site + (Year/Sampling_Period) + (1|Bag), data = Mgigas)
summary(m8)

#Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log_transform ~ SH_Tide + Site + (Year/Sampling_Period) + (1 |      Bag)
#   Data: Mgigas
#
#REML criterion at convergence: 1654
#
#Scaled residuals: 
#    Min      1Q  Median      3Q     Max 
#-2.0548 -0.5942 -0.1362  0.4805  2.8782 
#
#Random effects:
# Groups   Name        Variance Std.Dev.
# Bag      (Intercept) 0.2919   0.5402  
# Residual             3.5815   1.8925  
#Number of obs: 396, groups:  Bag, 54
#
#Fixed effects:
#                          Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)                 3.5900     0.3340 109.9698  10.749  < 2e-16 ***
#SH_TideTide                -0.4583     0.2867  43.2945  -1.599  0.11713    
#SiteMiddle                  0.8366     0.2950  47.9308   2.836  0.00668 ** 
#SiteSouth                  -0.7494     0.2950  47.9308  -2.540  0.01437 *  
#Year2022                   -0.0556     0.3689 113.7480  -0.151  0.88045    
#Year2021:Sampling_Period4  -0.6904     0.3642 339.6337  -1.896  0.05886 .  
#Year2022:Sampling_Period4  -1.7218     0.2230 339.6337  -7.720 1.31e-13 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#            (Intr) SH_TdT StMddl SitSth Yr2022 Y2021:
#SH_TideTide  0.000                                   
#SiteMiddle  -0.442  0.000                            
#SiteSouth   -0.442  0.000  0.500                     
#Year2022    -0.670 -0.389  0.000  0.000              
#Yr2021:S_P4 -0.545  0.000  0.000  0.000  0.494       
#Yr2022:S_P4  0.000  0.000  0.000  0.000 -0.302  0.000
#fit warnings:
#fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients

#### m9: log_transform ~ Site + (Year/Sampling_Period) + (1|Bag) =====
m9 <- lmer(log_transform ~ Site + (Year/Sampling_Period) + (1|Bag), data = Mgigas)
summary(m9)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method ['lmerModLmerTest']
#Formula: log_transform ~ Site + (Year/Sampling_Period) + (1 | Bag)
#Data: Mgigas
#
#REML criterion at convergence: 1655.8
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.0595 -0.5722 -0.1549  0.5177  2.8728 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Bag      (Intercept) 0.3179   0.5638  
#Residual             3.5805   1.8922  
#Number of obs: 396, groups:  Bag, 54
#
#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)                 3.5910     0.3376 110.4392  10.638  < 2e-16 ***
#  SiteMiddle                  0.8348     0.2999  49.2393   2.784   0.0076 ** 
#  SiteSouth                  -0.7505     0.2999  49.2393  -2.503   0.0157 *  
#  Year2022                   -0.2848     0.3430 140.4700  -0.830   0.4079    
#Year2021:Sampling_Period4  -0.6904     0.3642 339.8042  -1.896   0.0588 .  
#Year2022:Sampling_Period4  -1.7218     0.2230 339.8042  -7.721  1.3e-13 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) StMddl SitSth Yr2022 Y2021:
#  SiteMiddle  -0.444                            
#SiteSouth   -0.444  0.500                     
#Year2022    -0.725  0.000  0.000              
#Yr2021:S_P4 -0.539  0.000  0.000  0.531       
#Yr2022:S_P4  0.000  0.000  0.000 -0.325  0.000
#fit warnings:
#  fixed-effect model matrix is rank deficient so dropping 2 columns / coefficients

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5, m6, m7, m8, m9)
BIC(m_null, m1, m2, m3, m4, m5, m6, m7, m8, m9)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m8), resid(m8))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m8))
qqline(resid(m8))

#### Density Plot of Residuals ===============
plot(density(resid(m8)))
