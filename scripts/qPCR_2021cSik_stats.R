########## ~ C. SIKAMEA 2021 Baseline qPCR Stats ==============

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
Csikamea <- OsHV1 %>%
  filter(Species == "C. sikamea")

Csikamea$Site <- as.factor(Csikamea$Site)
is.factor(Csikamea$Site)

Csikamea$Site <- factor(Csikamea$Site, levels = c("North", "Middle", "South"))

View(Csikamea)

##########################
#### OsHV-1 Quantity =====
##########################

#### m_null: log_transform ~ 1  =====

m_null <- glm(log_transform ~ 1, family = gaussian(link = "identity"), data = Csikamea)
summary(m_null)

# Call:
# glm(formula = log_transform ~ 1, family = gaussian(link = "identity"), 
#    data = Csikamea)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.5313  -1.9203  -0.4849   1.8363   3.7314  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)   1.9203     0.1247    15.4   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 3.358544)
#
# Null deviance: 722.09  on 215  degrees of freedom
# Residual deviance: 722.09  on 215  degrees of freedom
# AIC: 877.66
#
# Number of Fisher Scoring iterations: 2

#### m1: log_transform ~ SH_Temp  =====

m1 <- glm(log_transform ~ SH_Temp, family = gaussian(link = "identity"), data = Csikamea)
summary(m1)

# Call:
# glm(formula = log_transform ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Csikamea)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.5217  -1.9106  -0.4946   1.8267   3.7217  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  1.91065    0.17675  10.810   <2e-16 ***
#  SH_Temp21    0.01928    0.24997   0.077    0.939    
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 3.374144)
#
# Null deviance: 722.09  on 215  degrees of freedom
# Residual deviance: 722.07  on 214  degrees of freedom
# AIC: 879.66
#
# Number of Fisher Scoring iterations: 2

#### m2: log_transform ~ SH_Tide =====

m2 <- glm(log_transform ~ SH_Tide, family = gaussian(link = "identity"), data = Csikamea)
summary(m2)

#Call:
#  glm(formula = log_transform ~ SH_Tide, family = gaussian(link = "identity"), 
#      data = Csikamea)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.6845  -1.7671  -0.3612   1.7639   3.8164  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.0734     0.1761   11.77   <2e-16 ***
#  SH_TideTide  -0.3063     0.2491   -1.23     0.22    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 3.350566)
#
#Null deviance: 722.09  on 215  degrees of freedom
#Residual deviance: 717.02  on 214  degrees of freedom
#AIC: 878.14
#
#Number of Fisher Scoring iterations: 2

#### m3: log_transform ~ Site =====

m3 <- glm(log_transform ~ Site, family = gaussian(link = "identity"), data = Csikamea)
summary(m3)

# Call:
#glm(formula = log_transform ~ Site, family = gaussian(link = "identity"), 
#    data = Csikamea)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.9132  -1.7137  -0.3478   1.7222   3.8386  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   2.3022     0.2146  10.727   <2e-16 ***
#  SiteMiddle   -0.5572     0.3035  -1.836   0.0678 .  
#SiteSouth    -0.5884     0.3035  -1.939   0.0538 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 3.315975)
#
#Null deviance: 722.09  on 215  degrees of freedom
#Residual deviance: 706.30  on 213  degrees of freedom
#AIC: 876.89
#
#Number of Fisher Scoring iterations: 2

#### m4: log_transform ~ Sampling_Period =====

m4 <- glm(log_transform ~ Sampling_Period, family = gaussian(link = "identity"), data = Csikamea)
summary(m4)

# Call:
#  glm(formula = log_transform ~ Sampling_Period, family = gaussian(link = "identity"), 
#      data = Csikamea)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -2.9457  -1.5059  -0.7006   1.4779   4.1457  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        2.3346     0.1722  13.561  < 2e-16 ***
#  Sampling_Period4  -0.8287     0.2435  -3.404 0.000793 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 3.200949)
#
# Null deviance: 722.09  on 215  degrees of freedom
# Residual deviance: 685.00  on 214  degrees of freedom
# AIC: 868.28
#
# Number of Fisher Scoring iterations: 2

#### m5: log_transform ~ Site + Sampling_Period =====
m5 <- glm(log_transform ~ Site + Sampling_Period, family = gaussian(link = "identity"), data = Csikamea)
summary(m5)

#Call:
#  glm(formula = log_transform ~ Site + Sampling_Period, family = gaussian(link = "identity"), 
#      data = Csikamea)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -3.3276  -1.3306  -0.5253   1.6727   3.7638  
#
# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
# (Intercept)        2.7165     0.2418  11.235  < 2e-16 ***
#  SiteMiddle        -0.5572     0.2961  -1.882 0.061253 .  
# SiteSouth         -0.5884     0.2961  -1.987 0.048195 *  
#  Sampling_Period4  -0.8287     0.2418  -3.427 0.000732 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
# (Dispersion parameter for gaussian family taken to be 3.156692)
#
# Null deviance: 722.09  on 215  degrees of freedom
# Residual deviance: 669.22  on 212  degrees of freedom
# AIC: 867.24
#
# Number of Fisher Scoring iterations: 2

#### m6: log_transform ~ Site + Sampling_Period + Site*Sampling_Period =====
m6 <- glm(log_transform ~ Site + Sampling_Period + Site*Sampling_Period, family = gaussian(link = "identity"), data = Csikamea)
summary(m6)

#Call:
#  glm(formula = log_transform ~ Site + Sampling_Period + Site * 
#        Sampling_Period, family = gaussian(link = "identity"), data = Csikamea)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.9408  -0.3468   0.0019   0.5626   2.3239  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                   0.6635     0.1680   3.951 0.000106 ***
#  SiteMiddle                    2.5961     0.2375  10.930  < 2e-16 ***
#  SiteSouth                     2.4171     0.2375  10.177  < 2e-16 ***
#  Sampling_Period4              3.2772     0.2375  13.798  < 2e-16 ***
#  SiteMiddle:Sampling_Period4  -6.3067     0.3359 -18.775  < 2e-16 ***
#  SiteSouth:Sampling_Period4   -6.0111     0.3359 -17.896  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 1.015466)
#
#Null deviance: 722.09  on 215  degrees of freedom
#Residual deviance: 213.25  on 210  degrees of freedom
#AIC: 624.21
#
#Number of Fisher Scoring iterations: 2

#### m7: log_transform ~ Site + Sampling_Period + Site*Sampling_Period + (1|Bag) =====
m7 <- lmer(log_transform ~ Site + Sampling_Period + Site*Sampling_Period + (1|Bag), data = Csikamea)
summary(m7)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: 
#  log_transform ~ Site + Sampling_Period + Site * Sampling_Period +  
#  (1 | Bag)
#Data: Csikamea
#
#REML criterion at convergence: 619.5
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.9191 -0.3296  0.0193  0.5074  2.3084 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Bag      (Intercept) 0.05312  0.2305  
#Residual             0.96546  0.9826  
#Number of obs: 216, groups:  Bag, 36
#
#Fixed effects:
#  Estimate Std. Error       df t value
#(Intercept)                   0.6635     0.1768  91.3951   3.754
#SiteMiddle                    2.5961     0.2500  91.3951  10.385
#SiteSouth                     2.4163     0.2501  90.7749   9.661
#Sampling_Period4              3.2772     0.2316 176.9941  14.151
#SiteMiddle:Sampling_Period4  -6.3067     0.3275 176.9941 -19.255
#SiteSouth:Sampling_Period4   -6.0111     0.3275 176.9941 -18.353
#Pr(>|t|)    
#(Intercept)                 0.000306 ***
#  SiteMiddle                   < 2e-16 ***
#  SiteSouth                   1.34e-15 ***
#  Sampling_Period4             < 2e-16 ***
#  SiteMiddle:Sampling_Period4  < 2e-16 ***
#  SiteSouth:Sampling_Period4   < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) StMddl SitSth Smp_P4 SM:S_P
#SiteMiddle  -0.707                            
#SiteSouth   -0.707  0.500                     
#Smplng_Prd4 -0.655  0.463  0.463              
#StMddl:S_P4  0.463 -0.655 -0.327 -0.707       
#StSth:Sm_P4  0.463 -0.328 -0.655 -0.707  0.500

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5, m6, m7)
BIC(m_null, m1, m2, m3, m4, m5, m6, m7)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m7), resid(m7))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m7))
qqline(resid(m7))

#### Density Plot of Residuals ===============
plot(density(resid(m7)))
