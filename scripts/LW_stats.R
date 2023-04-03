#### ~ L/W STATS ~ =====

### load libraries
library(tidyverse)

#### *** 2020: L/W *** ====
### load data sheet
LW2020 <- read_csv("data/Growth/2020/LW2020.csv")

glimpse(LW2020)
summary(LW2020)
tail(LW2020)
View(LW2020)

## clean data
#### now remove NAs from data sheet
colSums(is.na(LW2020)) ## No NAs

### order Site
LW2020$Site <-factor(LW2020$Site, c("North", "Middle", "South"))

### Sampling_Time as character
LW2020$Sampling_Time <- as.character(LW2020$Sampling_Time)

is.character(LW2020$Sampling_Time)

#### Model selection ====

#### m_null_2020: L ~ 1  =====
m_null_2020 <- glm(L ~ 1, family = gaussian(link = "identity"), data = LW2020)
summary(m_null_2020)

# Call:
#glm(formula = L ~ 1, family = gaussian(link = "identity"), data = LW2020)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.2145  -0.6630  -0.1625   0.6680   2.4025  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.77446    0.05236   91.19   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.87725)
#
#Null deviance: 279.84  on 319  degrees of freedom
#Residual deviance: 279.84  on 319  degrees of freedom
#AIC: 869.21
#
#Number of Fisher Scoring iterations: 2

#### m1: L ~ Sampling_Time  =====
m1 <- glm(L ~ Sampling_Time, family = gaussian(link = "identity"), data = LW2020)
summary(m1)

# Call:
#glm(formula = L ~ Sampling_Time, family = gaussian(link = "identity"), 
#    data = LW2020)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-2.36227  -0.44252   0.04404   0.46294   1.83573  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     4.20766    0.05899   71.33   <2e-16 ***
#  Sampling_Time5  1.13361    0.08342   13.59   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.5567187)
#
#Null deviance: 279.84  on 319  degrees of freedom
#Residual deviance: 177.04  on 318  degrees of freedom
#AIC: 724.69
#
#Number of Fisher Scoring iterations: 2

#### m2: L ~ Site  =====
m2 <- glm(L ~ Site, family = gaussian(link = "identity"), data = LW2020)
summary(m2)

#Call:
#  glm(formula = L ~ Site, family = gaussian(link = "identity"), 
#      data = LW2020)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.1792  -0.6659  -0.1608   0.6859   2.4228  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  4.80975    0.07411  64.901   <2e-16 ***
#  SiteSouth   -0.07058    0.10481  -0.673    0.501    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.8787556)
#
#Null deviance: 279.84  on 319  degrees of freedom
#Residual deviance: 279.44  on 318  degrees of freedom
#AIC: 870.75
#
#Number of Fisher Scoring iterations: 2

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2)
BIC(m_null, m1, m2)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m1), resid(m1))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m1))
qqline(resid(m1))

#### Density Plot of Residuals ===============
plot(density(resid(m1)))

#### *** 2021: C. sikamea L/W *** ====
### load data sheet
LW2021.sik <- read_csv("data/Growth/2021/LW2021_sikamea.csv")

glimpse(LW2021.sik)
summary(LW2021.sik)
tail(LW2021.sik)
View(LW2021.sik)

## clean data
#### now remove NAs from data sheet
colSums(is.na(LW2021.sik)) ## No NAs

### order Site
LW2021.sik$Site <-factor(LW2021.sik$Site, c("North", "Middle", "South"))

### Sampling_Time as character
LW2021.sik$Sampling_Time <- as.character(LW2021.sik$Sampling_Time)

is.character(LW2021.sik$Sampling_Time)

#### Model selection ====

#### m_null.sik: L ~ 1  =====
m_null.sik <- glm(L ~ 1, family = gaussian(link = "identity"), data = LW2021.sik)
summary(m_null.sik)

# Call:
#glm(formula = L ~ 1, family = gaussian(link = "identity"), data = LW2021.sik)
#
#Deviance Residuals: 
# Min       1Q   Median       3Q      Max  
#-1.1558  -0.4158  -0.1048   0.3682   1.8282  
#
#Coefficients:
 # Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.08282    0.02047   150.6   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.3003384)
#
#Null deviance: 215.04  on 716  degrees of freedom
#Residual deviance: 215.04  on 716  degrees of freedom
#AIC: 1175.3
#
#Number of Fisher Scoring iterations: 2

#### m3: L ~ SH_Temp  =====
m3 <- glm(L ~ SH_Temp, family = gaussian(link = "identity"), data = LW2021.sik)
summary(m3)

# Call:
#glm(formula = L ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = LW2021.sik)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1638  -0.4178  -0.0968   0.3662   1.8202  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.142098   0.152892  20.551   <2e-16 ***
#  SH_Temp     -0.003205   0.008191  -0.391    0.696    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.3006941)
#
#Null deviance: 215.04  on 716  degrees of freedom
#Residual deviance: 215.00  on 715  degrees of freedom
#AIC: 1177.2
#
#Number of Fisher Scoring iterations: 2

#### m4: L ~ SH_Tide  =====
m4 <- glm(L ~ SH_Tide, family = gaussian(link = "identity"), data = LW2021.sik)
summary(m4)

#Call:
#glm(formula = L ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = LW2021.sik)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.13358  -0.42258  -0.09876   0.36642   1.85042  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.06058    0.02814 108.747   <2e-16 ***
#  SH_TideTide  0.04718    0.04099   1.151     0.25    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.3002022)
#
#Null deviance: 215.04  on 716  degrees of freedom
#Residual deviance: 214.64  on 715  degrees of freedom
#AIC: 1176
#
#Number of Fisher Scoring iterations: 2

#### m5: L ~ Sampling_Time  =====
m5 <- glm(L ~ Sampling_Time, family = gaussian(link = "identity"), data = LW2021.sik)
summary(m5)

# Call:
#glm(formula = L ~ Sampling_Time, family = gaussian(link = "identity"), 
#    data = LW2021.sik)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.19673  -0.26292   0.00427   0.26227   1.47127  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     2.72692    0.02198  124.08   <2e-16 ***
#  Sampling_Time4  0.71281    0.03110   22.92   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1733789)
#
#Null deviance: 215.04  on 716  degrees of freedom
#Residual deviance: 123.97  on 715  degrees of freedom
#AIC: 782.37
#
#Number of Fisher Scoring iterations: 2

#### m6: L ~ Site  =====
m6 <- glm(L ~ Site, family = gaussian(link = "identity"), data = LW2021.sik)
summary(m6)

# Call:
#glm(formula = L ~ Site, family = gaussian(link = "identity"), 
#    data = LW2021.sik)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.32436  -0.37615  -0.03495   0.35605   1.65964  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.11915    0.03398  91.796  < 2e-16 ***
#  SiteMiddle   0.13220    0.04810   2.748  0.00614 ** 
#  SiteSouth   -0.24220    0.04815  -5.030 6.22e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.2771005)
#
#Null deviance: 215.04  on 716  degrees of freedom
#Residual deviance: 197.85  on 714  degrees of freedom
#AIC: 1119.6
#
#Number of Fisher Scoring iterations: 2

#### m7: L ~ Sampling_Time + Site  =====
m7 <- glm(L ~ Sampling_Time + Site, family = gaussian(link = "identity"), data = LW2021.sik)
summary(m7)

# Call:
#glm(formula = L ~ Sampling_Time + Site, family = gaussian(link = "identity"), 
#    data = LW2021.sik)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.04580  -0.27751  -0.01759   0.26679   1.30151  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)     2.76251    0.02884  95.779  < 2e-16 ***
#  Sampling_Time4  0.71328    0.02889  24.692  < 2e-16 ***
#  SiteMiddle      0.13369    0.03534   3.783 0.000168 ***
#  SiteSouth      -0.24220    0.03538  -6.846 1.64e-11 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1495833)
#
#Null deviance: 215.04  on 716  degrees of freedom
#Residual deviance: 106.65  on 713  degrees of freedom
#AIC: 678.52
#
#Number of Fisher Scoring iterations: 2

#### m8: L ~ Sampling_Time + Site + Sampling_Time*Site  =====
m8 <- glm(L ~ Sampling_Time + Site + Sampling_Time*Site, family = gaussian(link = "identity"), data = LW2021.sik)
summary(m8)

# Call:
#glm(formula = L ~ Sampling_Time + Site + Sampling_Time * Site, 
#    family = gaussian(link = "identity"), data = LW2021.sik)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.09688  -0.23871   0.00714   0.22057   1.17829  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                2.71143    0.03077  88.129  < 2e-16 ***
#  Sampling_Time4             0.81544    0.04351  18.741  < 2e-16 ***
#  SiteMiddle                -0.01690    0.04351  -0.388    0.698    
#SiteSouth                  0.06375    0.04360   1.462    0.144    
#Sampling_Time4:SiteMiddle  0.30288    0.06160   4.917 1.09e-06 ***
#  Sampling_Time4:SiteSouth  -0.61191    0.06166  -9.924  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.1135913)
#
#Null deviance: 215.042  on 716  degrees of freedom
#Residual deviance:  80.763  on 711  degrees of freedom
#AIC: 483.15
#
#Number of Fisher Scoring iterations: 2

#### AIC/BIC Scores ===============
AIC(m_null.sik, m3, m4, m5, m6, m7, m8)
BIC(m_null.sik, m3, m4, m5, m6, m7, m8)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m8), resid(m8))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m8))
qqline(resid(m8))

#### Density Plot of Residuals ===============
plot(density(resid(m8)))

#### Pairwise Comparisons ===============
## pairwise comparison for m8
emm_Lm8a <-  emmeans(m8, specs = ~ Site|Sampling_Time)
emm_Lm8a

pairwise_Lm8a <- contrast(emm_Lm8a, interaction = "pairwise")
pairwise_Lm8a

# Sampling_Time = 1:
#Site_pairwise  estimate     SE  df t.ratio p.value
#North - Middle   0.0169 0.0435 711   0.388  0.6978
#North - South   -0.0638 0.0436 711  -1.462  0.1441
#Middle - South  -0.0807 0.0436 711  -1.850  0.0648
#
#Sampling_Time = 4:
#  Site_pairwise  estimate     SE  df t.ratio p.value
#North - Middle  -0.2860 0.0436 711  -6.559  <.0001
#North - South    0.5482 0.0436 711  12.572  <.0001
#Middle - South   0.8341 0.0437 711  19.091  <.0001


#### *** 2021/2022: M. gigas L/W *** ====
### load data sheet
LW21_22.gigas <- read_csv("data/Growth/LW2021_2022_gigas.csv")

glimpse(LW21_22.gigas)
summary(LW21_22.gigas)
tail(LW21_22.gigas)
View(LW21_22.gigas)

## clean data
#### now remove NAs from data sheet
colSums(is.na(LW21_22.gigas)) ## No NAs

### order Site
LW21_22.gigas$Site <-factor(LW21_22.gigas$Site, c("North", "Middle", "South"))

### Sampling_Time as character
LW21_22.gigas$Sampling_Time <- as.character(LW21_22.gigas$Sampling_Time)

is.character(LW21_22.gigas$Sampling_Time)

#### Model selection ====

#### m_null_gigas: L ~ 1  =====
m_null_gigas <- glm(L ~ 1, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m_null_gigas)

# Call:
#glm(formula = L ~ 1, family = gaussian(link = "identity"), data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0458  -0.6126  -0.1818   0.5142   3.6262  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.49285    0.02721   128.4   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.7893876)
#
#Null deviance: 840.7  on 1065  degrees of freedom
#Residual deviance: 840.7  on 1065  degrees of freedom
#AIC: 2776.1
#
#Number of Fisher Scoring iterations: 2

#### m9: L ~ SH_Temp  =====
m9 <- glm(L ~ SH_Temp, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m9)

#Call:
#  glm(formula = L ~ SH_Temp, family = gaussian(link = "identity"), 
#      data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0959  -0.6254  -0.1741   0.5030   3.6899  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.90692    0.17537   22.28   <2e-16 ***
#  SH_Temp     -0.02275    0.00952   -2.39    0.017 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.7859103)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 836.21  on 1064  degrees of freedom
#AIC: 2772.4
#
#Number of Fisher Scoring iterations: 2

#### m10: L ~ SH_Tide  =====
m10 <- glm(L ~ SH_Tide, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m10)

# Call:
#glm(formula = L ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8697  -0.6387  -0.1707   0.5478   3.2704  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.31669    0.03194 103.844   <2e-16 ***
#  SH_TideTide  0.53196    0.05550   9.584   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.7273357)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 773.89  on 1064  degrees of freedom
#AIC: 2689.8
#
#Number of Fisher Scoring iterations: 2

#### m11: L ~ Site =====
m11 <- glm(L ~ Site, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m11)

# Call:
#(formula = L ~ Site, family = gaussian(link = "identity"), 
#    data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.1483  -0.6349  -0.1453   0.5259   3.5237  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.54994    0.04674  75.946  < 2e-16 ***
#  SiteMiddle   0.04539    0.06620   0.686  0.49310    
#SiteSouth   -0.21609    0.06610  -3.269  0.00111 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.7778171)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 826.82  on 1063  degrees of freedom
#AIC: 2762.3
#
#Number of Fisher Scoring iterations: 2


#### m12: L ~ SH_Temp + SH_Tide + Site =====
m12 <- glm(L ~ SH_Temp + SH_Tide + Site, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m12)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide + Site, family = gaussian(link = "identity"), 
#    data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0175  -0.6264  -0.1533   0.5306   3.2253  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)  3.734068   0.172513  21.645  < 2e-16 ***
#  SH_Temp     -0.019752   0.009074  -2.177 0.029714 *  
#  SH_TideTide  0.527987   0.054989   9.602  < 2e-16 ***
#  SiteMiddle   0.046468   0.063384   0.733 0.463650    
#SiteSouth   -0.215704   0.063294  -3.408 0.000679 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.7130925)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 756.59  on 1061  degrees of freedom
#AIC: 2671.7
#
#Number of Fisher Scoring iterations: 2

#### m13: L ~ SH_Temp + SH_Tide + Site + SH_Temp*SH_Tide =====
m13 <- glm(L ~ SH_Temp + SH_Tide + Site + SH_Temp*SH_Tide, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m13)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide + Site + SH_Temp * SH_Tide, 
#    family = gaussian(link = "identity"), data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0240  -0.6282  -0.1637   0.5284   3.2107  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)          3.78635    0.21374  17.714  < 2e-16 ***
#  SH_Temp             -0.02261    0.01141  -1.983 0.047651 *  
#  SH_TideTide          0.38635    0.34602   1.117 0.264435    
#SiteMiddle           0.04648    0.06341   0.733 0.463672    
#SiteSouth           -0.21565    0.06332  -3.406 0.000685 ***
#  SH_Temp:SH_TideTide  0.00781    0.01884   0.415 0.678506    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.7136495)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 756.47  on 1060  degrees of freedom
#AIC: 2673.5
#
#Number of Fisher Scoring iterations: 2

#### m14: L ~ SH_Temp + SH_Tide + Site + SH_Tide*Site =====
m14 <- glm(L ~ SH_Temp + SH_Tide + Site + SH_Tide*Site, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m14)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide + Site + SH_Tide * Site, 
#    family = gaussian(link = "identity"), data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0056  -0.6316  -0.1681   0.5324   3.2010  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)             3.719259   0.174460  21.319  < 2e-16 ***
#  SH_Temp                -0.019772   0.009077  -2.178   0.0296 *  
#  SH_TideTide             0.573786   0.095132   6.031 2.24e-09 ***
#  SiteMiddle              0.049645   0.077525   0.640   0.5221    
#SiteSouth              -0.173385   0.077443  -2.239   0.0254 *  
#  SH_TideTide:SiteMiddle -0.009478   0.134752  -0.070   0.9439    
#SH_TideTide:SiteSouth  -0.127674   0.134513  -0.949   0.3428    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.7136861)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 755.79  on 1059  degrees of freedom
#AIC: 2674.6
#
#Number of Fisher Scoring iterations: 2

#### m15: L ~ SH_Temp + SH_Tide + Site + SH_Temp*Site =====
m15 <- glm(L ~ SH_Temp + SH_Tide + Site + SH_Temp*Site, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m15)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide + Site + SH_Temp * Site, 
#    family = gaussian(link = "identity"), data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0069  -0.6323  -0.1457   0.5229   3.2119  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         3.841330   0.289955  13.248   <2e-16 ***
#  SH_Temp            -0.025651   0.015705  -1.633    0.103    
#SH_TideTide         0.528005   0.055035   9.594   <2e-16 ***
#  SiteMiddle         -0.147809   0.409448  -0.361    0.718    
#SiteSouth          -0.343815   0.409179  -0.840    0.401    
#SH_Temp:SiteMiddle  0.010677   0.022229   0.480    0.631    
#SH_Temp:SiteSouth   0.007045   0.022221   0.317    0.751    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.7142783)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 756.42  on 1059  degrees of freedom
#AIC: 2675.5
#
#Number of Fisher Scoring iterations: 2

#### m16: L ~ SH_Temp + SH_Tide + Site + (1|Year:Sampling_Time) =====
m16 <- lmer(L ~ SH_Temp + SH_Tide + Site + (1|Year:Sampling_Time), data = LW21_22.gigas)
summary(m16)

# Linear mixed model fit by REML. t-tests use Satterthwaite's  method
#[lmerModLmerTest]
#Formula: L ~ SH_Temp + SH_Tide + Site + (1 | Year:Sampling_Time)
#Data: LW21_22.gigas
#
#REML criterion at convergence: 1931.2
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-3.5415 -0.6679 -0.0936  0.6215  4.4721 
#
#Random effects:
#  Groups             Name        Variance Std.Dev.
#Year:Sampling_Time (Intercept) 0.5064   0.7116  
#Residual                       0.3449   0.5873  
#Number of obs: 1066, groups:  Year:Sampling_Time, 4
#
#Fixed effects:
#  Estimate Std. Error         df t value Pr(>|t|)    
#(Intercept)  3.592e+00  3.756e-01  3.703e+00   9.564 0.000963 ***
#  SH_Temp     -1.194e-02  6.327e-03  1.058e+03  -1.887 0.059488 .  
#SH_TideTide  1.476e-01  4.403e-02  1.060e+03   3.351 0.000833 ***
#  SiteMiddle   4.456e-02  4.408e-02  1.058e+03   1.011 0.312322    
#SiteSouth   -2.136e-01  4.402e-02  1.058e+03  -4.853  1.4e-06 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) SH_Tmp SH_TdT StMddl
#SH_Temp     -0.307                     
#SH_TideTide -0.027 -0.006              
#SiteMiddle  -0.057 -0.004  0.001       
#SiteSouth   -0.058 -0.002 -0.003  0.499

#### AIC/BIC Scores ===============
AIC(m_null_gigas, m9, m10, m11, m12, m13, m14, m15, m16)
BIC(m_null_gigas, m9, m10, m11, m12, m13, m14, m15, m16)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m16), resid(m16))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m16))
qqline(resid(m16))

#### Density Plot of Residuals ===============
plot(density(resid(m16)))
