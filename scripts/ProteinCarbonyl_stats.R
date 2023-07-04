#### ~ M. GIGAS PROTEIN CARBONYL STATS ~ =====

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(pbkrtest) ## to use with emmeans
library(ggeffects) ## another model plotting option

### load data sheet

PC <- read_csv("data/Protein_Carbonyl/ProteinCarbonyl_Stats.csv")
glimpse(PC)
summary(PC)
View(PC)

## re-organize site
PC$Site <- fct_relevel(PC$Site, "HIOC - North", "BBOC - Middle", "TBOC - South")

  ########################
#### Gaussian ==========
########################

#### Model selection ====
#### m_null: Protein Carbonyl ~ 1  =====

m_null <- glm(CarbPerProtein ~ 1, family = gaussian(link = "identity"), data = PC)
summary(m_null)

# Call:
# glm(formula = CarbPerProtein ~ 1, family = gaussian(link = "identity"), 
#    data = PC)
#
# Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
# -3.377  -1.392  -0.827  -0.008  31.855  

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.4733     0.2526   17.71   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 11.29405)
#
#Null deviance: 1987.8  on 176  degrees of freedom
#Residual deviance: 1987.8  on 176  degrees of freedom
#AIC: 934.4
#
#Number of Fisher Scoring iterations: 2

#### m1: Protein Carbonyl ~ SH_Temp  =====
m1 <- glm(CarbPerProtein ~ SH_Temp, family = gaussian(link = "identity"), data = PC)
summary(m1)

#Call:
#  glm(formula = CarbPerProtein ~ SH_Temp, family = gaussian(link = "identity"), 
#      data = PC)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-3.791  -1.285  -0.729   0.168  32.264  

# Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
# (Intercept)   2.0082     1.5279   1.314    0.190
# SH_Temp       0.1371     0.0838   1.636    0.104
#
# (Dispersion parameter for gaussian family taken to be 11.18754)
#
# Null deviance: 1987.8  on 176  degrees of freedom
# Residual deviance: 1957.8  on 175  degrees of freedom
#AIC: 933.71

#Number of Fisher Scoring iterations: 2

#### m2: Protein Carbonyl ~ SH_Tide  =====
m2 <- glm(CarbPerProtein ~ SH_Tide, family = gaussian(link = "identity"), data = PC)
summary(m2)

#Call:
#glm(formula = CarbPerProtein ~ SH_Tide, family = gaussian(link = "identity"), 
#    data = PC)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.2714  -1.4663  -0.7436   0.1771  31.3496  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.9737     0.3532  11.250   <2e-16 ***
#  SH_TideTide   1.0049     0.5009   2.006   0.0464 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 11.10323)
#
#Null deviance: 1987.8  on 176  degrees of freedom
#Residual deviance: 1943.1  on 175  degrees of freedom
#AIC: 932.37
#
# Number of Fisher Scoring iterations: 2

#### m3: Protein Carbonyl ~ Site  =====
m3 <- glm(CarbPerProtein ~ Site, family = gaussian(link = "identity"), data = PC)
summary(m3)

#Call:
#  glm(formula = CarbPerProtein ~ Site, family = gaussian(link = "identity"), 
#      data = PC)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.0198  -1.2750  -0.6594   0.0844  30.8722  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         5.4560     0.4304   12.68   <2e-16 ***
#  SiteBBOC - Middle  -1.3937     0.6113   -2.28   0.0238 *  
#  SiteTBOC - South   -1.5516     0.6061   -2.56   0.0113 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 10.92851)
#
#Null deviance: 1987.8  on 176  degrees of freedom
#Residual deviance: 1901.6  on 174  degrees of freedom
#AIC: 930.55
#
#Number of Fisher Scoring iterations: 2

#### m4: Protein Carbonyl ~ SH_Tide + Site  =====
m4 <- glm(CarbPerProtein ~ SH_Tide + Site, family = gaussian(link = "identity"), data = PC)
summary(m4)

#Call:
#  glm(formula = CarbPerProtein ~ SH_Tide + Site, family = gaussian(link = "identity"), 
#      data = PC)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-3.5364  -1.3750  -0.6206   0.1612  30.3556  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         4.9565     0.4903  10.109   <2e-16 ***
#  SH_TideTide         1.0161     0.4924   2.064   0.0405 *  
#  SiteBBOC - Middle  -1.4023     0.6056  -2.315   0.0218 *  
#  SiteTBOC - South   -1.5602     0.6005  -2.598   0.0102 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 10.72762)

#Null deviance: 1987.8  on 176  degrees of freedom
#Residual deviance: 1855.9  on 173  degrees of freedom
#AIC: 928.25
##
#Number of Fisher Scoring iterations: 2

#### m5: Protein Carbonyl ~ SH_Tide + Site + SH_Tide*Site  =====
m5 <- glm(CarbPerProtein ~ SH_Tide + Site + SH_Tide*Site, family = gaussian(link = "identity"), data = PC)
summary(m5)

# Call:
#glm(formula = CarbPerProtein ~ SH_Tide + Site + SH_Tide * Site, 
#    family = gaussian(link = "identity"), data = PC)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-4.1766  -1.1663  -0.6090   0.0602  29.7154  
#
#Coefficients:
#  Estimate Std. Error t value
#(Intercept)                     4.3377     0.5957   7.281
#SH_TideTide                     2.2752     0.8497   2.677
#SiteBBOC - Middle              -0.5032     0.8497  -0.592
#SiteTBOC - South               -0.5934     0.8425  -0.704
#SH_TideTide:SiteBBOC - Middle  -1.8195     1.2068  -1.508
#SH_TideTide:SiteTBOC - South   -1.9549     1.1966  -1.634
#Pr(>|t|)    
#(Intercept)                   1.15e-11 ***
#  SH_TideTide                    0.00814 ** 
#  SiteBBOC - Middle              0.55454    
#SiteTBOC - South               0.48216    
#SH_TideTide:SiteBBOC - Middle  0.13346    
#SH_TideTide:SiteTBOC - South   0.10416    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 10.64728)
#
#Null deviance: 1987.8  on 176  degrees of freedom
#Residual deviance: 1820.7  on 171  degrees of freedom
#AIC: 928.86
#
#Number of Fisher Scoring iterations: 2

#### AIC/BIC Scores ===============
AIC(m_null, m1, m2, m3, m4, m5)
BIC(m_null, m1, m2, m3, m4, m5)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m4), resid(m4))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m4))
qqline(resid(m4))

#### Density Plot of Residuals ===============
plot(density(resid(m4)))

#####################
#### Gamma =========
#####################

#### Model selection ====
#### m_null: Protein Carbonyl ~ 1  =====
m_null_Gamma <- glm(CarbPerProtein ~ 1, family = Gamma(link = "identity"), data = PC)
summary(m_null_Gamma)

#Call:
#glm(formula = CarbPerProtein ~ 1, family = Gamma(link = "identity"), 
#    data = PC)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1414  -0.3509  -0.1978  -0.0018   3.1707  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   4.4733     0.2526   17.71   <2e-16 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.5644078)
#
#Null deviance: 44.001  on 176  degrees of freedom
#Residual deviance: 44.001  on 176  degrees of freedom
#AIC: 753.59
#
#Number of Fisher Scoring iterations: 3


#### m6: Protein Carbonyl ~ SH_Temp  =====
m6 <- glm(CarbPerProtein ~ SH_Temp, family = Gamma(link = "identity"), data = PC)
summary(m6)

# Call:
#glm(formula = CarbPerProtein ~ SH_Temp, family = Gamma(link = "identity"), 
#    data = PC)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1993  -0.3332  -0.1766   0.0339   3.3905  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)
#(Intercept)  2.00816    1.53824   1.305    0.193
#SH_Temp      0.13708    0.08698   1.576    0.117
#
#(Dispersion parameter for Gamma family taken to be 0.5960394)
#
#Null deviance: 44.001  on 176  degrees of freedom
#Residual deviance: 42.500  on 175  degrees of freedom
#AIC: 749.19
#
#Number of Fisher Scoring iterations: 3

#### m7: Protein Carbonyl ~ SH_Tide  =====
m7 <- glm(CarbPerProtein ~ SH_Tide, family = Gamma(link = "identity"), data = PC)
summary(m7)

#Call:
#  glm(formula = CarbPerProtein ~ SH_Tide, family = Gamma(link = "identity"), 
#      data = PC)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.0619  -0.3463  -0.1830   0.0414   2.9358  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   3.9737     0.2939  13.522   <2e-16 ***
#  SH_TideTide   1.0049     0.4727   2.126   0.0349 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.4867282)
#
#Null deviance: 44.001  on 176  degrees of freedom
#Residual deviance: 41.755  on 175  degrees of freedom
#AIC: 745.94
#
#Number of Fisher Scoring iterations: 3

#### m8: Protein Carbonyl ~ Site  =====
m8 <- glm(CarbPerProtein ~ Site, family = Gamma(link = "identity"), data = PC)
summary(m8)

#Call:
#  glm(formula = CarbPerProtein ~ Site, family = Gamma(link = "identity"), 
#      data = PC)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.07688  -0.32418  -0.17193   0.02063   2.74320  

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         5.4560     0.4846  11.258  < 2e-16 ***
#  SiteBBOC - Middle  -1.3937     0.6061  -2.300  0.02266 *  
#  SiteTBOC - South   -1.5516     0.5943  -2.611  0.00981 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.4655112)

#Null deviance: 44.001  on 176  degrees of freedom
#Residual deviance: 39.929  on 174  degrees of freedom
#AIC: 739.72
#
#Number of Fisher Scoring iterations: 3

#### m9: Protein Carbonyl ~ SH_Tide + Site  =====
m9 <- glm(CarbPerProtein ~ SH_Tide + Site, family = Gamma(link = "identity"), data = PC)
summary(m9)

#Call:
#  glm(formula = CarbPerProtein ~ SH_Tide + Site, family = Gamma(link = "identity"), 
#      data = PC)

#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.01187  -0.32274  -0.16259   0.03007   2.63477  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         4.9529     0.4873  10.164   <2e-16 ***
#  SH_TideTide         0.8009     0.4311   1.858   0.0649 .  
#SiteBBOC - Middle  -1.2577     0.5711  -2.202   0.0290 *  
#  SiteTBOC - South   -1.4009     0.5602  -2.500   0.0133 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.430532)
#
#Null deviance: 44.001  on 176  degrees of freedom
#Residual deviance: 38.455  on 173  degrees of freedom
#AIC: 734.82
#
#Number of Fisher Scoring iterations: 6

#### m10: Protein Carbonyl ~ SH_Tide + Site + SH_Tide*Site  =====
m10 <- glm(CarbPerProtein ~ SH_Tide + Site + SH_Tide*Site, family = Gamma(link = "identity"), data = PC)
summary(m10)

# Call:
#glm(formula = CarbPerProtein ~ SH_Tide + Site + SH_Tide * Site, 
#    family = Gamma(link = "identity"), data = PC)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.03745  -0.30540  -0.16283   0.01474   2.36221  
#
#Coefficients:
#  Estimate Std. Error t value
#(Intercept)                     4.3377     0.4944   8.774
#SH_TideTide                     2.2752     0.9122   2.494
#SiteBBOC - Middle              -0.5032     0.6648  -0.757
#SiteTBOC - South               -0.5934     0.6531  -0.909
#SH_TideTide:SiteBBOC - Middle  -1.8195     1.1300  -1.610
#SH_TideTide:SiteTBOC - South   -1.9549     1.1085  -1.764
#Pr(>|t|)    
#(Intercept)                    1.7e-15 ***
#  SH_TideTide                     0.0136 *  
#  SiteBBOC - Middle               0.4502    
#SiteTBOC - South                0.3648    
#SH_TideTide:SiteBBOC - Middle   0.1092    
#SH_TideTide:SiteTBOC - South    0.0796 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for Gamma family taken to be 0.3896953)
#
#Null deviance: 44.001  on 176  degrees of freedom
#Residual deviance: 37.036  on 171  degrees of freedom
#AIC: 731.93
#
#Number of Fisher Scoring iterations: 3

#### m11: Protein Carbonyl ~ SH_Tide + Site + (1|Bag) =====
m11 <- glmer(CarbPerProtein ~ SH_Tide + Site + (1|Bag), family = Gamma(link = "identity"), data = PC)
summary(m11)

# Generalized linear mixed model fit by maximum likelihood (Laplace  Approximation)
#[glmerMod]
#Family: Gamma  ( identity )
#Formula: CarbPerProtein ~ SH_Tide + Site + (1 | Bag)
#Data: PC
#
#AIC      BIC   logLik deviance df.resid 
#714.1    733.2   -351.1    702.1      171 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-1.2711 -0.4790 -0.1669  0.1047  7.6055 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Bag      (Intercept) 1.300    1.1402  
#Residual             0.276    0.5254  
#Number of obs: 177, groups:  Bag, 36
#
#Fixed effects:
#  Estimate Std. Error t value Pr(>|z|)    
#(Intercept)         4.8513     0.5125   9.465   <2e-16 ***
#  SH_TideTide         0.8036     0.5163   1.556   0.1196    
#SiteBBOC - Middle  -1.1105     0.6320  -1.757   0.0789 .  
#SiteTBOC - South   -1.2907     0.6309  -2.046   0.0408 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) SH_TdT SBBO-M
#SH_TideTide -0.454              
#StBBOC-Mddl -0.602 -0.047       
#SitTBOC-Sth -0.594 -0.058  0.502

#### AIC/BIC Scores ===============
AIC(m_null_Gamma, m6, m7, m8, m9, m10, m11)
BIC(m_null_Gamma, m6, m7, m8, m9, m10, m11)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m9), resid(m9))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m9))
qqline(resid(m9))

#### Density Plot of Residuals ===============
plot(density(resid(m9)))
