#### ~ L/W STATS ~ =====

### load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

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

#### m2.bag: L ~ Sampling_Time + (1|Bag)  =====
m1.bag <- lmer(L ~ Sampling_Time + (1|Bag), data = LW2020)
summary(m1.bag)

#### AIC/BIC Scores ===============
AIC(m_null_2020, m1, m2, m1.bag)
BIC(m_null_2020, m1, m2, m1.bag)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m1.bag), resid(m1.bag))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m1.bag))
qqline(resid(m1.bag))

#### Density Plot of Residuals ===============
plot(density(resid(m1.bag)))


#### Plot 2020 Model =========
m1.plot <- ggpredict(m1.bag, terms = c("Sampling_Time"))

plot(m1.plot) +
  theme_classic() +
  #scale_color_brewer(palette = "Paired", direction = -1)  +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Length ~ Sampling_Time")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Sampling Time Point", 
       y = "Shell Length (cm)")

ggsave(filename = "fig_output/model_MgigasLength_Sampling.png", width = 5.10, height = 5.77, dpi = 300)

##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m1.DARKplot <- ggpredict(m1, terms = c("Sampling_Time"))
plot(m1.DARKplot) +
  dark_theme_classic() +
  #scale_color_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Length ~ Sampling_Time")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Sampling Time Point", 
       y = "Shell Length (cm)")

ggsave(filename = "fig_output/DARKmodel_MgigasLength_Sampling.png", width = 5.10, height = 5.77, dpi = 300)


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
  
#### m8.bag: L ~ Sampling_Time + Site + Sampling_Time*Site + (1|Bag)  =====
m8.bag <- lmer(L ~ Sampling_Time + Site + Sampling_Time*Site + (1|Bag), data = LW2021.sik)
summary(m8.bag)

#### AIC/BIC Scores ===============
AIC(m_null.sik, m3, m4, m5, m6, m7, m8, m8.bag)
BIC(m_null.sik, m3, m4, m5, m6, m7, m8, m8.bag)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m8.bag), resid(m8.bag))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m8.bag))
qqline(resid(m8.bag))

#### Density Plot of Residuals ===============
plot(density(resid(m8.bag)))

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


#### Plot 2021 C. Sikamea Model =========
m8.plot <- ggpredict(m8, terms = c("Sampling_Time", "Site"))

plot(m8.plot) +
  theme_classic() +
  #scale_color_brewer(palette = "Paired", direction = -1)  +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Length ~ Sampling_Time")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Sampling Time Point", 
       y = "Shell Length (cm)")

ggsave(filename = "fig_output/model_CsikameaLength_Sampling.png", width = 5.10, height = 5.77, dpi = 300)

##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m8.DARKplot <- ggpredict(m8, terms = c("Sampling_Time", "Site"))

plot(m8.DARKplot) +
  dark_theme_classic() +
  #scale_color_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Length ~ Sampling_Time")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Sampling Time Point", 
       y = "Shell Length (cm)")

ggsave(filename = "fig_output/DARKmodel_CsikameaLength_Sampling.png", width = 5.10, height = 5.77, dpi = 300)

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


#### m11a: L ~ Year/Sampling_Time =====

m11a <- glm(L ~ Year/Sampling_Time, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m11a)

#Call:
#glm(formula = L ~ Year/Sampling_Time, family = gaussian(link = "identity"), 
#    data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0901  -0.3951  -0.0450   0.3677   2.7639  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         -1.263e+03  1.111e+02  -11.36   <2e-16 ***
#  Year                 6.262e-01  5.498e-02   11.39   <2e-16 ***
#  Year:Sampling_Time4  3.616e-04  3.156e-05   11.46   <2e-16 ***
#  Year:Sampling_Time6  5.745e-04  2.229e-05   25.77   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.3610219)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 383.41  on 1062  degrees of freedom
#AIC: 1945.1
#
#Number of Fisher Scoring iterations: 2

#### m12: L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time =====
m12 <- glm(L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m12)

#Call:
#glm(formula = L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time, 
#    family = gaussian(link = "identity"), data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0826  -0.3920  -0.0531   0.3643   2.6253  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         -1.105e+03  1.174e+02  -9.409  < 2e-16 ***
#  SH_Temp             -1.191e-02  6.327e-03  -1.882 0.060121 .  
#SH_TideTide          1.461e-01  4.405e-02   3.317 0.000942 ***
#  SiteMiddle           4.456e-02  4.408e-02   1.011 0.312367    
#SiteSouth           -2.136e-01  4.402e-02  -4.853  1.4e-06 ***
#  Year                 5.481e-01  5.809e-02   9.437  < 2e-16 ***
#  Year:Sampling_Time4  3.630e-04  3.085e-05  11.767  < 2e-16 ***
#  Year:Sampling_Time6  5.742e-04  2.179e-05  26.355  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.3449315)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 364.94  on 1058  degrees of freedom
#AIC: 1900.5
#
#Number of Fisher Scoring iterations: 2

#### m13: L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time + SH_Temp*SH_Tide =====
m13 <- glm(L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time + SH_Temp*SH_Tide, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m13)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time + 
#      SH_Temp * SH_Tide, family = gaussian(link = "identity"), 
#    data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0866  -0.3890  -0.0542   0.3657   2.6322  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         -1.106e+03  1.176e+02  -9.409  < 2e-16 ***
#  SH_Temp             -1.054e-02  7.966e-03  -1.324    0.186    
#SH_TideTide          2.129e-01  2.408e-01   0.884    0.377    
#SiteMiddle           4.455e-02  4.410e-02   1.010    0.313    
#SiteSouth           -2.137e-01  4.404e-02  -4.851 1.41e-06 ***
#  Year                 5.488e-01  5.816e-02   9.436  < 2e-16 ***
#  Year:Sampling_Time4  3.630e-04  3.086e-05  11.761  < 2e-16 ***
#  Year:Sampling_Time6  5.742e-04  2.180e-05  26.343  < 2e-16 ***
#  SH_Temp:SH_TideTide -3.701e-03  1.312e-02  -0.282    0.778    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.3452318)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 364.91  on 1057  degrees of freedom
#AIC: 1902.4
#
#Number of Fisher Scoring iterations: 2


#### m14: L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time + SH_Tide*Site =====
m14 <- glm(L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time + SH_Tide*Site, family = gaussian(link = "identity"), data = LW21_22.gigas)
summary(m14)

# Call:
#glm(formula = L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time + 
#      SH_Tide * Site, family = gaussian(link = "identity"), data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-2.07137  -0.39428  -0.06215   0.37039   2.60257  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)            -1.105e+03  1.174e+02  -9.414  < 2e-16 ***
#  SH_Temp                -1.192e-02  6.326e-03  -1.885  0.05971 .  
#SH_TideTide             1.944e-01  6.962e-02   2.792  0.00533 ** 
#  SiteMiddle              4.934e-02  5.389e-02   0.916  0.36004    
#SiteSouth              -1.703e-01  5.383e-02  -3.163  0.00160 ** 
#  Year                    5.483e-01  5.808e-02   9.441  < 2e-16 ***
#  Year:Sampling_Time4     3.629e-04  3.085e-05  11.763  < 2e-16 ***
#  Year:Sampling_Time6     5.741e-04  2.178e-05  26.357  < 2e-16 ***
#  SH_TideTide:SiteMiddle -1.434e-02  9.366e-02  -0.153  0.87833    
#SH_TideTide:SiteSouth  -1.308e-01  9.350e-02  -1.399  0.16223    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.344817)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 364.13  on 1056  degrees of freedom
#AIC: 1902.1
#
#Number of Fisher Scoring iterations: 2

#### m15: L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time + (1|Bag) =====
m15 <- lmer(L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time + (1|Bag), data = LW21_22.gigas)
summary(m15)

#Call:
#glm(formula = L ~ SH_Temp + SH_Tide + Site + Year/Sampling_Time, 
#    family = gaussian(link = "identity"), data = LW21_22.gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-2.0826  -0.3920  -0.0531   0.3643   2.6253  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         -1.105e+03  1.174e+02  -9.409  < 2e-16 ***
#  SH_Temp             -1.191e-02  6.327e-03  -1.882 0.060121 .  
#SH_TideTide          1.461e-01  4.405e-02   3.317 0.000942 ***
#  SiteMiddle           4.456e-02  4.408e-02   1.011 0.312367    
#SiteSouth           -2.136e-01  4.402e-02  -4.853  1.4e-06 ***
#  Year                 5.481e-01  5.809e-02   9.437  < 2e-16 ***
#  Year:Sampling_Time4  3.630e-04  3.085e-05  11.767  < 2e-16 ***
#  Year:Sampling_Time6  5.742e-04  2.179e-05  26.355  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 0.3449315)
#
#Null deviance: 840.70  on 1065  degrees of freedom
#Residual deviance: 364.94  on 1058  degrees of freedom
#AIC: 1900.5
#
#Number of Fisher Scoring iterations: 2


#### AIC/BIC Scores ===============
AIC(m_null_gigas, m9, m10, m11, m12, m13, m14, m15)
BIC(m_null_gigas, m9, m10, m11, m12, m13, m14, m15)

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m12), resid(m12))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m12))
qqline(resid(m12))

#### Density Plot of Residuals ===============
plot(density(resid(m12)))

#### Plot 2021/2022 M. gigas Model =========
m12.plot <- ggpredict(m12 terms = c("Site", "SH_Temp", "SH_Tide"))

plot(m12.plot) +
  theme_classic() + 
  #scale_color_brewer(palette = "Paired", direction = -1)  +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Length ~ Sampling_Time")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Site", 
       y = "Shell Length (cm)")

ggsave(filename = "fig_output/model_Mgigas_20-21.png", width = 5.10, height = 5.77, dpi = 300)

##### DARK PLOTS: ggdark / black background =================
library(ggdark)

12.DARKplot <- ggpredict(m12, terms = c("Site", "SH_Temp", "SH_Tide"))

plot(m12.DARKplot) +
  dark_theme_classic() +
  #scale_color_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Length ~ Sampling_Time")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Site", 
       y = "Shell Length (cm)")

ggsave(filename = "fig_output/DARKmodel_Mgigas_20-21.png", width = 5.10, height = 5.77, dpi = 300)

