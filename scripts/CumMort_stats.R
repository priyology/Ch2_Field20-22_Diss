########## 2022 Cumulative Mortality Stats ==============

### load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

#### ** 2020 Mortality ** ===============

### load data sheet
CumMorts2020 <- read_csv("data/Mortality/2020/CumMort2020.csv")
glimpse(CumMorts2020)
summary(CumMorts2020)
View(CumMorts2020)

### change attributes about statistical factors
CumMorts2020$Year <- as.factor(CumMorts2020$Year) ## factor
is.factor(CumMorts2020$Year) ## TRUE

#### m_null: Mortality ~ 1 ===============
m_null <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = CumMorts2020)
summary(m_null)

#Call:
#glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#    data = CumMorts2020)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.4368  -0.4368  -0.4368  -0.4368   2.1895  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -2.30149    0.07775   -29.6   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 1219.4  on 1999  degrees of freedom
#Residual deviance: 1219.4  on 1999  degrees of freedom
#AIC: 1221.4

#Number of Fisher Scoring iterations: 5


#### m1: Mortality ~ Site ===============
m1 <- glm(Mortality ~ Site, family = binomial(link = "logit"), data = CumMorts2020)
summary(m1)

# Call:
#glm(formula = Mortality ~ Site, family = binomial(link = "logit"), 
#    data = CumMorts2020)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.4781  -0.4781  -0.3921  -0.3921   2.2820  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -2.1113     0.1019 -20.723  < 2e-16 ***
#  SiteSouth    -0.4155     0.1580  -2.629  0.00856 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 1219.4  on 1999  degrees of freedom
#Residual deviance: 1212.4  on 1998  degrees of freedom
#AIC: 1216.4
#
#Number of Fisher Scoring iterations: 5







#### Pairwise Comparisons ===============
# pairwise comparison for m12
#emm_m6 <-  emmeans(m6, specs = ~ MHW)
#emm_m6
#pairwise_m6 <- contrast(emm_m6, interaction = "pairwise")
#pairwise_m6

#### AIC/BIC Scores ===============
AIC(m_null, m1a, m2a, m3a)
BIC(m_null, m1a, m2a, m3a)

#### Test Assumptions: m9 ===============
#???

#### Plot Model ========
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

m1.Plot <- ggpredict(m1, terms = c("Site"))

plot(m1.Plot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("M. gigas: Mortality ~ Site, family = binomial(link = logit)")), 
       #subtitle = "Post-Outplanting Mortality",
       x = "Site", 
       y = "Mortality")

ggsave(filename = "fig_output/model_2020Mortality.png", width = 5.10, height = 5.77, dpi = 300)


##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m1.DARKPlot <- ggpredict(m1, terms = c("Site"))

plot(m1.DARKPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("M. gigas: Mortality ~ Site, family = binomial(link = logit)")), 
       #subtitle = "Post-Outplanting Mortality",
       x = "Site", 
       y = "Mortality")

ggsave(filename = "fig_output/DARKmodel_2020Mortality.png", width = 5.10, height = 5.77, dpi = 300)


#### ** 2021 Mortality ** ===============

### load data sheet
CumMorts2021 <- read_csv("data/Mortality/2021/CumMort2021.csv")
glimpse(CumMorts2021)
summary(CumMorts2021)
View(CumMorts2021)

### change attributes about statistical factors
CumMorts2021$SH_Temp <- as.character(CumMorts2021$SH_Temp) ## factor
is.character(CumMorts2021$SH_Temp) ## TRUE

#### 2021: M. gigas ====

CumMorts2021_gigas <- CumMorts2021 %>% 
  filter(Species == "M. gigas")

#### m_null: Mortality ~ 1 ===============
m_gigas_null <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = CumMorts2021_gigas)
summary(m_gigas_null)

#Call:
#  glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#      data = CumMorts2021_gigas)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-1.134  -1.134  -1.134   1.221   1.221  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)   
#(Intercept) -0.10182    0.03147  -3.236  0.00121 **
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 5604  on 4049  degrees of freedom
#Residual deviance: 5604  on 4049  degrees of freedom
#AIC: 5606
#
#Number of Fisher Scoring iterations: 3


#### m2: Mortality ~ SH_Temp ===============
m2 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = CumMorts2021_gigas)
summary(m2)

#Call:
#  glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#      data = CumMorts2021_gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.2744  -0.9994  -0.9994   1.0834   1.3665  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.22514    0.04473   5.034 4.81e-07 ***
#  SH_Temp21   -0.65950    0.06380 -10.337  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 5604.0  on 4049  degrees of freedom
#Residual deviance: 5495.7  on 4048  degrees of freedom
#AIC: 5499.7
#
#Number of Fisher Scoring iterations: 4


#### m3: Mortality ~ Site ===============
m3 <- glm(Mortality ~ Site, family = binomial(link = "logit"), data = CumMorts2021_gigas)
summary(m3)

# Call:
#glm(formula = Mortality ~ Site, family = binomial(link = "logit"), 
#    data = CumMorts2021_gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5344  -0.3650  -0.3650   0.8583   2.3418  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  0.69648    0.05777  12.057   <2e-16 ***
#  SiteNorth    0.11231    0.08253   1.361    0.174    
#SiteSouth   -3.37182    0.12499 -26.976   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 5604.0  on 4049  degrees of freedom
#Residual deviance: 4030.1  on 4047  degrees of freedom
#AIC: 4036.1
#
#Number of Fisher Scoring iterations: 5

#### m4: Mortality ~ SH_Temp + Site ===============
m4 <- glm(Mortality ~ SH_Temp + Site, family = binomial(link = "logit"), data = CumMorts2021_gigas)
summary(m4)

# Call:
#glm(formula = Mortality ~ SH_Temp + Site, family = binomial(link = "logit"), 
#    data = CumMorts2021_gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7869  -0.4423  -0.2696   0.7094   2.5818  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  1.25145    0.07632  16.397   <2e-16 ***
#  SH_Temp21   -1.02139    0.08149 -12.534   <2e-16 ***
#  SiteNorth    0.11865    0.08484   1.399    0.162    
#SiteSouth   -3.52663    0.12876 -27.390   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 5604.0  on 4049  degrees of freedom
#Residual deviance: 3864.5  on 4046  degrees of freedom
#AIC: 3872.5
#
#Number of Fisher Scoring iterations: 5

#### m5: Mortality ~ SH_Temp + Site + SH_Temp*Site ===============
m5 <- glm(Mortality ~ SH_Temp + Site + SH_Temp*Site, family = binomial(link = "logit"), data = CumMorts2021_gigas)
summary(m5)

# Call:
#glm(formula = Mortality ~ SH_Temp + Site + SH_Temp * Site, family = binomial(link = "logit"), 
#    data = CumMorts2021_gigas)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8193  -0.3715  -0.3584   0.6818   2.3567  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           1.4428     0.0979  14.738  < 2e-16 ***
#  SH_Temp21            -1.3390     0.1246 -10.746  < 2e-16 ***
#  SiteNorth            -0.1022     0.1364  -0.749   0.4537    
#SiteSouth            -4.0819     0.1827 -22.337  < 2e-16 ***
#  SH_Temp21:SiteNorth   0.3669     0.1751   2.095   0.0361 *  
#  SH_Temp21:SiteSouth   1.2653     0.2544   4.973 6.58e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 5604.0  on 4049  degrees of freedom
#Residual deviance: 3840.1  on 4044  degrees of freedom
#AIC: 3852.1
#
#Number of Fisher Scoring iterations: 5


#### AIC/BIC Scores ===============
AIC(m_gigas_null, m2, m3, m4, m5)

#              df      AIC
#m_gigas_null  1 5606.010
#m2            2 5499.686
#m3            3 4036.117
#m4            4 3872.511
#m5            6 3852.148

BIC(m_gigas_null, m2, m3, m4, m5)

#             df      BIC
#m_gigas_null  1 5612.316
#m2            2 5512.299
#m3            3 4055.036
#m4            4 3897.737
#m5            6 3889.987

#### Pairwise Comparisons ===============
## pairwise comparison for m5
emm_Lm5a <-  emmeans(m5, specs = ~ Site|SH_Temp)
emm_Lm5a

pairwise_Lm5a <- contrast(emm_Lm5a, interaction = "pairwise")
pairwise_Lm5a

# SH_Temp = 16:
#Site_pairwise  estimate    SE  df z.ratio p.value
#Middle - North    0.102 0.136 Inf   0.749  0.4537
#Middle - South    4.082 0.183 Inf  22.337  <.0001
#North - South     3.980 0.181 Inf  21.967  <.0001
#
#SH_Temp = 21:
#  Site_pairwise  estimate    SE  df z.ratio p.value
#Middle - North   -0.265 0.110 Inf  -2.410  0.0160
#Middle - South    2.817 0.177 Inf  15.913  <.0001
#North - South     3.081 0.178 Inf  17.356  <.0001

#### Plot Model ========
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

m5.Plot <- ggpredict(m5, terms = c("SH_Temp", "Site"))

plot(m5.Plot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("M. gigas: Mortality ~ SH_Temp + Site + SH_Temp*Site , family = binomial(link = logit)")), 
       #subtitle = "Post-Outplanting Mortality",
       x = "Site", 
       y = "Mortality")

ggsave(filename = "fig_output/model_2021gigasMortality.png", width = 5.10, height = 5.77, dpi = 300)


##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m5.DARKPlot <- ggpredict(m5, terms = c("SH_Temp", "Site"))

plot(m5.DARKPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("M. gigas: Mortality ~ SH_Temp + Site + SH_Temp*Site , family = binomial(link = logit)")), 
       #subtitle = "Post-Outplanting Mortality",
       x = "Site", 
       y = "Mortality")

ggsave(filename = "fig_output/DARKmodel_2021gigasMortality.png", width = 5.10, height = 5.77, dpi = 300)



#### 2021: C. sikamea ====

CumMorts2021_sikamea <- CumMorts2021 %>% 
  filter(Species == "C. sikamea")

#### m_null: Mortality ~ 1 ===============
m_sikamea_null <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = CumMorts2021_sikamea)
summary(m_sikamea_null)

# Call:
# glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#    data = CumMorts2021_sikamea)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.3968  -0.3968  -0.3968  -0.3968   2.2719  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -2.50191    0.04154  -60.23   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 4441.4  on 8279  degrees of freedom
#Residual deviance: 4441.4  on 8279  degrees of freedom
#AIC: 4443.4
#
#Number of Fisher Scoring iterations: 5

#### m6: Mortality ~ SH_Temp ===============
m6 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = CumMorts2021_sikamea)
summary(m6)

# Call:
#glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#    data = CumMorts2021_sikamea)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.4172  -0.4172  -0.3791  -0.3791   2.3103  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -2.59698    0.05868 -44.253   <2e-16 ***
#  SH_Temp21    0.19908    0.08311   2.395   0.0166 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 4441.4  on 8279  degrees of freedom
#Residual deviance: 4435.7  on 8278  degrees of freedom
#AIC: 4439.7
#
#Number of Fisher Scoring iterations: 5

#### m7: Mortality ~ SH_Tide ===============
m7 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = CumMorts2021_sikamea)
summary(m7)

# Call:
# glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#    data = CumMorts2021_sikamea)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.3985  -0.3985  -0.3954  -0.3954   2.2749  
#
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -2.49315    0.06089 -40.945   <2e-16 ***
#  SH_TideTide -0.01633    0.08328  -0.196    0.845    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 4441.4  on 8279  degrees of freedom
#Residual deviance: 4441.4  on 8278  degrees of freedom
#AIC: 4445.4
#
#Number of Fisher Scoring iterations: 5

#### m8: Mortality ~ Site ===============
m8 <- glm(Mortality ~ Site, family = binomial(link = "logit"), data = CumMorts2021_sikamea)
summary(m8)

# Call:
# glm(formula = Mortality ~ Site, family = binomial(link = "logit"), 
#    data = CumMorts2021_sikamea)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6619  -0.6619  -0.1873  -0.1620   2.9460  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.0342     0.1456 -27.706   <2e-16 ***
#  SiteNorth    -0.2921     0.2221  -1.315    0.189    
#SiteSouth     2.6274     0.1533  17.141   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 4441.4  on 8279  degrees of freedom
#Residual deviance: 3605.2  on 8277  degrees of freedom
#AIC: 3611.2
#
#Number of Fisher Scoring iterations: 7

#### m9: Mortality ~ SH_Temp + Site ===============
m9 <- glm(Mortality ~ SH_Temp + Site, family = binomial(link = "logit"), data = CumMorts2021_sikamea)
summary(m9)

# Call:
#glm(formula = Mortality ~ SH_Temp + Site, family = binomial(link = "logit"), 
#    data = CumMorts2021_sikamea)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6975  -0.6312  -0.1776  -0.1536   2.9819  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -4.14178    0.15237 -27.182   <2e-16 ***
#  SH_Temp21    0.22252    0.08789   2.532   0.0113 *  
#  SiteNorth   -0.29215    0.22216  -1.315   0.1885    
#SiteSouth    2.62967    0.15332  17.152   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 4441.4  on 8279  degrees of freedom
#Residual deviance: 3598.8  on 8276  degrees of freedom
#AIC: 3606.8
#
#Number of Fisher Scoring iterations: 7

#### m10: Mortality ~ SH_Temp + Site + SH_Temp*Site ===============
m10 <- glm(Mortality ~ SH_Temp + Site + SH_Temp*Site, family = binomial(link = "logit"), data = CumMorts2021_sikamea)
summary(m10)

# Call:
#glm(formula = Mortality ~ SH_Temp + Site + SH_Temp * Site, family = binomial(link = "logit"), 
#    data = CumMorts2021_sikamea)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6930  -0.6351  -0.1789  -0.1464   3.0135  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -4.20740    0.21478 -19.589   <2e-16 ***
#  SH_Temp21            0.34748    0.29224   1.189    0.234    
#SiteNorth           -0.32251    0.33061  -0.975    0.329    
#SiteSouth            2.70902    0.22494  12.043   <2e-16 ***
#  SH_Temp21:SiteNorth  0.05529    0.44652   0.124    0.901    
#SH_Temp21:SiteSouth -0.15310    0.30756  -0.498    0.619    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 4441.4  on 8279  degrees of freedom
#Residual deviance: 3598.2  on 8274  degrees of freedom
#AIC: 3610.2
#
#Number of Fisher Scoring iterations: 7

#### AIC/BIC Scores ===============
AIC(m_sikamea_null, m6, m7, m8, m9, m10)

#               df      AIC
#m_sikamea_null  1 4443.412
#m6              2 4439.680
#m7              2 4445.373
#m8              3 3611.186
#m9              4 3606.780
#m10             6 3610.224


BIC(m_sikamea_null, m6, m7, m8, m9, m10)
#               df      BIC
#m_sikamea_null  1 4450.433
#m6              2 4453.723
#m7              2 4459.417
#m8              3 3632.250
#m9              4 3634.866
#m10             6 3652.353

#### Pairwise Comparisons ===============
## pairwise comparison for m8
emm_Lm9a <-  emmeans(m9, specs = ~ Site|SH_Temp)
emm_Lm9a

pairwise_Lm9a <- contrast(emm_Lm9a, interaction = "pairwise")
pairwise_Lm9a

# SH_Temp = 16:
#Site_pairwise  estimate    SE  df z.ratio p.value
#Middle - North    0.292 0.222 Inf   1.315  0.1885
#Middle - South   -2.630 0.153 Inf -17.152  <.0001
#North - South    -2.922 0.174 Inf -16.744  <.0001
#
#SH_Temp = 21:
#  Site_pairwise  estimate    SE  df z.ratio p.value
#Middle - North    0.292 0.222 Inf   1.315  0.1885
#Middle - South   -2.630 0.153 Inf -17.152  <.0001
#North - South    -2.922 0.174 Inf -16.744  <.0001

#Results are given on the log odds ratio (not the response) scale. 

#### Plot Model ========
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

m9.Plot <- ggpredict(m9, terms = c("SH_Temp", "Site"))

plot(m9.Plot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("C. sikamea: Mortality ~ SH_Temp + Site, family = binomial(link = logit)")), 
       #subtitle = "Post-Outplanting Mortality",
       x = "Site", 
       y = "Mortality")

ggsave(filename = "fig_output/model_2021gigasMortality.png", width = 5.10, height = 5.77, dpi = 300)


##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m9.DARKPlot <- ggpredict(m9, terms = c("SH_Temp", "Site"))

plot(m9.DARKPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("C. sikamea: Mortality ~ SH_Temp + Site, family = binomial(link = logit)")), 
       #subtitle = "Post-Outplanting Mortality",
       x = "Site", 
       y = "Mortality")

ggsave(filename = "fig_output/DARKmodel_2021sikameaMortality.png", width = 5.10, height = 5.77, dpi = 300)




#### ** 2022 Mortality ** ===============

### load data sheet
CumMorts2022 <- read_csv("data/Mortality/2022/CumMort2022.csv")
glimpse(CumMorts2022)
summary(CumMorts2022)
View(CumMorts2022)

#### m_2022_null: Mortality ~ 1 ===============
m_2022_null <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = CumMorts2022)
summary(m_2022_null)

# Call:
#glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#    data = CumMorts2022)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6257  -0.6257  -0.6257  -0.6257   1.8586  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.53148    0.02757  -55.55   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 8424.1  on 8999  degrees of freedom
#AIC: 8426.1
#
#Number of Fisher Scoring iterations: 4

#### m11: Mortality ~ SH_Temp ===============
m11 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = CumMorts2022)
summary(m11)

#Call:
#  glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#      data = CumMorts2022)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6854  -0.6854  -0.5623  -0.5623   1.9609  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -2.853530   0.173772 -16.421  < 2e-16 ***
#  SH_Temp      0.072599   0.009313   7.795 6.44e-15 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 8362.5  on 8998  degrees of freedom
#AIC: 8366.5
#
#Number of Fisher Scoring iterations: 4

#### m12: Mortality ~ SH_Tide ===============
m12 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = CumMorts2022)
summary(m12)

#Call:
#  glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#      data = CumMorts2022)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.6689  -0.6689  -0.5806  -0.5806   1.9306  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.38352    0.03724 -37.155   <2e-16 ***
#  SH_TideTide -0.31154    0.05552  -5.612    2e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 8392.4  on 8998  degrees of freedom
#AIC: 8396.4
#
#Number of Fisher Scoring iterations: 4

#### m13: Mortality ~ Site ===============
m13 <- glm(Mortality ~ Site, family = binomial(link = "logit"), data = CumMorts2022)
summary(m13)

# Call:
#glm(formula = Mortality ~ Site, family = binomial(link = "logit"), 
#    data = CumMorts2022)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7550  -0.7550  -0.7019  -0.3724   2.3251  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.10931    0.04228 -26.239  < 2e-16 ***
#  SiteNorth   -0.16610    0.06116  -2.716  0.00661 ** 
#  SiteSouth   -1.52441    0.08438 -18.066  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 7984.4  on 8997  degrees of freedom
#AIC: 7990.4
#
#Number of Fisher Scoring iterations: 5


#### m14: Mortality ~ SH_Temp + SH_Tide ===============
m14 <- glm(Mortality ~ SH_Temp + SH_Tide, family = binomial(link = "logit"), data = CumMorts2022)
summary(m14)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide, family = binomial(link = "logit"), 
#    data = CumMorts2022)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7323  -0.6367  -0.6019  -0.5206   2.0325  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -2.70924    0.17563 -15.426  < 2e-16 ***
#  SH_Temp      0.07286    0.00933   7.809 5.78e-15 ***
#  SH_TideTide -0.31369    0.05571  -5.631 1.79e-08 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 8330.6  on 8997  degrees of freedom
#AIC: 8336.6
#
#Number of Fisher Scoring iterations: 4

#### m15: Mortality ~ SH_Temp + Site ===============
m15 <- glm(Mortality ~ SH_Temp + Site, family = binomial(link = "logit"), data = CumMorts2022)
summary(m15)

# Call:
#glm(formula = Mortality ~ SH_Temp + Site, family = binomial(link = "logit"), 
#    data = CumMorts2022)
##
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.8278  -0.6790  -0.6297  -0.3299   2.4241  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -2.48786    0.17974 -13.841  < 2e-16 ***
#  SH_Temp      0.07586    0.00952   7.968 1.61e-15 ***
#  SiteNorth   -0.16762    0.06144  -2.728  0.00637 ** 
#  SiteSouth   -1.53365    0.08461 -18.126  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 7920.0  on 8996  degrees of freedom
#AIC: 7928
#
#Number of Fisher Scoring iterations: 5


#### m16: Mortality ~ SH_Tide + Site ===============
m16 <- glm(Mortality ~ SH_Tide + Site, family = binomial(link = "logit"), data = CumMorts2022)
summary(m16)

#Call:
#  glm(formula = Mortality ~ SH_Tide + Site, family = binomial(link = "logit"), 
#      data = CumMorts2022)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.8075  -0.7008  -0.6505  -0.3423   2.3944  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.95322    0.04974 -19.162  < 2e-16 ***
#  SH_TideTide -0.32554    0.05675  -5.736 9.68e-09 ***
#  SiteNorth   -0.16688    0.06130  -2.722  0.00649 ** 
#  SiteSouth   -1.52917    0.08450 -18.097  < 2e-16 ***
#  ---
 # Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 7951.2  on 8996  degrees of freedom
#AIC: 7959.2
#
#Number of Fisher Scoring iterations: 5

#### m17: Mortality ~ SH_Temp + SH_Tide + Site ===============
m17 <- glm(Mortality ~ SH_Temp + SH_Tide + Site, family = binomial(link = "logit"), data = CumMorts2022)
summary(m17)

# Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide + Site, family = binomial(link = "logit"), 
#      data = CumMorts2022)
#
# Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
# -0.8846  -0.7151  -0.5819  -0.3027   2.4925  
#
# Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
# (Intercept) -2.336386   0.181715 -12.857  < 2e-16 ***
#  SH_Temp      0.076187   0.009541   7.985 1.41e-15 ***
#  SH_TideTide -0.328254   0.056990  -5.760 8.42e-09 ***
#  SiteNorth   -0.168431   0.061589  -2.735  0.00624 ** 
#  SiteSouth   -1.538622   0.084737 -18.158  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 7886.6  on 8995  degrees of freedom
#AIC: 7896.6
#
#Number of Fisher Scoring iterations: 5

#### m18: Mortality ~ SH_Temp + SH_Tide + Site + SH_Temp*SH_Tide ===============
m18 <- glm(Mortality ~ SH_Temp + SH_Tide + Site + SH_Temp*SH_Tide, family = binomial(link = "logit"), data = CumMorts2022)
summary(m18)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Site + SH_Temp * 
#      SH_Tide, family = binomial(link = "logit"), data = CumMorts2022)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.8809  -0.7188  -0.5777  -0.3004   2.4985  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         -2.267518   0.240894  -9.413  < 2e-16 ***
#  SH_Temp              0.072434   0.012866   5.630 1.81e-08 ***
#  SH_TideTide         -0.481470   0.357534  -1.347  0.17810    
#SiteNorth           -0.168423   0.061587  -2.735  0.00624 ** 
#  SiteSouth           -1.538504   0.084733 -18.157  < 2e-16 ***
#  SH_Temp:SH_TideTide  0.008325   0.019174   0.434  0.66417    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 7886.4  on 8994  degrees of freedom
#AIC: 7898.4
#
#Number of Fisher Scoring iterations: 5

#### m19: Mortality ~ SH_Temp + SH_Tide + Site + SH_Temp*Site ===============
m19 <- glm(Mortality ~ SH_Temp + SH_Tide + Site + SH_Temp*Site, family = binomial(link = "logit"), data = CumMorts2022)
summary(m19)

#Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Site + SH_Temp * 
#      Site, family = binomial(link = "logit"), data = CumMorts2022)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.8967  -0.7167  -0.5061  -0.3246   2.4371  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       -1.27516    0.26037  -4.898 9.70e-07 ***
#  SH_Temp            0.01797    0.01414   1.271 0.203637    
#SH_TideTide       -0.33051    0.05719  -5.779 7.50e-09 ***
#  SiteNorth         -2.77552    0.39406  -7.043 1.88e-12 ***
#  SiteSouth         -1.82017    0.52162  -3.489 0.000484 ***
#  SH_Temp:SiteNorth  0.14142    0.02106   6.715 1.88e-11 ***
#  SH_Temp:SiteSouth  0.01595    0.02825   0.565 0.572375    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 7837.4  on 8993  degrees of freedom
#AIC: 7851.4
#
#Number of Fisher Scoring iterations: 5

#### m20: Mortality ~ SH_Temp + SH_Tide + Site + SH_Tide*Site ===============
m20 <- glm(Mortality ~ SH_Temp + SH_Tide + Site + SH_Tide*Site, family = binomial(link = "logit"), data = CumMorts2022)
summary(m20)

#Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Site + SH_Tide * 
 #     Site, family = binomial(link = "logit"), data = CumMorts2022)

#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.8855  -0.7041  -0.5725  -0.3184   2.4525  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           -2.334951   0.184053 -12.686  < 2e-16 ***
#  SH_Temp                0.076237   0.009545   7.987 1.38e-15 ***
#  SH_TideTide           -0.333380   0.085474  -3.900 9.60e-05 ***
#  SiteNorth             -0.141852   0.083241  -1.704   0.0884 .  
#SiteSouth             -1.626009   0.116000 -14.017  < 2e-16 ***
#  SH_TideTide:SiteNorth -0.059154   0.123826  -0.478   0.6329    
#SH_TideTide:SiteSouth  0.194114   0.169666   1.144   0.2526    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 8424.1  on 8999  degrees of freedom
#Residual deviance: 7884.4  on 8993  degrees of freedom
#AIC: 7898.4
#
#Number of Fisher Scoring iterations: 5


#### AIC/BIC Scores ===============

AIC(m_2022_null, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20)

#                       df      AIC
#m_2022_null  1 8426.127
#m11          2 8366.547
#m12          2 8396.421
#m13          3 7990.359
#m14          3 8336.623
#m15          4 7928.013
#m16          4 7959.229
#m17          5 7896.607
#m18          6 7898.419
#m19          7 7851.377
#m20          7 7898.433

BIC(m_2022_null, m11, m12, m13, m14, m15, m16, m17, m18, m19, m20)

#               df      BIC
#m_2022_null  1 8433.232
#m11          2 8380.757
#m12          2 8410.631
#m13          3 8011.674
#m14          3 8357.938
#m15          4 7956.432
#m16          4 7987.649
#m17          5 7932.132
#m18          6 7941.049
#m19          7 7901.112
#m20          7 7948.168


## pairwise comparison for m19
emm_Lm19a <-  emmeans(m19, specs = ~ Site|SH_Tide, var = "SH_Temp")
emm_Lm19a

pairwise_Lm19a <- contrast(emm_Lm19a, interaction = "pairwise")
pairwise_Lm19a

#SH_Tide = No Tide:
#Site_pairwise  estimate     SE  df z.ratio p.value
#Middle - North     0.23 0.0632 Inf   3.641  0.0003
#Middle - South     1.53 0.0848 Inf  18.083  <.0001
#North - South      1.30 0.0871 Inf  14.966  <.0001
#
#SH_Tide = Tide:
#  Site_pairwise  estimate     SE  df z.ratio p.value
#Middle - North     0.23 0.0632 Inf   3.641  0.0003
#Middle - South     1.53 0.0848 Inf  18.083  <.0001
#North - South      1.30 0.0871 Inf  14.966  <.0001

# Results are averaged over the levels of: SH_Temp 
# Results are given on the log odds ratio (not the response) scale. 

#### Plot Model ========
library(RColorBrewer)
par(mar=c(3,4,2,2))
display.brewer.all()

m19.Plot <- ggpredict(m19, terms = c("Site", "SH_Temp", "SH_Tide"))

plot(m19.Plot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("M. gigas: Mortality ~ SH_Temp + SH_Tide + Site + SH_Temp*Site, family = binomial(link = logit)")), 
       #subtitle = "Post-Outplanting Mortality",
       x = "Site", 
       y = "Mortality")

ggsave(filename = "fig_output/model_2022Mortality.png", width = 5.10, height = 5.77, dpi = 300)


##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m19.DARKPlot <- ggpredict(m19, terms = c("Site", "SH_Temp", "SH_Tide"))

plot(m19.DARKPlot) +
  stat_smooth(method ="glm", se=FALSE, method.args = list(family=binomial), lty = 2) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu") +
  labs(title = expression(paste("M. gigas: Mortality ~ SH_Temp + SH_Tide + Site + SH_Temp*Site, family = binomial(link = logit)")), 
       #subtitle = "Post-Outplanting Mortality",
       x = "Site", 
       y = "Mortality")

ggsave(filename = "fig_output/DARKmodel_2022Mortality.png", width = 5.10, height = 5.77, dpi = 300)



