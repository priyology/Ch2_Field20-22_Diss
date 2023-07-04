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

### change attributes about statistical factors
CumMorts2020$Time_Pt <- as.character(CumMorts2020$Time_Pt) ## factor
is.character(CumMorts2020$Time_Pt) ## TRUE

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

#### m1.bag: Mortality ~ Site + (1|Bag_Numb) ===============
m1.bag <- glmer(Mortality ~ Site + (1|Bag_Numb), family = binomial(link = "logit"), data = CumMorts2020)
summary(m1.bag)

#### Pairwise Comparisons ===============
# pairwise comparison for m12
#emm_m6 <-  emmeans(m6, specs = ~ MHW)
#emm_m6
#pairwise_m6 <- contrast(emm_m6, interaction = "pairwise")
#pairwise_m6

#### AIC/BIC Scores ===============
#AIC(m_null, m1, m2a, m3a)

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

CumMorts2021_sikamea$Site <- factor(CumMorts2021_sikamea$Site, levels = c("North", "Middle", "South"))

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

#### m9.bag: Mortality ~ SH_Temp + Site + (1|Bag_Numb) ===============
m9.bag <- glmer(Mortality ~ SH_Temp + Site + (1|Bag_Numb), family = binomial(link = "logit"), data = CumMorts2021_sikamea)
summary(m9.bag)

#Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [
#  glmerMod]
#Family: binomial  ( logit )
#Formula: Mortality ~ SH_Temp + Site + (1 | Bag_Numb)
#Data: CumMorts2021_sikamea
#
#AIC      BIC   logLik deviance df.resid 
#3504.3   3539.4  -1747.1   3494.3     8275 
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-0.7984 -0.3020 -0.1236 -0.0962 11.4156 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Bag_Numb (Intercept) 0.3063   0.5534  
#Number of obs: 8280, groups:  Bag_Numb, 36
#
#Fixed effects:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  -4.2945     0.2524 -17.016   <2e-16 ***
#  SH_Temp21     0.2998     0.2323   1.291    0.197    
#SiteNorth    -0.2987     0.3211  -0.930    0.352    
#SiteSouth     2.6628     0.2770   9.613   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:

#  (Intr) SH_T21 StNrth
#SH_Temp21 -0.483              
#SiteNorth -0.588 -0.004       
#SiteSouth -0.707  0.023  0.538

#### AIC/BIC Scores ===============
AIC(m_sikamea_null, m6, m7, m8, m9, m10, m9.bag)

#               df      AIC
#m_sikamea_null  1 4443.412
#m6              2 4439.680
#m7              2 4445.373
#m8              3 3611.186
#m9              4 3606.780
#m10             6 3610.224


BIC(m_sikamea_null, m6, m7, m8, m9, m10, m9.bag)
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
# Results are given on the log odds ratio (not the response) scale

#### Test Assumptions ===============
#### Pairwise Plot of Residuals ===============
plot(fitted(m20), resid(m20))
abline(0,0)

#### Q-Q plot of Residuals ===============
qqnorm(resid(m9))
qqline(resid(m9))

#### Density Plot of Residuals ===============
plot(density(resid(m9)))
. 

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




#### ** 2021 & 2022 M. GIGAS Mortality ** ===============
MgigasMorts21.22 <- read_csv("data/Mortality/Cumulative/Mgigas_21-22_CumMort.csv")
glimpse(MgigasMorts21.22)
summary(MgigasMorts21.22)
View(MgigasMorts21.22)

#Confirming this is M. gigas in 2021 & 2022
unique(MgigasMorts21.22$Species)
unique(MgigasMorts21.22$Year)

### ref.site
MgigasMorts21.22$Site <- factor(MgigasMorts21.22$Site, levels = c("North", "Middle", "South"))

#### m_null_gigas: Mortality ~ 1 ===============
m_null_gigas <- glm(Mortality ~ 1, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m_null_gigas)

# #Call:
#  glm(formula = Mortality ~ 1, family = binomial(link = "logit"), 
#      data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.7932  -0.7932  -0.7932   1.6185   1.6185  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.99521    0.01972  -50.47   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 15220  on 13049  degrees of freedom
#AIC: 15222
#
#Number of Fisher Scoring iterations: 4

#### m21: Mortality ~ SH_Temp ===============

m21 <- glm(Mortality ~ SH_Temp, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m21)

# Call:
#glm(formula = Mortality ~ SH_Temp, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.8133  -0.8133  -0.7706   1.5920   1.6487  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -1.376535   0.127267  -10.82  < 2e-16 ***
#  SH_Temp      0.020958   0.006895    3.04  0.00237 ** 
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 15211  on 13048  degrees of freedom
#AIC: 15215
#
#Number of Fisher Scoring iterations: 4

#### m22: Mortality ~ SH_Tide ===============

m22 <- glm(Mortality ~ SH_Tide, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m22)

# Call:
#glm(formula = Mortality ~ SH_Tide, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.8954  -0.8954  -0.5806   1.4885   1.9306  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.70686    0.02299  -30.74   <2e-16 ***
#  SH_TideTide -0.98820    0.04716  -20.95   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 14731  on 13048  degrees of freedom
#AIC: 14735
#
#Number of Fisher Scoring iterations: 4

#### m23: Mortality ~ Year ===============

m23 <- glm(Mortality ~ Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m23)

#Call:
#  glm(formula = Mortality ~ Year, family = binomial(link = "logit"), 
#      data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1345  -0.6257  -0.6257   1.2209   1.8586  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) 2889.24127   84.57665   34.16   <2e-16 ***
#  Year          -1.42966    0.04184  -34.17   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 14028  on 13048  degrees of freedom
#AIC: 14032
#
#Number of Fisher Scoring iterations: 4

#### m24: Mortality ~ Site ===============

m24 <- glm(Mortality ~ Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m24)

# Call:
#glm(formula = Mortality ~ Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.9748  -0.9535  -0.3701   1.3946   2.3302  

#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.49736    0.03127 -15.907   <2e-16 ***
#  SiteNorth   -0.05513    0.04437  -1.242    0.214    
#SiteSouth   -2.14911    0.06853 -31.361   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 13600  on 13047  degrees of freedom
#AIC: 13606
#
#Number of Fisher Scoring iterations: 5

#### m25: Mortality ~ SH_Temp + SH_Tide ===============

m25 <- glm(Mortality ~ SH_Temp + SH_Tide, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m25)

#Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide, family = binomial(link = "logit"), 
#      data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-0.9117  -0.8822  -0.5933   1.4688   1.9519  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.998628   0.131425  -7.598    3e-14 ***
#  SH_Temp      0.015980   0.007079   2.257    0.024 *  
#  SH_TideTide -0.984872   0.047189 -20.871   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 14726  on 13047  degrees of freedom
#AIC: 14732
#
#Number of Fisher Scoring iterations: 4

#### m26: Mortality ~ SH_Temp + Year ===============

m26 <- glm(Mortality ~ SH_Temp + Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m26)

#Call:
#  glm(formula = Mortality ~ SH_Temp + Year, family = binomial(link = "logit"), 
#      data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1361  -0.6270  -0.6244   1.2193   1.8606  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  2.888e+03  8.491e+01   34.01   <2e-16 ***
#  SH_Temp      1.487e-03  7.422e-03    0.20    0.841    
#Year        -1.429e+00  4.200e-02  -34.02   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 14028  on 13047  degrees of freedom
#AIC: 14034
#
#Number of Fisher Scoring iterations: 4

#### m27: Mortality ~ SH_Temp + Site ===============

m27 <- glm(Mortality ~ SH_Temp + Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m27)

# Call:
#glm(formula = Mortality ~ SH_Temp + Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.0006  -0.9461  -0.3819   1.3652   2.3605  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.92322    0.13633  -6.772 1.27e-11 ***
#  SH_Temp      0.02343    0.00729   3.214  0.00131 ** 
#  SiteNorth   -0.05518    0.04440  -1.243  0.21389    
#SiteSouth   -2.15051    0.06855 -31.372  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 13590  on 13046  degrees of freedom
#AIC: 13598
#
#Number of Fisher Scoring iterations: 5

#### m28: Mortality ~ SH_Tide + Year ===============

m28 <- glm(Mortality ~ SH_Tide + Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m28)

#Call:
#  glm(formula = Mortality ~ SH_Tide + Year, family = binomial(link = "logit"), 
#      data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1345  -0.6689  -0.5806   1.2209   1.9306  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) 2590.21921   98.54889  26.284   <2e-16 ***
#  SH_TideTide   -0.31154    0.05552  -5.612    2e-08 ***
#  Year          -1.28170    0.04875 -26.290   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 13996  on 13047  degrees of freedom
#AIC: 14002
#
#Number of Fisher Scoring iterations: 4

#### m29: Mortality ~ SH_Tide + Site ===============

m29 <- glm(Mortality ~ SH_Tide + Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m29)

# Call:
#glm(formula = Mortality ~ SH_Tide + Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1103  -0.7094  -0.4218   1.2460   2.6446  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.15994    0.03476  -4.601  4.2e-06 ***
#  SH_TideTide -1.09134    0.04928 -22.144  < 2e-16 ***
#  SiteNorth   -0.05825    0.04561  -1.277    0.202    
#SiteSouth   -2.21491    0.06942 -31.907  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 13058  on 13046  degrees of freedom
#AIC: 13066
#
#Number of Fisher Scoring iterations: 5

#### m30: Mortality ~ Year + Site ===============

m30 <- glm(Mortality ~ Year + Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m30)

# Call:
#glm(formula = Mortality ~ Year + Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4430  -0.7500  -0.5588   0.9333   2.6365  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) 3367.90580   94.31284  35.710   <2e-16 ***
#  Year          -1.66616    0.04665 -35.715   <2e-16 ***
#  SiteNorth     -0.06397    0.04780  -1.338    0.181    
#SiteSouth     -2.38353    0.07227 -32.982   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12235  on 13046  degrees of freedom
#AIC: 12243
#
#Number of Fisher Scoring iterations: 5

#### m31: Mortality ~ SH_Temp + SH_Tide + Year ===============

m31 <- glm(Mortality ~ SH_Temp + SH_Tide + Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m31)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1361  -0.6702  -0.5818   1.2193   1.9326  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  2.589e+03  9.884e+01  26.192   <2e-16 ***
#  SH_Temp      1.491e-03  7.431e-03   0.201    0.841    
#SH_TideTide -3.115e-01  5.552e-02  -5.612    2e-08 ***
#  Year        -1.281e+00  4.889e-02 -26.201   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 13996  on 13046  degrees of freedom
#AIC: 14004
#
#Number of Fisher Scoring iterations: 4

#### m32: Mortality ~ SH_Temp + SH_Tide + Site ===============

m32 <- glm(Mortality ~ SH_Temp + SH_Tide + Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m32)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.1310  -0.7263  -0.4318   1.2245   2.6650  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) -0.489611   0.141711  -3.455  0.00055 ***
#  SH_Temp      0.018071   0.007528   2.401  0.01637 *  
#  SH_TideTide -1.087735   0.049311 -22.059  < 2e-16 ***
#  SiteNorth   -0.058283   0.045628  -1.277  0.20148    
#SiteSouth   -2.215788   0.069431 -31.913  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 13053  on 13045  degrees of freedom
#AIC: 13063
#
#Number of Fisher Scoring iterations: 5

#### m33: Mortality ~ SH_Temp + Year + Site ===============

m33 <- glm(Mortality ~ SH_Temp + Year + Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m33)

# Call:
#glm(formula = Mortality ~ SH_Temp + Year + Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4449  -0.7517  -0.5577   0.9317   2.6383  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  3.366e+03  9.465e+01  35.564   <2e-16 ***
#  SH_Temp      1.698e-03  7.931e-03   0.214    0.830    
#Year        -1.665e+00  4.681e-02 -35.574   <2e-16 ***
#  SiteNorth   -6.397e-02  4.781e-02  -1.338    0.181    
#SiteSouth   -2.384e+00  7.227e-02 -32.982   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12235  on 13045  degrees of freedom
#AIC: 12245
#
#Number of Fisher Scoring iterations: 5

#### m34: Mortality ~ SH_Tide + Year + Site ===============

m34 <- glm(Mortality ~ SH_Tide + Year + Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m34)

#Call:
#glm(formula = Mortality ~ SH_Tide + Year + Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4433  -0.7143  -0.5583   0.9330   2.7012  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept) 3043.85888  108.17951  28.137  < 2e-16 ***
#  SH_TideTide   -0.33634    0.05767  -5.832 5.48e-09 ***
#  Year          -1.50582    0.05351 -28.138  < 2e-16 ***
#  SiteNorth     -0.06420    0.04789  -1.341     0.18    
#SiteSouth     -2.38604    0.07228 -33.009  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12201  on 13045  degrees of freedom
#AIC: 12211
#
#Number of Fisher Scoring iterations: 5

#### m35: Mortality ~ SH_Tide + Year + Site ===============

m35 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m35)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4452  -0.7159  -0.5572   0.9314   2.7031  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)  3.042e+03  1.085e+02  28.044  < 2e-16 ***
#  SH_Temp      1.704e-03  7.945e-03   0.215     0.83    
#SH_TideTide -3.363e-01  5.767e-02  -5.832 5.48e-09 ***
#  Year        -1.505e+00  5.366e-02 -28.048  < 2e-16 ***
#  SiteNorth   -6.420e-02  4.789e-02  -1.341     0.18    
#SiteSouth   -2.386e+00  7.228e-02 -33.009  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12201  on 13044  degrees of freedom
#AIC: 12213
#
#Number of Fisher Scoring iterations: 5

#### m36: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide*Site ===============

m36 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m36)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide * 
#      Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4634  -0.7017  -0.4912   0.9162   2.6945  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            3.129e+03  1.105e+02  28.311  < 2e-16 ***
#  SH_Temp                1.723e-03  7.988e-03   0.216   0.8293    
#SH_TideTide           -3.795e-01  7.929e-02  -4.786  1.7e-06 ***
#  Year                  -1.548e+00  5.466e-02 -28.315  < 2e-16 ***
#  SiteNorth             -1.307e-02  5.715e-02  -0.229   0.8192    
#SiteSouth             -2.696e+00  8.700e-02 -30.991  < 2e-16 ***
#  SH_TideTide:SiteNorth -1.863e-01  1.077e-01  -1.730   0.0837 .  
#SH_TideTide:SiteSouth  1.272e+00  1.511e-01   8.420  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12117  on 13042  degrees of freedom
#AIC: 12133
#
#Number of Fisher Scoring iterations: 5

#### m37: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide*SH_Temp ===============

m37 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide*SH_Temp, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m37)

#Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide * 
#        SH_Temp, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4833  -0.7683  -0.5352   0.8997   2.7978  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          3.087e+03  1.091e+02  28.289  < 2e-16 ***
#  SH_Temp             -3.509e-02  9.609e-03  -3.652  0.00026 ***
#  SH_TideTide         -2.477e+00  3.215e-01  -7.703 1.32e-14 ***
#  Year                -1.527e+00  5.397e-02 -28.290  < 2e-16 ***
#  SiteNorth           -6.449e-02  4.800e-02  -1.344  0.17906    
#SiteSouth           -2.391e+00  7.235e-02 -33.047  < 2e-16 ***
#  SH_Temp:SH_TideTide  1.181e-01  1.732e-02   6.816 9.39e-12 ***
#  ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12154  on 13043  degrees of freedom
#AIC: 12168
#
#Number of Fisher Scoring iterations: 5

#### m38: Mortality ~ SH_Temp + SH_Tide + Year + Site + Year*Site ===============

m38 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m38)

#Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + Year * 
#       Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5364  -0.7025  -0.3996   0.8568   2.3964  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)     3.333e+03  1.543e+02  21.597  < 2e-16 ***
#  SH_Temp         1.746e-03  8.042e-03   0.217  0.82811    
#SH_TideTide    -3.255e-01  5.675e-02  -5.736 9.68e-09 ***
#  Year           -1.649e+00  7.634e-02 -21.600  < 2e-16 ***
#  SiteNorth       5.644e+02  2.078e+02   2.715  0.00662 ** 
#  SiteSouth      -3.727e+03  3.050e+02 -12.220  < 2e-16 ***
#  Year:SiteNorth -2.792e-01  1.028e-01  -2.716  0.00662 ** 
#  Year:SiteSouth  1.843e+00  1.509e-01  12.213  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11981  on 13042  degrees of freedom
#AIC: 11997
#
#Number of Fisher Scoring iterations: 5

#### m39: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Site ===============

m39 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m39)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.4954  -0.7442  -0.5573   0.8898   2.7057  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       3057.88557  108.81417  28.102  < 2e-16 ***
#  SH_Temp             -0.05249    0.01187  -4.423 9.72e-06 ***
#  SH_TideTide         -0.33803    0.05782  -5.846 5.02e-09 ***
#  Year                -1.51228    0.05382 -28.099  < 2e-16 ***
#  SiteNorth           -2.08434    0.31195  -6.682 2.36e-11 ***
#  SiteSouth           -3.29381    0.48012  -6.860 6.87e-12 ***
#  SH_Temp:SiteNorth    0.11107    0.01694   6.556 5.53e-11 ***
#  SH_Temp:SiteSouth    0.05001    0.02599   1.924   0.0543 .  
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12158  on 13042  degrees of freedom
#AIC: 12174
#
#Number of Fisher Scoring iterations: 5

#### m40: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Year ===============

m40 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m40)

#Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#        Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6440  -0.7436  -0.4485   0.7741   2.8107  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)   1.234e+04  6.682e+02  18.465  < 2e-16 ***
#  SH_Temp      -5.037e+02  3.550e+01 -14.190  < 2e-16 ***
#  SH_TideTide  -3.401e-01  5.800e-02  -5.864 4.51e-09 ***
#  Year         -6.104e+00  3.305e-01 -18.466  < 2e-16 ***
#  SiteNorth    -6.547e-02  4.836e-02  -1.354    0.176    
#SiteSouth    -2.446e+00  7.373e-02 -33.180  < 2e-16 ***
#  SH_Temp:Year  2.492e-01  1.756e-02  14.190  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11995  on 13043  degrees of freedom
#AIC: 12009
#
#Number of Fisher Scoring iterations: 5

#### m41: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide*Year ===============

m41 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide*Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m41)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide * Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6440  -0.7474  -0.4485   0.7741   2.8157  
#
#Coefficients: (2 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)               1.221e+04  7.439e+02  16.413   <2e-16 ***
#  SH_Temp                  -4.967e+02  3.974e+01 -12.499   <2e-16 ***
#  SH_TideTide              -4.814e-01  3.631e-01  -1.326    0.185    
#Year                     -6.040e+00  3.680e-01 -16.413   <2e-16 ***
#  SiteNorth                -6.547e-02  4.836e-02  -1.354    0.176    
#SiteSouth                -2.446e+00  7.373e-02 -33.179   <2e-16 ***
#  SH_Temp:SH_TideTide       7.685e-03  1.949e-02   0.394    0.693    
#SH_Temp:Year              2.457e-01  1.966e-02  12.498   <2e-16 ***
#  SH_TideTide:Year                 NA         NA      NA       NA    
#SH_Temp:SH_TideTide:Year         NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11995  on 13042  degrees of freedom
#AIC: 12011
#
#Number of Fisher Scoring iterations: 5

#### m42: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide*Site ===============

m42 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m42)

#Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#        SH_Tide * Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5625  -0.7314  -0.4679   0.8362   2.7421  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                    3.194e+03  1.117e+02  28.596  < 2e-16 ***
#  SH_Temp                       -9.476e-02  1.461e-02  -6.484 8.92e-11 ***
#  SH_TideTide                   -2.609e+00  4.678e-01  -5.578 2.44e-08 ***
#  Year                          -1.579e+00  5.524e-02 -28.591  < 2e-16 ***
#  SiteNorth                     -2.180e+00  3.808e-01  -5.724 1.04e-08 ***
#  SiteSouth                     -3.666e+00  5.929e-01  -6.183 6.29e-10 ***
#  SH_Temp:SH_TideTide            1.235e-01  2.551e-02   4.843 1.28e-06 ***
#  SH_Temp:SiteNorth              1.189e-01  2.066e-02   5.755 8.65e-09 ***
#  SH_Temp:SiteSouth              5.294e-02  3.228e-02   1.640    0.101    
#SH_TideTide:SiteNorth         -7.073e-02  6.986e-01  -0.101    0.919    
#SH_TideTide:SiteSouth          1.209e+00  9.867e-01   1.226    0.220    
#SH_Temp:SH_TideTide:SiteNorth -7.936e-03  3.748e-02  -0.212    0.832    
#SH_Temp:SH_TideTide:SiteSouth  2.986e-03  5.299e-02   0.056    0.955    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12024  on 13037  degrees of freedom
#AIC: 12050
#
#Number of Fisher Scoring iterations: 5

#### m43: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Year*Site ===============

m43 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m43)

# 
#Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#        Year * Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8193  -0.7167  -0.3810   0.6513   2.4371  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)             1.416e+04  1.103e+03  12.835  < 2e-16 ***
#  SH_Temp                -5.778e+02  5.792e+01  -9.975  < 2e-16 ***
#  SH_TideTide            -3.305e-01  5.719e-02  -5.779 7.50e-09 ***
#  Year                   -7.003e+00  5.456e-01 -12.835  < 2e-16 ***
#  SiteNorth               3.028e+03  1.575e+03   1.923   0.0545 .  
#SiteSouth              -1.276e+04  2.198e+03  -5.806 6.40e-09 ***
#  SH_Temp:Year            2.858e-01  2.865e-02   9.974  < 2e-16 ***
#  SH_Temp:SiteNorth      -1.374e+02  8.262e+01  -1.663   0.0963 .  
#SH_Temp:SiteSouth       4.794e+02  1.177e+02   4.075 4.61e-05 ***
#  Year:SiteNorth         -1.499e+00  7.791e-01  -1.924   0.0543 .  
#Year:SiteSouth          6.311e+00  1.087e+00   5.804 6.46e-09 ***
#  SH_Temp:Year:SiteNorth  6.803e-02  4.087e-02   1.665   0.0960 .  
#SH_Temp:Year:SiteSouth -2.371e-01  5.820e-02  -4.074 4.62e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11678  on 13037  degrees of freedom
#AIC: 11704
#
#Number of Fisher Scoring iterations: 5

#### m44: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide*Site + Year*Site ====

m44 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide*Site + Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m44)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Tide * 
#      Site + Year * Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5364  -0.7017  -0.3837   0.8568   2.3558  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            3.329e+03  1.651e+02  20.162  < 2e-16 ***
#  SH_Temp                1.747e-03  8.044e-03   0.217 0.828071    
#SH_TideTide           -3.302e-01  8.506e-02  -3.882 0.000104 ***
#  Year                  -1.647e+00  8.167e-02 -20.164  < 2e-16 ***
#  SiteNorth              5.109e+02  2.364e+02   2.162 0.030656 *  
#  SiteSouth             -3.553e+03  3.443e+02 -10.321  < 2e-16 ***
#  SH_TideTide:SiteNorth -5.892e-02  1.233e-01  -0.478 0.632656    
#SH_TideTide:SiteSouth  1.914e-01  1.693e-01   1.131 0.258217    
#Year:SiteNorth        -2.527e-01  1.169e-01  -2.162 0.030651 *  
#  Year:SiteSouth         1.757e+00  1.703e-01  10.314  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11979  on 13040  degrees of freedom
#AIC: 11999
#
#Number of Fisher Scoring iterations: 5

##### m45: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Site + Year*Site ====

m45 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Site + Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m45)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      Site + Year * Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6009  -0.6997  -0.3875   0.8065   2.4221  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        3.413e+03  1.559e+02  21.885  < 2e-16 ***
#  SH_Temp           -5.650e-02  1.212e-02  -4.662 3.13e-06 ***
#  SH_TideTide       -3.270e-01  5.688e-02  -5.749 8.99e-09 ***
#  Year              -1.688e+00  7.713e-02 -21.884  < 2e-16 ***
#  SiteNorth          4.506e+02  2.095e+02   2.151  0.03144 *  
#  SiteSouth         -3.829e+03  3.066e+02 -12.487  < 2e-16 ***
#  SH_Temp:SiteNorth  1.139e-01  1.748e-02   6.517 7.18e-11 ***
#  SH_Temp:SiteSouth  7.909e-02  2.458e-02   3.217  0.00129 ** 
#  Year:SiteNorth    -2.240e-01  1.036e-01  -2.162  0.03062 *  
#  Year:SiteSouth     1.892e+00  1.516e-01  12.477  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11937  on 13040  degrees of freedom
#AIC: 11957
#
#Number of Fisher Scoring iterations: 5

##### m46: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Site + SH_Tide*Site ====

m46 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Site + SH_Tide*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m46)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      Site + Year * Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6009  -0.6997  -0.3875   0.8065   2.4221  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        3.413e+03  1.559e+02  21.885  < 2e-16 ***
#  SH_Temp           -5.650e-02  1.212e-02  -4.662 3.13e-06 ***
#  SH_TideTide       -3.270e-01  5.688e-02  -5.749 8.99e-09 ***
#  Year              -1.688e+00  7.713e-02 -21.884  < 2e-16 ***
#  SiteNorth          4.506e+02  2.095e+02   2.151  0.03144 *  
#  SiteSouth         -3.829e+03  3.066e+02 -12.487  < 2e-16 ***
#  SH_Temp:SiteNorth  1.139e-01  1.748e-02   6.517 7.18e-11 ***
#  SH_Temp:SiteSouth  7.909e-02  2.458e-02   3.217  0.00129 ** 
#  Year:SiteNorth    -2.240e-01  1.036e-01  -2.162  0.03062 *  
#  Year:SiteSouth     1.892e+00  1.516e-01  12.477  < 2e-16 ***
# ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11937  on 13040  degrees of freedom
#AIC: 11957
#
#Number of Fisher Scoring iterations: 5


##### m47: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Site + SH_Temp*Year ====

m47 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*Site + SH_Temp*Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m47)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      Site + SH_Temp * Year, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7355  -0.7333  -0.4842   0.7082   2.8968  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        1.292e+04  6.955e+02  18.570  < 2e-16 ***
#  SH_Temp           -5.323e+02  3.671e+01 -14.499  < 2e-16 ***
#  SH_TideTide       -3.412e-01  5.809e-02  -5.874 4.26e-09 ***
#  Year              -6.388e+00  3.440e-01 -18.569  < 2e-16 ***
#  SiteNorth         -2.243e+00  3.235e-01  -6.934 4.09e-12 ***
#  SiteSouth         -4.882e+00  5.178e-01  -9.429  < 2e-16 ***
#  SH_Temp:SiteNorth  1.176e-01  1.726e-02   6.814 9.48e-12 ***
#  SH_Temp:SiteSouth  1.320e-01  2.763e-02   4.778 1.77e-06 ***
#  SH_Temp:Year       2.633e-01  1.816e-02  14.498  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11940  on 13041  degrees of freedom
#AIC: 11958
#
#Number of Fisher Scoring iterations: 5


##### m48: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year ====

m48 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m48)

#Call:
#  glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#        SH_Tide + SH_Temp * Site + SH_Temp * Year, family = binomial(link = "logit"), 
#      data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7356  -0.7373  -0.4842   0.7082   2.9026  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.277e+04  7.681e+02  16.622  < 2e-16 ***
#  SH_Temp             -5.243e+02  4.080e+01 -12.851  < 2e-16 ***
#  SH_TideTide         -5.036e-01  3.647e-01  -1.381    0.167    
#Year                -6.315e+00  3.800e-01 -16.620  < 2e-16 ***
#  SiteNorth           -2.244e+00  3.236e-01  -6.935 4.06e-12 ***
#  SiteSouth           -4.885e+00  5.178e-01  -9.434  < 2e-16 ***
#  SH_Temp:SH_TideTide  8.823e-03  1.955e-02   0.451    0.652    
#SH_Temp:SiteNorth    1.176e-01  1.726e-02   6.815 9.40e-12 ***
#  SH_Temp:SiteSouth    1.322e-01  2.763e-02   4.784 1.72e-06 ***
#  SH_Temp:Year         2.593e-01  2.018e-02  12.848  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11940  on 13040  degrees of freedom
#AIC: 11960
#
#Number of Fisher Scoring iterations: 5

##### m49: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Tide*Site ====

m49 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Tide*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m49)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Site + SH_Tide * Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5615  -0.7323  -0.4693   0.8369   2.7398  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)           3193.40707  111.62059  28.609  < 2e-16 ***
#  SH_Temp                 -0.09386    0.01328  -7.069 1.56e-12 ***
#  SH_TideTide             -2.55974    0.32329  -7.918 2.42e-15 ***
#  Year                    -1.57894    0.05520 -28.603  < 2e-16 ***
#  SiteNorth               -2.13629    0.31928  -6.691 2.22e-11 ***
#  SiteSouth               -3.68960    0.47290  -7.802 6.09e-15 ***
#  SH_Temp:SH_TideTide      0.12080    0.01738   6.951 3.62e-12 ***
#  SH_Temp:SiteNorth        0.11656    0.01724   6.761 1.37e-11 ***
#  SH_Temp:SiteSouth        0.05425    0.02557   2.122   0.0338 *  
#  SH_TideTide:SiteNorth   -0.21735    0.10896  -1.995   0.0461 *  
#  SH_TideTide:SiteSouth    1.26520    0.15179   8.335  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 12024  on 13039  degrees of freedom
#AIC: 12046
#
#Number of Fisher Scoring iterations: 5

##### m50: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year ====

m50 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m50)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Site + SH_Temp * Year, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7356  -0.7373  -0.4842   0.7082   2.9026  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.277e+04  7.681e+02  16.622  < 2e-16 ***
#  SH_Temp             -5.243e+02  4.080e+01 -12.851  < 2e-16 ***
#  SH_TideTide         -5.036e-01  3.647e-01  -1.381    0.167    
#Year                -6.315e+00  3.800e-01 -16.620  < 2e-16 ***
#  SiteNorth           -2.244e+00  3.236e-01  -6.935 4.06e-12 ***
#  SiteSouth           -4.885e+00  5.178e-01  -9.434  < 2e-16 ***
#  SH_Temp:SH_TideTide  8.823e-03  1.955e-02   0.451    0.652    
#SH_Temp:SiteNorth    1.176e-01  1.726e-02   6.815 9.40e-12 ***
#  SH_Temp:SiteSouth    1.322e-01  2.763e-02   4.784 1.72e-06 ***
#  SH_Temp:Year         2.593e-01  2.018e-02  12.848  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11940  on 13040  degrees of freedom
#AIC: 11960
#
#Number of Fisher Scoring iterations: 5

##### m51: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + Year*Site ====
m51 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m51)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Site + Year * Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.5967  -0.7157  -0.3917   0.8097   2.5252  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          3.457e+03  1.570e+02  22.012  < 2e-16 ***
#  SH_Temp             -9.631e-02  1.346e-02  -7.156 8.31e-13 ***
#  SH_TideTide         -2.490e+00  3.223e-01  -7.726 1.11e-14 ***
#  Year                -1.709e+00  7.766e-02 -22.008  < 2e-16 ***
#  SiteNorth            4.549e+02  2.098e+02   2.169  0.03010 *  
#  SiteSouth           -3.814e+03  3.063e+02 -12.450  < 2e-16 ***
#  SH_Temp:SH_TideTide  1.192e-01  1.735e-02   6.867 6.58e-12 ***
#  SH_Temp:SiteNorth    1.155e-01  1.753e-02   6.589 4.44e-11 ***
#  SH_Temp:SiteSouth    7.999e-02  2.465e-02   3.245  0.00117 ** 
#  Year:SiteNorth      -2.261e-01  1.037e-01  -2.179  0.02930 *  
#  Year:SiteSouth       1.885e+00  1.515e-01  12.439  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11890  on 13039  degrees of freedom
#AIC: 11912
#
#Number of Fisher Scoring iterations: 5

##### m52: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Year + SH_Tide*Site ====
m52 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Year + SH_Tide*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m52)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Year + SH_Tide * Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.6777  -0.7415  -0.3993   0.7494   2.8070  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            1.274e+04  7.627e+02  16.698   <2e-16 ***
#  SH_Temp               -5.196e+02  4.063e+01 -12.787   <2e-16 ***
#  SH_TideTide           -4.651e-01  3.647e-01  -1.275   0.2022    
#Year                  -6.300e+00  3.773e-01 -16.697   <2e-16 ***
#  SiteNorth             -1.341e-02  5.791e-02  -0.232   0.8168    
#SiteSouth             -2.783e+00  8.917e-02 -31.206   <2e-16 ***
#  SH_Temp:SH_TideTide    4.215e-03  1.938e-02   0.217   0.8278    
#SH_Temp:Year           2.570e-01  2.010e-02  12.786   <2e-16 ***
#  SH_TideTide:SiteNorth -1.878e-01  1.085e-01  -1.731   0.0834 .  
#SH_TideTide:SiteSouth  1.350e+00  1.527e-01   8.843   <2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11903  on 13040  degrees of freedom
#AIC: 11923
#
#Number of Fisher Scoring iterations: 5

##### m53: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Year + Year*Site ====
m53 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Year + Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m53)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Year + Year * Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7869  -0.7188  -0.4403   0.6729   2.5818  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.372e+04  8.006e+02  17.139  < 2e-16 ***
#  SH_Temp             -5.594e+02  4.198e+01 -13.327  < 2e-16 ***
#  SH_TideTide         -4.815e-01  3.575e-01  -1.347  0.17810    
#Year                -6.787e+00  3.960e-01 -17.139  < 2e-16 ***
#  SiteNorth            5.803e+02  2.119e+02   2.738  0.00618 ** 
#  SiteSouth           -4.022e+03  3.116e+02 -12.905  < 2e-16 ***
#  SH_Temp:SH_TideTide  8.325e-03  1.917e-02   0.434  0.66417    
#SH_Temp:Year         2.767e-01  2.076e-02  13.326  < 2e-16 ***
#  Year:SiteNorth      -2.871e-01  1.048e-01  -2.738  0.00617 ** 
#  Year:SiteSouth       1.988e+00  1.541e-01  12.899  < 2e-16 ***
# # ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11751  on 13040  degrees of freedom
#AIC: 11771
#
#Number of Fisher Scoring iterations: 5

##### m54: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year + SH_Tide*Site ====
m54 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year + SH_Tide*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m54)

#Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Site + SH_Temp * Year + SH_Tide * Site, 
#    family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.7614  -0.7165  -0.4186   0.6903   2.8610  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)            1.309e+04  7.801e+02  16.775  < 2e-16 ***
#  SH_Temp               -5.367e+02  4.142e+01 -12.957  < 2e-16 ***
#  SH_TideTide           -4.453e-01  3.658e-01  -1.217    0.223    
#Year                  -6.473e+00  3.859e-01 -16.774  < 2e-16 ***
#  SiteNorth             -2.240e+00  3.301e-01  -6.785 1.16e-11 ***
#  SiteSouth             -4.756e+00  4.975e-01  -9.561  < 2e-16 ***
#  SH_Temp:SH_TideTide    2.529e-03  1.968e-02   0.129    0.898    
#SH_Temp:SiteNorth      1.199e-01  1.748e-02   6.855 7.12e-12 ***
#  SH_Temp:SiteSouth      1.072e-01  2.675e-02   4.009 6.11e-05 ***
#  SH_Temp:Year           2.655e-01  2.049e-02  12.955  < 2e-16 ***
#  SH_TideTide:SiteNorth -1.694e-01  1.093e-01  -1.550    0.121    
#SH_TideTide:SiteSouth  1.342e+00  1.539e-01   8.723  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11852  on 13038  degrees of freedom
#AIC: 11876
#
#Number of Fisher Scoring iterations: 5

##### m55: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year + Year*Site ====
m55 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year + Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m55)

#Call:
# glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#        SH_Tide + SH_Temp * Site + SH_Temp * Year + Year * Site, 
#      family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8172  -0.7204  -0.4400   0.6527   2.5720  
#
#Coefficients:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)          1.393e+04  8.076e+02  17.247  < 2e-16 ***
#  SH_Temp             -5.654e+02  4.221e+01 -13.394  < 2e-16 ***
#  SH_TideTide         -4.974e-01  3.590e-01  -1.386  0.16588    
#Year                -6.889e+00  3.995e-01 -17.246  < 2e-16 ***
#  SiteNorth            4.341e+02  2.140e+02   2.028  0.04255 *  
#  SiteSouth           -4.103e+03  3.168e+02 -12.949  < 2e-16 ***
#  SH_Temp:SH_TideTide  9.111e-03  1.923e-02   0.474  0.63573    
#SH_Temp:SiteNorth    1.218e-01  1.790e-02   6.801 1.04e-11 ***
#  SH_Temp:SiteSouth    6.947e-02  2.546e-02   2.729  0.00636 ** 
#  SH_Temp:Year         2.796e-01  2.088e-02  13.392  < 2e-16 ***
#  Year:SiteNorth      -2.159e-01  1.059e-01  -2.039  0.04142 *  
#  Year:SiteSouth       2.028e+00  1.567e-01  12.936  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11704  on 13038  degrees of freedom
#AIC: 11728
#
#Number of Fisher Scoring iterations: 5

##### m56: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year + SH_Tide*Site * Year*Site ====
m56 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Year + SH_Temp*Site + SH_Tide*Site * Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m56)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Year + SH_Temp * Site + SH_Tide * Site * 
#      Year * Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8174  -0.7209  -0.4278   0.6526   2.5726  
#
#Coefficients: (3 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                 1.393e+04  8.082e+02  17.237  < 2e-16 ***
#  SH_Temp                    -5.654e+02  4.225e+01 -13.382  < 2e-16 ***
#  SH_TideTide                -4.992e-01  3.600e-01  -1.387  0.16553    
#Year                       -6.891e+00  3.998e-01 -17.235  < 2e-16 ***
#  SiteNorth                   3.648e+02  2.426e+02   1.504  0.13267    
#SiteSouth                  -3.935e+03  3.549e+02 -11.089  < 2e-16 ***
#  SH_Temp:SH_TideTide         9.336e-03  1.934e-02   0.483  0.62928    
#SH_Temp:Year                2.796e-01  2.090e-02  13.380  < 2e-16 ***
#  SH_Temp:SiteNorth           1.221e-01  1.792e-02   6.813 9.56e-12 ***
#  SH_Temp:SiteSouth           6.908e-02  2.545e-02   2.714  0.00664 ** 
#  SH_TideTide:SiteNorth      -7.592e-02  1.248e-01  -0.608  0.54313    
#SH_TideTide:SiteSouth       1.855e-01  1.699e-01   1.092  0.27497    
#SH_TideTide:Year                   NA         NA      NA       NA    
#Year:SiteNorth             -1.816e-01  1.200e-01  -1.513  0.13021    
#Year:SiteSouth              1.945e+00  1.756e-01  11.077  < 2e-16 ***
#  SH_TideTide:Year:SiteNorth         NA         NA      NA       NA    
#SH_TideTide:Year:SiteSouth         NA         NA      NA       NA    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11702  on 13036  degrees of freedom
#AIC: 11730
#
#Number of Fisher Scoring iterations: 5

##### m57: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year + SH_Tide*Site * Year*Site + SH_Temp*SH_Tide*Site ====
m57 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Year + SH_Temp*Site + SH_Tide*Site * Year*Site + SH_Temp*SH_Tide*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m57)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Year + SH_Temp * Site + SH_Tide * Site * 
#      Year * Site + SH_Temp * SH_Tide * Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8219  -0.7266  -0.4298   0.6495   2.5673  
#
#Coefficients: (3 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                    1.394e+04  8.094e+02  17.224  < 2e-16 ***
#  SH_Temp                       -5.656e+02  4.230e+01 -13.373  < 2e-16 ***
#  SH_TideTide                   -6.702e-01  4.962e-01  -1.351   0.1768    
#Year                          -6.896e+00  4.004e-01 -17.223  < 2e-16 ***
#  SiteNorth                      3.568e+02  2.433e+02   1.466   0.1426    
#SiteSouth                     -3.933e+03  3.567e+02 -11.026  < 2e-16 ***
#  SH_Temp:SH_TideTide            1.878e-02  2.700e-02   0.696   0.4867    
#SH_Temp:Year                   2.797e-01  2.092e-02  13.371  < 2e-16 ***
#  SH_Temp:SiteNorth              1.275e-01  2.186e-02   5.835 5.39e-09 ***
#  SH_Temp:SiteSouth              7.647e-02  3.194e-02   2.394   0.0167 *  
#  SH_TideTide:SiteNorth          2.284e-01  7.148e-01   0.320   0.7493    
#SH_TideTide:SiteSouth          5.636e-01  9.926e-01   0.568   0.5702    
#SH_TideTide:Year                      NA         NA      NA       NA    
#Year:SiteNorth                -1.777e-01  1.203e-01  -1.476   0.1398    
#Year:SiteSouth                 1.943e+00  1.765e-01  11.012  < 2e-16 ***
#  SH_TideTide:Year:SiteNorth            NA         NA      NA       NA    
#SH_TideTide:Year:SiteSouth            NA         NA      NA       NA    
#SH_Temp:SH_TideTide:SiteNorth -1.656e-02  3.815e-02  -0.434   0.6642    
#SH_Temp:SH_TideTide:SiteSouth -2.054e-02  5.279e-02  -0.389   0.6972    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11701  on 13034  degrees of freedom
#AIC: 11733
#
#Number of Fisher Scoring iterations: 5

##### m58: Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Site + SH_Temp*Year + SH_Tide*Site * Year*Site + SH_Temp*Year*Site ====
m58 <- glm(Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp*SH_Tide + SH_Temp*Year + SH_Temp*Site + SH_Tide*Site * Year*Site + SH_Temp*Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m58)

# Call:
#glm(formula = Mortality ~ SH_Temp + SH_Tide + Year + Site + SH_Temp * 
#      SH_Tide + SH_Temp * Year + SH_Temp * Site + SH_Tide * Site * 
#      Year * Site + SH_Temp * Year * Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8193  -0.7218  -0.3798   0.6513   2.4028  
#
#Coefficients: (3 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                 1.399e+04  1.151e+03  12.153  < 2e-16 ***
#  SH_Temp                    -5.683e+02  6.064e+01  -9.371  < 2e-16 ***
#  SH_TideTide                -5.172e-01  3.607e-01  -1.434   0.1516    
#Year                       -6.917e+00  5.692e-01 -12.152  < 2e-16 ***
#  SiteNorth                   2.977e+03  1.578e+03   1.886   0.0593 .  
#SiteSouth                  -1.260e+04  2.203e+03  -5.718 1.08e-08 ***
#  SH_Temp:SH_TideTide         1.033e-02  1.938e-02   0.533   0.5939    
#SH_Temp:Year                2.810e-01  2.999e-02   9.370  < 2e-16 ***
#  SH_Temp:SiteNorth          -1.385e+02  8.265e+01  -1.676   0.0937 .  
#SH_Temp:SiteSouth           4.798e+02  1.177e+02   4.078 4.55e-05 ***
#  SH_TideTide:SiteNorth      -8.098e-02  1.254e-01  -0.646   0.5183    
#SH_TideTide:SiteSouth       1.896e-01  1.694e-01   1.119   0.2630    
#SH_TideTide:Year                   NA         NA      NA       NA    
#Year:SiteNorth             -1.473e+00  7.806e-01  -1.888   0.0591 .  
#Year:SiteSouth              6.228e+00  1.089e+00   5.717 1.09e-08 ***
#  SH_TideTide:Year:SiteNorth         NA         NA      NA       NA    
#SH_TideTide:Year:SiteSouth         NA         NA      NA       NA    
#SH_Temp:Year:SiteNorth      6.859e-02  4.088e-02   1.678   0.0934 .  
#SH_Temp:Year:SiteSouth     -2.373e-01  5.820e-02  -4.077 4.56e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11675  on 13034  degrees of freedom
#AIC: 11707
#
#Number of Fisher Scoring iterations: 5

##### m59: Mortality ~ SH_Temp + Year + Site + SH_Temp*Site + SH_Temp*Year + Year*Site + SH_Temp*Year*Site====
m59 <- glm(Mortality ~ SH_Temp + Year + Site + SH_Temp*Year + SH_Temp*Site + Year*Site + SH_Temp*Year*Site, family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m59)

#Call:
#glm(formula = Mortality ~ SH_Temp + Year + Site + SH_Temp * Year + 
#      SH_Temp * Site + Year * Site + SH_Temp * Year * Site, family = binomial(link = "logit"), 
#    data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8193  -0.7373  -0.3903   0.6513   2.3674  
#
#Coeffici#ents:
#  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)             1.448e+04  1.101e+03  13.144  < 2e-16 ***
#  SH_Temp                -5.776e+02  5.789e+01  -9.978  < 2e-16 ***
#  Year                   -7.160e+00  5.447e-01 -13.144  < 2e-16 ***
#  SiteNorth               3.004e+03  1.574e+03   1.909   0.0563 .  
#SiteSouth              -1.277e+04  2.197e+03  -5.812 6.19e-09 ***
#  SH_Temp:Year            2.857e-01  2.863e-02   9.977  < 2e-16 ***
#  SH_Temp:SiteNorth      -1.362e+02  8.257e+01  -1.649   0.0992 .  
#SH_Temp:SiteSouth       4.794e+02  1.176e+02   4.075 4.60e-05 ***
#  Year:SiteNorth         -1.487e+00  7.786e-01  -1.910   0.0561 .  
#Year:SiteSouth          6.315e+00  1.087e+00   5.810 6.24e-09 ***
#  SH_Temp:Year:SiteNorth  6.741e-02  4.084e-02   1.650   0.0989 .  
#SH_Temp:Year:SiteSouth -2.371e-01  5.818e-02  -4.075 4.61e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11711  on 13038  degrees of freedom
#AIC: 11735
#
#Number of Fisher Scoring iterations: 5

##### m60: Mortality ~ SH_Temp + Year + Site + SH_Temp*Site + SH_Temp*Year + Year*Site + SH_Temp*Year*Site + (1|Bag)  ====
m60 <- glm(Mortality ~ SH_Temp + Year + Site + SH_Temp*Year + SH_Temp*Site + Year*Site + SH_Temp*Year*Site + (1|Bag_Numb), family = binomial(link = "logit"), data = MgigasMorts21.22)
summary(m60)

# Call:
#glm(formula = Mortality ~ SH_Temp + Year + Site + SH_Temp * Year + 
#      SH_Temp * Site + Year * Site + SH_Temp * Year * Site + (1 | 
#                                                                Bag_Numb), family = binomial(link = "logit"), data = MgigasMorts21.22)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-1.8193  -0.7373  -0.3903   0.6513   2.3674  
#
#Coefficients: (1 not defined because of singularities)
#Estimate Std. Error z value Pr(>|z|)    
#(Intercept)              1.748e+04  1.125e+03  15.540  < 2e-16 ***
#  SH_Temp                 -7.138e+02  5.888e+01 -12.123  < 2e-16 ***
#  Year                    -8.647e+00  5.564e-01 -15.542  < 2e-16 ***
#  SiteMiddle              -3.004e+03  1.574e+03  -1.909   0.0563 .  
#SiteSouth               -1.577e+04  2.209e+03  -7.140 9.33e-13 ***
#  1 | Bag_NumbTRUE                NA         NA      NA       NA    
#SH_Temp:Year             3.531e-01  2.912e-02  12.124  < 2e-16 ***
#  SH_Temp:SiteMiddle       1.362e+02  8.257e+01   1.649   0.0992 .  
#SH_Temp:SiteSouth        6.155e+02  1.181e+02   5.211 1.88e-07 ***
#  Year:SiteMiddle          1.487e+00  7.786e-01   1.910   0.0561 .  
#Year:SiteSouth           7.802e+00  1.093e+00   7.140 9.35e-13 ***
#  SH_Temp:Year:SiteMiddle -6.741e-02  4.084e-02  -1.650   0.0989 .  
#SH_Temp:Year:SiteSouth  -3.045e-01  5.843e-02  -5.211 1.88e-07 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for binomial family taken to be 1)
#
#Null deviance: 15220  on 13049  degrees of freedom
#Residual deviance: 11711  on 13038  degrees of freedom
#AIC: 11735
#
#Number of Fisher Scoring iterations: 5


