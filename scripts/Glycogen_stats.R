#### ~ M. GIGAS GLYCOGEN STATS ~ =====

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

Gly <- read_csv("data/Glycogen/2022/Glycogen2022.csv")
glimpse(Gly)
summary(Gly)
View(Gly)

Gly$Site <- fct_relevel(Gly$Site, "HIOC - North", "BBOC - Middle", "TBOC - South")

#### Model selection ====
#### m_null: Glycogen ~ 1  =====

m_null <- glm(Glycogen ~ 1, family = gaussian(link = "identity"), data = Gly)
summary(m_null)

# Call:
#glm(formula = Glycogen ~ 1, family = gaussian(link = "identity"), 
#    data = Gly)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-7.421  -5.298  -2.474   3.260  44.791  
#
#Coefficients:
#  Estimate Std. Error t value
#(Intercept)   7.2526     0.3839   18.89
#Pr(>|t|)    
#(Intercept)   <2e-16 ***
#  ---
#  Signif. codes:  
#  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’
#0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 52.91646)
#
#Null deviance: 18944  on 358  degrees of freedom
#Residual deviance: 18944  on 358  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 2446.6
#
#Number of Fisher Scoring iterations: 2

#### m1: Glycogen ~ SH_Temp  =====
m1 <- glm(Glycogen ~ SH_Temp, family = gaussian(link = "identity"), data = Gly)
summary(m1)

# Call:
#glm(formula = Glycogen ~ SH_Temp, family = gaussian(link = "identity"), 
#    data = Gly)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-7.733  -5.219  -2.427   2.953  45.101  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   9.1169     2.3375   3.900 0.000115 ***
#  SH_Temp      -0.1035     0.1280  -0.809 0.419319    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 52.96769)
#
#Null deviance: 18944  on 358  degrees of freedom
#Residual deviance: 18909  on 357  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 2447.9
#
#Number of Fisher Scoring iterations: 2

#### m2: Glycogen ~ SH_Tide  =====
m2 <- glm(Glycogen ~ SH_Tide, family = gaussian(link = "identity"), data = Gly)
summary(m2)

#Call:
#  glm(formula = Glycogen ~ SH_Tide, family = gaussian(link = "identity"), 
#      data = Gly)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-7.236  -5.278  -2.630   3.081  44.445  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   7.5987     0.5423  14.011   <2e-16 ***
#  SH_TideTide  -0.6942     0.7681  -0.904    0.367    
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 52.94352)
#
#Null deviance: 18944  on 358  degrees of freedom
#Residual deviance: 18901  on 357  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 2447.7
#
#Number of Fisher Scoring iterations: 2

#### m3: Glycogen ~ Sampling  =====
m3 <- glm(Glycogen ~ Sampling, family = gaussian(link = "identity"), data = Gly)
summary(m3)

#Call:
#  glm(formula = Glycogen ~ Sampling, family = gaussian(link = "identity"), 
#      data = Gly)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-8.282  -4.958  -2.815   2.864  45.647  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   5.2530     0.9682   5.426 1.07e-07 ***
#  Sampling      0.5720     0.2545   2.247   0.0252 *  
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 52.32442)
#
#Null deviance: 18944  on 358  degrees of freedom
#Residual deviance: 18680  on 357  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 2443.5
#
#Number of Fisher Scoring iterations: 2

#### m4: Glycogen ~ Site  =====
## Reorder levels in Site for data analysis
Gly$Site <- factor(Gly$Site, levels=c("HIOC - North", "BBOC - Middle", "TBOC - South"))

m4 <- glm(Glycogen ~ Site, family = gaussian(link = "identity"), data = Gly)
summary(m4)

#Call:
#  glm(formula = Glycogen ~ Site, family = gaussian(link = "identity"), 
#      data = Gly)
#
#Deviance Residuals: 
#  Min      1Q  Median      3Q     Max  
#-9.637  -4.134  -1.112   1.822  39.814  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         2.8296     0.5666   4.994 9.28e-07 ***
#  SiteBBOC - Middle   3.8312     0.7996   4.791 2.44e-06 ***
#  SiteHIOC - North    9.4009     0.7996  11.757  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 38.20385)
#
#Null deviance: 18944  on 358  degrees of freedom
#Residual deviance: 13601  on 356  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 2331.6
#
#Number of Fisher Scoring iterations: 2


#### m5: Glycogen ~ Sampling + Site  =====

m5 <- glm(Glycogen ~ Sampling + Site, family = gaussian(link = "identity"), data = Gly)
summary(m5)

#Call:
#  glm(formula = Glycogen ~ Sampling + Site, family = gaussian(link = "identity"), 
#      data = Gly)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-10.482   -3.860   -0.866    2.095   40.659  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)         0.8635     0.9391   0.920  0.35844    
#Sampling            0.5638     0.2157   2.613  0.00934 ** 
#  SiteBBOC - Middle   3.8240     0.7932   4.821 2.12e-06 ***
#  SiteHIOC - North    9.3938     0.7932  11.843  < 2e-16 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 37.58825)
##
#Null deviance: 18944  on 358  degrees of freedom
#Residual deviance: 13344  on 355  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 2326.8
#
#Number of Fisher Scoring iterations: 2

#### m6: Glycogen ~ Sampling + Site + Sampling*Site  =====

m6 <- glm(Glycogen ~ Sampling + Site + Sampling*Site, family = gaussian(link = "identity"), data = Gly)
summary(m6)
#
#Call:
#  glm(formula = Glycogen ~ Sampling + Site + Sampling * Site, family = gaussian(link = "identity"), 
#      data = Gly)
#
#Deviance Residuals: 
#  Min       1Q   Median       3Q      Max  
#-11.800   -2.117   -0.705    1.424   37.409  
#
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept)                  1.3560     1.2734   1.065  0.28768    
#Sampling                     0.4226     0.3354   1.260  0.20857    
#SiteBBOC - Middle           -4.7431     1.7998  -2.635  0.00878 ** 
#  SiteHIOC - North            16.4859     1.7998   9.160  < 2e-16 ***
#  Sampling:SiteBBOC - Middle   2.4483     0.4734   5.172 3.89e-07 ***
#  Sampling:SiteHIOC - North   -2.0258     0.4734  -4.280 2.42e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#(Dispersion parameter for gaussian family taken to be 30.12321)
#
#Null deviance: 18944  on 358  degrees of freedom
#Residual deviance: 10633  on 353  degrees of freedom
#(1 observation deleted due to missingness)
#AIC: 2249.2
#
#Number of Fisher Scoring iterations: 2

#### m7: Glycogen ~ Sampling + Site + Sampling*Site + (1|Bag)  =====

m7 <- lmer(Glycogen ~ Sampling + Site + Sampling*Site + (1|Bag), data = Gly)
summary(m7)

# Linear mixed model fit by REML. t-tests use Satterthwaite's method [
#lmerModLmerTest]
#Formula: Glycogen ~ Sampling + Site + Sampling * Site + (1 | Bag)
#Data: Gly
#
#REML criterion at convergence: 2234.4
#
#Scaled residuals: 
#  Min      1Q  Median      3Q     Max 
#-2.1068 -0.3890 -0.1256  0.2517  6.8270 
#
#Random effects:
#  Groups   Name        Variance Std.Dev.
#Bag      (Intercept)  0.6335  0.7959  
#Residual             29.5324  5.4344  
#Number of obs: 359, groups:  Bag, 36
#
#Fixed effects:
#  Estimate Std. Error       df t value Pr(>|t|)    
#(Intercept)                  1.3565     1.2816 322.9010   1.058  0.29065    
#Sampling                     0.4223     0.3321 320.5711   1.271  0.20450    
#SiteBBOC - Middle           -4.7437     1.8115 322.8268  -2.619  0.00924 ** 
#  SiteHIOC - North            16.4853     1.8115 322.8268   9.101  < 2e-16 ***
#  Sampling:SiteBBOC - Middle   2.4485     0.4687 320.3598   5.224 3.16e-07 ***
#  Sampling:SiteHIOC - North   -2.0255     0.4687 320.3598  -4.321 2.07e-05 ***
#  ---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
#
#Correlation of Fixed Effects:
#  (Intr) Smplng SBBO-M SHIO-N S:SB-M
#Sampling    -0.904                            
#StBBOC-Mddl -0.708  0.639                     
#StHIOC-Nrth -0.708  0.639  0.501              
#Smp:SBBOC-M  0.640 -0.709 -0.904 -0.453       
#Smp:SHIOC-N  0.640 -0.709 -0.453 -0.904  0.502

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

#### Pairwise Comparisons ===============
## pairwise comparison for m5
emm_Lm7a <-  emmeans(m7, specs = ~ Site|Sampling)
emm_Lm7a

pairwise_Lm7a <- contrast(emm_Lm7a, interaction = "pairwise")
pairwise_Lm7a

#### Plot Model ========
## using ggeffects
## https://strengejacke.github.io/ggeffects/articles/introduction_plotcustomize.html

m6.plot_bySite <- ggpredict(m6, terms = c("Sampling", "Site"))
plot(m6.plot_bySite) +
  theme_classic() +
  #scale_color_brewer(palette = "Paired", direction = -1)  +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Glycogen ~ Sampling + Site")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Sampling Time Point", 
       y = "Glycogen (umol glycosyl units/g protein)")

ggsave(filename = "fig_output/model_Mgigas_Gly_Sampling-Site.png", width = 5.10, height = 5.77, dpi = 300)

m6.plot_bySampling <- ggpredict(m6, terms = c("Site", "Sampling"))
plot(m6.plot_bySampling) +
  theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Glycogen ~ Sampling + Site")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Site", 
       y = "Glycogen (umol glycosyl units/g protein)")

ggsave(filename = "fig_output/model_Mgigas_Gly_Site_Sampling.png", width = 5.10, height = 5.77, dpi = 300)

##### DARK PLOTS: ggdark / black background =================
library(ggdark)

m6.DARKplot_bySite <- ggpredict(m6, terms = c("Sampling", "Site"))
plot(m6.DARKplot_bySite) +
  dark_theme_classic() +
  scale_color_brewer(palette = "RdYlBu", direction = -1)  +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Glycogen ~ Sampling + Site")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Sampling Time Point", 
       y = "Glycogen (umol glycosyl units/g protein)")

ggsave(filename = "fig_output/DARKmodel_Mgigas_Gly_Sampling-Site.png", width = 5.10, height = 5.77, dpi = 300)

m6.DARKplot_bySampling <- ggpredict(m6, terms = c("Site", "Sampling"))
plot(m6.DARKplot_bySampling) +
  dark_theme_classic() +
  scale_color_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title = expression(paste(italic("M. gigas"), ": glm(Glycogen ~ Sampling + Site")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x = "Site", 
       y = "Glycogen (umol glycosyl units/g protein)")

ggsave(filename = "fig_output/DARKmodel_Mgigas_Gly_Site_Sampling.png", width = 5.10, height = 5.77, dpi = 300)

