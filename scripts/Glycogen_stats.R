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

#### Model selection ====
#### m_null: Glycogen ~ 1  =====

m_null <- glm(Glycogen ~ 1, family = gaussian(link = "identity"), data = Gly)
summary(m_null)


#### m1: Glycogen ~ SH_Temp  =====
#### m2: Glycogen ~ SH_Tide  =====
#### m3: Glycogen ~ Sampling  =====
#### m4: Glycogen ~ Site  =====
