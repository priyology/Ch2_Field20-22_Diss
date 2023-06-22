########## ~ M. GIGAS 2020 Baseline qPCR Stats ==============

## load libraries
library(tidyverse)
library(gtsummary) # for producing tables: 
library(broom.mixed) ## to use with gtsummary
library(lme4) ##glm
library(lmerTest) ##p-values
library(emmeans) ## comparisons
library(ggeffects) ## another model plotting option

########## 2020 Cohort OsHV-1 Data ==============
OsHV1 <- read_csv("data/qPCR/qPCR_runs.csv")
glimpse(OsHV1)
summary(OsHV1)
tail(OsHV1)
View(OsHV1)

#### now remove NAs from data sheet
colSums(is.na(OsHV1)) #No NAs

### Year as character
OsHV1$Year <- as.character(OsHV1$Year)
is.character(OsHV1$Year)

### Sampling_Period as character
OsHV1$Sampling_Period <- as.character(OsHV1$Sampling_Period)
is.character(OsHV1$Sampling_Period)

### Cohort as character
OsHV1$Cohort <- as.character(OsHV1$Cohort)
is.character(OsHV1$Cohort)

#### *** 2020 v 2022 Cohort OsHV-1 by Year *** ===== 
Baseline <- OsHV1_20v22 <-OsHV1 %>%
  filter(Cohort == "2020")


