########## LW_2020 - 2022 ==============

### load libraries
library(tidyverse)

#### *** 2021: Benthic Chlorophyll *** ====
### load data sheet
LW2020_og <- read_csv("data/Growth/2020/LW_2020_T1_T5.csv")
LW2020 <- LW2020_og %>%
  pivot_wider(names_from = L_W, values_from = Size_cm)

glimpse(LW2020)
summary(LW2020)
tail(LW2020)
View(LW2020)

## clean data
#### now remove NAs from data sheet
colSums(is.na(LW2020)) ## Date: No NAs

### order Site
LW2020$Site <-factor(LW2020$Site, c("North", "Middle", "South"))

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.LW2020 <- LW2020 %>%
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

AllStats.LW2020

#### By Sampling_Time ====

SamplingStats.LW2020 <- LW2020 %>%
  group_by(Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SamplingStats.LW2020

#### By Site====

SiteStats.LW2020 <- LW2020 %>%
  group_by(Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SiteStats.LW2020

#### By Bag by Sampling_Time ====

BagStats.LW2020 <- LW2020 %>%
  group_by(Sampling_Time,Bag) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

BagStats.LW2020

#### *** 2021: Benthic Chlorophyll *** ====
### load data sheet
LW2021_og <- read_csv("data/Growth/2021/LW_2021_T1_T4.csv")
glimpse(LW2021_og)

LW2021 <- LW2021_og %>%
  pivot_wider(names_from = L_W, values_from = Size_cm)
