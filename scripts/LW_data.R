########## LW_2020 - 2022 ==============

### load libraries
library(tidyverse)

#### *** 2020: L/W *** ====
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

### Sampling_Time as character
LW2020$Sampling_Time <- as.character(LW2020$Sampling_Time)

is.character(LW2020$Sampling_Time)

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

## plot
ggplot(LW2020, aes(x = Sampling_Time, y = L)) +
  geom_boxplot()

#### By Site====

SiteStats.LW2020 <- LW2020 %>%
  group_by(Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SiteStats.LW2020

## plot
ggplot(LW2020, aes(x = Site, y = L)) +
  geom_boxplot()


#### By Bag by Sampling_Time ====

BagStats.LW2020 <- LW2020 %>%
  group_by(Sampling_Time,Bag) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

BagStats.LW2020

## plot
ggplot(LW2020, aes(x = Site, y = L, color = Site)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()


#### *** 2021: L/W *** ====
### load data sheet
LW2021_og <- read_csv("data/Growth/2021/LW_2021_T1_T4.csv")
glimpse(LW2021_og)

## clean data
### order Site
LW2021_og$Site <-factor(LW2021_og$Site, c("North", "Middle", "South"))

### Sampling_Time as character
LW2021_og$Sampling_Time <- as.character(LW2021_og$Sampling_Time)
is.character(LW2021_og$Sampling_Time)

### SH_Temp as character
LW2021_og$SH_Temp <- as.character(LW2021_og$SH_Temp)
is.character(LW2021_og$SH_Temp)

#### now remove NAs from data sheet
colSums(is.na(LW2021_og)) ## Size_cm: 6 NAs, Sampling_Time: 3 NAs

LW2021_og.na <- LW2021_og %>% 
  filter(!is.na(Sampling_Time), 
         !is.na(Size_cm)) # omit all NAs
    
colSums(is.na(LW2021_og.na))

LW2021 <- LW2021_og.na %>%
  pivot_wider(names_from = L_W, values_from = Size_cm)

#### *** 2021: M. gigas *** ====
LW2021.gigas <- LW2021 %>% 
  filter(Species == "M. gigas")

### filter out NA
colSums(is.na(LW2021.gigas)) ## Size_cm: 2 NAs in L

LW2021.gigas.na <- LW2021.gigas %>% 
  filter(!is.na(L)) # omit all NAs

colSums(is.na(LW2021.gigas.na)) 

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.2021gigas <- LW2021.gigas.na %>%
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

AllStats.2021gigas 

#### By SH_Temp ====

SHTemp_Stats.2021gigas <- LW2021.gigas.na %>%
  group_by(SH_Temp) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTemp_Stats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  geom_boxplot()

#### By Sampling_Time ====

SamplingStats.2021gigas <- LW2021.gigas.na %>%
  group_by(Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SamplingStats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = Sampling_Time, y = L, color = Sampling_Time)) +
  geom_boxplot()


#### By Site ====

SiteStats.2021gigas <- LW2021.gigas.na %>%
  group_by(Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SiteStats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = Site, y = L, color = Site)) +
  geom_boxplot()

#### By SH_Temp + Site ====

SHTempSite_Stats.2021gigas <- LW2021.gigas.na %>%
  group_by(SH_Temp, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSite_Stats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  facet_wrap(~Site) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time ====

SHTempSampling_Stats.2021gigas <- LW2021.gigas.na %>%
  group_by(SH_Temp, Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSampling_Stats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time + Site ====

SHTempSamplingSite_Stats.2021gigas <- LW2021.gigas.na %>%
  group_by(SH_Temp, Sampling_Time, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSamplingSite_Stats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = Site, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()

#### *** 2021: C. sikamea *** ====
LW2021.sikamea <- LW2021 %>% 
  filter(Species == "C. sikamea")

### filter out NAs
colSums(is.na(LW2021.sikamea))

LW2021.sikamea.na <- LW2021.sikamea %>% 
  filter(!is.na(L), 
         !is.na(W)) # omit all NAs

colSums(is.na(LW2021.sikamea.na))

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.2021sikamea <- LW2021.sikamea.na %>%
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

AllStats.2021sikamea 

#### By SH_Temp ====

SHTemp_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Temp) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTemp_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  geom_boxplot()

#### By SH_Tide ====

SHTide_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Tide) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTide_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = SH_Tide, y = L, color = SH_Tide)) +
  geom_boxplot()

#### By Sampling_Time ====

SamplingTime_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SamplingTime_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = Sampling_Time, y = L, color = Sampling_Time)) +
  geom_boxplot()

#### By SH_Temp + Site ====

SHTempSite_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Temp, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSite_Stats.2021sikamea

## plot
ggplot(LW2021.sikamea.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  facet_wrap(~Site) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time ====

SHTempSampling_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Temp, Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSampling_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time + Site ====

SHTempSamplingSite_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Temp, Sampling_Time, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSamplingSite_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = Site, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()


#### *** 2022: L/W *** ====
### load data sheet
LW2022_og <- read_csv("data/Growth/2022/LW_2022_T1_T6.csv")
glimpse(LW2022_og)

## clean data
### order Site
LW2022_og$Site <-factor(LW2022_og$Site, c("North", "Middle", "South"))

### Sampling_Time as character
LW2022_og$Sampling_Time <- as.character(LW2022_og$Sampling_Time)
is.character(LW2022_og$Sampling_Time)

### SH_Temp as character
LW2022_og$SH_Temp <- as.character(LW2022_og$SH_Temp)
is.character(LW2022_og$SH_Temp)
