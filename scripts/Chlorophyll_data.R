########## Chlorophyll 2021-2022 ==============

##### ChlData ============

### load libraries
library(tidyverse)

#### *** 2021: Benthic Chlorophyll *** ====
### load data sheet
BChl2021 <- read_csv("data/Chlorophyll/2021/Chla_Benthic_2021.csv")
glimpse(BChl2021)
summary(BChl2021)
tail(BChl2021)
View(BChl2021)

## clean data
#### now remove NAs from data sheet
colSums(is.na(BChl2021)) ## Date: 4 NAs

BChl2021_2 <- BChl2021 %>% 
  filter(!is.na(Date)) # omit the 4 NAs in Date
colSums(is.na(BChl2021_2))


BChl2021_2$Sampling_Period <- as.character(BChl2021_2$Sampling_Period)
is.character(BChl2021_2$Sampling_Period)

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.B2021 <- BChl2021_2 %>%
  summarize(
    MeanChla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

AllStats.B2021


 
#### By Sampling ====

SamplingB2021.Stats <- BChl2021_2 %>%
  group_by(Sampling_Period) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

SamplingB2021.Stats

## plot
Plot.B2021SamplingStats <- ggplot(BChl2021_2, aes(x = Sampling_Period, y = Chl_a)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2021)")), 
       x = "Sampling_Period", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))
       
Plot.B2021SamplingStats 


#### By Site ====

SiteB2021.Stats <- BChl2021_2 %>%
  group_by(Lease) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

SiteB2021.Stats

## plot
Plot.B2021SiteStats <- ggplot(BChl2021_2, aes(x = factor(Lease, c("HIOC", "BBOC", "TBOC")), y = Chl_a)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2021)")), 
       x = "Lease", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))

Plot.B2021SiteStats 

#### By Sampling Period & Site ====

B2021SamplingSite.Stats <- BChl2021_2 %>%
  group_by(Lease, Sampling_Period) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

B2021SamplingSite.Stats

## plot

Plot.B2021SiteStats <- ggplot(BChl2021_2, aes(x = Sampling_Period, y = Chl_a)) +
  geom_boxplot() +
  facet_wrap(~factor(Lease, c("HIOC", "BBOC", "TBOC"))) +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2021)")), 
       x = "Lease", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))

Plot.B2021SiteStats 

#### *** 2022: Benthic Chlorophyll *** ====
### load data sheet
BChl2022 <- read_csv("data/Chlorophyll/2022/Chla_Benthic_2022.csv")
glimpse(BChl2022)
summary(BChl2022)
tail(BChl2022)
View(BChl2022)

## clean data
#### now remove NAs from data sheet
colSums(is.na(BChl2022)) # no NAs

BChl2022$Sampling_Period <- as.character(BChl2022$Sampling_Period)
is.character(BChl2022$Sampling_Period)

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.B2022 <- BChl2022 %>%
  summarize(
    MeanChla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

AllStats.B2022


#### By Sampling ====

SamplingB2022.Stats <- BChl2022 %>%
  group_by(Sampling_Period) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

SamplingB2022.Stats

## plot
Plot.B2022SamplingStats <- ggplot(BChl2022, aes(x = Sampling_Period, y = Chl_a)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2022)")), 
       x = "Sampling_Period", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))

Plot.B2022SamplingStats 

#### No Grouping ====

AllStats.B2022 <- BChl2022 %>%
  summarize(
    MeanChla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

AllStats.B2022


#### By Site ====

SiteB2022.Stats <- BChl2022 %>%
  group_by(Lease) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

SiteB2022.Stats

## plot
Plot.B2022SiteStats <- ggplot(BChl2022, aes(x = factor(Lease, c("HIOC", "BBOC", "TBOC")), y = Chl_a)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2022)")), 
       x = "Sampling_Period", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))

Plot.B2022SiteStats 

#### By Sampling Period & Site ====

B2022SamplingSite.Stats <- BChl2022 %>%
  group_by(Lease, Sampling_Period) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

B2022SamplingSite.Stats

## plot

Plot.B2022SiteStats <- ggplot(BChl2022, aes(x = Sampling_Period, y = Chl_a)) +
  geom_boxplot() +
  facet_wrap(~factor(Lease, c("HIOC", "BBOC", "TBOC"))) +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2022)")), 
       x = "Lease", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))

Plot.B2022SiteStats 

#### *** 2022: Water Chlorophyll *** ====
### load data sheet
WChl2022 <- read_csv("data/Chlorophyll/2022/Chla_Seawater_2022.csv")
glimpse(WChl2022)
summary(WChl2022)
tail(WChl2022)
View(WChl2022)

## clean data
#### now remove NAs from data sheet
colSums(is.na(WChl2022)) # 3 NAs Datee

## omit Notes column which is all NAs / info
WChl2022_2 <- WChl2022 %>% 
  filter(!is.na(Date)) # omit the 3 NAs in Date

colSums(is.na(WChl2022_2)) ## All NAs under Date

WChl2022_2$Sampling_Period <- as.character(WChl2022_2$Sampling_Period)
is.character(WChl2022_2$Sampling_Period)

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.B2022 <- BChl2022 %>%
  summarize(
    MeanChla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

AllStats.B2022


#### By Sampling ====

SamplingB2022.Stats <- BChl2022 %>%
  group_by(Sampling_Period) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

SamplingB2022.Stats

## plot
Plot.B2022SamplingStats <- ggplot(BChl2022, aes(x = Sampling_Period, y = Chl_a)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2022)")), 
       x = "Sampling_Period", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))

Plot.B2022SamplingStats 

#### No Grouping ====

AllStats.B2022 <- BChl2022 %>%
  summarize(
    MeanChla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

AllStats.B2022


#### By Site ====

SiteB2022.Stats <- BChl2022 %>%
  group_by(Lease) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

SiteB2022.Stats

## plot
Plot.B2022SiteStats <- ggplot(BChl2022, aes(x = factor(Lease, c("HIOC", "BBOC", "TBOC")), y = Chl_a)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2022)")), 
       x = "Sampling_Period", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))

Plot.B2022SiteStats 

#### By Sampling Period & Site ====

B2022SamplingSite.Stats <- BChl2022 %>%
  group_by(Lease, Sampling_Period) %>% 
  summarize(
    Mean_Chla = mean(Chl_a),
    SD_Chla = sd(Chl_a),
    SE_Chla = SD_Chla/sqrt(n()))

B2022SamplingSite.Stats

## plot

Plot.B2022SiteStats <- ggplot(BChl2022, aes(x = Sampling_Period, y = Chl_a)) +
  geom_boxplot() +
  facet_wrap(~factor(Lease, c("HIOC", "BBOC", "TBOC"))) +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Benthic Chl ", italic("a"), " in Tomales Bay (2022)")), 
       x = "Lease", 
       y = expression(paste("Chlorophyll ", italic("a"), " (mg/m"^3,")")))

Plot.B2022SiteStats 
