#### ~ C. SIKAMEA 2021 qPCR DATA ~ =====

## load libraries
library(tidyverse)
library(scales) # to access break formatting functions for log scale
library(ggdark)

########## 2020 Cohort OsHV-1 Data ==============
OsHV1 <- read_csv("data/qPCR/qPCR_runs.csv")
glimpse(OsHV1)
summary(OsHV1)
tail(OsHV1)
View(OsHV1)

#### now remove NAs from data sheet
colSums(is.na(OsHV1))

### Year_Sampled as character
OsHV1$Year <- as.character(OsHV1$Year)
is.character(OsHV1$Year)

### Sampling_Period as character
OsHV1$Sampling_Period <- as.character(OsHV1$Sampling_Period)
is.character(OsHV1$Sampling_Period)

### Cohort as character
OsHV1$Cohort <- as.character(OsHV1$Cohort)
is.character(OsHV1$Cohort)

#### Mean, SD, SE, min, max for C. sikeamea: Site, SH_Temp, SH_Tide  ===== 
Csik_Stats1 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(factor(Site, c("North", "Middle", "South")), SH_Temp, SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Csik_Stats1

#### Mean, SD, SE, min, max for C. sikeamea: Site, SH_Temp ===== 
Csik_Stats2 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(factor(Site, c("North", "Middle", "South")), SH_Temp) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Csik_Stats2

#### Mean, SD, SE, min, max for C. sikeamea: Site, SH_Tide ===== 
Csik_Stats3 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(factor(Site, c("North", "Middle", "South")), SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Csik_Stats3


#### Mean, SD, SE, min, max for C. sikeamea: SH_Temp, SH_Tide ===== 
Csik_Stats4 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(SH_Temp, SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Csik_Stats4


#### Mean, SD, SE, min, max for C. sikeamea: Site ===== 
Csik_Stats5 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(factor(Site, c("North", "Middle", "South"))) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Csik_Stats5

#### Mean, SD, SE, min, max for C. sikeamea: SH_Temp, SH_Tide ===== 
Csik_Stats6 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(SH_Temp) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Csik_Stats6

#### Mean, SD, SE, min, max for C. sikeamea: SH_Temp, SH_Tide ===== 
Csik_Stats7 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Csik_Stats7
