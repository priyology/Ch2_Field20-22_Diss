#### ~ M. GIGAS 2020 Baseline qPCR DATA ~ =====

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

#### Mean, SD, SE, min, max for 2020 Cohort by Year ===== 
OsHV1 %>%
  filter(Cohort == "2020") %>% 
  group_by(Year, Site) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))
