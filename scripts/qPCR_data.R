#### ~ M. GIGAS 2020 qPCR DATA ~ =====

## load libraries
library(tidyverse)
library(scales) # to access break formatting functions for log scale

########## 2020 Cohort OsHV-1 Data ==============
OsHV1 <- read_csv("data/qPCR/qPCR_runs.csv")
glimpse(OsHV1)
summary(OsHV1)
tail(OsHV1)
View(OsHV1)

#### now remove NAs from data sheet
colSums(is.na(OsHV1))

### Year_Sampled as character
OsHV1$Year_Sampled <- as.character(OsHV1$Year_Sampled)
is.character(OsHV1$Year_Sampled)

### Sampling_Period as character
OsHV1$Sampling_Period <- as.character(OsHV1$Sampling_Period)
is.character(OsHV1$Sampling_Period)

### Cohort as character
OsHV1$Cohort <- as.character(OsHV1$Cohort)
is.character(OsHV1$Cohort)

#### 2020 Cohort Stats by Year ===== 
OsHV1 %>%
  filter(Cohort == "2020") %>% 
  group_by(Year_Sampled, Site) %>%
  summarize(Mean_Copies = mean(log_transform),
            SD_Copies = sd(log_transform),
            SE_Copies = SD_Copies/sqrt(n()))

OsHV1 %>%
filter(Cohort == "2020" ) %>% 
ggplot(aes(x = Site, y = log_transform, color = Site)) +
  geom_jitter(size = 5, alpha = 0.5) +
  facet_wrap(~Year_Sampled) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic()


#### 2020 v 2022 Cohort Stats by Year =====
OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort) %>%
  summarize(Mean_Copies = mean(log_transform),
            SD_Copies = sd(log_transform),
            SE_Copies = SD_Copies/sqrt(n()))

OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  ggplot(aes(x = Bag, y = log_transform, color = Bag)) +
  geom_jitter(size = 5, alpha = 0.5) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic()
  
