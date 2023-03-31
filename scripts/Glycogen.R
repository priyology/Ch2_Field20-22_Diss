########## 2022 Glucose & Glycogen ==============

### load libraries
library(tidyverse)

Gly <- read_csv("data/Glycogen/2022/Glycogen2022.csv")
glimpse(Gly)
summary(Gly)
tail(Gly)
View(Gly)

#### Omit NAs=========

#### now remove NAs from data sheet
colSums(is.na(Gly)) ## Glycogen content: 1

### Change data type
Gly$Sampling <- as.character(Gly$Sampling) ## make Sampling a character
is.character(Gly$Sampling) ## True

Gly$SH_Temp <- as.character(Gly$SH_Temp) ## make Sampling a character
is.character(Gly$SH_Temp) ## True


## omit Notes column which is all NAs / info
Gly2 <- Gly %>% 
  filter(!is.na(Glycogen)) # omit the 93 NAs in CI
colSums(is.na(Gly2)) ## All NAs under CI

#histogram of all glycogen
ggplot(Gly2, aes(Glycogen)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "Glycogen Histogram",
       x = "Glycogen",
       y = "Counts")

HighGly <- filter(Gly2, Glycogen > 20)
nrow(HighGly) #23

#### Figures, Mean, SD, SE =====

#### No Grouping ====


AllStats <- Gly2 %>%
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

AllStats

#### All Grouping: Sampling, Site, SH_Temp, SH_Tide ====


Stats_Samp_Site_Temp_Tide <- Gly2 %>%
  group_by(Sampling, Site, SH_Temp, SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Stats_Samp_Site_Temp_Tide

