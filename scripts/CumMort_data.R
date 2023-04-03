########## 2020 - 2022 Mortality ==============

### load libraries
library(tidyverse)
library(ggdark)

########## 2020 Mortality ==============
Morts2020 <- read_csv("data/Mortality/2020/Mortality2020.csv")
glimpse(Morts2020)
summary(Morts2020)
tail(Morts2020)
View(Morts2020)

TotalMort2020_perBag <- Morts2020 %>% 
  group_by(site, bag_numb) %>% 
  summarize(TotalMort = sum(mort_numb))

TotalMort2020_perBag


TotalMort2020_perSite <- Morts2020 %>% 
  group_by(site) %>% 
  summarize(TotalMort = sum(mort_numb))

TotalMort2020_perSite

#Plot.TotalMort2020_perSite <- 
ggplot(data = Morts2020, aes(x = mort_numb, y = site, fill = site)) +
  geom_bar(stat="identity") +
  dark_theme_classic()

########## 2021 Mortality ==============

Morts2021 <- read_csv("data/Mortality/2021/Mortality2021.csv")
glimpse(Morts2021)
summary(Morts2021)
tail(Morts2021)
View(Morts2021)

TotalMort2021_perBag <- Morts2021 %>% 
  group_by(Site, Species, Temp_Hardening, Tide_Hardening, Bag_Numb) %>% 
  summarize(TotalMort = sum(Mortality_Total))

print(TotalMort2021_perBag, n = 54)

TotalMort2021_perSite <- Morts2021 %>% 
  group_by(Site, Species, Temp_Hardening, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_perSite

TotalMort2021_perSpecies <- Morts2021 %>% 
  group_by(Species, Temp_Hardening, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_perSpecies

TotalMort2021_perSHtemp <- Morts2021 %>% 
  group_by(Species, Temp_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_perSHtemp

TotalMort2021_perSHtide <- Morts2021 %>% 
  group_by(Species, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_perSHtide

## Plot
Morts2021 %>% 
  filter(Species == "C_sikamea") %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = Mortality_Total, fill = Site)) +
  facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  dark_theme_classic()

Morts2021 %>% 
  filter(Species == "C_gigas") %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = Mortality_Total, fill = Site)) +
  facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  dark_theme_classic()


########## 2022 Mortality ==============

Morts2022 <- read_csv("data/Mortality/2022/Mortality2022.csv")
glimpse(Morts2022)
summary(Morts2022)
tail(Morts2022)
View(Morts2022)

Morts2022$Temp_Hardening <- as.character(Morts2022$Temp_Hardening)
is.character(Morts2022$Temp_Hardening)

TotalMort2022_perBag <- Morts2022 %>% 
  group_by(Site, Temp_Hardening, Tide_Hardening, Bag_Numb) %>% 
  summarize(TotalMort = sum(Mortality_Total))

print(TotalMort2022_perBag, n = 54)

TotalMort2022_perSite <- Morts2022 %>% 
  group_by(Site, Species, Temp_Hardening, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2022_perSite

TotalMort2022_perSHtemp <- Morts2022 %>% 
  group_by(Species, Temp_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2022_perSHtemp

TotalMort2022_perSHtide <- Morts2022 %>% 
  group_by(Species, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2022_perSHtide

## Plot
Morts2022 %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = Mortality_Total, fill = Site)) +
  facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  dark_theme_classic()

Morts2022 %>% 
  ggplot(aes(x = factor(Temp_Hardening), y = Mortality_Total, fill = Tide_Hardening)) +
  geom_bar(stat="identity", position=position_dodge()) +
  dark_theme_classic()
