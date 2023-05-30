########## 2022 Protein Carbonyl -- Field Experiment ==============

### load libraries
library(tidyverse)

PC <- read_csv("data/Protein_Carbonyl/ProteinCarbonyl_2022.csv")
glimpse(PC)
summary(PC)
tail(PC)
View(PC)

### Change data type

PC$SH_Temp <- as.character(PC$SH_Temp) ## make Sampling a character
is.character(PC$SH_Temp) ## True

#### Omit NAs=========

#### now remove NAs from data sheet
colSums(is.na(PC)) ## CarbPerProtein: 3

## omit Notes column which is all NAs / info
PC2 <- PC %>%
  filter(!is.na(CarbPerProtein)) # omit the 3 NAs in PC
colSums(is.na(PC2)) ## All NAs under PC

write_csv(PC2, file = "data/Protein_Carbonyl/ProteinCarbonyl_Stats.csv")

#histogram of all PC
ggplot(PC2, aes(CarbPerProtein)) +
  geom_histogram() +
  theme_classic() +
  scale_fill_brewer(palette = "RdYlBu", direction = -1) +
  labs(title = "PC",
       x = "Protein Carbonyl (nmole_carbonyl/mg_protein)",
       y = "Counts")

HighPC <- filter(PC2, CarbPerProtein > 15)
nrow(HighPC) #2

#### Figures, Mean, SD, SE =====

#### No Grouping ====


AllStats <- PC2 %>%
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

AllStats

#### By Sampling ====

Sampling.Stats <- PC2 %>%
  group_by(Sampling) %>% 
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

Sampling.Stats

## plot
Plot.SamplingStats <- ggplot(PC2, aes(x = Sampling, y = CarbPerProtein, color = Sampling)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl", italic("M. gigas"))), 
       x = "Site", 
       y = "Protein Carbonyl (nmole_carbonyl/mg_protein)")

Plot.SamplingStats

#### By Site ====

Site.Stats <- PC2 %>%
  group_by(Site) %>% 
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

Site.Stats

## plot

Plot.Site <- ggplot(PC2, aes(x = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")), y = CarbPerProtein, color = Site)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl", italic("M. gigas"))), 
       x = "Site", 
       y = "Protein Carbonyl (nmole_carbonyl/mg_protein)")

Plot.Site

#### By SH_Temp ====

SH_Temp.Stats <- PC2 %>%
  group_by(SH_Temp) %>% 
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

SH_Temp.Stats

## plot

Plot.SH_Temp <- ggplot(PC2, aes(x = SH_Temp, y = CarbPerProtein, color = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Protein Carbonyl (nmole_carbonyl/mg_protein)")

Plot.SH_Temp


#### By SH_Tide ====

SH_Tide.Stats <- PC2 %>%
  group_by(SH_Tide) %>% 
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

SH_Tide.Stats

## plot

Plot.SH_Tide <- ggplot(PC2, aes(x = SH_Tide, y = CarbPerProtein, color = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl ", italic("M. gigas"))), 
       x = "SH_Tide", 
       y = "CarbPerProtein (nmole_carbonyl/mg_protein)")

Plot.SH_Tide

#### By Site, SH_Temp ====

Site_Temp.Stats <- PC2 %>%
  group_by(Site, SH_Temp) %>% 
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

Site_Temp.Stats

## plot

Plot.Site_Temp <- ggplot(PC2, aes(x = SH_Temp, y = CarbPerProtein, color = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")))) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "CarbPerProtein (nmole_carbonyl/mg_protein)")

Plot.Site_Temp

#### By Site, SH_Tide ====

Site_Tide.Stats <- PC2 %>%
  group_by(Site, SH_Tide) %>% 
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

Site_Tide.Stats

## plot

Plot.Site_Tide <- ggplot(PC2, aes(x = SH_Tide, y = CarbPerProtein, color = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")))) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl ", italic("M. gigas"))), 
       x = "SH_Tide", 
       y = "CarbPerProtein (nmole_carbonyl/mg_protein)")

Plot.Site_Tide

#### SH_Temp, SH_Tide ====

Temp_Tide.Stats <- PC2 %>%
  group_by(SH_Temp, SH_Tide) %>% 
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

Temp_Tide.Stats

## plot

Plot.Temp_Tide <- ggplot(PC2, aes(x = SH_Temp, y = CarbPerProtein, color = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "CarbPerProtein (nmole_carbonyl/mg_protein)")

Plot.Temp_Tide


#### Site, SH_Temp, SH_Tide ====

Site_Temp_Tide.Stats <- PC2 %>%
  group_by(Site, SH_Temp, SH_Tide) %>% 
  summarize(
    MeanPC = mean(CarbPerProtein),
    SD_PC = sd(CarbPerProtein),
    SE_PC = SD_PC/sqrt(n()))

Site_Temp_Tide.Stats

## plot

Plot.Site_Temp_Tide <- ggplot(PC2, aes(x = SH_Temp, y = CarbPerProtein, color = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")))) +
  facet_wrap(~SH_Tide) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "CarbPerProtein (nmole_carbonyl/mg_protein)")

Plot.Site_Temp_Tide


## All Stats Plot

Plot.AllStats <- ggplot(PC2, aes(x = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")), y = CarbPerProtein, color = Site)) +
  geom_boxplot() +
  facet_grid(SH_Temp ~ SH_Tide) +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Protein Carbonyl ", italic("M. gigas"))), 
       x = "Site", 
       y = "CarbPerProtein (nmole_carbonyl/mg_protein)")

Plot.AllStats


#### TBD: CarbPerProtein Summary Stats Bar Plot ===========
library(ggdark)

PCMean <- read_csv("data/CarbPerProtein/2022/PC_SumStats.csv")
glimpse(PCMean)

PCMean$SH_Temp <- as.character(PCMean$SH_Temp)
PCMean$Sampling <- as.character(PCMean$Sampling)

PCMean_SampSites  <-  PC2 %>% 
  group_by(Sampling, Site) %>% 
  summarize(MeanPC = mean(CarbPerProtein),
            SDPC = sd(CarbPerProtein),
            SEPC = SDPC/sqrt(n()))

PCMean_SampSites

PCMean_SampSites$Sampling <- as.character(PCMean_SampSites$Sampling)

BarPlot.PC <- ggplot(PCMean_SampSites, aes(x = factor(Site, c("TBOC - South", "BBOC - Middle", "HIOC - North")), y = MeanPC, fill = Sampling, group = factor(Sampling, c("5", "2")))) + #, fill = SH_Temp, group = SH_Temp)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = MeanPC-SEPC, ymax = MeanPC + SEPC), width=.2, position=position_dodge(.9)) +
  #facet_grid(SH_Temp ~ factor(Sampling, c("June", "August"))) +
  dark_theme_classic() + 
  coord_flip()


#scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
#labs(title=expression(paste("Protein Carbonyl ", italic("M. gigas"))), 
#     x = "Site", 
#     y = "CarbPerProtein (nmole_carbonyl/mg_protein)")

BarPlot.PC

