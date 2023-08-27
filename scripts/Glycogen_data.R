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

## omit Notes column which is all NAs / info
Gly2 <- Gly %>% 
  filter(!is.na(Glycogen)) # omit the 1 NA in Glycogen
colSums(is.na(Gly2)) ## All NAs under Glycogen


### Change data type
Gly$Sampling <- as.character(Gly$Sampling) ## make Sampling a character
is.character(Gly$Sampling) ## True

Gly$SH_Temp <- as.character(Gly$SH_Temp) ## make Sampling a character
is.character(Gly$SH_Temp) ## True

Gly %>% 
  group_by(Sampling) %>% 
  reframe(n = n())


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

#### By Sampling ====

Sampling.Stats <- Gly2 %>%
  group_by(Sampling) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Sampling.Stats

## plot
Plot.SamplingStats <- ggplot(Gly2, aes(x = Sampling, y = Glycogen, color = Sampling)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "Site", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.SamplingStats

#### By Site ====

Site.Stats <- Gly2 %>%
  group_by(Site) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Site.Stats

## plot

Plot.Site <- ggplot(Gly2, aes(x = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")), y = Glycogen, color = Site)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "Site", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Site

#### By SH_Temp ====

SH_Temp.Stats <- Gly2 %>%
  group_by(SH_Temp) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

SH_Temp.Stats

## plot

Plot.SH_Temp <- ggplot(Gly2, aes(x = SH_Temp, y = Glycogen, color = SH_Temp)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.SH_Temp


#### By SH_Tide ====

SH_Tide.Stats <- Gly2 %>%
  group_by(SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

SH_Tide.Stats

## plot

Plot.SH_Tide <- ggplot(Gly2, aes(x = SH_Tide, y = Glycogen, color = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Tide", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.SH_Tide

#### By Sampling, Site ====

Sampling_Site.Stats <- Gly2 %>%
  group_by(Sampling, Site) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Sampling_Site.Stats

## plot

Plot.Sampling_Site <- ggplot(Gly2, aes(x = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")), y = Glycogen, color = Sampling)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "Site", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Sampling_Site

#### By Sampling, SH_Temp ====

Sampling_Temp.Stats <- Gly2 %>%
  group_by(Sampling, SH_Temp) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Sampling_Temp.Stats

## plot

Plot.Sampling_Temp <- ggplot(Gly2, aes(x = SH_Temp, y = Glycogen, color = Sampling)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Sampling_Temp


#### By Sampling, SH_Tide ====

Sampling_Tide.Stats <- Gly2 %>%
  group_by(Sampling, SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Sampling_Tide.Stats

## plot

Plot.Sampling_Tide <- ggplot(Gly2, aes(x = SH_Tide, y = Glycogen, color = Sampling)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Tide", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Sampling_Tide

#### By Site, SH_Temp ====

Site_Temp.Stats <- Gly2 %>%
  group_by(Site, SH_Temp) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Site_Tide.Stats

## plot

Plot.Site_Temp <- ggplot(Gly2, aes(x = SH_Temp, y = Glycogen, color = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")))) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Site_Temp

#### By Site, SH_Tide ====

Site_Tide.Stats <- Gly2 %>%
  group_by(Site, SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Site_Tide.Stats

## plot

Plot.Site_Tide <- ggplot(Gly2, aes(x = SH_Tide, y = Glycogen, color = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")))) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Tide", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Site_Tide

#### SH_Temp, SH_Tide ====

Temp_Tide.Stats <- Gly2 %>%
  group_by(SH_Temp, SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Temp_Tide.Stats

## plot

Plot.Temp_Tide <- ggplot(Gly2, aes(x = SH_Temp, y = Glycogen, color = SH_Tide)) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Temp_Tide

#### Sampling, Site, SH_Temp ====

Sampling_Site_Temp.Stats <- Gly2 %>%
  group_by(Sampling, Site, SH_Temp) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Sampling_Site_Temp.Stats

## plot

Plot.Sampling_Site_Temp <- ggplot(Gly2, aes(x = SH_Temp, y = Glycogen, color = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")))) +
  facet_wrap(~Sampling) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Sampling_Site_Temp

#### Sampling, Site, SH_Tide ====

Sampling_Site_Tide.Stats <- Gly2 %>%
  group_by(Sampling, Site, SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Sampling_Site_Tide.Stats

## plot

Plot.Sampling_Site_Tide <- ggplot(Gly2, aes(x = SH_Tide, y = Glycogen, color = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")))) +
  facet_wrap(~Sampling) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Tide", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Sampling_Site_Tide

#### Sampling, SH_Temp, SH_Tide ====

Sampling_Temp_Tide.Stats <- Gly2 %>%
  group_by(SiteSampling, SH_Temp, SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Sampling_Temp_Tide.Stats

## plot

Plot.Sampling_Temp_Tide <- ggplot(Gly2, aes(x = SH_Temp, y = Glycogen, color = Sampling)) +
  facet_wrap(~SH_Tide) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Sampling_Temp_Tide

BarPlot.Gly <- ggplot(Gly2, aes(x = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")), y = Glycogen, fill = SH_Temp)) +
  facet_grid(Sampling~SH_Tide) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Sampling_Temp_Tide

#### Site, SH_Temp, SH_Tide ====

Site_Temp_Tide.Stats <- Gly2 %>%
  group_by(Site, SH_Temp, SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Site_Temp_Tide.Stats

## plot

Plot.Site_Temp_Tide <- ggplot(Gly2, aes(x = SH_Temp, y = Glycogen, color = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")))) +
  facet_wrap(~SH_Tide) +
  geom_boxplot() +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "SH_Temp", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.Site_Temp_Tide



#### All Grouping: Sampling, Site, SH_Temp, SH_Tide ====

Stats_Samp_Site_Temp_Tide <- Gly2 %>%
  group_by(Sampling, Site, SH_Temp, SH_Tide) %>% 
  summarize(
    MeanGly = mean(Glycogen),
    SD_Gly = sd(Glycogen),
    SE_Gly = SD_Gly/sqrt(n()))

Stats_Samp_Site_Temp_Tide

#write_csv(Stats_Samp_Site_Temp_Tide, "data/Glycogen/2022/Gly_SumStats.csv")

## All Stats Plot

Plot.AllStats <- ggplot(Gly2, aes(x = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")), y = Glycogen, color = Sampling)) +
  geom_boxplot() +
  facet_grid(SH_Temp ~ SH_Tide) +
  theme_classic() +
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
       x = "Site", 
       y = "Glycogen (umol glycosyl units/g protein)")

Plot.AllStats


#### Glycogen Summary Stats Bar Plot ===========
library(ggdark)

GlyMean <- read_csv("data/Glycogen/2022/Gly_SumStats.csv")
glimpse(GlyMean)
GlyMean

GlyMean$SH_Temp <- as.character(GlyMean$SH_Temp)
GlyMean$Sampling <- as.character(GlyMean$Sampling)

### Glycogen: Plot ======

BarPlot.Gly <- ggplot(GlyMean, aes(x = factor(Site, c("HIOC - North", "BBOC - Middle", "TBOC - South")), y = MeanGly, fill = SH_Temp, group = SH_Temp)) + #, fill = SH_Temp, group = SH_Temp)) +
  facet_grid(factor(Sampling, c("June", "August")) ~ SH_Tide) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = MeanGly-SE_Gly, ymax = MeanGly + SE_Gly), width=.2, position=position_dodge(.9), color = "black") +
  ylim(0,22)+
  theme_classic() 
               
  #scale_fill_manual(values=c("#4575B4", "#FDAE61")) +
  #labs(title=expression(paste("Glycogen Content ", italic("M. gigas"))), 
  #     x = "Site", 
  #     y = "Glycogen (umol glycosyl units/g protein)")

BarPlot.Gly



#### ** PPT: Glycogen 2022 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
GlyPlot_fig <- read_pptx()
GlyPlot_fig <- add_slide(GlyPlot_fig , layout = "Title and Content", master = "Office Theme")
GlyPlot_fig <-  ph_with(x = GlyPlot_fig, value = BarPlot.Gly, location = ph_location_fullsize() )
GlyPlot_fig  <- ph_with(x = GlyPlot_fig, "Plot", location = ph_location_type(type = "title") )
print(GlyPlot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = BarPlot.Gly) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

############################################



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

