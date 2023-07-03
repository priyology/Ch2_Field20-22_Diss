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

### Sampling_Period as character / factor
OsHV1$Sampling_Period <- as.character(OsHV1$Sampling_Period)
is.character(OsHV1$Sampling_Period)

OsHV1$Sampling_Period <- as.factor(OsHV1$Sampling_Period)
is.factor(OsHV1$Sampling_Period)

### Cohort as character
OsHV1$Cohort <- as.character(OsHV1$Cohort)
is.character(OsHV1$Cohort)

#### Mean, SD, SE, min, max for 2020 Cohort ===== 
BaselineStats <- OsHV1 %>%
  filter(Year == "2020") %>% 
  group_by(Site, Sampling_Period) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

BaselineStats

#### Mean, SD, SE, min, max for 2020 Cohort ===== 
BaselineStats <- OsHV1 %>%
  filter(Year == "2020") %>% 
  group_by(Site, Sampling_Period) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

BaselineStats

#### 2020 Baseline Plot ====
BaselineStats.plot <- ggplot(BaselineStats, aes(x = factor(Site, c("South", "North")), y = Mean_Copies), fill = Sampling_Period) +
  facet_wrap(~ Sampling_Period) +
  geom_bar(stat = "identity", position = position_dodge2()) +
  geom_errorbar(aes(ymin = Mean_Copies-SE_Copies, ymax = Mean_Copies + SE_Copies), width=.1, position=position_dodge2(.9)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  coord_flip() +
  theme_classic()

BaselineStats.plot

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
BaselineStats.plot_fig <- read_pptx()
BaselineStats.plot_fig <- add_slide(BaselineStats.plot_fig , layout = "Title and Content", master = "Office Theme")
BaselineStats.plot_fig <-  ph_with(x = BaselineStats.plot_fig, value = BaselineStats.plot, location = ph_location_fullsize() )
BaselineStats.plot_fig  <- ph_with(x = BaselineStats.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(BaselineStats.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = BaselineStats.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)


#### Mean, SD, SE, min, max for 2020 Cohort by Year + Site ===== 
Yr3Stats <- OsHV1 %>%
  filter(Cohort == "2020") %>%
  group_by(Sampling_Period, Site, Year) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Yr3Stats

#### 2020-2022 Plot ====
Yr3Stats.plot <- ggplot(Yr3Stats, aes(x = factor(Site, c("South", "Middle", "North")), y = Mean_Copies)) +
  facet_wrap(Year~Sampling_Period) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean_Copies-SE_Copies, ymax = Mean_Copies + SE_Copies), width=.1, position=position_dodge(.9)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  coord_flip() +
  theme_classic()

Yr3Stats.plot

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Yr3Stats.plot_fig <- read_pptx()
Yr3Stats.plot_fig <- add_slide(Yr3Stats.plot_fig , layout = "Title and Content", master = "Office Theme")
Yr3Stats.plot_fig <-  ph_with(x = Yr3Stats.plot_fig, value = Yr3Stats.plot, location = ph_location_fullsize() )
Yr3Stats.plot_fig  <- ph_with(x = Yr3Stats.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Yr3Stats.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Yr3Stats.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)