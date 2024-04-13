#### ~ M. GIGAS 2020 qPCR DATA ~ =====

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

#### 2020 Cohort Stats by Year ===== 
OsHV1 %>%
  filter(Cohort == "2020") %>% 
  group_by(Year, Site) %>%
  summarize(Mean_Copies = mean(log_transform),
            SD_Copies = sd(log_transform),
            SE_Copies = SD_Copies/sqrt(n()))

#### 2020 Cohort Stats by Year ===== 
OsHV1 %>%
  group_by(Cohort, Year, Site) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min = min(Copies_per_mgTissue),
            max = max(Copies_per_mgTissue))

OsHV1 %>%
filter(Cohort == "2020" ) %>% 
ggplot(aes(x = Site, y = log_transform, color = Site)) +
  geom_boxplot() +
  geom_jitter(size = 5, alpha = 0.5) +
  facet_wrap(~Year) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  dark_theme_classic()


#### 2020 v 2022 Cohort Stats by Year =====
BarPlot.OsHV1_dark <- OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort) %>%
  ggplot(aes(x = Cohort, y = Copies_per_mgTissue, fill = Cohort)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  dark_theme_classic()

BarPlot.OsHV1_dark

LogBarPlot.OsHV1 <- OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort) %>%
  ggplot(aes(x = Cohort, y = log_transform, fill = Cohort)) +
  geom_boxplot() +
 # scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic()

LogBarPlot.OsHV1

BarPlot.OsHV1 <- OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort) %>%
  ggplot(aes(x = Cohort, y = Copies_per_mgTissue, fill = Cohort)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
  #coord_flip() +
  theme_classic()

BarPlot.OsHV1

#### Mean, SD, SE for 2020 v 2022 comparison ==========

OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

#### Log-transform Mean, SD, SE for 2020 v 2022 comparison ==========

OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort) %>%
  summarize(Mean_Copies = mean(log_transform),
            SD_Copies = sd(log_transform),
            SE_Copies = SD_Copies/sqrt(n()))


OsHV1_20_22.plot <- ggplot(OsHV1_20_22.plotdata, aes(x = Cohort, y = Mean_Copies)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean_Copies-SE_Copies, ymax = Mean_Copies + SE_Copies), width=.1, position=position_dodge(.9)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  dark_theme_classic()
  
OsHV1_20_22.plot
  
  
#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
BarPlot.OsHV1_fig <- read_pptx()
BarPlot.OsHV1_fig <- add_slide(BarPlot.OsHV1_fig , layout = "Title and Content", master = "Office Theme")
BarPlot.OsHV1_fig <-  ph_with(x = BarPlot.OsHV1_fig, value = BarPlot.OsHV1, location = ph_location_fullsize() )
BarPlot.OsHV1_fig  <- ph_with(x = BarPlot.OsHV1_fig, "Plot", location = ph_location_type(type = "title") )
print(BarPlot.OsHV1_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = BarPlot.OsHV1) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################
