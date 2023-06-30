#### ~ M. GIGAS 2021 + 2022 qPCR DATA ~ =====

## load libraries
library(tidyverse)
library(scales) # to access break formatting functions for log scale
library(ggdark)

########## 2021/2022 OsHV-1 Data ==============
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
Mgig_Stats1 <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None") %>% 
  group_by(factor(Site, c("North", "Middle", "South")), SH_Temp, SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Mgig_Stats1

#### Mean, SD, SE, min, max for C. sikeamea: Site, SH_Temp ===== 
Mgig_Stats2 <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None") %>% 
  group_by(factor(Site, c("North", "Middle", "South")), SH_TempLev) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Mgig_Stats2

#### Mean, SD, SE, min, max for C. sikeamea: Site, SH_Tide ===== 
Mgig_Stats3 <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None") %>% 
  group_by(factor(Site, c("North", "Middle", "South")), SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Mgig_Stats3

#### Mean, SD, SE, min, max for C. sikeamea: SH_Temp, SH_Tide ===== 
Mgig_Stats4 <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None") %>% 
  group_by(SH_TempLev, SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Mgig_Stats4


#### Mean, SD, SE, min, max for C. sikeamea: Site ===== 
Mgig_Stats5 <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None") %>% 
  group_by(factor(Site, c("North", "Middle", "South"))) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Mgig_Stats5

#### Mean, SD, SE, min, max for C. sikeamea: SH_Temp, SH_Tide ===== 
Mgig_Stats6 <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None") %>% 
  group_by(SH_TempLev) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Mgig_Stats6

#### Mean, SD, SE, min, max for C. sikeamea: SH_Temp, SH_Tide ===== 
Mgig_Stats7 <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None") %>% 
  group_by(SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Mgig_Stats7

#### Mean, SD, SE, min, max for C. sikeamea: Site, Year, Sampling_Period ===== 
Mgig_Stats8 <- OsHV1 %>%
  filter(Species == "M. gigas",
         SH_TempLev != "None",
         SH_Tide != "None") %>% 
  group_by(Year, Sampling_Period, Site) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Mgig_Stats8


#### M. gigas Plot - Site & Year/Sampling_Period ====
Mgig.plot <- ggplot(Mgig_Stats8, aes(x = factor(Site, c("South", "Middle", "North")), y = Mean_Copies)) +
  facet_wrap(Year~Sampling_Period) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean_Copies-SE_Copies, ymax = Mean_Copies + SE_Copies), width=.1, position=position_dodge(.9)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  coord_flip() +
  theme_classic()

Mgig.plot

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Mgig.plot_fig <- read_pptx()
Mgig.plot_fig <- add_slide(Mgig.plot_fig , layout = "Title and Content", master = "Office Theme")
Mgig.plot_fig <-  ph_with(x = Mgig.plot_fig, value = Mgig.plot, location = ph_location_fullsize() )
Mgig.plot_fig  <- ph_with(x = Mgig.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Mgig.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Mgig.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)