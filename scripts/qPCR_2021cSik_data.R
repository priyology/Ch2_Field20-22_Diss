#### ~ C. SIKAMEA 2021 qPCR DATA ~ =====

## load libraries
library(tidyverse)
library(scales) # to access break formatting functions for log scale
library(ggdark)

########## 2021 OsHV-1 Data ==============
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

#### Mean, SD, SE, min, max for C. sikeamea: Site, Sampling_Period ===== 
Csik_Stats8 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(Site, Sampling_Period, SH_Temp, SH_Tide) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

View(Csik_Stats8)

#### C. sikamea Plot - Site & Sampling_Period ====
Csik.plot <- ggplot(Csik_Stats8, aes(x = factor(Site, c("North", "Middle", "South")), y = Mean_Copies, fill = SH_Temp)) +
  facet_grid(Sampling_Period~SH_Tide) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_errorbar(aes(ymin = Mean_Copies-SE_Copies, ymax = Mean_Copies + SE_Copies), width=.1, position=position_dodge(.9), color = "black") +
  scale_y_log10(breaks=c(.0001, .001, .01,.1,1, 10, 100, 1000, 10000, 100000, 1000000, 10000000, 100000000, 1000000000, 10000000000, 10000000000),
                #breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic()

Csik.plot

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Csik.plot_fig <- read_pptx()
Csik.plot_fig <- add_slide(Csik.plot_fig , layout = "Title and Content", master = "Office Theme")
Csik.plot_fig <-  ph_with(x = Csik.plot_fig, value = Csik.plot, location = ph_location_fullsize() )
Csik.plot_fig  <- ph_with(x = Csik.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Csik.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Csik.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#### PRESENTATION: Mean, SD, SE, min, max for C. sikeamea: Site, Sampling_Period ===== 
Csik_Stats9 <- OsHV1 %>%
  filter(Species == "C. sikamea") %>% 
  group_by(Site) %>%
  summarize(Mean_Copies = mean(Copies_per_mgTissue),
            SD_Copies = sd(Copies_per_mgTissue),
            SE_Copies = SD_Copies/sqrt(n()),
            min_Copies = min(Copies_per_mgTissue),
            max_Copies = max(Copies_per_mgTissue))

Csik_Stats9

#### PRESENTATION: C. sikamea Plot - Site & Sampling_Period ====
PresCsik.plot <- ggplot(Csik_Stats9, aes(x = factor(Site, c("South", "Middle", "North")), y = Mean_Copies)) +
  geom_bar(stat = "identity") +
  ylim(0,100000000) +
  geom_errorbar(aes(ymin = Mean_Copies-SE_Copies, ymax = Mean_Copies + SE_Copies), width=.1, position=position_dodge(.9)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  coord_flip() +
  theme_classic()

PresCsik.plot

#### PRESENTATION: officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
PresCsik.plot_fig <- read_pptx()
PresCsik.plot_fig <- add_slide(PresCsik.plot_fig , layout = "Title and Content", master = "Office Theme")
PresCsik.plot_fig <-  ph_with(x = PresCsik.plot_fig, value = PresCsik.plot, location = ph_location_fullsize() )
PresCsik.plot_fig  <- ph_with(x = PresCsik.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(PresCsik.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = PresCsik.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)
