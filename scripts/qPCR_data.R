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
  geom_boxplot() +
  geom_jitter(size = 5, alpha = 0.5) +
  facet_wrap(~Year_Sampled) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  theme_classic()


#### 2020 v 2022 Cohort Stats by Year =====
BarPlot.OsHV1 <- OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort) %>%
  ggplot(aes(x = Cohort, y = log_transform, fill = Cohort)) +
  geom_boxplot() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  dark_theme_classic()

BarPlot.OsHV1


OsHV1_20_22.plotdata <- OsHV1 %>%
  filter(Sample_Date == "27-Aug-22") %>% 
  group_by(Cohort) %>%
  summarize(Mean_Copies = mean(log_transform),
            SD_Copies = sd(log_transform),
            SE_Copies = SD_Copies/sqrt(n()))

OsHV1_20_22.plotdata

OsHV1_20_22.plot <- ggplot(OsHV1_20_22.plotdata, aes(x = Cohort, y = Mean_Copies)) +
  geom_bar(stat = "identity") +
  geom_errorbar(aes(ymin = Mean_Copies-SE_Copies, ymax = Mean_Copies + SE_Copies), width=.1, position=position_dodge(.9)) +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  dark_theme_classic()
  
OsHV1_20_22.plot
  
  
  #### ** PPT: '20 v '22 cohort (OsHV-1) ==============
  
  #### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
  library(officer)
  ## initialize R object representing .pptx file. 
  OsHV1Plot_fig <- read_pptx()
  OsHV1Plot_fig <- add_slide(OsHV1Plot_fig, layout = "Title and Content", master = "Office Theme")
  OsHV1Plot_fig <-  ph_with(x = OsHV1Plot_fig, value = BarPlot.OsHV1, location = ph_location_fullsize() )
  OsHV1Plot_fig  <- ph_with(x = OsHV1Plot_fig, "Plot", location = ph_location_type(type = "title") )
  print(OsHV1Plot_fig, target = "presentations/plot.pptx")
  
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
  
  ############################################
  
