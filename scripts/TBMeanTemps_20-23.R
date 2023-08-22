########## TB Temp Data: March 30, 2023 ==============

##### Water Data ============

### load libraries
library(tidyverse)
library(extrafont)

#### North: Tom's Point (HIOC) ====
### load data sheet
TBTemps <- read_csv("data/Temperature/TBMeanTemps_20-23.csv")
glimpse(TBTemps)
summary(TBTemps)
tail(TBTemps)
View(TBTemps)

TBTemps$Year <- as.character(TBTemps$Year) ## make Year character
is.character(TBTemps$Year) ## TRUE

colSums(is.na(TBTemps))

TBTemps <- TBTemps %>% 
  filter(!is.na(Year))

TBTemps

#### Plots =======

library("RColorBrewer")
brewer.pal(11, "Spectral")
#[1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5"
#[10] "#3288BD" "#5E4FA2"

TBTemps.plot <- 
  ggplot(TBTemps, aes(x = factor(Site, c("North", "Middle", "South")), y = Mean, shape = Year)) +
  geom_point(stat ="identity", pch = 21, size = 6, aes(fill = Site), color = "black") + 
  geom_errorbar(aes(ymin = Mean-SE_Min, ymax = Mean+SE_Max), width = 0.5) + 
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  scale_y_continuous(
    limits = c(16,21),
    breaks = c(16, 17, 18, 19, 20, 21)) + 
  theme_classic() +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                     guide = "none") +
  theme(text = element_text(family = "Georgia"),
        legend.position = "top") +
  labs(title = expression(paste("Mean Temperature in Tomales Bay: 2020-2022")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Site", 
       y = "Temperature (°C)")

TBTemps.plot

## made long on purpose!
ggsave(filename = "fig_output/Plot_MeanTBtemps.png", width = 5.10, height = 5.77, dpi = 300)


### dark plot
library(ggdark)

TBTemps.DARKplot <- 
  ggplot(TBTemps, aes(x = factor(Site, c("South", "Middle", "North")), y = Mean, color = Site, shape = Year)) +
  geom_point(stat ="identity", size = 6) + 
  geom_errorbar(aes(ymin = Mean-SE_Min, ymax = Mean+SE_Max), width = 0.5) + 
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  scale_y_continuous(
    limits = c(16,21),
    breaks = c(16, 17, 18, 19, 20, 21)) + 
  coord_flip() +
  dark_theme_classic() +
  scale_color_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                     guide = "none") +
  theme(legend.position="top") +
  labs(title = expression(paste("Mean Temperature in Tomales Bay: 2020-2022")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Site", 
       y = "Temperature (°C)")

TBTemps.DARKplot

## made long on purpose!
ggsave(filename = "fig_output/DARKPlot_MeanTBtemps.png", width = 5.10, height = 5.77, dpi = 300)

#### ** PPT: Mean TB Temps Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)

## initialize R object representing .pptx file. 
TBTemps.plot_fig <- read_pptx()
TBTemps.plot_fig <- add_slide(TBTemps.plot_fig , layout = "Title and Content", master = "Office Theme")
TBTemps.plot_fig <-  ph_with(x = TBTemps.plot_fig, value = TBTemps.plot, location = ph_location_fullsize() )
TBTemps.plot_fig  <- ph_with(x = TBTemps.plot_fig , "Experiment Mortality", location = ph_location_type(type = "title") )
print(TBTemps.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = TBTemps.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

## once in PPT, right-click on image and select "edit picture" to futz with individual components of the graph


#### Plot: Hours >= 16C / 18C / 21C =======

### load data sheet
PropThreshold <- read_csv("data/Temperature/TBProp16__20-22.csv")
View(PropThreshold)

PropThreshold$Year <- as.character(PropThreshold$Year) ## make Year character
is.character(PropThreshold$Year) ## TRUE

PropThreshold$Temp <- as.character(PropThreshold$Temp) ## make Temp character
is.character(PropThreshold$Temp) ## TRUE

colSums(is.na(PropThreshold))

#### Plot: Hours >= 16C =======

Hours16 <- PropThreshold %>% 
  filter(!is.na(Year),
         Temp == "16")

Hours16
View(Hours16)


library("RColorBrewer")
brewer.pal(11, "Spectral")
#[1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5"
#[10] "#3288BD" "#5E4FA2"

Hours16.plot <- 
  ggplot(Hours16, aes(x = factor(Site, c("North", "Middle", "South")), y = Proportion, fill = Site, group = factor(Year, c("2020", "2021", "2022")))) +
  geom_bar(stat="identity", position = position_dodge()) +
  ylim(0, 1) + 
  theme_classic() +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                     guide = "none") +
  theme(text = element_text(family = "Georgia"),
        legend.position = "top") +
  labs(title = expression(paste("Proportion of time spent at/above 16°C")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Site", 
       y = "Proportion of time")

Hours16.plot

## made long on purpose!
ggsave(filename = "fig_output/Plot_prop16.png", width = 5.10, height = 5.77, dpi = 300)


#### Plot: Hours >= 18C =======

Hours18 <- PropThreshold %>% 
  filter(!is.na(Year),
         Temp == "18")

Hours18
View(Hours18)

library("RColorBrewer")
brewer.pal(11, "Spectral")
#[1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5"
#[10] "#3288BD" "#5E4FA2"

Hours18.plot <- 
  ggplot(Hours18, aes(x = factor(Site, c("North", "Middle", "South")), y = Proportion, fill = Site, group = factor(Year, c("2020", "2021", "2022")))) +
  geom_bar(stat="identity", position = position_dodge()) +
  ylim(0, 1)+
  theme_classic() +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                    guide = "none") +
  theme(text = element_text(family = "Georgia"),
        legend.position = "top") +
  labs(title = expression(paste("Proportion of time spent at/above 18°C")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Site", 
       y = "Proportion of time")

Hours18.plot

## made long on purpose!
ggsave(filename = "fig_output/Plot_prop18.png", width = 5.10, height = 5.77, dpi = 300)


#### Plot: Hours >= 21C =======

Hours21 <- PropThreshold %>% 
  filter(!is.na(Year),
         Temp == "21")

Hours21
View(Hours21)

library("RColorBrewer")
brewer.pal(11, "Spectral")
#[1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5"
#[10] "#3288BD" "#5E4FA2"

Hours21.plot <- 
  ggplot(Hours21, aes(x = factor(Site, c("North", "Middle", "South")), y = Proportion, fill = Site, group = factor(Year, c("2020", "2021", "2022")))) +
  geom_bar(stat="identity", position = position_dodge()) +
  ylim(0, 1) + 
  theme_classic() +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                    guide = "none") +
  theme(text = element_text(family = "Georgia"),
        legend.position = "top") +
  labs(title = expression(paste("Proportion of time spent at/above 21°C")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Site", 
       y = "Proportion of time")

Hours21.plot


## made long on purpose!
ggsave(filename = "fig_output/Plot_prop21.png", width = 5.10, height = 5.77, dpi = 300)



#### PropThreshold Plot Grid =====
library(cowplot)
PropThresholdPlot <- plot_grid(Hours16.plot, Hours18.plot, Hours21.plot, labels = c('A', 'B', 'C'), label_size = 12, nrow = 1)

PropThresholdPlot


#### ** PPT: Prop >-16C/18C/21C Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Hours16.plot_fig <- read_pptx()
Hours16.plot_fig <- add_slide(Hours16.plot_fig , layout = "Title and Content", master = "Office Theme")
Hours16.plot_fig <-  ph_with(x = Hours16.plot_fig, value = PropThresholdPlot, location = ph_location_fullsize() )
Hours16.plot_fig  <- ph_with(x = Hours16.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Hours16.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = PropThresholdPlot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

########################

#### TB Temps 2020 =====
### load data sheet
TBTemps2020 <- read_csv("data/Temperature/2020/Summer2020Temps_Hourly.csv")
glimpse(TBTemps2020)
summary(TBTemps2020)
tail(TBTemps2020)
View(TBTemps2020)

TBTemps2020$Time_Pt <- as.character(TBTemps2020$Time_Pt) ## make Time_Pt character
is.character(TBTemps2020$Time_Pt) ## TRUE

TBTemps2020_summary <- TBTemps2020 %>% 
  group_by(Site, Time_Pt) %>% 
  reframe(mean = mean(WaterTemp_C),
          SD = sd(WaterTemp_C),
          SE = SD/sqrt(n()))

TBTemps2020_summary

#### Plots =======

library("RColorBrewer")
brewer.pal(11, "Spectral")
#[1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5"
#[10] "#3288BD" "#5E4FA2"

### Plot
library(extrafont)
font_import()
loadfonts(device = "win")

TBTemps2020.plot <- ggplot(TBTemps2020_summary, aes(x = Time_Pt, y = mean, color = Site, group = Site)) +
  geom_point(stat ="identity", size = 4) + 
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE), width = 0.4) + 
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  scale_y_continuous(
    limits = c(15,22),
    breaks = c(15, 16, 17, 18, 19, 20, 21, 22)) + 
  theme_classic() +
  scale_color_manual(values=c("#ABD9E9", "#D53E4F"), #"#FDAE61"
                     guide = "none") +
  theme(text = element_text(family = "Georgia"),
        legend.position = "top") +
  labs(title = expression(paste("2020")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Sampling Time", 
       y = "Temperature (°C)")

TBTemps2020.plot




#### TB Temps 2021 =====
### load data sheet
TBTemps2021 <- read_csv("data/Temperature/2021/Temps_2021_TimePt.csv")
glimpse(TBTemps2021)
summary(TBTemps2021)
tail(TBTemps2021)
View(TBTemps2021)

TBTemps2021$time_pt <- as.character(TBTemps2021$time_pt) ## make time_pt character
is.character(TBTemps2021$time_pt) ## TRUE

TBTemps2021_summary <- TBTemps2021 %>%
  filter(Sensor == "Water") %>% 
  group_by(Site, time_pt) %>% 
  reframe(mean = mean(Temp_C),
          SD = sd(Temp_C),
          SE = SD/sqrt(n()))

TBTemps2021_summary

#### Plots =======

library("RColorBrewer")
brewer.pal(11, "Spectral")
#[1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5"
#[10] "#3288BD" "#5E4FA2"

### Plot
library(extrafont)
font_import()
loadfonts(device = "win")

TBTemps2021.plot <- ggplot(TBTemps2021_summary, aes(x = time_pt, y = mean, color = Site, group = Site)) +
  geom_point(stat ="identity", size = 4) + 
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE), width = 0.4) + 
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  scale_y_continuous(
    limits = c(15,22),
    breaks = c(15, 16, 17, 18, 19, 20, 21, 22)) + 
  theme_classic() +
  scale_color_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"), 
                     guide = "none") +
  theme(text = element_text(family = "Georgia"),
        legend.position = "top") +
  labs(title = expression(paste("2021")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Sampling Time", 
       y = "Temperature (°C)")

TBTemps2021.plot


#### TB Temps 2022 =====
### load data sheet
TBTemps2022 <- read_csv("data/Temperature/2022/Temps_2022_TimePt.csv")
glimpse(TBTemps2022)
summary(TBTemps2022)
tail(TBTemps2022)
View(TBTemps2022)

TBTemps2022$time_pt <- as.character(TBTemps2022$time_pt) ## make time_pt character
is.character(TBTemps2022$time_pt) ## TRUE

TBTemps2022_summary <- TBTemps2022 %>%
  filter(Sensor == "Water") %>% 
  group_by(Site, time_pt) %>% 
  reframe(mean = mean(Temp_C),
          SD = sd(Temp_C),
          SE = SD/sqrt(n()))

TBTemps2022_summary

#### Plots =======

library("RColorBrewer")
brewer.pal(11, "Spectral")
#[1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5"
#[10] "#3288BD" "#5E4FA2"

### Plot
library(extrafont)
font_import()
loadfonts(device = "win")

TBTemps2022.plot <- ggplot(TBTemps2022_summary, aes(x = time_pt, y = mean, color = Site, group = Site)) +
  geom_point(stat ="identity", size = 4) + 
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE), width = 0.4) + 
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  scale_y_continuous(
    limits = c(15,22),
    breaks = c(15, 16, 17, 18, 19, 20, 21, 22)) + 
  theme_classic() +
  scale_color_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"), 
                     guide = "none") +
  theme(text = element_text(family = "Georgia"),
        legend.position = "top") +
  labs(title = expression(paste("2022")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Sampling Time", 
       y = "Temperature (°C)")

TBTemps2022.plot



#### Temps Plot Grid ====
library(cowplot)
TBTempsPlot <- plot_grid(TBTemps2020.plot, TBTemps2021.plot, TBTemps2022.plot, labels = c('A', 'B', 'C'), label_size = 12, nrow = 1)

TBTempsPlot

#### ** PPT: 2020 - Mean TB Temps Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)

## initialize R object representing .pptx file. 
TBTemps.plot_fig <- read_pptx()
TBTemps.plot_fig <- add_slide(TBTemps.plot_fig , layout = "Title and Content", master = "Office Theme")
TBTemps.plot_fig <-  ph_with(x = TBTemps.plot_fig, value = TBTempsPlot, location = ph_location_fullsize() )
TBTemps.plot_fig  <- ph_with(x = TBTemps.plot_fig , "Experiment Mortality", location = ph_location_type(type = "title") )
print(TBTemps.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = TBTempsPlot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

## once in PPT, right-click on image and select "edit picture" to futz with individual components of the graph

