########## TB Temp Data: March 30, 2023 ==============

##### Water Data ============

### load libraries
library(tidyverse)

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
  ggplot(TBTemps, aes(x = factor(Site, c("South", "Middle", "North")), y = Mean, color = Site, shape = Year)) +
  geom_point(stat ="identity", size = 6) + 
  geom_errorbar(aes(ymin = Mean-SE_Min, ymax = Mean+SE_Max), width = 0.5) + 
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  scale_y_continuous(
    limits = c(16,21),
    breaks = c(16, 17, 18, 19, 20, 21)) + 
  coord_flip() +
  theme_classic() +
  scale_color_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                     guide = "none") +
  theme(legend.position="top") +
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


#### Plot: Hours >= 16C =======

### load data sheet
Hours16 <- read_csv("data/Temperature/TBProp16__20-22.csv")

Hours16$Year <- as.character(Hours16$Year) ## make Year character
is.character(Hours16$Year) ## TRUE

colSums(is.na(Hours16))

Hours16 <- Hours16 %>% 
  filter(!is.na(Year))

Hours16
View(Hours16)


library("RColorBrewer")
brewer.pal(11, "Spectral")
#[1] "#9E0142" "#D53E4F" "#F46D43" "#FDAE61" "#FEE08B" "#FFFFBF" "#E6F598" "#ABDDA4" "#66C2A5"
#[10] "#3288BD" "#5E4FA2"

Hours16.plot <- 
  ggplot(Hours16, aes(x = factor(Site, c("South", "Middle", "North")), y = Proportion, fill = Site, group = factor(Year, c("2022", "2021", "2020")))) +
  geom_bar(stat="identity", position = position_dodge()) +
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022')))
  #scale_y_continuous(
  #  limits = c(17,21),
  #  breaks = c(17, 18, 19, 20, 21)) + 
  coord_flip() +
  theme_classic() +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                     guide = "none") +
  theme(legend.position="none") +
  labs(title = expression(paste("Proportion of time spent at/above 16°C")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Site", 
       y = "Proportion of time")

Hours16.plot

## made long on purpose!
ggsave(filename = "fig_output/Plot_prop16.png", width = 5.10, height = 5.77, dpi = 300)


### dark plot
library(ggdark)

Hours16.DARKplot <- 
  ggplot(Hours16, aes(x = factor(Site, c("South", "Middle", "North")), y = Proportion, fill = Site, group = Year)) +
  geom_bar(stat="identity", position = position_dodge()) +
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  #scale_y_continuous(
  #  limits = c(17,21),
  #  breaks = c(17, 18, 19, 20, 21)) + 
  coord_flip() +
  dark_theme_classic() +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                    guide = "none") +
  theme(legend.position="none") +
  labs(title = expression(paste("Mean Temperature in Tomales Bay: 2020-2022")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Site", 
       y = "Temperature (°C)")

Hours16.DARKplot

## made long on purpose!
ggsave(filename = "fig_output/DARKPlot_prop16.png", width = 5.10, height = 5.77, dpi = 300)

#### ** PPT: Prop >-16C Plot ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Hours16.plot_fig <- read_pptx()
Hours16.plot_fig <- add_slide(Hours16.plot_fig , layout = "Title and Content", master = "Office Theme")
Hours16.plot_fig <-  ph_with(x = Hours16.plot_fig, value = Hours16.plot, location = ph_location_fullsize() )
Hours16.plot_fig  <- ph_with(x = Hours16.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(Hours16.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Hours16.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)


