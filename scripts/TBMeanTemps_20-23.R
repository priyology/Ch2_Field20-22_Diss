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
  geom_point(stat="identity", size = 4) +
  geom_errorbar(aes(ymin = Mean-SE_Min, ymax = Mean+SE_Max), width = 0.1) + 
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  scale_y_continuous(
    limits = c(17,21),
    breaks = c(17, 18, 19, 20, 21)) + 
  coord_flip() +
  theme_classic() +
  scale_color_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                     guide = "none") +
  #theme(legend.position="none") +
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
  geom_point(stat="identity", size = 4) +
  geom_errorbar(aes(ymin = Mean-SE_Min, ymax = Mean+SE_Max), width = 0.1) + 
  #facet_wrap(~factor(Year, level=c('2020', '2021', '2022'))) +
  scale_y_continuous(
    limits = c(17,21),
    breaks = c(17, 18, 19, 20, 21)) + 
  coord_flip() +
  dark_theme_classic() +
  scale_color_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                     guide = "none") +
  #theme(legend.position="none") +
  labs(title = expression(paste("Mean Temperature in Tomales Bay: 2020-2022")), 
       #subtitle = "Gamma distribution: link = 'identity'",
       x= "Site", 
       y = "Temperature (°C)")

TBTemps.DARKplot

## made long on purpose!
ggsave(filename = "fig_output/DARKPlot_MeanTBtemps.png", width = 5.10, height = 5.77, dpi = 300)

