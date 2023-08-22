########## 2020 - 2022 Mortality ==============

### load libraries
library(tidyverse)
library(ggdark)

########## 2020 Mortality ==============
Morts2020 <- read_csv("data/Mortality/2020/Mortality2020.csv")
glimpse(Morts2020)
summary(Morts2020)
tail(Morts2020)
View(Morts2020)

TotalMort2020_perBag <- Morts2020 %>% 
  group_by(site, bag_numb, time_pt) %>% 
  summarize(TotalMort = sum(mort_numb),
            PropMort = (sum(mort_numb)/250))

TotalMort2020_perBag


PropMort2020_perSite <- TotalMort2020_perBag %>% 
  group_by(site, time_pt) %>% 
  summarize(MeanMort = mean(PropMort),
            SD_Mort = sd(PropMort),
            SE_Mort = SD_Mort/sqrt(n()),
            min_Mort = min(PropMort),
            max_Mort = max(PropMort))

View(PropMort2020_perSite)

Plot.Mort2020 <-  ggplot(data = PropMort2020_perSite, aes(x = time_pt, y = MeanMort, fill = site)) +
  facet_wrap(~ site) + 
  geom_bar(stat="identity") +
  geom_errorbar(aes(ymin = MeanMort-SE_Mort, ymax = MeanMort+SE_Mort), width=.1, position=position_dodge(.9)) +
  coord_flip() +
  theme_classic()

Plot.Mort2020


#### ** PPT: SH_gigas Morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.Mort2020_fig <- read_pptx()
Plot.Mort2020_fig <- add_slide(Plot.Mort2020_fig , layout = "Title and Content", master = "Office Theme")
Plot.Mort2020_fig <-  ph_with(x = Plot.Mort2020_fig, value = Plot.Mort2020, location = ph_location_fullsize() )
Plot.Mort2020_fig  <- ph_with(x = Plot.Mort2020_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.Mort2020_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.Mort2020) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################

######## 2021 & 2022 Mortality =========
PropMorts21.22 <- read_csv("data/Mortality/Proportional/PropMort_21_22.csv")
glimpse(PropMorts21.22)
summary(PropMorts21.22)
tail(PropMorts21.22)
View(PropMorts21.22)

#### SH proportional mortality: M.gigas ====

SH.PropMort.gigas <- PropMorts21.22 %>% 
  filter(Species == "M. gigas") %>% 
  group_by(Species, Year, SH_Temp, SH_Tide) %>% 
  summarize(MeanMort = mean(PropMort),
            SD_Mort = sd(PropMort),
            SE_Mort = SD_Mort/sqrt(n()),
            min_Mort = min(PropMort),
            max_Mort = max(PropMort))

SH.PropMort.gigas

## Plot

Plot.SH.PropMort.gigas <- SH.PropMort.gigas %>% 
  ggplot(aes(x = factor(SH_Temp, c("Low","High")), y = MeanMort, fill = SH_Temp, group = SH_Tide)) +
  facet_wrap(~Year) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = MeanMort-SE_Mort, ymax = MeanMort + SE_Mort), width=.1, position=position_dodge(.9), color = "black") +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F")) + #,
  # guide = "none") +
  #coord_flip() +
  theme_classic()

Plot.SH.PropMort.gigas

#########################################################


#### ** PRESENTATION - PPT: M. gigas site morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.SH.PropMort.gigas_fig <- read_pptx()
Plot.SH.PropMort.gigas_fig <- add_slide(Plot.SH.PropMort.gigas_fig , layout = "Title and Content", master = "Office Theme")
Plot.SH.PropMort.gigas_fig <-  ph_with(x = Plot.SH.PropMort.gigas_fig, value = Plot.SH.PropMort.gigas, location = ph_location_fullsize() )
Plot.SH.PropMort.gigas_fig  <- ph_with(x = Plot.SH.PropMort.gigas_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.SH.PropMort.gigas_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.SH.PropMort.gigas) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################


#### All proportional mortality: Site ====

AllPropMort.Site <- PropMorts21.22 %>% 
  group_by(Site, Species, Year) %>% 
  summarize(MeanMort = mean(PropMort),
            SD_Mort = sd(PropMort),
            SE_Mort = SD_Mort/sqrt(n()),
            min_Mort = min(PropMort),
            max_Mort = max(PropMort))

AllPropMort.Site

## Plot

Plot.AllPropMort.Site <- AllPropMort %>% 
  ggplot(aes(x = factor(Site, c("South", "Middle", "North")), y = MeanMort, fill = Site, group = Species)) +
  facet_wrap(~Year) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = MeanMort-SE_Mort, ymax = MeanMort + SE_Mort), width=.1, position=position_dodge(.9), color = "black") +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F")) + #,
  # guide = "none") +
  coord_flip() +
  theme_classic()

Plot.AllPropMort.Site

#########################################################


#### ** PRESENTATION - PPT: M. gigas site morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.AllPropMort_fig <- read_pptx()
Plot.AllPropMort_fig <- add_slide(Plot.AllPropMort_fig , layout = "Title and Content", master = "Office Theme")
Plot.AllPropMort_fig <-  ph_with(x = Plot.AllPropMort_fig, value = Plot.AllPropMort, location = ph_location_fullsize() )
Plot.AllPropMort_fig  <- ph_with(x = Plot.AllPropMort_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.AllPropMort_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.AllPropMort) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################



#### M. gigas proportional mortality =====

PropMort.Gigas21 <- PropMorts21.22 %>% 
  filter(Species == c("M. gigas"),
         Year == "2021") %>%
  group_by(Site, SH_Temp) %>% 
  summarize(MeanMort = mean(PropMort),
            SD_Mort = sd(PropMort),
            SE_Mort = SD_Mort/sqrt(n()),
            min_Mort = min(PropMort),
            max_Mort = max(PropMort))

PropMort.Gigas21

## Plot

Plot.PropMort.gigas21 <- PropMort.Gigas21 %>% 
  ggplot(aes(x = factor(Site, c("South", "Middle", "North")), y = MeanMort, fill = SH_Temp, group = SH_Temp)) +
  #facet_grid(Year~SH_Temp) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = MeanMort-SE_Mort, ymax = MeanMort + SE_Mort), width=.1, position=position_dodge(.9), color = "black") +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F")) + #,
                   # guide = "none") +
  coord_flip() +
  theme_classic()

Plot.PropMort.gigas21

#########################################################


#### ** PPT: M. gigas site morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.PropMort.gigas21_fig <- read_pptx()
Plot.PropMort.gigas21_fig <- add_slide(Plot.PropMort.gigas21_fig , layout = "Title and Content", master = "Office Theme")
Plot.PropMort.gigas21_fig <-  ph_with(x = Plot.PropMort.gigas21_fig, value = Plot.PropMort.gigas21, location = ph_location_fullsize() )
Plot.PropMort.gigas21_fig  <- ph_with(x = Plot.PropMort.gigas21_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.PropMort.gigas21_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.PropMort.gigas21) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################

PropMort.Gigas22 <- PropMorts21.22 %>% 
  filter(Species == c("M. gigas"),
         Year == "2022") %>%
  group_by(Site) %>% 
  summarize(MeanMort = mean(PropMort),
            SD_Mort = sd(PropMort),
            SE_Mort = SD_Mort/sqrt(n()),
            min_Mort = min(PropMort),
            max_Mort = max(PropMort))

PropMort.Gigas22

## Plot

Plot.PropMort.gigas22 <- PropMort.Gigas22 %>% 
  ggplot(aes(x = factor(Site, c("South", "Middle", "North")), y = MeanMort, group = SH_Temp, fill = SH_Temp)) +
  facet_wrap(~SH_Tide) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = MeanMort-SE_Mort, ymax = MeanMort + SE_Mort), width=.1, position=position_dodge(.9), color = "black") +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F")) + #,
  # guide = "none") +
  coord_flip() +
  theme_classic()

Plot.PropMort.gigas22

#### ** PPT: M. gigas site morts 2022 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.PropMort.gigas22_fig <- read_pptx()
Plot.PropMort.gigas22_fig <- add_slide(Plot.PropMort.gigas22_fig , layout = "Title and Content", master = "Office Theme")
Plot.PropMort.gigas22_fig <-  ph_with(x = Plot.PropMort.gigas22_fig, value = Plot.PropMort.gigas22, location = ph_location_fullsize() )
Plot.PropMort.gigas22_fig  <- ph_with(x = Plot.PropMort.gigas22_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.PropMort.gigas22_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.PropMort.gigas22) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################

## C. sikamea 2021
PropMort.Csik <- PropMorts21.22 %>% 
  filter(Species == "C. sikamea") %>% 
  group_by(Site) %>% 
  summarize(MeanMort = mean(PropMort),
            SD_Mort = sd(PropMort),
            SE_Mort = SD_Mort/sqrt(n()),
            min_Mort = min(PropMort),
            max_Mort = max(PropMort))

PropMort.Csik

## Plot

Plot.PropMort.Csik <- PropMort.Csik %>% 
  ggplot(aes(x = factor(Site, c("South", "Middle", "North")), y = MeanMort, fill = Site)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = MeanMort-SE_Mort, ymax = MeanMort + SE_Mort), width=.1, position=position_dodge(.9), color = "black") +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                    guide = "none") +
  coord_flip() +
  theme_classic()

Plot.PropMort.Csik

#########################################################


#### ** PPT: C. sikamea site morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.PropMort.Csik_fig <- read_pptx()
Plot.PropMort.Csik_fig <- add_slide(Plot.PropMort.Csik_fig , layout = "Title and Content", master = "Office Theme")
Plot.PropMort.Csik_fig <-  ph_with(x = Plot.PropMort.Csik_fig, value = Plot.PropMort.Csik, location = ph_location_fullsize() )
Plot.PropMort.Csik_fig  <- ph_with(x = Plot.PropMort.Csik_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.PropMort.Csik_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.PropMort.Csik) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

########## 2021 Mortality ==============

#### Cumulative Mortality 2021 =====

Morts2021 <- read_csv("data/Mortality/2021/Mortality2021.csv")
glimpse(Morts2021)
summary(Morts2021)
tail(Morts2021)
View(Morts2021)

Morts2021$Temp_Hardening <- as.character(Morts2021$Temp_Hardening)
is.character(Morts2021$Temp_Hardening)

TotalMort2021_perBag <- Morts2021 %>% 
  group_by(Site, Species, Temp_Hardening, Tide_Hardening, Bag_Numb) %>% 
  summarize(TotalMort = sum(Mortality_Total))

print(TotalMort2021_perBag, n = 54)

TotalMort2021_perSite <- Morts2021 %>% 
  group_by(Site, Species, Temp_Hardening, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_perSite

TotalMort2021_JustSite <- Morts2021 %>% 
  group_by(Species, Site) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_JustSite

TotalMort2021_perSpecies <- Morts2021 %>% 
  group_by(Species, Temp_Hardening, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_perSpecies

TotalMort2021_perSHtemp <- Morts2021 %>% 
  group_by(Species, Temp_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_perSHtemp

TotalMort2021_perSHtide <- Morts2021 %>% 
  group_by(Species, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2021_perSHtide

## Plot

SiteMorts2021.sik <- Morts2021 %>% 
  filter(Species == "C_sikamea") %>% 
  ggplot(aes(x = factor(Site, c("South", "Middle", "North")), y = Mortality_Total, fill = Site)) +
  #facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                    guide = "none") +
  coord_flip() +
  dark_theme_classic()

SiteMorts2021.sik

Morts2021 %>% 
  filter(Species == "C_sikamea") %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = Mortality_Total, fill = Site)) +
  facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  dark_theme_classic()

Morts2021 %>% 
  filter(Species == "C_gigas") %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = Mortality_Total, fill = Site)) +
  facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  dark_theme_classic()

Morts2021

SH_gigasMorts2021.Plot <- Morts2021 %>% 
  filter(Species == "C_gigas") %>% 
  group_by(Temp_Hardening, Tide_Hardening) %>% 
  summarize(SHmorts = sum(Mortality_Total)) %>% 
  ggplot(aes(x = Temp_Hardening, y = SHmorts, fill = Temp_Hardening, group = Tide_Hardening)) +
  #facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  dark_theme_classic()

SH_gigasMorts2021.Plot



#### ** PPT: SH_gigas Morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
SH_gigasMorts2021.Plot_fig <- read_pptx()
SH_gigasMorts2021.Plot_fig <- add_slide(SH_gigasMorts2021.Plot_fig , layout = "Title and Content", master = "Office Theme")
SH_gigasMorts2021.Plot_fig <-  ph_with(x = SH_gigasMorts2021.Plot_fig, value = SH_gigasMorts2021.Plot, location = ph_location_fullsize() )
SH_gigasMorts2021.Plot_fig  <- ph_with(x = SH_gigasMorts2021.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(SH_gigasMorts2021.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = SH_gigasMorts2021.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

SH_sikMorts2021.Plot <- Morts2021 %>% 
  filter(Species == "C_sikamea") %>% 
  group_by(Temp_Hardening, Tide_Hardening) %>% 
  summarize(SHmorts = sum(Mortality_Total)) %>% 
  ggplot(aes(x = Temp_Hardening, y = SHmorts, fill = Tide_Hardening, group = Tide_Hardening)) +
  #facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0,1000) +
  dark_theme_classic()

SH_sikMorts2021.Plot

#### ** PPT: SH_sik Morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
SH_sikMorts2021.Plot_fig <- read_pptx()
SH_sikMorts2021.Plot_fig <- add_slide(SH_sikMorts2021.Plot_fig , layout = "Title and Content", master = "Office Theme")
SH_sikMorts2021.Plot_fig <-  ph_with(x = SH_sikMorts2021.Plot_fig, value = SH_sikMorts2021.Plot, location = ph_location_fullsize() )
SH_sikMorts2021.Plot_fig  <- ph_with(x = SH_sikMorts2021.Plot_fig, "Plot", location = ph_location_type(type = "title") )
print(SH_sikMorts2021.Plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = SH_sikMorts2021.Plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

SiteMorts2021.gigas <- Morts2021 %>% 
  filter(Species == "C_gigas") %>% 
  ggplot(aes(x = factor(Site, c("South", "Middle", "North")), y = Mortality_Total, fill = Site)) +
  #facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                    guide = "none") +
  coord_flip() +
  dark_theme_classic()

SiteMorts2021.gigas

SiteMorts2021 <- Morts2021 %>% 
  group_by(Site, Species) %>%
  summarize(TotalMorts2021 = sum(Mortality_Total)) %>% 
  ggplot(aes(x = factor(Site, c("South", "Middle", "North")), y = TotalMorts2021, fill = Site, group = factor(Species, c("C_gigas", "C_sikamea")))) +
  #facet_grid(Species~.) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                    guide = "none") +
  coord_flip() +
  dark_theme_classic()

SiteMorts2021

#### ** PPT: Site Morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
SiteMorts2021plot_fig <- read_pptx()
SiteMorts2021plot_fig <- add_slide(SiteMorts2021plot_fig , layout = "Title and Content", master = "Office Theme")
SiteMorts2021plot_fig <-  ph_with(x = SiteMorts2021plot_fig, value = SiteMorts2021, location = ph_location_fullsize() )
SiteMorts2021plot_fig  <- ph_with(x = SiteMorts2021plot_fig, "Plot", location = ph_location_type(type = "title") )
print(SiteMorts2021plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = SiteMorts2021) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

########## 2022 Mortality ==============

Morts2022 <- read_csv("data/Mortality/2022/Mortality2022.csv")
glimpse(Morts2022)
summary(Morts2022)
tail(Morts2022)
View(Morts2022)

Morts2022$Temp_Hardening <- as.character(Morts2022$Temp_Hardening)
is.character(Morts2022$Temp_Hardening)

PropMorts2022 <- read_csv("data/Mortality/2022/Mortality2022.csv")
glimpse(PropMorts2022)
summary(PropMorts2022)
tail(PropMorts2022)
View(PropMorts2022)

PropMort2022 <- PropMorts2022 %>% 
  group_by(Site, Sampling_Interval, Temp_Hardening, Tide_Hardening) %>% 
  summarize(MeanMort = mean(PropDead),
            SD_Mort = sd(PropDead),
            SE_Mort = SD_Mort/sqrt(n()),
            min_Mort = min(PropDead),
            max_Mort = max(PropDead))

View(PropMort2022)

TotalMort2022_perBag <- Morts2022 %>% 
  group_by(Site, Temp_Hardening, Tide_Hardening, Bag_Numb) %>% 
  summarize(TotalMort = sum(Mortality_Total))

print(TotalMort2022_perBag, n = 54)

TotalMort2022_perSite <- Morts2022 %>% 
  group_by(Site, Species, Temp_Hardening, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2022_perSite

TotalMort2022_JustSite <- Morts2022 %>% 
  group_by(Species, Site) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2022_JustSite


TotalMort2022_perSHtemp <- Morts2022 %>% 
  group_by(Species, Temp_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2022_perSHtemp

TotalMort2022_perSHtide <- Morts2022 %>% 
  group_by(Species, Tide_Hardening) %>% 
  summarize(TotalMort = sum(Mortality_Total))

TotalMort2022_perSHtide

SH_gigasMorts2022.plot <- Morts2022 %>% 
  filter(Species == "M_gigas") %>% 
  group_by(Temp_Hardening, Tide_Hardening) %>% 
  summarize(SHmorts = sum(Mortality_Total)) %>% 
  ggplot(aes(x = Temp_Hardening, y = SHmorts, fill = Tide_Hardening, group = Tide_Hardening)) +
  #facet_grid(Tide_Hardening ~ Temp_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0, 1000) +
  dark_theme_classic()

SH_gigasMorts2022.plot







#### ** PPT: SH_gigas Morts 2022 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
SH_gigasMorts2022.plot_fig <- read_pptx()
SH_gigasMorts2022.plot_fig <- add_slide(SH_gigasMorts2022.plot_fig , layout = "Title and Content", master = "Office Theme")
SH_gigasMorts2022.plot_fig <-  ph_with(x = SH_gigasMorts2022.plot_fig, value = SH_gigasMorts2022.plot, location = ph_location_fullsize() )
SH_gigasMorts2022.plot_fig  <- ph_with(x = SH_gigasMorts2022.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(SH_gigasMorts2022.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = SH_gigasMorts2022.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

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


########## Cumulative Mortality: Site ==============
SiteMorts20_22 <- read_csv("data/Mortality/Cumulative/Mort_Site_Mgigas_Csikamea.csv")
glimpse(SiteMorts20_22)
SiteMorts20_22

SiteMorts20_22.plot <- SiteMorts20_22 %>% 
  filter(Species == "M. gigas") %>% 
  ggplot(aes(x = factor(Site, c("South", "Middle", "North")), y = TotalMort, fill = Site, group = factor(Year, c("2022", "2021", "2020")))) +
  #facet_grid(Species~.) +
  geom_bar(stat="identity", position=position_dodge()) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F"),
                    guide = "none") +
  coord_flip() +
  dark_theme_classic()

SiteMorts20_22.plot


#### ** PPT: Prop Morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
SiteMorts20_22.plot_fig <- read_pptx()
SiteMorts20_22.plot_fig <- add_slide(SiteMorts20_22.plot_fig , layout = "Title and Content", master = "Office Theme")
SiteMorts20_22.plot_fig <-  ph_with(x = SiteMorts20_22.plot_fig, value = SiteMorts20_22.plot, location = ph_location_fullsize() )
SiteMorts20_22.plot_fig  <- ph_with(x = SiteMorts20_22.plot_fig, "Plot", location = ph_location_type(type = "title") )
print(SiteMorts20_22.plot_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = SiteMorts20_22.plot) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)


######################################################

######### Instantaneous Mortality: 2021 ============

PropMorts2021 <- read_csv("data/Mortality/2021/Mortality2021.csv")
glimpse(PropMorts2021)
summary(PropMorts2021)
tail(PropMorts2021)
View(PropMorts2021)

PropMort2021 <- PropMorts2021 %>% 
  group_by(Species, Site, Sampling_Interval, Temp_Hardening, Tide_Hardening) %>% 
  summarize(MeanMort = mean(Prop_Dead),
            SD_Mort = sd(Prop_Dead),
            SE_Mort = SD_Mort/sqrt(n()),
            min_Mort = min(Prop_Dead),
            max_Mort = max(Prop_Dead))

View(PropMort2021)


#### Proportional Mortality 2021 + 2022 =====
PropMort21_22 <- read_csv("data/Mortality/Proportional/21-22_PropMorts_bySH.csv")
glimpse(PropMort21_22)
summary(PropMort21_22)
tail(PropMort21_22)
View(PropMort21_22)

## make Temp character
PropMort21_22$SH_Temp <- as.character(PropMort21_22$SH_Temp)
is.character(PropMort21_22$SH_Temp)

PropMort_2021_sik <- PropMort21_22 %>% 
  filter(Species == "C. sikamea" & Year == 2021) %>% 
  ggplot(aes(x = SH_Temp, y = PropMort, fill = SH_Tide, group = SH_Tide)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0, 0.25) +
  dark_theme_classic()

PropMort_2021_sik


#### ** PPT: Prop Sikamea Morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
PropMort_2021_sik.fig <- read_pptx()
PropMort_2021_sik.fig <- add_slide(PropMort_2021_sik.fig, layout = "Title and Content", master = "Office Theme")
PropMort_2021_sik.fig <-  ph_with(x = PropMort_2021_sik.fig, value = PropMort_2021_sik, location = ph_location_fullsize() )
PropMort_2021_sik.fig  <- ph_with(x = PropMort_2021_sik.fig, "Plot", location = ph_location_type(type = "title") )
print(PropMort_2021_sik.fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = PropMort_2021_sik) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

PropMort_2021_gigas <- PropMort21_22 %>% 
  filter(Species == "M. gigas" & Year == 2021) %>% 
  ggplot(aes(x = SH_Temp, y = PropMort, fill = SH_Temp, group = SH_Tide)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0, 0.6) +
  dark_theme_classic()

PropMort_2021_gigas


#### ** PPT: Prop Gigas Morts 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
PropMort_2021_gigas_fig <- read_pptx()
PropMort_2021_gigas_fig <- add_slide(PropMort_2021_gigas_fig , layout = "Title and Content", master = "Office Theme")
PropMort_2021_gigas_fig <-  ph_with(x = PropMort_2021_gigas_fig, value = PropMort_2021_gigas, location = ph_location_fullsize() )
PropMort_2021_gigas_fig  <- ph_with(x = PropMort_2021_gigas_fig, "Plot", location = ph_location_type(type = "title") )
print(PropMort_2021_gigas_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = PropMort_2021_gigas) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################

PropMort_2022_gigas <- PropMort21_22 %>% 
  filter(Species == "M. gigas" & Year == 2022) %>% 
  ggplot(aes(x = SH_Temp, y = PropMort, fill = SH_Tide, group = SH_Tide)) +
  geom_bar(stat="identity", position=position_dodge()) +
  ylim(0, 0.25) +
  dark_theme_classic()

PropMort_2022_gigas

#### ** PPT: Prop Gigas Morts 2022 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
PropMort_2022_gigas_fig <- read_pptx()
PropMort_2022_gigas_fig <- add_slide(PropMort_2022_gigas_fig , layout = "Title and Content", master = "Office Theme")
PropMort_2022_gigas_fig <-  ph_with(x = PropMort_2022_gigas_fig, value = PropMort_2022_gigas, location = ph_location_fullsize() )
PropMort_2022_gigas_fig  <- ph_with(x = PropMort_2022_gigas_fig, "Plot", location = ph_location_type(type = "title") )
print(PropMort_2022_gigas_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = PropMort_2022_gigas) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

######################################################