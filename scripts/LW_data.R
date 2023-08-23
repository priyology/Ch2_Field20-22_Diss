########## LW_2020 - 2022 ==============

### load libraries
library(tidyverse)

#### *** 2020: L/W *** ====
### load data sheet
LW2020_og <- read_csv("data/Growth/2020/LW_2020_T1_T5.csv")
LW2020 <- LW2020_og %>%
  pivot_wider(names_from = L_W, values_from = Size_cm)

glimpse(LW2020)
summary(LW2020)
tail(LW2020)
View(LW2020)

## clean data
#### now remove NAs from data sheet
colSums(is.na(LW2020)) ## Date: No NAs

### order Site
LW2020$Site <-factor(LW2020$Site, c("North", "Middle", "South"))

### Sampling_Time as character
LW2020$Sampling_Time <- as.character(LW2020$Sampling_Time)

is.character(LW2020$Sampling_Time)

write_csv(LW2020, file = "data/Growth/2020/LW2020.csv")

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.LW2020 <- LW2020 %>%
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

AllStats.LW2020

#### By Sampling_Time ====

SamplingStats.LW2020 <- LW2020 %>%
  group_by(Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SamplingStats.LW2020

## plot
ggplot(LW2020, aes(x = Sampling_Time, y = L)) +
  geom_boxplot()

#### By Site====

SiteStats.LW2020 <- LW2020 %>%
  filter(Sampling_Time == "5") %>% 
  group_by(Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SiteStats.LW2020

## plot
Plot.2020.L <- SiteStats.LW2020 %>% 
  ggplot(aes(x = factor(Site, c("North", "South")), y = Mean_Length, fill = Site)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = Mean_Length - SE_Length, ymax = Mean_Length + SE_Length), width=.1, position=position_dodge(.9), color = "black") +
  scale_fill_manual(values=c("#ABD9E9", "#D53E4F")) + #,
  # guide = "none") +
  ylim(0,6) +
  theme_classic()

Plot.2020.L

#### ** PPT: M. gigas Shell Lengths 2020 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.2020.L_fig <- read_pptx()
Plot.2020.L_fig <- add_slide(Plot.2020.L_fig , layout = "Title and Content", master = "Office Theme")
Plot.2020.L_fig <-  ph_with(x = Plot.2020.L_fig, value = Plot.2020.L, location = ph_location_fullsize() )
Plot.2020.L_fig  <- ph_with(x = Plot.2020.L_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.2020.L_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.2020.L) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################



#### By Bag by Sampling_Time ====

BagStats.LW2020 <- LW2020 %>%
  group_by(Sampling_Time,Bag) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

BagStats.LW2020

## plot
ggplot(LW2020, aes(x = Site, y = L, color = Site)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()


#### *** 2021: L/W *** ====
### load data sheet
LW2021_og <- read_csv("data/Growth/2021/LW_2021_T1_T4.csv")
glimpse(LW2021_og)

## clean data
### order Site
LW2021_og$Site <-factor(LW2021_og$Site, c("North", "Middle", "South"))

### Sampling_Time as character
LW2021_og$Sampling_Time <- as.character(LW2021_og$Sampling_Time)
is.character(LW2021_og$Sampling_Time)

### SH_Temp as character
LW2021_og$SH_Temp <- as.character(LW2021_og$SH_Temp)
is.character(LW2021_og$SH_Temp)

#### now remove NAs from data sheet
colSums(is.na(LW2021_og)) ## Size_cm: 6 NAs, Sampling_Time: 3 NAs

LW2021_og.na <- LW2021_og %>% 
  filter(!is.na(Sampling_Time), 
         !is.na(Size_cm)) # omit all NAs
    
colSums(is.na(LW2021_og.na))

LW2021 <- LW2021_og.na %>%
  pivot_wider(names_from = L_W, values_from = Size_cm)

#### *** 2021: M. gigas *** ====
LW2021.gigas <- LW2021 %>% 
  filter(Species == "M. gigas")

### filter out NA
colSums(is.na(LW2021.gigas)) ## Size_cm: 2 NAs in L

LW2021.gigas.na <- LW2021.gigas %>% 
  filter(!is.na(L)) # omit all NAs

colSums(is.na(LW2021.gigas.na)) 

write_csv(LW2021.gigas.na, file = "data/Growth/2021/LW2021_gigas.csv")

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.2021gigas <- LW2021.gigas.na %>%
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

AllStats.2021gigas 

#### By SH_Temp ====

glimpse(LW2021.gigas.na)

SHTemp_Stats.2021gigas <- LW2021.gigas.na %>%
  filter(Sampling_Time == "4") %>% 
  group_by(Site, SH_Temp) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTemp_Stats.2021gigas 

## plot

Plot.L.gigas21 <- SHTemp_Stats.2021gigas %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = Mean_Length, group = SH_Temp, fill = SH_Temp)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = Mean_Length - SE_Length, ymax = Mean_Length + SE_Length), width=.1, position=position_dodge(.9), color = "black") +
  scale_fill_manual(values=c("#ABD9E9", "#FDAE61")) + #,
  # guide = "none") +
  ylim(0,6) +
  theme_classic()

Plot.L.gigas21

#### ** PPT: M. gigas lengths 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.L.gigas21_fig <- read_pptx()
Plot.L.gigas21_fig <- add_slide(Plot.L.gigas21_fig , layout = "Title and Content", master = "Office Theme")
Plot.L.gigas21_fig <-  ph_with(x = Plot.L.gigas21_fig, value = Plot.L.gigas21, location = ph_location_fullsize() )
Plot.L.gigas21_fig  <- ph_with(x = Plot.L.gigas21_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.L.gigas21_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.L.gigas21) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################

#### By Sampling_Time ====

SamplingStats.2021gigas <- LW2021.gigas.na %>%
  group_by(Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SamplingStats.2021gigas 




#### By Site ====

SiteStats.2021gigas <- LW2021.gigas.na %>%
  group_by(Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SiteStats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = Site, y = L, color = Site)) +
  geom_boxplot()

#### By SH_Temp + Site ====

SHTempSite_Stats.2021gigas <- LW2021.gigas.na %>%
  group_by(SH_Temp, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSite_Stats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  facet_wrap(~Site) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time ====

SiteSampling_Stats.2021gigas <- LW2021.gigas.na %>%
  group_by(Sampling_Time, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SiteSampling_Stats.2021gigas
## plot
ggplot(LW2021.gigas.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time + Site ====

SHTempSamplingSite_Stats.2021gigas <- LW2021.gigas.na %>%
  group_by(SH_Temp, Sampling_Time, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSamplingSite_Stats.2021gigas 

## plot
ggplot(LW2021.gigas.na, aes(x = Site, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()

#### *** 2021: C. sikamea *** ====
LW2021.sikamea <- LW2021 %>% 
  filter(Species == "C. sikamea")

### filter out NAs
colSums(is.na(LW2021.sikamea))

LW2021.sikamea.na <- LW2021.sikamea %>% 
  filter(!is.na(L), 
         !is.na(W)) # omit all NAs

colSums(is.na(LW2021.sikamea.na))

write_csv(LW2021.sikamea.na, file = "data/Growth/2021/LW2021_sikamea.csv")

#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.2021sikamea <- LW2021.sikamea.na %>%
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

AllStats.2021sikamea 

#### By SH_Temp ====

glimpse(LW2021.sikamea.na)

SHTemp_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(Site, SH_Temp, SH_Tide) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTemp_Stats.2021sikamea 

SampTime_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SampTime_Stats.2021sikamea 

## plot

Plot.2021.sik <- SHTemp_Stats.2021sikamea %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = Mean_Length, group = SH_Temp, fill = SH_Temp)) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = Mean_Length - SE_Length, ymax = Mean_Length + SE_Length), width=.1, position=position_dodge(.9), color = "black") +
  facet_wrap(~SH_Tide) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9", "#D53E4F")) + #,
  # guide = "none") +
  ylim(0,6) +
  theme_classic()

Plot.2021.sik

#### ** PPT: C. sikamea Shell Lengths 2021 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.2021.sik_fig <- read_pptx()
Plot.2021.sik_fig <- add_slide(Plot.2021.sik_fig , layout = "Title and Content", master = "Office Theme")
Plot.2021.sik_fig <-  ph_with(x = Plot.2021.sik_fig, value = Plot.2021.sik, location = ph_location_fullsize() )
Plot.2021.sik_fig  <- ph_with(x = Plot.2021.sik_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.2021.sik_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.2021.sik) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################

#### By SH_Tide ====

SHTide_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Tide) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTide_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = SH_Tide, y = L, color = SH_Tide)) +
  geom_boxplot()

#### By Sampling_Time ====

SamplingTime_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SamplingTime_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = Sampling_Time, y = L, color = Sampling_Time)) +
  geom_boxplot()

#### By SH_Temp + Site ====

SHTempSite_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Temp, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSite_Stats.2021sikamea

## plot
ggplot(LW2021.sikamea.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  facet_wrap(~Site) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time ====

SHTempSampling_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Temp, Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSampling_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = SH_Temp, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time + Site ====

SHTempSamplingSite_Stats.2021sikamea <- LW2021.sikamea.na %>%
  group_by(SH_Temp, Sampling_Time, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSamplingSite_Stats.2021sikamea 

## plot
ggplot(LW2021.sikamea.na, aes(x = Site, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()


#### *** 2022: L/W *** ====
### load data sheet
LW2022_og <- read_csv("data/Growth/2022/LW_2022_T1_T6.csv")
glimpse(LW2022_og)

## clean data

### order Site
LW2022_og$Site <-factor(LW2022_og$Site, c("North", "Middle", "South"))

### Sampling_Time as character
LW2022_og$Sampling_Time <- as.character(LW2022_og$Sampling_Time)
is.character(LW2022_og$Sampling_Time)

### SH_Temp as character
LW2022_og$SH_Temp <- as.character(LW2022_og$SH_Temp)
is.character(LW2022_og$SH_Temp)

#### now remove NAs from data sheet
colSums(is.na(LW2022_og)) ## Size_cm: 2 NAs

LW2022_og.na <- LW2022_og %>% 
  filter(!is.na(Size_cm)) # omit all NAs

colSums(is.na(LW2022_og.na))

## to resolve "Values from `Size_cm` 
# are not uniquely identified; output will contain list-cols.
LW2022.col <- LW2022_og.na %>%
  group_by(L_W) %>% 
  mutate(row = row_number()) %>% 
  pivot_wider(names_from = L_W, values_from = Size_cm) %>% 
  select(-row)

glimpse(LW2022.col)

### remove NAs
colSums(is.na(LW2022.col))

LW2022 <- LW2022.col%>% 
  filter(!is.na(L),
         !is.na(W)) # omit all NAs

colSums(is.na(LW2022))

write_csv(LW2022, file = "data/Growth/2022/LW2022.csv")



#### Figures, Mean, SD, SE =====

#### No Grouping ====

AllStats.2022 <- LW2022 %>%
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

AllStats.2022

#### By SH_Temp ====

SHTemp_Stats.2022 <- LW2022 %>%
  group_by(SH_Temp) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTemp_Stats.2022

## plot
ggplot(LW2022, aes(x = SH_Temp, y = L, fill = SH_Temp)) +
  geom_boxplot()

#### By SH_Tide ====

SHTide_Stats.LW2022 <- LW2022 %>%
  group_by(SH_Tide) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTide_Stats.LW2022

## plot
ggplot(LW2022, aes(x = SH_Tide, y = L, fill = SH_Tide)) +
  geom_boxplot()

#### By Sampling_Time ====

SamplingTime_Stats.LW2022 <- LW2022 %>%
  group_by(Sampling_Time) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SamplingTime_Stats.LW2022

## plot
ggplot(LW2022, aes(x = Sampling_Time, y = L, fill = Sampling_Time)) +
  geom_boxplot()

#### By Site ====

SHTempSite_Stats.LW2022 <- LW2022 %>%
  group_by(Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSite_Stats.LW2022

## plot
ggplot(LW2022, aes(x = SH_Temp, y = L, fill = SH_Temp)) +
  facet_wrap(~Site) +
  geom_boxplot()

#### By SH_Temp + Sampling_Time ====

SHTempSampling_Stats.LW2022 <- LW2022 %>%
  group_by(SH_Temp, SH_Tide, Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTempSampling_Stats.LW2022

## plot
ggplot(LW2022, aes(x = SH_Temp, y = L, fill = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()

#### By SH_Tide + Sampling_Time + Site ====

SHTideSamplingSite_Stats.LW2022 <- LW2022 %>%
  filter(Sampling_Time == "6") %>% 
  group_by(Site) %>% 
  summarize(
    Mean_Length = mean(L),
    SD_Length = sd(L),
    SE_Length = SD_Length/sqrt(n()))

SHTideSamplingSite_Stats.LW2022 

## plot
ggplot(LW2022, aes(x = Site, y = L, color = SH_Temp)) +
  facet_wrap(~Sampling_Time) +
  geom_boxplot()


glimpse(LW2022)
unique(LW2022$Sampling_Time)

## plot

Plot.2022.gigas <- SHTempSampling_Stats.LW2022 %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = Mean_Length, group = SH_Temp, fill = SH_Temp)) +
  facet_wrap(~SH_Tide) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = Mean_Length - SE_Length, ymax = Mean_Length + SE_Length), width=.1, position=position_dodge(.9), color = "black") +
  scale_fill_manual(values=c("#ABD9E9", "#FDAE61")) + #,
  # guide = "none") +
  ylim(0,6) +
  theme_classic()

Plot.2022.gigas

#### ** PPT:M. gigas Shell Lengths 2022 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Plot.2022.gigas_fig <- read_pptx()
Plot.2022.gigas_fig <- add_slide(Plot.2022.gigas_fig , layout = "Title and Content", master = "Office Theme")
Plot.2022.gigas_fig <-  ph_with(x = Plot.2022.gigas_fig, value = Plot.2022.gigas, location = ph_location_fullsize() )
Plot.2022.gigas_fig  <- ph_with(x = Plot.2022.gigas_fig, "Plot", location = ph_location_type(type = "title") )
print(Plot.2022.gigas_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Plot.2022.gigas) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################