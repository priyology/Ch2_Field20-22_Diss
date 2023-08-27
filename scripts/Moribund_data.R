######### Moribund Data == 2021 & 2022 ==========

#### Generate CSV: Moribund 2021 =====

Morts2021 <- read_csv("data/Mortality/2021/Mortality2021.csv")
glimpse(Morts2021)
summary(Morts2021)
tail(Morts2021)
View(Morts2021)

Morts2021$Temp_Hardening <- as.character(Morts2021$Temp_Hardening)
is.character(Morts2021$Temp_Hardening)

Morts2021 %>% 
  group_by(Species) %>% 
  summarize(TotalMoribund = sum(Moribund_Numb))

TotalMoribund2021_perBag <- Morts2021 %>% 
  group_by(Site, Species, Temp_Hardening, Tide_Hardening, Bag_Numb) %>% 
  summarize(TotalMoribund = sum(Moribund_Numb))

print(TotalMoribund2021_perBag, n = 54)
View(TotalMoribund2021_perBag)

#####################################################

#### Generate CSV: Moribund 2022 =====

Morts2022 <- read_csv("data/Mortality/2022/Mortality2022.csv")
glimpse(Morts2022)
summary(Morts2022)
tail(Morts2022)
View(Morts2022)

Morts2022$Temp_Hardening <- as.character(Morts2022$Temp_Hardening)
is.character(Morts2022$Temp_Hardening)

Morts2022 %>% 
  summarize(TotalMoribund = sum(Moribund_Numb))

TotalMoribund2022_perBag <- Morts2022 %>% 
  group_by(Site, Species, Temp_Hardening, Tide_Hardening, Bag_Numb) %>% 
  summarize(TotalMoribund = sum(Moribund_Numb)) %>% 
  mutate(Prop_Moribund = TotalMoribund/250)

print(TotalMoribund2022_perBag, n = 36)
View(TotalMoribund2022_perBag)

write.csv(TotalMoribund2022_perBag, "data/Mortality/Moribund/Moribund_2022.csv")

#####################################################

#### Proportion Moribund =====

PropMoribund_21_22 <- read_csv("data/Mortality/Moribund/PropMoribund_21_22.csv")
glimpse(PropMoribund_21_22)
summary(PropMoribund_21_22)
tail(PropMoribund_21_22)
View(PropMoribund_21_22)

### make Year character
PropMoribund_21_22$Year <- as.character(PropMoribund_21_22$Year)
is.character(PropMoribund_21_22$Year)

#### Moribund: C. sikamea ======
Moribund.sik21 <- PropMoribund_21_22 %>% 
  filter(Species == "C_sikamea") %>% 
  group_by(Site, Temp_Hardening, Tide_Hardening) %>% 
  reframe(mean = mean(Prop_Moribund),
          SD = sd(Prop_Moribund),
          SE = SD/sqrt(n()))

Moribund.sik21

Moribund.sik21_Plot <- Moribund.sik21 %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = mean, group = factor(Temp_Hardening, c("Low", "High")), fill = Temp_Hardening)) +
  facet_wrap(~Tide_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9"),
                    guide = "none") +
  theme_classic()

Moribund.sik21_Plot

#### Moribund: M. gigas 2021 ======
Moribund.gigas21 <- PropMoribund_21_22 %>% 
  filter(Species == "M_gigas",
         Year == "2021") %>% 
  group_by(Site, Temp_Hardening, Tide_Hardening) %>% 
  reframe(mean = mean(Prop_Moribund),
          SD = sd(Prop_Moribund),
          SE = SD/sqrt(n()))

Moribund.gigas21

Moribund.gigas21_Plot <- Moribund.gigas21 %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = mean, group = factor(Temp_Hardening, c("Low", "High")), fill = Temp_Hardening)) +
  facet_wrap(~Tide_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9"),
                    guide = "none") +
  theme_classic()

Moribund.gigas21_Plot

#### Moribund: M. gigas 2022 ======
Moribund.gigas22 <- PropMoribund_21_22 %>% 
  filter(Species == "M_gigas",
         Year == "2022") %>% 
  group_by(Site, Temp_Hardening, Tide_Hardening) %>% 
  reframe(mean = mean(Prop_Moribund),
          SD = sd(Prop_Moribund),
          SE = SD/sqrt(n()))

Moribund.gigas22

Moribund.gigas22_Plot <- Moribund.gigas22 %>% 
  ggplot(aes(x = factor(Site, c("North", "Middle", "South")), y = mean, group = factor(Temp_Hardening, c("Low", "High")), fill = Temp_Hardening)) +
  facet_wrap(~Tide_Hardening) +
  geom_bar(stat="identity", position=position_dodge()) +
  geom_errorbar(aes(ymin = mean-SE, ymax = mean+SE), width=.1, position=position_dodge(.9)) +
  scale_fill_manual(values=c("#FDAE61", "#ABD9E9"),
                    guide = "none") +
  theme_classic()

Moribund.gigas22_Plot


library(cowplot)
Moribund_grid <- plot_grid(NULL, Moribund.gigas21_Plot, Moribund.gigas22_Plot, Moribund.sik21_Plot)


#### ** PPT: Moribund 2021-2022 ==============

#### officeR directly exports the plot to your desired file into a powerpoint slide-shaped image ===== 
library(officer)
## initialize R object representing .pptx file. 
Moribund_grid_fig <- read_pptx()
Moribund_grid_fig <- add_slide(Moribund_grid_fig , layout = "Title and Content", master = "Office Theme")
Moribund_grid_fig <-  ph_with(x = Moribund_grid_fig, value = Moribund_grid, location = ph_location_fullsize() )
Moribund_grid_fig  <- ph_with(x = Moribund_grid_fig, "Plot", location = ph_location_type(type = "title") )
print(Moribund_grid_fig, target = "presentations/plot.pptx")

#### R2PPT / RDCOMClient =====

#install.packages("devtools", dependencies = TRUE)

library(RDCOMClient)
library(R2PPT)

## Step 1: Save as a temporary file
TEMP_FILE <- paste(tempfile(), ".wmf", sep="")
ggsave(TEMP_FILE, plot = Moribund_grid) # Saving the plot to the temporary file


## Step 2: Open a blank PPT slide
mkppt <- PPT.Init (method = "RDCOMClient")
mkppt <- PPT.AddBlankSlide(mkppt)

## Step 3: Export graph to PPT slide
mkppt <- PPT.AddGraphicstoSlide(mkppt, file = TEMP_FILE)

unlink(TEMP_FILE)

#########################################################