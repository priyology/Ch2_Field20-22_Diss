########## 2020 Temperatures ==============

##### Water Data ============

### load libraries
library(tidyverse)

#### North: Tom's Point (HIOC) ====
### load data sheet
Temps2020 <- read_csv("data/Temperature/2020/Summer2020Temps_Hourly.csv")
glimpse(Temps2020)
summary(Temps2020)
tail(Temps2020)
View(Temps2020)

## clean data

Temps2020 <- Temps2020 %>% 
  mutate(Date_Time = mdy_hm(Date_Time)) %>%  # set date/time format
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) 

## plot Air & Water Temp
Temps2020 %>%
  ggplot(aes(date, WaterTemp_C, color = Site)) +
  geom_point()

#### Stats

Temps2020 %>% 
  group_by(Site) %>% 
  summarize(meanWater = mean(WaterTemp_C),
            SDWater = sd(WaterTemp_C),
            SEWater = SDWater/sqrt(n()),
            maxWater = max(WaterTemp_C),
            minWater = min(WaterTemp_C))

#### HIOC Temps Above 16 deg C

Temps2020 %>% 
  filter(Site == "HIOC",
    date < "2020-10-16", #October 16, 2020
         WaterTemp_C > 15.9) %>% 
  summarize(count = n())

#1704/6 # divide 10 minute intervals into hours: 1239.5 hours
1704 / 24 # 71 days / 100 days that temps were above the threshold for OsHV-1
71/100 # 71% of deployment

### HIOC Temps Above 16 deg C

Temps2020 %>% 
  filter(Site == "TBOC",
         date < "2020-10-17", #October 17, 2020
         WaterTemp_C > 15.9) %>% 
  summarize(count = n())

#2366/6 # divide 10 minute intervals into hours: 1239.5 hours
2366 / 24 # 98 days / 101 days that temps were above the threshold for OsHV-1
98.58/101 #97.6% of deployment


### code for "if/else" statement with deplyr: case_when
Temps2020 %>% 
  mutate(WaterTemp_corrected = case_when(Air_Temp_C >= Water_Temp_C ~ "High Tide", 
                                         Air_Temp_C < Water_Temp_C ~ "Low Tide"))

### does not provide accurate high/low tide. Will use NOAA tide charts:
## HIOC - TB Entrance (North): https://tidesandcurrents.noaa.gov/noaatidepredictions.html?id=9415469 
## BBOC - Blake's Landing (Middle): 
## TBOC - Reynolds (South): 


## write new CSV with high & low tide
#write_csv(Temps2020, "data/Temperature/2022/TomsPoint_2022_Tide.csv")

## hours spent at high & low tide

## mean temps at high and low tide