########## 2022 Temperatures ==============

### load libraries
library(tidyverse)

#### North: Tom's Point (HIOC) ====
### load data sheet
Temps_North <- read_csv("data/Temperature/2022/TomsPoint_2022.csv")
glimpse(Temps_North)
summary(Temps_North)
tail(Temps_North)
View(Temps_North)

## clean data

Temps_North <- Temps_North %>% 
  mutate(Date_Time = mdy_hm(Date_Time)) %>%  # set date/time format
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) 

## plot Air & Water Temp
Temps_North %>%
  pivot_longer(cols=c('AirTemp_C', 'WaterTemp_C'),
               names_to='Logger',
               values_to='Temp_C') %>% 
  ggplot(aes(date, Temp_C, color = Logger)) +
  geom_point()

## stats
Temps_North %>% 
  summarize(meanAir = mean(AirTemp_C),
            meanWater = mean(WaterTemp_C),
            SDAir = sd(AirTemp_C), #0.611
            SDWater = sd(WaterTemp_C), #0.611
            SEAir = SDAir/sqrt(n()), #0.0180
            SEWater = SDWater/sqrt(n()), #0.0180
            maxAir = max(AirTemp_C), #17.5
            maxWater = max(WaterTemp_C), #17.5
            minAir = min(AirTemp_C),
            minWater = min(WaterTemp_C))

# Time spent above 16 deg C
Temps_North %>% 
  filter(date < "2022-08-27", #August 27, 2022
         WaterTemp_C > 15.9) %>% 
  summarize(count = n())

8401/6 # divide 10 minute intervals into hours: 1400.167 hours
1400.167 / 24 # 58.3 days / 98 days that temps were above the threshold for OsHV-1
58.3/98 # 59.5% of deployment

### code for "if/else" statement with deplyr: case_when
Temps_North %>% 
mutate(WaterTemp_corrected = case_when(Air_Temp_C >= Water_Temp_C ~ "High Tide", 
                          Air_Temp_C < Water_Temp_C ~ "Low Tide"))

### does not provide accurate high/low tide. Will use NOAA tide charts:
## HIOC - TB Entrance (North): https://tidesandcurrents.noaa.gov/noaatidepredictions.html?id=9415469 
## BBOC - Blake's Landing (Middle): 
## TBOC - Reynolds (South): 


## write new CSV with high & low tide
#write_csv(Temps_North, "data/Temperature/2022/TomsPoint_2022_Tide.csv")

## hours spent at high & low tide

## mean temps at high and low tide


#### Middle: Hamlet (BBOC) ====

Temps_Middle <- read_csv("data/Temperature/2022/Hamlet_2022.csv")
glimpse(Temps_Middle)
summary(Temps_Middle)
tail(Temps_Middle)
View(Temps_Middle)

## clean data

Temps_Middle <- Temps_Middle %>% 
  mutate(Date_Time = mdy_hm(Date_Time)) %>%  # set date/time format
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) 

## plot Air & Water Temp
Temps_Middle %>%
  pivot_longer(cols=c('AirTemp_C', 'WaterTemp_C'),
               names_to='Logger',
               values_to='Temp_C') %>% 
  ggplot(aes(date, Temp_C, color = Logger)) +
  geom_point()


Temps_Middle %>% 
  summarize(meanAir = mean(AirTemp_C),
            meanWater = mean(WaterTemp_C),
            SDAir = sd(AirTemp_C), #0.611
            SDWater = sd(WaterTemp_C), #0.611
            SEAir = SDAir/sqrt(n()), #0.0180
            SEWater = SDWater/sqrt(n()), #0.0180
            maxAir = max(AirTemp_C), #17.5
            maxWater = max(WaterTemp_C), #17.5
            minAir = min(AirTemp_C),
            minWater = min(WaterTemp_C))

# Time spent above 16 deg C
Temps_Middle %>% 
  filter(date < "2022-08-26", #August 26, 2022
         WaterTemp_C > 15.9) %>% 
  summarize(count = n())

10984/6 # divide 10 minute intervals into hours: 1400.167 hours
1830.667 / 24 # 76.3 days / 97 days that temps were above the threshold for OsHV-1
76.3/97 # 78.6% of deployment


#### South: Tomasini Point (TBOC) ====

Temps_South <- read_csv("data/Temperature/2022/TomasiniPt_2022.csv")
glimpse(Temps_South)
summary(Temps_South)
tail(Temps_South)
View(Temps_South)

## clean data

Temps_South <- Temps_South %>% 
  mutate(Date_Time = mdy_hm(Date_Time)) %>%  # set date/time format
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) 

## plot Air & Water Temp
Temps_South %>%
  pivot_longer(cols=c('AirTemp_C', 'WaterTemp_C'),
               names_to='Logger',
               values_to='Temp_C') %>% 
  ggplot(aes(date, Temp_C, color = Logger)) +
  geom_point()

## stats
Temps_South %>% 
  summarize(meanAir = mean(AirTemp_C),
            meanWater = mean(WaterTemp_C),
            SDAir = sd(AirTemp_C), #0.611
            SDWater = sd(WaterTemp_C), #0.611
            SEAir = SDAir/sqrt(n()), #0.0180
            SEWater = SDWater/sqrt(n()), #0.0180
            maxAir = max(AirTemp_C), #17.5
            maxWater = max(WaterTemp_C), #17.5
            minAir = min(AirTemp_C),
            minWater = min(WaterTemp_C))

# Time spent above 16 deg C
Temps_South %>% 
  filter(date < "2022-08-25", #August 25, 2022
         WaterTemp_C > 15.9) %>% 
  summarize(count = n())

12373/6 # divide 10 minute intervals into hours: 1400.167 hours
2062.167 / 24 # 85.9 days / 96 days that temps were above the threshold for OsHV-1
85.9/96 # 89% of deployment


