########## 2021 Temperatures ==============

##### Water Data ============

### load libraries
library(tidyverse)

#### North: Tom's Point (HIOC) ====
### load data sheet
Temps_North <- read_csv("data/Temperature/2021/Water_HIOC_North_18Oct2021.csv")
glimpse(Temps_North)
summary(Temps_North)
tail(Temps_North)
View(Temps_North)

## clean data
Temps_North <- Temps_North %>% 
  mutate(Date_Time = mdy_hm(Date_Time)) %>%  # set date/time format
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) 



## plot
Temps_North %>% 
  ggplot(aes(date, WaterTemp_C)) +
  geom_point()

## stats
Temps_North %>% 
  summarize(meanWater = mean(WaterTemp_C),
            SDWater = sd(WaterTemp_C),
            SEWater = SDWater/sqrt(n()),
            maxWater = max(WaterTemp_C),
            minWater = min(WaterTemp_C))


## Time spent above 16deg C
Temps_North %>% 
  filter(date < "2021-09-07", #September 7, 2021
         WaterTemp_C > 15.9) %>% 
  summarize(count = n())

5938/6 # divide 10 minute intervals into hours: 989.67 hours
989.67 / 24 # 41.2 days / 59 days that temps were above the threshold for OsHV-1
41.2/59 #69.8% of deployment

## Time spent above 18deg C
Temps_North %>% 
  filter(date < "2021-09-07", #September 7, 2021
         WaterTemp_C > 17.9) %>% 
  summarize(count = n())

4202/6 # divide 10 minute intervals into hours: 700.33 hours
700.33 / 24 # 29.18 days / 59 days that temps were above the threshold for OsHV-1
29.18/59 #49.4% of deployment

## Time spent above 21deg C
Temps_North %>% 
  filter(date < "2021-09-07", #September 7, 2021
         WaterTemp_C > 20.9) %>% 
  summarize(count = n())

1260/6 # divide 10 minute intervals into hours: 210 hours
210 / 24 # 8.75 days / 59 days that temps were above the threshold for OsHV-1
8.75/59 #14.8% of deployment


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

Temps_Middle <- read_csv("data/Temperature/2021/Water_BBOC_Middle_18Oct2021.csv")
glimpse(Temps_Middle)
summary(Temps_Middle)
tail(Temps_Middle)
View(Temps_Middle)

## clean data

## check for NAs
colSums(is.na(Temps_Middle)) ## 12 in WaterTemp_C

Temps_Middle <- Temps_Middle %>% 
  filter(!is.na(WaterTemp_C)) %>% 
  mutate(Date_Time = mdy_hm(Date_Time)) %>%  # set date/time format
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE)

Temps_Middle %>% 
  ggplot(aes(date, WaterTemp_C)) +
  geom_point()

Temps_Middle %>% 
  summarize(meanWater = mean(WaterTemp_C),
            SDWater = sd(WaterTemp_C),
            SEWater = SDWater/sqrt(n()),
            maxWater = max(WaterTemp_C),
            minWater = min(WaterTemp_C))

#### Temps Above 16 deg C
Temps_Middle %>% 
  filter(date < "2021-09-06", #September 6, 2021
         WaterTemp_C > 15.9) %>% 
  summarize(count = n())

7231/6 # divide 10 minute intervals into hours: 1205.167 hours
1205.167 / 24 # 50.2 days / 58 days that temps were above the threshold for OsHV-1
50.2/58 #86.6% of deployment

#### Temps Above 18 deg C
Temps_Middle %>% 
  filter(date < "2021-09-06", #September 6, 2021
         WaterTemp_C > 17.9) %>% 
  summarize(count = n())

6110/6 # divide 10 minute intervals into hours: 1018.333 hours
1018.333 / 24 # 42.4 days / 58 days that temps were above the threshold for OsHV-1
42.43/58 #73.2% of deployment


#### Temps Above 21 deg C
Temps_Middle %>% 
  filter(date < "2021-09-06", #September 6, 2021
         WaterTemp_C > 20.9) %>% 
  summarize(count = n())

2267/6 # divide 10 minute intervals into hours: 377.83 hours
377.83 / 24 # 15.74 days / 58 days that temps were above the threshold for OsHV-1
15.74/58 #27.1% of deployment



#### South: Tomasini Point (TBOC) ====

Temps_South <- read_csv("data/Temperature/2021/Water_TBOC_South_18Oct2021.csv")
glimpse(Temps_South)
summary(Temps_South)
tail(Temps_South)
View(Temps_South)

## clean data

Temps_South <- Temps_South %>% 
  mutate(Date_Time = mdy_hm(Date_Time)) %>%  # set date/time format
  separate(Date_Time, into = c('date', 'time'), sep=' ', remove = FALSE) 

Temps_South %>% 
  ggplot(aes(date, WaterTemp_C)) +
  geom_point()

Temps_South %>% 
  summarize(meanWater = mean(WaterTemp_C),
            SDWater = sd(WaterTemp_C),
            SEWater = SDWater/sqrt(n()),
            maxWater = max(WaterTemp_C),
            minWater = min(WaterTemp_C))

#### Temps Above 16 deg C
Temps_South %>% 
  filter(date < "2021-09-05", #September 5, 2021
         WaterTemp_C > 15.9) %>% 
  summarize(count = n())

7437/6 # divide 10 minute intervals into hours: 1239.5 hours
1239.5 / 24 # 51 days / 59 days that temps were above the threshold for OsHV-1
51.6/59 # 87.4% of deployment

#### Temps Above 18 deg C
Temps_South %>% 
  filter(date < "2021-09-05", #September 5, 2021
         WaterTemp_C > 17.9) %>% 
  summarize(count = n())

7203/6 # divide 10 minute intervals into hours: 1200.5 hours
1200.5 / 24 # 50.02 days / 59 days that temps were above the threshold for OsHV-1
50.02/59 # 84.7% of deployment

#### Temps Above 21 deg C
Temps_South %>% 
  filter(date < "2021-09-05", #September 5, 2021
         WaterTemp_C > 20.9) %>% 
  summarize(count = n())

5010/6 # divide 10 minute intervals into hours: 835 hours
835 / 24 # 34.792 days / 59 days that temps were above the threshold for OsHV-1
34.792/59 # 59.0% of deployment
