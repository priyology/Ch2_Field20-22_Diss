######### Tomales Bay Map with field sites ================================


######### PRESENTATION-READY MAP ==============================
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(maptools) ##scalebar
library(ggsn) ##scale bar: http://oswaldosantos.github.io/ggsn/ ; 
library(cowplot)

## ggmap intro: https://appsilon.com/r-ggmap/

## get Maps API Key
register_google(key = "AIzaSyAromYd5yoy--uNE9ANyPyWCS1PdGZwYGg", write = TRUE) #that is my "Maps API Key": https://console.cloud.google.com/apis/credentials?project=garbage-cat 

#create a data.frame with Hog Island, Bodega Bay & Tomales Bay oyster company leases
sites.df <- data.frame(
  lon = c(-122.947833, -122.927504, -122.865700),
  lat = c(38.218050, 38.205616, 38.120200))
glimpse(sites.df)

sites.labelsA <- data.frame(
  lon = c(-122.947833, -122.927504, -122.865700),
  lat = c(38.218050, 38.205616, 38.120200),
  site.name = c("Hog Island Oyster Co.", "Bodega Bay Oyster Co.", "Tomales Bay Oyster Co."))
glimpse(sites.labelsA)

sites.labelsB <- data.frame(
  lon = c(-122.947833, -122.927504, -122.865700),
  lat = c(38.218050, 38.205616, 38.120200),
  site.name = c("North", "Middle", "South"))
glimpse(sites.labelsB)

landmarks.df <- data.frame(
  lon = c(-121.761846, -123.071649, -122.894181, -122.267464),
  lat = c(38.538226, 38.317992, 38.161813, 37.814870))
glimpse(landmarks.df)

landmarks.labels <- data.frame(
  lon = c(-121.761846, -123.071649, -122.894181, -122.267464),
  lat = c(38.538226, 38.317992, 38.161813, 37.814870),
  site.name = c("UCD", "BML", "HIOC", "Home"))
glimpse(landmarks.labels)

BML_TB.labels <- data.frame(
  lon = c(-123.071649, -122.894181),
  lat = c(38.317992, 38.161813),
  site.name = c("Bodega Marine Lab", "Hog Island Oyster Co."))
glimpse(BML_TB.labels)

#load a googlemap 
get_googlemap(center = "Tomales Bay", zoom = 12, markers = sites.df, scale = 2,  maptype = "hybrid") %>% ggmap()

## generate high quality maps using geom_point() to generate markers

# satellite style map of California with Zoom
TB_Map <- get_map("Tomales Bay", zoom =  12, maptype = "satellite")

ggmap(TB_Map) +
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'purple', alpha = 0.7,  size = 5) +
  geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.05, nudge_y = 0.006, hjust = 1, size = 4, color = "white")

# Tone-Lite Map
qmap("Tomales Bay", zoom = 12, scale = 2, source = "stamen", maptype = "toner-lite") +
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'purple', alpha = 0.5,  size = 4) +
  geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.012, nudge_y = 0.005, hjust = 1, size = 5, fontface = "bold", color = "purple")
  
# Watercolor Map
World_watercolor <- qmap("Atlantic Ocean", zoom = 1, scale = 2, source = "stamen", maptype = "watercolor") ## World - Artistic

World_watercolor

TB_watercolor <- qmap("Tomales Bay", zoom = 12, scale = 2, source = "stamen", maptype = "watercolor") ## Tomales Bay - Artistic

TB_watercolor


TB_watercolorA <- qmap("Tomales Bay", zoom = 12, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5,  size = 4) +
  geom_text(data = sites.labelsA, aes(x = lon, y = lat, label = site.name), nudge_x = -0.004, nudge_y = 0.001, hjust = 1, size = 3, fontface = "bold")

TB_watercolorA

ggsave("fig_output/WatercolorMap_OysterCoNames.png", dpi = 320, bg='transparent') 

TB_watercolorB <- qmap("Tomales Bay", zoom = 11, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = BML_TB.labels, aes(x = lon, y = lat), color = 'black', alpha = 0.5,  size = 5) +
  geom_text(data = BML_TB.labels, aes(x = lon, y = lat, label = site.name), nudge_x = 0.150, nudge_y = -0.015, hjust = 1, size = 4, fontface = "bold")

TB_watercolorB

ggsave("fig_output/WatercolorMap_TB_BML.png", dpi = 320, bg='transparent')

TB_watercolorC <- qmap("Tomales Bay", zoom = 12, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5,  size = 4) +
  geom_text(data = sites.labelsB, aes(x = lon, y = lat, label = site.name), nudge_x = 0.035, nudge_y = 0.001, hjust = 1, size = 5, fontface = "bold")

TB_watercolorC

ggsave("fig_output/WatercolorMap_N_M_S.png", dpi = 320, bg='transparent') 


SFBay_watercolor <- qmap("San Pablo Bay", zoom = 8, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = landmarks.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5, size = 4) +
  geom_text(data = landmarks.labels, aes(x = lon, y = lat, label = site.name), nudge_x = -0.07, nudge_y = 0.008, hjust = 1, size = 5)

SFBay_watercolor

TB_SFBay <- qmap("Nicasio", zoom = 10, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5, size = 4) +
  geom_text(data = sites.labels, aes(x = lon, y = lat, label = site.name), nudge_x = -0.07, nudge_y = 0.008, hjust = 1)

TB_SFBay

CA_watercolor_labels <- qmap("San Francisco", zoom = 7, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = landmarks.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5, size = 4) +
  geom_text(data = landmarks.labels, aes(x = lon, y = lat, label = site.name), nudge_x = -0.15, nudge_y = 0.008, hjust = 1)

CA_watercolor_labels

CA_watercolor <- qmap("Fresno, CA, USA", zoom = 6, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = sites.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5, size = 4)

CA_watercolor

CA_watercolor_BayArea <- qmap("Fresno, CA, USA", zoom = 6, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = landmarks.df, aes(x = lon, y = lat), color = 'black', alpha = 0.5, size = 1.5)

CA_watercolor_BayArea

#make inset maps for sites AND for landmarks
sites_inset_map = ggdraw() +
  draw_plot(TB_watercolor) +
  draw_plot(CA_watercolor, x = 0.638, y = 0.628, width = 0.3, height = 0.4)

sites_inset_map

#run sites_inset_map before running ggsave
ggsave("fig_output/WatercolorMap_sites.png", dpi = 320, bg='transparent') 

landmarks_inset_map = ggdraw() +
  draw_plot(SFBay_watercolor) +
  draw_plot(CA_watercolor_BayArea, x = 0.655, y = 0.700, width = 0.3, height = 0.3)

landmarks_inset_map

#run landmarks_inset_map before running ggsave
ggsave("fig_output/WatercolorMap_landmarks.png", dpi = 320, bg='transparent')

##### For Point Blue Talk ============

PointBlue.labels <- data.frame(
  lon = c(-121.761846, -123.071649, -122.894181, -122.267464, -122.59045745951444),
  lat = c(38.538226, 38.317992, 38.161813, 37.814870, 38.22806296304263),
  site.name = c("UCD", "BML", "HIOC", "Home", "PB"))
glimpse(PointBlue.labels)

PointBlue_watercolor <- qmap("San Pablo Bay", zoom = 9, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = PointBlue.labels, aes(x = lon, y = lat), color = 'black', alpha = 0.5, size = 2.5) +
  geom_text(data = PointBlue.labels, aes(x = lon, y = lat, label = site.name), nudge_x = -0.015, nudge_y = -0.035, hjust = 1, size = 3.5)

PointBlue_watercolor

#run PointBlue_watercolor before running ggsave
ggsave("fig_output/PointBlue_map.png", dpi = 320, bg='transparent') 


##### For SFSU Talk ============

HIOC.label <- data.frame(
  lon = c(-122.893559),
  lat = c( 38.161959),
  site.name = c("HIOC"))
glimpse(HIOC.label)

Base.labels <- data.frame(
  lon = c(-121.761846, -123.071649, -122.446565),
  lat = c(38.538226, 38.317992, 37.889047),
  site.name = c("UCD", "BML", "EOS"))
glimpse(EOS.labels)

EOS_watercolor <- qmap("San Pablo Bay", zoom = 9, scale = 2, source = "stamen", maptype = "watercolor") + ## Tomales Bay - Artistic
  geom_point(data = Base.labels, aes(x = lon, y = lat), color = 'black', alpha = 0.5, size = 4) +
  geom_text(data = Base.labels, aes(x = lon, y = lat, label = site.name), nudge_x = -0.040, nudge_y = -0.035, hjust = 1, size = 4)

EOS_watercolor

#run PointBlue_watercolor before running ggsave
ggsave("fig_output/SFSU_EOS_map.png", dpi = 320, bg='transparent')


##### For SDSU Talk ============

SDSU.labels <- data.frame(
  lon = c(-121.761846, -117.071382),
  lat = c(38.538226, 32.777375),
  site.name = c("UCD", "SDSU"))
glimpse(SDSU.labels)

SDSU_watercolor <- qmap("San Luis Obispo, CA, USA", zoom = 6, scale = 2, source = "stamen", maptype = "watercolor") + ## California - Artistic
  geom_point(data = SDSU.labels, aes(x = lon, y = lat), color = 'black', alpha = 0.5, size = 2) +
  geom_text(data = SDSU.labels, aes(x = lon, y = lat, label = site.name), nudge_x = -0.155, nudge_y = -0.035, hjust = 1, size = 4)

SDSU_watercolor

#run SDSU_watercolor before running ggsave
ggsave("fig_output/SDSU_map.png", dpi = 320, bg='transparent')

SDSU_TB_inset_map = ggdraw() +
  draw_plot(SFBay_watercolor) +
  draw_plot(SDSU_watercolor, x = 0.670, y = 0.700, width = 0.3, height = 0.3)

SDSU_TB_inset_map

#run SDSU_watercolor before running ggsave
ggsave("fig_output/SDSU_TB_inset_map.png", dpi = 320, bg='transparent')
  

######### PUBLICATION-READY MAP ================================

## Based on Ben Rubinoff's R Code in Grosholz Lab Drive

## Load Libraries
library(tidyverse)
library(ggspatial)
library(ggplot2)
library(sf)
library(cowplot)

## Create Date Frame with latitude ('38') and longitude ('-122') of sites
sites<- data.frame(longitude = c(-122.947833, -122.927504, -122.865700), 
                   latitude = c(38.218050, 38.205616, 38.120200), 
                   site = c("HI", "BB", "TB"))

## Read in the shape files of NOAA coasline data and CA state outline
# Download the Continuously Updated Shoreline Product here: https://shoreline.noaa.gov/data/datasheets/cusp.html
# You can use the data explorer to download the file for your region (you need all 4 files to read in the .shp file)
# It will likely download a whole folder called "CUSP". You need the CUSPLine.shp file, you generate this via Download > CUSP > By Rectangle (and then draw the rectangle over your region) 
# Download the CA State boundary here to your desktop: https://data.ca.gov/dataset/e212e397-1277-4df3-8c22-40721b095f33/resource/3db1e426-fb51-44f5-82d5-a54d7c6e188b/download/ca-state-boundary.zip

# Load shape files
Tomales_Coastline <- st_read("map_CUSP/CUSPLine.shp")
CA_State <- st_read("CA_State_Boundary/CA_State_TIGER2016.shp")

## Create the main plot of sites
Tomales <- ggplot(data = Tomales_Coastline) +
  geom_sf()+ # Sets the shape file as the primary map layer 
  coord_sf(xlim = c(-123.05, -122.8), ylim = c(38.05, 38.3), expand = FALSE) + # limits the range of lat and long, specify this based on your site coordinates
  annotation_scale(aes(location = "bl")) + # adds scale bar to bottom left
  annotation_north_arrow(aes(location = "tl"), style = north_arrow_nautical())+ #adds a north arrow with nautical style to top left
  geom_point(data = sites, aes(x = longitude, y = latitude), size = 3) + # adds in points for each site
  geom_text(data = sites, aes(x = longitude, y = latitude, label = site), nudge_x = 0.025, nudge_y = 0.011, hjust = 1) + # adds in label for each site
  theme_bw() +
  labs(x= "Longitude", y = "Latitude") +
  theme(panel.grid.major = element_blank(), # Removes the grid
        panel.grid.minor = element_blank())

Tomales

## Create the inset plot of California 

# Create points that will be connected with geom_path in the CA plot. Note that you must repeat the first point to make the path connect to make a box
Box_overlay <- data.frame(longitude = c(-123.5, -123.5, -122.8, -122.8, -123.5), 
                          latitude = c(38.05,  38.8,  38.8,  38.05, 38.05))

California <- ggplot(data = CA_State) +
  geom_sf() +
  geom_spatial_path(data = Box_overlay, aes(x = longitude, y = latitude), color = "red", size = 0.6) + #Creates red box to points listed above
  theme_map() +
  theme(panel.background = element_rect(colour = "black", fill=NA, size=0.6)) #Adds border to plot

## Combine the plots and specify the inset plot size and position
gg_inset_map = ggdraw() +
  draw_plot(Tomales) +
  draw_plot(California, x = 0.628, y = 0.705, width = 0.3, height = 0.3)

gg_inset_map

ggsave("fig_output/TomalesMap.png", dpi = 320)
