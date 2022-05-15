#Importing the libraries and setting directories
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(tmap)
library(readr)
library(tidyr)
options(scipen=999)
## IMPORTING AND FORMATING THE DATA ##
setwd("../northern_india_nepal_and_pakistan_disease_prevention_map_may_29_2019_movement_between_administrative_regions/")
#Importing the movement data (already agregated in one file)
dt <- read_delim("../Delhi Tiles Til 26 March 2020/Delhi_Tiles_till_2603.csv", ";", trim_ws = TRUE)
#Keeping the dates before the lockdown implementation to get an averaged value week day vs week-end
dt <- dt[2:45]
colnames(dt)[6:ncol(dt)] <- paste(
  weekdays(
    parse_date_time(
      colnames(dt[6:ncol(dt)]),
      orders="ymd HM")
    ),".",
  c("5h30am","1h30pm","9h30pm"), 
  sep = "")
#Averaging the number of people moving by timestep, week-days (wd) and week-end (we) separately
mean <- dt[grep("5h30",colnames(dt))]
mean <- mean[-(grep("samedi|dimanche",colnames(mean)))]
dt$wd.5h30am <- round(rowMeans(mean),0)

mean <- dt[grep("1h30",colnames(dt))]
mean <- mean[-(grep("samedi|dimanche",colnames(mean)))]
dt$wd.1h30pm <- round(rowMeans(mean),0)

mean <- dt[grep("9h30",colnames(dt))]
mean <- mean[-(grep("samedi|dimanche",colnames(mean)))]
dt$wd.9h30pm <- round(rowMeans(mean),0)

mean <- dt[grep("5h30",colnames(dt))]
mean <- mean[grep("samedi|dimanche",colnames(mean))]
dt$we.5h30am <- round(rowMeans(mean),0)

mean <- dt[grep("1h30",colnames(dt))]
mean <- mean[grep("samedi|dimanche",colnames(mean))]
dt$we.1h30pm <- round(rowMeans(mean),0)

mean <- dt[grep("9h30",colnames(dt))]
mean <- mean[grep("samedi|dimanche",colnames(mean))]
dt$we.9h30pm <- round(rowMeans(mean),0)

#Keeping only the averaged values for weekdays and weekends
dt <- dt[-(6:44)]
#Writing the output df in a csv file for safety !
write.csv(dt, "./delhi_tiles_avg_28febto8march.csv")

## RECREATING THE TILES ##
#Creating an sf object from the movement dataset
dt_ua <- st_as_sf(dt, wkt = "Geometry") %>% 
  st_set_crs(4326) %>% 
  st_transform(3857) 
#Importing the L3 boundaries layer to join it to the tiles
subd <- st_read("../SDT_shape_2011/India_L3_Administrative_Boundaries.shp") %>%
  st_transform(3857)
#Isolating all the extremeties ("from" and "to") of the movement flows
start <- dt_ua 
start$Geometry <- st_line_sample(start$Geometry, sample = 0)
start$x <- st_coordinates(start)[,1]
start$y <- st_coordinates(start)[,2]
end <- dt_ua 
end$Geometry <- st_line_sample(end$Geometry, sample = 1)
end$x <- st_coordinates(end)[,1]
end$y <- st_coordinates(end)[,2]
#Generating the point grid with the unique values of the flow extremeties
tiles <- unique(start[c("x", "y")]) %>%
  rbind(unique(end[c("x", "y")])) %>%
  unique() %>%
  st_join(subd[c("L3_NAME", "L3_CODE")])
#Creating the voronoi polygons from the point grid
vor <- st_voronoi(st_combine(tiles))
plot(vor, col=0)
