#Importing the libraries and setting directories
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(tmap)
library(readr)
library(tidyr)
options(scipen=999)
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
