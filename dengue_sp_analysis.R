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
#Importing and reading the movement data
list_csv <- list.files()
data_csv <- lapply(list_csv, read.csv, header=TRUE, sep=",")
#Keeping only the fields of interest
data_csv <- lapply(data_csv, subset, select=c(Starting.Location, Ending.Location, Starting.Region.Name, Ending.Region.Name, Crisis..People.Moving, Geometry))
#Looping to merge all the files into one, keeping the location information, and joining the movement values for the different timesteps to it.
i=1
for (i in 1:length(data_csv)){colnames(data_csv[[i]])[5] <- substr(list_csv[i], 1, nchar(list_csv[i]) - 4)}
data_csv <- data_csv %>% reduce(left_join, by = c("Starting.Location", "Ending.Location"))
data_csv <- cbind(data_csv[1:4],data_csv[6],data_csv[5:ncol(data_csv)] %>% select(-contains(c("Region", "Location", "Geometry"))))
