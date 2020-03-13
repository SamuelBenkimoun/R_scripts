library(purrr)
# setting working directory where the csv data is
setwd("~/northern_india_nepal_and_pakistan_disease_prevention_map_may_29_2019_movement_between_tiles")
# store the content of the whole folder
list_csv <- list.files()
# read the csv data
data_csv <- lapply(list_csv, read.csv, header=TRUE, sep=",")
# keeping only the spatial extent and the population data of each file
data_csv <- lapply(data_csv, subset, select=c(Geometry, Crisis..People.Moving))
# keeping only the tiles id and region name to be joined later (on the basis of geometry) to label the recordings
data_csv2 <- lapply(data_csv, subset, select=c(Geometry, Starting.Region.Name, Ending.Region.Name, Starting.Location, Ending.Location))
# for the population rename the column by filename [date + hour] for further joint
for (i in 1:length(data_csv)){colnames(data_csv[[i]])[4] <- substr(list_csv[i], 1, nchar(list_csv[i]) - 4)}
# applying a left join by geometry to have every population data by time step for every spatial entity
data_csv <- data_csv %>% reduce(left_join, by = "Geometry")
# applying a left join by geometry to have all the labels ready
data_csv2 <- data_csv2 %>% reduce(left_join, by = "Geometry")
# removing superfulous columns from the joint
data_csv2 <- subset(data_csv2, select = c(Geometry, Starting.Region.Name.x, Ending.Region.Name.x, Starting.Location.x, Ending.Location.x))
# joining the countings and the region names
data_csv2 <- left_join(data_csv2, data_csv, by= c("Geometry"="Geometry"))
# writing the resulting csv file
write.csv(data_csv2, file="../mobilites_tiles_joined.csv")
# rewriting a file without intern mobilities
data_csv3_wo <- subset(data_csv3, Starting.Location.x != Ending.Location.x)
write.csv(data_csv3_wo, file="../mobilites_tiles_joined_wo_auto.csv")
