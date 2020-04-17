library(dplyr)
library(purrr)
# setting working directory where the csv data is
setwd("./GIS Data/Northern-India-Nepal-And-Pakistan-Disease-Prevention-Map-May-29-2019 (1)/northern_india_nepal_and_pakistan_disease_prevention_map_may_29_2019_facebook_population_administrative_regions")
# store the content of the whole folder
list_csv <- list.files()
# read the csv data
data_csv <- lapply(list_csv, read.csv, header=TRUE, sep=",")
# keeping only the spatial id and the population data of each file
data_csv <- lapply(data_csv, subset, select=c(Spaco.Id, Crisis..People))
# to keep only the part after the underscore (date and time) in the filename
list_csv <- sub("^[^_]*_", "", list_csv)
# for the population rename the column by filename [date + hour] for further joint
for (i in 1:length(data_csv)){colnames(data_csv[[i]])[2] <- substr(list_csv[i], 1, nchar(list_csv[i]) - 4)}
# applying a left join by spatial id to have every population data by time step for every spatial entity
data_csv <- data_csv %>% reduce(left_join, by = "Spaco.Id")
# writing the resulting csv file
write.table(data_csv, ".", sep=";")




