library(purrr)
# setting working directory where the csv data is
setwd("~/northern_india_nepal_and_pakistan_disease_prevention_map_may_29_2019_movement_between_tiles")
# store the content of the whole folder
list_csv <- list.files()
# read the csv data
data_csv <- lapply(list_csv, read.csv, header=TRUE, sep=",")
# separate the spatial information in another list, in order to extract all mobility flow combination on the observed time perio
data_csv2 <- lapply(data_csv, subset, select=c(Geometry, Starting.Region.Name, Ending.Region.Name, Starting.Location, Ending.Location))
# pasting all the list content vertically in one df
data_csv2 <- do.call(rbind,data_csv2)
# identify lines with duplicates (i.e. redudant mobility combination)
duplicates <- which(duplicated(data_csv2))
# remove them from the df, to have a reference df with all the locations
data_csv2 <- data_csv2[-duplicates,]
# in another list, we separate the information about number of people at each time step 
data_csv <- lapply(data_csv, subset, select=c(Geometry, Crisis..People.Moving))
# we rename the population row by the time step date (if contained in the filename) 
for (i in 1:length(data_csv)){colnames(data_csv[[i]])[2] <- substr(list_csv[i], 1, nchar(list_csv[i]) - 4)}
# alternative loop in case the time step date in containted in the content of the file itself)
# for (i in 1:length(data_csv2)){colnames(data_csv2[[i]])[3] <- as.character(data_csv2[[i]]$Date.Time[[1]])} 
# loop to left join population information at each time step to the reference spatial df
for (i in 1:length(data_csv)){data_csv2 <- left_join(data_csv2, as.data.frame(data_csv[i]), by= c("Geometry"="Geometry"))}
# replace NA values by 0
data_csv2[is.na(data_csv2)] = 0
# writing the resulting file in csv format
# alternative possibility to remove the intra-mobility, if existing
# data_csv2 <- subset(data_csv2, Starting.Location.x != Ending.Location.x)
write.csv(data_csv2, file="../mobilites_tiles_joined.csv")
