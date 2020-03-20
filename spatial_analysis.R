library(readr)
library(sf)
# reading the file with averaged week data
semaine_type_5km <- read_delim("semaine_type_5km.csv","\t", escape_double = FALSE, trim_ws = TRUE)
# including the spatial component by indicating the wkt field
semaine <- st_as_sf(semaine_type_5km, wkt = "Geometry")
# creating a first variable to separate departure locations
semaine_starting <- semaine
# extracting the first vertices of mobility flows corresponding to departure locations
semaine_starting$Geometry <- st_line_sample(semaine$Geometry, sample = 0)
# creating a second variable to keep arrival locations only
semaine_ending <- semaine
# extracting the last vertices of mobility flows corresponding to arrival locations
semaine_ending$Geometry <- st_line_sample(semaine$Geometry, sample = 1)
# grouping all the mobilities by same starting point, by giving a group id to similar geometries 
semaine_starting$group = sapply(st_equals(semaine_starting), max)
# creating a pivot table summarizing all the population flows by tile of departure
starting_pivot <- aggregate(cbind(semaine_starting[6:26]), by=list(group = semaine_starting$group), FUN=sum)
# grouping all the mobilities by same destination point, by giving a group id to similar geometries 
semaine_ending$group = sapply(st_equals(semaine_ending), max)
# creating a pivot table summarizing all the population flows by tile of destination
ending_pivot <- aggregate(cbind(semaine_ending[6:26]), by=list(group = semaine_ending$group), FUN=sum)
