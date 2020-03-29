library(readr)
library(sf)
library(ggmap)
library(ggforce)
library(scales)
library(magick)
library(ggmap)
library(RColorBrewer)
# importing the base CSV
semaine_moyenne_Delhi_5km_220519_020719 <- read_delim("~/semaine_moyenne_Delhi_5km_220519-020719.csv", ";", escape_double = FALSE, locale = locale(decimal_mark = ","), trim_ws = TRUE)
# reading it as a spatial object
semaine <- st_as_sf(semaine_moyenne_Delhi_5km_220519_020719, wkt = "Geometry")
# creating a first file for starting locations
semaine_starting <- semaine
# extracting the first vertex to get starting coordinates only
semaine_starting$Geometry <- st_line_sample(semaine$Geometry, sample = 0)
# creating a second file for ending locations
semaine_ending <- semaine
# extracting the first vertex to get ending coordinates only
semaine_ending$Geometry <- st_line_sample(semaine$Geometry, sample = 1)
# creating pivot tables by grouping similar geometries and making the sum of their incoming/outgoing population 
semaine_starting$group = sapply(st_equals(semaine_starting), max)
starting_pivot <- aggregate(cbind(semaine_starting[6:26]), by=list(group = semaine_starting$group), FUN=sum)
semaine_ending$group = sapply(st_equals(semaine_ending), max)
ending_pivot <- aggregate(cbind(semaine_ending[6:26]), by=list(group = semaine_ending$group), FUN=sum)
# extracting the locality names apart to join it with the pivot tables
label_start <- subset(st_drop_geometry(semaine_starting), select = c("group", "Starting.Region.Name"))
label_end <- subset(st_drop_geometry(semaine_ending), select = c("group", "Ending.Region.Name"))
# removing the duplicates in both starting and ending localities
duplicates_s <- which(duplicated(label_start))
label_start <- label_start[-duplicates_s,]
duplicates_e <- which(duplicated(label_end))
label_end <- label_end[-duplicates_e,]
# merging with the pivot tables
starting_pivot <- merge(starting_pivot, label_start, by='group')
ending_pivot <- merge(ending_pivot, label_end, by='group')
# setting the CRS for both files as WGS 84 in the perspective of mapping
st_set_crs(starting_pivot, 4326)
st_set_crs(ending_pivot, 4326)
# extracting the coordinates to create an X and Y field in the pivot tables, in foster of ggplot formats
sp_xy <- do.call(rbind, st_geometry(starting_pivot)) %>% as_tibble() %>% setNames(c("Longitude","Latitude"))
sp_coords <- cbind(st_drop_geometry(starting_pivot), sp_coords)
ep_xy <- do.call(rbind, st_geometry(ending_pivot)) %>% as_tibble() %>% setNames(c("Longitude","Latitude"))
ep_coords <- cbind(st_drop_geometry(ending_pivot),ep_xy)
# creating the different directories for the output files
output_dir <- file.path('.', 'Outputs')
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
}
output_dir <- file.path('./Outputs', 'Outgoing_Mobilities')
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
}
output_dir <- file.path('./Outputs', 'Incoming_Mobilities')
if (!dir.exists(output_dir)){
  dir.create(output_dir)
} else {
}
#initializing a variable for category, if discretization is used
sp_coords$cat <- 0
ep_coords$cat <- 0
#discretizing the variable with 8 clusters (k-means method) (if needed, otherwise can be mapped as a continuous variable)
disc_s <- discretize(sp_coords[[2]], method = "cluster", breaks = 8, onlycuts = TRUE)
disc_e <- discretize(ep_coords[[2]], method = "cluster", breaks = 8, onlycuts = TRUE)
# extracting the bounding box of the dataset to request the corresponding background map from Stamen
sp_bb <- st_bbox(starting_pivot)
ni.stamen <- get_stamenmap(c(left = sp_bb[[1]], bottom = sp_bb[[2]],right = sp_bb[[3]],top = sp_bb[[4]]), zoom = 7)


