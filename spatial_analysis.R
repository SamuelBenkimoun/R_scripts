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

# iterating to map all the timesteps of the dataset
for (i in 2:(length(sp_coords)-4)){
  print(colnames(sp_coords[i]))
  # redefining the category variable as per discretization previously established (to make results comparable)
  sp_coords <- mutate(sp_coords, cat = cut(sp_coords[[i]], disc_s, dig.lab=6))
  
  # subset and ordering of 5000+ people tiles for dynamic labelling on the map
  sp_coords3 <- subset(sp_coords, sp_coords[[i]]>5000)
  # ordering the list so that the display priority for labelling goes to the most prolific tiles
  sp_coords3 <- sp_coords3[order(sp_coords3[[i]], decreasing=TRUE),]
  # mapping with the stamen background, and applying a logarithmic scale given the exponential profile of the distribution
  map <- ggmap(ni.stamen) + 
    geom_point(
      data = sp, 
      mapping = aes(
        x = sp_coords$Longitude, 
        y = sp_coords$Latitude,
        #colour = cat,
        colour = log(sp_coords[[i]]),
        shape="square"), 
      alpha = .9
    ) + 
  # legend customization
  guides(
      shape=FALSE
    ) + 
    #scale_color_viridis_c() +
    scale_color_gradient2(
      low = "white", 
      mid = "#e0e0ec", 
      high = "#000276", 
      na.value = "#ffffff", 
      midpoint=median(log(sp_coords[[2]])), 
      limits=c(0, 
               max(log(sp_coords[[2]]))), 
      labels=c(0,
               as.integer(exp(2.5)), 
               as.integer(exp(5)), 
               as.integer(exp(7.5)), 
               as.integer(exp(10)))
    )+
    #integration of labels from the +5000 (or else) people tiles
    geom_text(
      data= sp_coords3, 
      aes(
        label= paste(sp_coords3$Starting.Region.Name)), 
      colour="black",
      fontface = "bold",
      check_overlap = TRUE, 
      nudge_y = -0.2, 
      size=3.2
    ) + 
    #integration of the scalebar
    ggsn::scalebar(
      starting_pivot, 
      dist = 100, 
      dist_unit ='km', 
      transform=TRUE, 
      st.size=3, 
      height=0.01, 
      model = 'WGS84', 
      location = "topright"
    ) + 
    # title with dynamic updating for each timestep
    ggtitle(
      "Sum of outgoing mobilities per tiles", 
      subtitle=paste("Timestep (Day_hhmm):", colnames(sp_coords[i]), sep=" ")
    ) + 
    labs(
      caption= "Projection: WGS84, Source: Facebook Data For Good, Stamen Maps", 
      colour = "Number of people moving:"
    )+ 
    # adjustments on the axe and peripheric areas of the map
    xlab("Longitude") + 
    ylab("Latitude") + 
    theme_classic() + 
    theme(
      plot.title = element_text(face = "bold", hjust = 0.5), 
      plot.subtitle = element_text(face = "italic", hjust = 0.5)
    ) + 
    scale_x_unit(unit = 'degrees') + 
    scale_y_unit(unit= 'degrees')
  # writing the maps in the output folder
  ggsave(
    paste(colnames(sp_coords[i]), ".png", sep=""), 
    plot=map, 
    device= "png", 
    path= './Outputs/', 
    height = 676/38, #38 is the pixel/cm ratio
    width = 1000/38, 
    units = "cm"
    )
}

