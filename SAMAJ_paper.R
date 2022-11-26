# Importing the libraries
library(readr)
library(readxl)
library(tidyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggdark)
library(dplyr)
library(reshape2)
library(sf)
library(corrr)
library(FactoMineR)
library(factoextra)
library(igraph)
# Disabling the scientific notation
options(scipen=999)

### CREATING THE GRAPH OF TOTAL NUMBER OF MOVEMENTS BY DATE
#Writing the function needed to sum the colmuns of the movement dataframes
sumcoldf <-function(x, y = c("total_people_moving", "date")){
  out <- as.data.frame(colSums(x))
  out$Date <- rownames(out)
  row.names(out) <- 1:nrow(out)
  colnames(out) <- y
  out <- out[,c(2,1)]
  out
}
#Importing mobility data corresponding to the lockdown implementation period (until 26 March 2020)
Delhi_Tiles_til_2603 <- read_delim("~/Delhi Tiles Til 26 March 2020/Delhi_Tiles_till_2603.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#To determine the time-step to retain
sumcoldf(Delhi_Tiles_til_2603[7:100])
#Preparing the data to plot distributions and curve of total people moving
dd <- Delhi_Tiles_til_2603[7:100]
dd <- dd[grep("0530", colnames(dd))]
#Keeping the timesteps at 5:30am
colnames(dd) <- gsub('.0530', '', colnames(dd)) 
#Computing the total movements for each time-step 
cd <- sumcoldf(dd)
#Formating to long format 
dd <- gather(dd, date, people)
#Interpreting the date column as such
dd$date <- gsub('\\.', "/", dd$date) %>% 
  ymd()
cd$date <- gsub('\\.', "/", cd$date) %>% 
  ymd()

#Drawing the curve of cumulated trips registered (in thousands) and the number of people by mobility flow (Figure 4)
image <- ggplot(dd, aes(x = date, y = jitter(people))) +
  geom_point(alpha=0.05, color="#337DFF")+
  geom_line(data = cd, size = 1.3, aes(x = date, y = total_people_moving/1000))+
  geom_vline(aes(color = "Holi", xintercept = as.Date("2020-03-10")), linetype="dotted", size=1.8)+
  geom_vline(aes(color = "Janata Curfew", xintercept = as.Date("2020-03-22")), linetype="dotted", size=1.8)+
  geom_vline(aes(color = "Lockdown", xintercept = as.Date("2020-03-24")), linetype="dotted", size=1.8)+
  scale_color_manual(name = "Impactful events", values = c("Holi" = "MediumSlateBlue", "Janata Curfew" = "orange", "Lockdown" = "red"))+
  ggtitle("Facebook users moving at 2km-tiles level")+
  labs(x = "", y = "(Dots) Distribution of people moving by flow")+
  dark_theme_gray()+
  theme(text = element_text(size=20),
        plot.title = element_text(size=18, face="bold", hjust = 0.5),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.position = 'bottom',
        legend.text=element_text(size=10),
        legend.title=element_text(size=10))+
  scale_y_continuous(breaks = c(0,250,500,750,1000), sec.axis = sec_axis(~ . * 1000, name = "(Curve) Total movements in the area", breaks = c(0,250000,500000,750000,1000000)))
ggsave(file="../../Rplot_FB2kmDelhi.svg", plot=image, width=10, height=8)

### RELOCATING FB TILES WITH NUMBER OF USERS INTO THE SUBDISTRICT GEOMETRIES
# Setting working directory where to get a more recent dataset to compute the FB users ratio in a post-lockdown context
setwd("~/FB India Mobilities Between Tiles 2021 /Delhi Coronavirus Disease Prevention Map Mar 21 2020 Mobility Between Tiles")
# Read the csv data
list_csv <- list.files()
data_csv <- lapply(list_csv, read.csv, header=TRUE, sep=",")
# Calculating the penetration rate, 30 nov 2021 3rd timestep (evening, to have people supposedly at home)
users <- as.data.frame(data_csv[6])
# Replacing NA values by 0
users[is.na(users)] = 0
# Summing all the incoming movements by destination
users <- aggregate(users["n_baseline"], by = list(end_lon = users$end_lon, end_lat = users$end_lat, end_polygon_name = users$end_polygon_name), FUN = sum)
# Creating the geometries for each destination (points)
users <- st_as_sf(x = users, coords = c(x = "end_lon", y= "end_lat"), crs = 4326)
# Create the voronoi polygons to recreate the tiles around the destination points
v <- st_combine(st_geometry(users)) %>%
  st_voronoi() %>%
  st_collection_extract() %>%
  st_make_valid()
v <- v[unlist(st_intersects(users, v))]
# Adding the attributes from the 31 nov movement dataset to the voronoi polygons
pv <- st_set_geometry(users, v) %>%
  st_make_valid()
# Compute the area and removing the superfluous polygons on the edges
pv$area <- st_area(pv)
pv <- subset(pv, area < 3*min(pv$area))
plot(pv)
# Importing the Indian subdistrict geographical layer
subd <- st_read("~/SDT_shape_2011/India_L3_Administrative_Boundaries.shp")
# Fixing the geometries, filtering with the extend of our previous polygons and computing the area by subdistrict
subd <- subd %>%
  st_make_valid() %>%
  st_filter(pv) %>%
  mutate(area_sdt = st_area(geometry))
# Intersecting the two layers, to reattribute the FB users population in each Sub district (pro-rata of overlapping area) and calculate the penetration rate at last
inter <- st_intersection(pv, subd)
# Importing the list of the localities that are covered enough by the dataset
Delhi_wider_urban_area_70km <- read_csv("~/Delhi_wider_urban_area_70km.csv")
mobility_areas <- Delhi_wider_urban_area_70km$L3_NAME
mobility_areas <- mobility_areas[!(mobility_areas %in% c("Tijara", "Hathin", "Nuh", "Kosli", "Matenhail","Gohana","Samalkha", "Ganaur","Baraut"))]
# Subsetting intersection layer
inter <- subset(inter, L3_NAME %in% mobility_areas) %>%
  subset(!as.numeric(L3_CODE) == 402)
# Compute the area to redistribue the users at the pro-rata of overlapping area
inter$area_int <- st_area(inter$geometry)
inter$ratio_int <- inter$area_int/inter$area
inter$users_int <- inter$n_baseline * inter$ratio_int
# Recreating the subdistricts with the FB users population and ratio of users
inter <- aggregate(inter[c("area_int", "users_int")], by = list(L3_NAME = inter$L3_NAME, area_sdt = inter$area_sdt), FUN = sum)

### FORMATING THE CENSUS OF INDIA DATA AND LINKING IT WITH THE SUBDISTRICT GEOMETRIES AND NUMBER OF FB USERS
#Reading the census files related to households
setwd("~/Census_2011_NCR/Households/Census 2011 Households NCT-UP-HR Subdistricts")
list_csv <- list.files()
hh <- lapply(list_csv, function (x) {
  read_excel(x, skip = 6)
})
#Keeping only the sub-districts and total value
hh <- lapply(hh, subset, grepl("Sub-Dist", `9`))
hh <- lapply(hh, subset, `10` == "Total")
#Building a unique table with all the information
hh <- do.call(rbind,hh)
hh <- hh[c(5,12,61,62,64,130,133,134,136,137)]
colnames(hh) <- c("Subdistt","HH_GCONDITION", "SIX", "NINE_MORE", "RENTED", "COMP_INTERNET", "MOB", "MOBxLAND", "SCOOTER", "CAR")
hh <- hh %>% 
  mutate(SIX_MORE_HH = SIX + NINE_MORE) %>%
  mutate(MOBILE = MOB + MOBxLAND) %>%
  dplyr::select(c("Subdistt","HH_GCONDITION", "SIX_MORE_HH", "RENTED", "COMP_INTERNET", "MOBILE", "SCOOTER", "CAR"))

#Importing the census tables related to religion
setwd("~/Census_2011_NCR/Religion")
rel <- lapply(list.files(), read_excel)
#Keeping the sub-district information only and building a unique table
rel <- lapply(rel, subset, grepl("Sub-District", Name))
rel <- do.call(rbind,rel)
#Computing the rate of each religion
i = 1
rel$hindu <- 0
rel$muslim <- 0
rel$sikh <- 0
for (i in 1:(nrow(rel)-4)){
  rel[i, 86] <- rel[i+1, 8]/ rel[i, 8] * 100
  rel[i, 87] <- rel[i+2, 8]/ rel[i, 8] * 100
  rel[i, 88] <- rel[i+4, 8]/ rel[i, 8] * 100
  i = i+27
}
rel <- subset(rel, Religion == "Total" & TRU == "Total")
#Computing the women ratio from the same table
rel$F_RATIO <- rel$TOT_F/rel$TOT_P * 100

#Importing the rest of the Census data
setwd("~/Census_2011_NCR")
data_csv <- lapply(list.files(pattern = ".xlsx"), read_excel)
#Keeping the sub-district information only and building a unique table
data_csv <- lapply(data_csv, subset, Level == "SUB-DISTRICT")
data_csv <- do.call(rbind,data_csv)
#Computing an urban rate by dividing the total population by the urban population
data_csv$URB_RATE = 0
i = 1
for (i in 1:(nrow(data_csv)-2)){
  if (data_csv[i,]$TRU == "Total"){
    data_csv[i,]$URB_RATE = data_csv[i+2,]$TOT_P/data_csv[i,]$TOT_P*100
  }
}
data_csv <- subset(data_csv, TRU == "Total")
#Subsetting the fields of interest
data_csv <- subset(data_csv, select = c("Subdistt","Name", "TOT_P", "P_06", "P_SC", "P_LIT", "TOT_WORK_P", "MAIN_CL_P", "MAIN_AL_P", "MAIN_HH_P", "MAIN_OT_P", "MARGWORK_P", "NON_WORK_P", "URB_RATE"))
#Computing rate values for the fields of interest
data_csv <- transform(data_csv, P_06 = P_06/TOT_P*100) %>%
  transform(P_SC = P_SC/TOT_P*100) %>%
  transform(P_LIT = P_LIT/TOT_P*100) %>%
  transform(MAIN_CL_P = MAIN_CL_P/TOT_WORK_P*100) %>%
  transform(MAIN_AL_P = MAIN_AL_P/TOT_WORK_P*100) %>%
  transform(MAIN_HH_P = MAIN_HH_P/TOT_WORK_P*100) %>%
  transform(MAIN_OT_P = MAIN_OT_P/TOT_WORK_P*100) %>%
  transform(MARGWORK_P = MARGWORK_P/TOT_WORK_P*100) %>%
  transform(NON_WORK_P = NON_WORK_P/TOT_P*100) %>%
  transform(WORK_RATE = TOT_WORK_P/TOT_P*100)
# Given that some areas are finally not in the mobility dataset (50km radius approx), selection of the common areas with the Census data
data_csv <- subset(data_csv, Name %in% mobility_areas) %>%
  subset(!as.numeric(Subdistt) == 402)
# Merging the different parts of the Census data (standard information, plus households equipment and religion tables) 
data_csv <- merge(data_csv, rel[,c(3,10,86,87,88,89)]) %>%
  merge(hh)
# Joining the Census information to the geographical layer with the subdistricts and FB users information
inter <- left_join(inter, data_csv, by = c("L3_NAME"="Name"))
# Computing the rate of FB users locatable as compared to the census population
inter <- mutate(inter, fb_ratio = users_int / ((area_int/area_sdt)*TOT_P))
# Computing the density for further use
inter <- mutate(inter, DENSITY = as.numeric(TOT_P/(area_sdt*0.000001)))

# To look at the pairwise correlation of Facebook users with the other census variables
correl <- inter[6:33] %>% 
  #inter2[c(6:28,30:40)] %>%
  st_drop_geometry() %>% 
  mutate(fb_ratio = as.numeric(fb_ratio)) %>% 
  correlate() %>%
  as.data.frame() %>%
  dplyr::select(c("term","fb_ratio"))

########################################TO DELETE POTENTIALLY#####################################
## PCA ON THE WHOLE DATASET ##
variables <- c("P_06", "P_SC", "P_LIT", "MAIN_CL_P", "MAIN_AL_P", "MAIN_HH_P", "MAIN_OT_P", "MARGWORK_P", "WORK_RATE", "URB_RATE", "F_RATIO", "hindu", "muslim", "HH_GCONDITION", "DENSITY")
res.pca <- PCA(st_drop_geometry(inter[variables]), scale.unit = TRUE, ncp = 5, graph = TRUE)
fviz_pca_ind(res.pca)
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# At the light of the different tests, reducing the number of variables the minimise the redundancy 
variables_reduced <- c("MAIN_OT_P", "F_RATIO", "hindu","HH_GCONDITION", "P_06", "DENSITY")
correlate(st_drop_geometry(inter[variables_reduced]))
inter_red <- inter[c("L3_NAME",variables_reduced)] %>%
  st_drop_geometry()
########################################TO DELETE POTENTIALLY#####################################

# Subsetting the needed time-steps from the mobility analysis over the lockdown period (scattered in several files)
Delhi_Tiles_til_2603 <- read_delim("~/Delhi Tiles Til 26 March 2020/Delhi_Tiles_till_2603.csv", ";", escape_double = FALSE, trim_ws = TRUE)
Delhi_Tiles_til_2603 <- Delhi_Tiles_til_2603[c(2,3,4,10,98)]

# Importing the rest of mobility data for the continuation of the observation window (May and June 2020) and subsetting the dates and fields needed
Delhi_Tiles_May <- read_csv("~/Delhi Tiles 2km from May 2020/Delhi Mvt Point/Delhi Coronavirus Disease Prevention Map Mar 21 2020 Id  Movement between Tiles__2020-05-08 0000.csv")
Delhi_Tiles_May <-  Delhi_Tiles_May[c("geometry", "start_polygon_name", "end_polygon_name", "n_crisis")]
colnames(Delhi_Tiles_May)[1] <- c("Geometry")
colnames(Delhi_Tiles_May)[4] <- c("2020.05.08.0530")

Delhi_Tiles_May2 <- read_csv("~/Delhi Tiles 2km from May 2020/Delhi Mvt Point/Delhi Coronavirus Disease Prevention Map Mar 21 2020 Id  Movement between Tiles__2020-05-26 0000.csv")
Delhi_Tiles_May2 <-  Delhi_Tiles_May2[c("geometry", "start_polygon_name", "end_polygon_name", "n_crisis")]
colnames(Delhi_Tiles_May2)[1] <- c("Geometry")
colnames(Delhi_Tiles_May2)[4] <- c("2020.05.26.0530")

Delhi_Tiles_May3 <- read_csv("~/Delhi Tiles 2km from May 2020/Delhi Mvt Point/Delhi Coronavirus Disease Prevention Map Mar 21 2020 Id  Movement between Tiles__2020-06-22 0000.csv")
Delhi_Tiles_May3 <-  Delhi_Tiles_May3[c("geometry", "start_polygon_name", "end_polygon_name", "n_crisis")]
colnames(Delhi_Tiles_May3)[1] <- c("Geometry")
colnames(Delhi_Tiles_May3)[4] <- c("2020.06.22.0530")

# Merging all the different files in a single one
mob_tiles <- merge(Delhi_Tiles_til_2603, Delhi_Tiles_May) %>%
  merge(Delhi_Tiles_May2) %>%
  merge(Delhi_Tiles_May3)
# Adding the geometry to make is a spatial feature
mob_tiles <- st_as_sf(mob_tiles, wkt = "Geometry")

#Keeping the origin of the flows to join the tile id
start <- mob_tiles
start$Geometry <- st_line_sample(mob_tiles$Geometry, sample = 0) 
#joining the L3 level from the Census 2011
start <- st_set_crs(start, 4326) %>% 
  st_join(pv["id_tile"]) 
#Keeping the destination of the flows to join the tile id
end <- mob_tiles
end$Geometry <- st_line_sample(mob_tiles$Geometry, sample = 1) 
#joining the L3 level from the Census 2011
end <- st_set_crs(end, 4326) %>% 
  st_join(pv["id_tile"]) 
#Joining both start and end information for category and sub-district name as per Census 2011 
mob_tiles$start_tile <- start$id_tile
mob_tiles$end_tile <- end$id_tile
#Fixing artbitrarily the 0 values to 5, since it actually just means <10
mob_tiles <- st_drop_geometry(mob_tiles)
mob_tiles[,5:9][mob_tiles[, 5:9] == 0] <- 5

### AGGREGATING TOTAL OUTGOING MOBILITIES BY L3 ADMIN
L3_mob <- aggregate(mob_tiles[5:9], by = list(id_tile=mob_tiles$start_tile), FUN=sum) %>% 
  as_tibble() %>% 
  merge(pv[c("id_tile")]) %>%
  st_as_sf(crs = st_crs(4326)) %>%
  mutate(area = st_area(.$geometry)) %>%
  st_intersection(subd[c("L3_NAME", "L3_CODE")]) %>%
  mutate(ratio_int = st_area(.$geometry)/area)
# Distributing the number of outgoing movements using the pro-rata of overlapping (to switch from the 2km tile boundaries to sub-district boundaries)
L3_mob[2:6] <- (sweep(L3_mob[,2:6], 1, L3_mob$ratio_int,FUN="*"))[1:5] %>% 
  round(1)
L3_mob <- aggregate(L3_mob[c(2:6)], by=list(L3_NAME=L3_mob$L3_NAME), FUN=sum)

### ADDING THE CENTRALITY DEGREES OF ADMINS WITH NETWORK ORIENTED MOBILITIES
# Creating a table, specifying for each flow between tiles which share of the number of people should go to which L3 based on overlapping ratio (both in departure L3 and arrival L3)
coeff_tiles <- st_intersection(pv, subset(subd,L3_NAME %in% mobility_areas)) %>%
  subset(!as.numeric(L3_CODE) == 402) %>%
  mutate(area_int = st_area(.$geometry)) %>%
  mutate(ratio_int_tile = area_int/area)
# Creating the OD table between L3, applying the coefficients of overlapping from the origin and destination tiles
network <- left_join(mob_tiles, st_drop_geometry(coeff_tiles[c("id_tile","ratio_int_tile","L3_NAME")]), by=c("start_tile"="id_tile")) %>% 
  left_join(st_drop_geometry(coeff_tiles[c("id_tile","ratio_int_tile","L3_NAME")]), by=c("end_tile"="id_tile")) %>% 
  mutate(coef_area = ratio_int_tile.x * ratio_int_tile.y)
network[5:9] <- sweep(network[5:9],1, network$coef_area, FUN = "*") %>% 
  round(1)
# Removing areas that are not covered enough
network <- aggregate(network[5:9], by = list(start=network$L3_NAME.x, end=network$L3_NAME.y), FUN=sum) %>%
  subset(start %in% mobility_areas [! mobility_areas %in% c("Rewari","Rohtak","Palwal")]) %>%
  subset(end %in% mobility_areas [! mobility_areas %in% c("Rewari","Rohtak","Palwal")])
# Building the graph and computing various indicators of centrality and connectivity
nodes <- st_drop_geometry(L3_mob) %>% 
  mutate(L3_NAME = stringr::str_trunc(L3_NAME, 5, ellipsis = ""))
links <- network %>% 
  #subset(X2020.02.26.0530>15) %>%
  #set_rownames(1:nrow(links)) %>%
  mutate(start = stringr::str_trunc(start, 5, ellipsis = "")) %>%
  mutate(end = stringr::str_trunc(end, 5, ellipsis = ""))
net <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 
degree(net)
ceb <- cluster_edge_betweenness(net)
membership(ceb)[order(membership(ceb))]
cfg <- cluster_fast_greedy(as.undirected(net))
V(net)$community <- cfg$membership
l <- layout_with_fr(net)
l <- norm_coords(l, ymin=-5, ymax=5, xmin=-5, xmax=5)
plot(cfg, as.undirected(net))
plot(ceb,net, vertex.label=NA, edge.arrow.size=.02)
plot(as.undirected(simplify(net, remove.loops = T)), 
     edge.arrow.size=.02, 
     edge.curved=0.1,
     #vertex.size=V(net)$
     vertex.color=V(net)$community,
     vertex.frame.color="#555555",
     vertex.label=V(net)$L3_NAME, 
     vertex.label.color="black",
     edge.width=log10(E(net)$'2020.02.26.0530'),
     layout=layout_with_graphopt,
     vertex.label.cex=.7)
# Attaching the indicators computed for the "nodes" (=sub-districts) in the network analysis to the L3 table (which still contains the census information of the sub-districts)
L3_mob$degree <- degree(net)
L3_mob$betwenness <- betweenness(net)
L3_mob$eigen <- eigen_centrality(net, weights = sqrt(E(net)$'2020.02.26.0530'))$vector
L3_mob$page <- page_rank(net, weights = E(net)$'2020.02.26.0530')$vector*100
L3_mob$eccentricity <- eccentricity(net)
L3_mob[L3_mob$L3_NAME %in% c("Kharkhoda","Khekada"), "eccentricity"] <- max(L3_mob$eccentricity)+1
L3_mob$cfg <- membership(cfg)

### PCA ON THE SUBDISTRICTS INCLUDING CENSUS DATA AND CENTRALITY INDICATORS
# Creating another dataset for PCA related manipulations
inter2 <- inter %>%
  st_drop_geometry() %>%
  merge(st_drop_geometry(L3_mob[c("L3_NAME", "degree", "betwenness", "page", "eccentricity", "eigen")]))
# Removing two sub-districts still insufficiently covered by movement data (to few users captured in our perimeter)
inter2 <- subset(inter2,!(L3_NAME %in% c("Rewari", "Palwal", "Rohtak")))
# Renaming some columns in a more explicit way
colnames(inter2)[c(20,21,18,36,38)] <- c("HINDUS", "MUSLIMS", "OCCUPATION_RATE", "w_page", "w_eigen")
# Selecting the variables of interest
variables_reduced <- c("P_06", "P_SC", "P_LIT", "MAIN_CL_P", "MAIN_AL_P", "MAIN_HH_P", "MAIN_OT_P", "MARGWORK_P", "OCCUPATION_RATE", "URB_RATE", "F_RATIO", "HINDUS", "MUSLIMS", "HH_GCONDITION", "DENSITY", "RENTED", "SIX_MORE_HH" ,"COMP_INTERNET", "MOBILE", "SCOOTER", "CAR" ,"degree", "betwenness", "w_page", "eccentricity", "w_eigen")
# PCA with a plot for the two main axis, used in Figure 3
res.pca <- PCA(X = inter2[c(variables_reduced)] %>%
                 set_rownames(inter2$L3_NAME), 
               scale.unit = TRUE, 
               ncp = 5, 
               graph = TRUE
)

### FITTING A REGRESSION ON THE LEVEL OF MOVEMENT USING THE CENTRALITY INDICATORS AND CENSUS DATA OF THE SUBDISTRICTS AT EACH TIME STEP 
L3_mob <- merge(inter2, L3_mob[c("L3_NAME", "X2020.02.26.0530", "X2020.03.26.0530", "X2020.05.08.0530", "X2020.05.26.0530", "X2020.06.22.0530")])
# Scaling the density to avoid too much magnitude in the regression
L3_mob <- mutate(L3_mob, DENSITY_c = as.numeric(scale(L3_mob$DENSITY)))
#Converting the total movement values into an index from 0 to 100, based on a pre-lockdown baseline (26 feb)
L3_mob[40:43] <- (sweep(L3_mob[,40:43], 1, L3_mob[,39], "/")*100) %>% 
  round(1)
#Writing the file to map it in a GIS, as in Figure 5
write_csv(L3_mob,"./Subdistrict_movements_base100.csv")



