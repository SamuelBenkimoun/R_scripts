library(sf)
library(tidyverse)
library(leaflet)
library(readxl)
library(igraph)
library(tidygraph)
library(classInt)
library(ggthemes)
library(spdep)
library(spatstat)
library(surveillance)
library(factoextra)
library(brms)

options(scipen = 999)

## Importing the spatial data and setting the coordinates to the same system (4326)
dt <- st_read("/Users/utilisateur/Documents/Charbon/Geo Data/Delhi Urban Area + Flows Analysis/Mobility Tiles With UA 2/tiles_with_ua.shp") %>%
  st_set_crs(3857)
colnames(dt)[5:10] <- c("wd.5h30am", "wd.1h30pm", "wd.9h30pm", "we.5h30am", "we.1h30pm", "we.9h30pm") # Harmonizing column names
nct <- st_read("/Users/utilisateur/Documents/Charbon/Geo Data/NCT_boundaries.gpkg") %>%
  st_transform(4326)
dengue_cases <- st_read("./Documents/Charbon/Geo Data/Dengue_delhi/2008_2009_2010_lite_2.shp")%>%
  st_transform(4326)
rainfall_north <- read.csv2("./Documents/Charbon/Geo Data/delhi north rainfall.csv", sep = ",")
rainfall_south <- read.csv2("./Documents/Charbon/Geo Data/delhi south rainfall.csv", sep = ",")
## Create separate df for each year
dengue_cases$date_deb <- as.Date(dengue_cases$date_deb)
dengue_cases$date_fin <- as.Date(dengue_cases$date_deb)

dengue_2008 <- dengue_cases[format(dengue_cases$date_deb, "%Y") == "2008", ]
dengue_2009 <- dengue_cases[format(dengue_cases$date_deb, "%Y") == "2009", ]
dengue_2010 <- dengue_cases[format(dengue_cases$date_deb, "%Y") == "2010", ]

## Format the people's movement network within the tiles

#Adding some long lat fields, more convenient for mapping
dt <- dt %>%
  mutate(
    origin_lon = map_dbl(geometry, ~st_coordinates(.x)[1, 1]),
    origin_lat = map_dbl(geometry, ~st_coordinates(.x)[1, 2]),
    dest_lon = map_dbl(geometry, ~st_coordinates(.x)[2, 1]),
    dest_lat = map_dbl(geometry, ~st_coordinates(.x)[2, 2])
  )

#Potentiel filtering ?
dt_combined <- dt

# Extracting the coordinates of the edges with admin location and number of people travelling it
edges <- dt_combined %>% st_drop_geometry() %>%
  select(origin_lon, origin_lat, dest_lon, dest_lat, 
         L3_NAME_s, L3_CODE_s, L3_NAME_n,  L3_CODE_n, #subdistrict name of the departure and arrival tile (centroid)
         wd.5h30am, wd.1h30pm, wd.9h30pm, we.5h30am, we.1h30pm, we.9h30pm) #timestep, to adapt accordingly

# Creating the nodes df
nodes <- unique(
  dt_combined %>% st_drop_geometry() %>%
    select(origin_lon, origin_lat, L3_NAME_s) %>%
    rename(lon = origin_lon, lat = origin_lat, subdt = L3_NAME_s)
)

# Generating tiles unique ids based on the coordinates
edges$start_id <- paste0(edges$origin_lon, "-", edges$origin_lat)
edges$end_id <- paste0(edges$dest_lon, "-", edges$dest_lat)
nodes$node_id <- paste0(nodes$lon, "-", nodes$lat)

# Replacing the 0 and under 5 averaged values (below 10 users moving) by an average number of 5
edges[, 9:14] <- lapply(edges[, 9:14], function(x) ifelse(x < 5, 5, x))

## Attaching the socio-economic variables to each tile, taking into account the neighboring environment

# Creating the grid, movement between tiles spatial resolution

resolution <- dt %>% st_transform(4326) %>%
  mutate(length = as.numeric(st_length(geometry))) %>% select("length") %>% st_drop_geometry() %>% min()
#dist_matrix <- st_distance(st_transform(nodes_sf, 4326)) %>% sort(decreasing = FALSE)
#dist_matrix[as.numeric(dist_matrix) > 0]
#resolution = 2148.8

grids <- list()
for (i in 1:nrow(nodes)) {
  lon <- nodes$lon[i]
  lat <- nodes$lat[i]
  # Create a bounding box around the node using the resolution value
  grid_bbox <- st_bbox(c(xmin = lon - resolution/2,
                         xmax = lon + resolution/2,
                         ymin = lat - resolution/2,
                         ymax = lat + resolution/2),
                       crs = st_crs(4326))  # Assuming WGS 84 CRS
  # Create a grid for each node using st_make_grid
  grid <- st_make_grid(st_as_sfc(grid_bbox), cellsize = resolution, square = TRUE)
  # Store the grid
  grids[[i]] <- grid
}

# Combine all grids into a single object for visualization or analysis
all_grids <- do.call(c, grids) 
# Alternate: grid computed in QGIS from the nodes
all_grids <- st_read("./Documents/Charbon/Geo Data/grid_around_FB_nodes_4326.gpkg")
colnames(all_grids)[6] <- "geometry"
st_geometry(all_grids) <- "geometry"

# Plotting the check that the grid matches the nodes
grid_sf <- st_sf(geometry = all_grids, crs = 4326) 
nodes_sf <- st_as_sf(nodes, coords = c("lon", "lat"), crs = 3857) %>% 
  st_transform(4326)
leaflet() %>%
  addTiles() %>%
  addPolygons(data = grid_sf, color = "blue", weight = 1, fillOpacity = 0.2) %>%
  addCircleMarkers(data = nodes_sf, radius = 1, color = "red", fill = TRUE, fillOpacity = 1)
# Giving the node attribute to the grids
grid_sf <- st_join(grid_sf, nodes_sf["node_id"]) 
grid_sf <- select(grid_sf, c("node_id", "geometry"))

## Importing population and household, religion and population data to characterize the tiles
# Households assets
path = "./Documents/Charbon/Geo Data/Census_2011_NCR/Households/Census 2011 Households NCT-UP-HR Subdistricts/"
list_csv <- list.files(path = path)
setwd(path)
hh <- lapply(list_csv, function (x) {
  read_excel(x, skip = 6)
})
# Filtering the information at the most precise scale "Sub-dist" 
hh <- lapply(hh, subset, grepl("Sub-Dist", `9`))
hh <- lapply(hh, subset, `10` == "Total")
hh <- do.call(rbind,hh)
# Retaining only a selected set of variables and renaming the columns
hh <- hh[, c(5,6,12, 13, 14, 23, 30, 40, 42, 46, 49, 61, 62, 63, 64, 65, 72, 73, 82, 83, 85, 91, 92, 93, 96, 102, 106, 107, 108, 109, 115, 121, 127, 130, 133, 134, 136, 137)]
colnames(hh) <- c(
  "L3_CODE","L3_NAME",
  "Hh_Good_condition","Hh_Livable","Hh_Dilapidated",
  "Roof_mud_wood_grass","Roof_concrete",
  "Walls_concrete",
  "Floor_mud", "Floor_cement",
  "No_exclusive_room",
  "6_8_household","9+_household",
  "Owned","Rented","Any_other_status",
  "Tapwater_treated", "Tapwater_untreated",
  "Water_source_within", "Water_source_nearby",
  "Lighting_electricity",
  "Latrine_facility","Latrine_sewer","Latrine_septic","Latrine_open_pit","No_latrine_open",
  "Water_drainage_closed", "Water_drainage_open","No_drainage",
  "Cooking_wood","Cooking_electricity",
  "Kitchen_inside",
  "Banking_service",
  "Computer_internet",
  "Mobile_phone","Mobile_and_landline",
  "Scooter","Car"
)
hh$L3_CODE <- as.numeric(hh$L3_CODE)
hh$Large_households = hh$`9+_household` + hh$`6_8_household`
hh <- hh %>% select(-`9+_household`, -`6_8_household`)


# Religion, computing the ratios by spatial unit for the three main religious groups in the region
setwd("~/Documents/Charbon/Geo Data/Census_2011_NCR/Religion/")
rel <- lapply(list.files(), read_excel)
rel <- lapply(rel, subset, grepl("Sub-District", Name))
rel <- do.call(rbind,rel)
rel <- subset(rel, TRU == "Total") %>%
  subset(Religion %in% c("Total", "Hindu", "Muslim", "Sikh"))
tot_rel <- rel %>%
  filter(Religion == "Total") %>%
  select(State, District, Subdistt, TOT_P) %>%
  rename(TOT_P_Total = TOT_P)
rate_hindus = rel %>% filter(Religion %in% c("Hindu")) %>%
  left_join(tot_rel, by = c("State", "District", "Subdistt")) %>%
  mutate(Ratio_Hindus = TOT_P / TOT_P_Total * 100) %>%
  select(Ratio_Hindus)
rate_muslims = rel %>% filter(Religion %in% c("Muslim")) %>%
  left_join(tot_rel, by = c("State", "District", "Subdistt")) %>%
  mutate(Ratio_Muslims = TOT_P / TOT_P_Total * 100) %>%
  select(Ratio_Muslims)
rate_sikhs = rel %>% filter(Religion %in% c("Sikh")) %>%
  left_join(tot_rel, by = c("State", "District", "Subdistt")) %>%
  mutate(Ratio_Sikhs = TOT_P / TOT_P_Total * 100) %>%
  select(Ratio_Sikhs)

tot_rel <- tot_rel %>% mutate(
  Hindus = rate_hindus$Ratio_Hindus,
  Muslims = rate_muslims$Ratio_Muslims,
  Sikhs = rate_sikhs$Ratio_Sikhs
) %>%
  select("Subdistt", "Hindus", "Muslims", "Sikhs")
colnames(tot_rel)[1] <- "L3_CODE"
tot_rel$L3_CODE <- as.numeric(tot_rel$L3_CODE)


#Importing the L3 (subdistricts) retained in the classification
sdt <- st_read("../../India_L3_2011/India_L3_Administrative_Boundaries.shp")

sdt %>% select(c("L3_CODE", "L3_NAME")) %>%
  merge(hh) %>%
  merge(tot_rel)

#Population, already spatialised and at wards level this time
wards_pop <- st_read("../../WARD 2011/delhi_wards_pop2011_ok.shp")
wards_pop <- st_transform(wards_pop, 4326)
wards_pop <- wards_pop %>%
  select(c("ward", "TOT_P", "TOT_F", "P_06", "P_SC", "P_ST", "P_LIT", "TOT_WORK_P",  "MAINWORK_P",  "MAIN_CL_P",  "MAIN_AL_P", "MAIN_HH_P", "MAIN_OT_P", "MARGWORK_P", "NON_WORK_P", "NON_WORK_M"))
wards_pop <- wards_pop %>%
  mutate(
    F_RATIO = TOT_F / TOT_P *100,
    P_06 = P_06 / TOT_P *100,
    P_SC = P_SC / TOT_P *100,
    P_ST = P_ST / TOT_P *100,
    P_LIT = P_LIT / TOT_P *100,
    NON_WORK_P = NON_WORK_P / TOT_P *100,
    MAIN_CL_P = MAIN_CL_P / TOT_WORK_P *100,
    MAIN_AL_P = MAIN_AL_P / TOT_WORK_P *100,
    MAIN_HH_P = MAIN_HH_P / TOT_WORK_P *100,
    MAIN_OT_P = MAIN_OT_P / TOT_WORK_P *100,
    MARGWORK_P = MARGWORK_P / TOT_WORK_P *100,
    NON_WORK_M = NON_WORK_M / (TOT_P - TOT_F) *100
  )

wards_pop
sdt <-  sdt %>% 
  st_make_valid() %>%
  mutate(sdt_area = st_area(.$geometry))
grid_sf <- grid_sf %>% 
  st_make_valid() %>%
  mutate(grid_area = st_area(.$geometry))

# attributes <- colnames(st_drop_geometry(wards_pop))[2:17]
# wards_inter <-  st_intersection(wards_pop, grid_sf) %>%
#   mutate(inter_area = st_area(.$geometry)) %>%
#   mutate(area_ratio = as.numeric(inter_area/grid_area)) #otherwise it is in units
# wards_inter <- wards_inter %>% mutate(
#   across(c(attributes), ~ . * area_ratio) 
# )
# wards_aggregate <- aggregate(wards_inter[c(attributes, "area_ratio")], 
#                                                       by= list(wards_inter$node_id), FUN = sum)
# 
# colnames(wards_aggregate)[1] <- "node_id"
# wards_aggregate <- wards_aggregate %>% subset(area_ratio > 0.75)

weighted_intersection <- function(grid_sf, wards_pop, attributes, id_col) {
  grid_sf <-  grid_sf %>% mutate(grid_area = st_area(.$geometry))
  wards_inter <- st_intersection(wards_pop, grid_sf) %>%
    mutate(inter_area = st_area(.$geometry)) %>%
    mutate(area_ratio = as.numeric(inter_area / grid_area))
  #Weighting the values based on overlapping surface
  wards_inter <- wards_inter %>%
    mutate(across(all_of(attributes), ~ . * area_ratio))
  #Summing the weighted values
  wards_aggregate <- aggregate(wards_inter[c(attributes, "area_ratio")], 
                               by = list(wards_inter[[id_col]]), FUN = sum)
  #Rename the id column to merge the file easily later on
  colnames(wards_aggregate)[1] <- id_col
  #Keeping those tiles above the threshold, to avoid having some which are not overlapping enough with the attributes layer
  area_ratio_threshold = 0.75
  wards_aggregate <- wards_aggregate %>%
    subset(area_ratio > area_ratio_threshold)
  return(wards_aggregate)
}
attributes <- colnames(st_drop_geometry(wards_pop))[2:17]
ww <- weighted_intersection(grid_sf, wards_pop, attributes, "node_id")
sdt_hh <- sdt[c("L3_CODE", "L3_NAME")] %>% merge(hh)
attributes <- colnames(st_drop_geometry(sdt_hh))[3:37]
ws <- weighted_intersection(grid_sf, sdt_hh, attributes, "node_id")
w <- left_join(st_drop_geometry(ww), st_drop_geometry(ws), by="node_id")
# Now the grid is ready with all the socio-demographic variables
grid_sf <- st_join(grid_sf, nodes_sf["subdt"]) %>%
  select(c("node_id", "subdt", "geometry"))
grid_weighted <- left_join(w, grid_sf, by="node_id")
grid_weighted <- grid_weighted %>% st_as_sf()

## ADDING THE MOVEMENT DATA INTO THE TILES

## ADDING THE DENGUE CASES INTO THE TILES
# Function to count the number of cases by tiles
count_cases <- function (surface, points, id_field, count_field) {
  count <- points %>% 
    st_join(surface[c("node_id")]) %>% #the id column has to be explicitly named "node_id"
    st_drop_geometry() %>%
    group_by(node_id) %>%
    count()
  colnames(count)[2] <- count_field
  return(count)
}
  
# Be sure that the identifer is called "node_id" for it to sum properly the nomber of cases
cases_total <- count_cases(grid_weighted, dengue_cases, count_field = "dengue_total")
cases_08 <- count_cases(grid_weighted, dengue_2008, count_field = "dengue_08")
cases_09 <- count_cases(grid_weighted, dengue_2009, count_field = "dengue_09")
cases_10 <- count_cases(grid_weighted, dengue_2010, count_field = "dengue_10")

cases_combined <- left_join(cases_total, cases_08, by = "node_id") %>%
  left_join(cases_09, by = "node_id") %>%
  left_join(cases_10, by = "node_id")

grid_weighted <- left_join(grid_weighted, cases_combined, by= "node_id") %>% 
  tidyr::replace_na(
  list(dengue_total = 0,
       dengue_08 = 0,
       dengue_09 = 0,
       dengue_10 = 0)) 

# Computing the incidences for 100 000 inhabitants
grid_weighted <- grid_weighted %>% mutate(
  incidence_total = round((dengue_total / TOT_P) * 100000,1),
  incidence_08 = round((dengue_08 / TOT_P) * 100000,1),
  incidence_09 = round((dengue_09 / TOT_P) * 100000,1),
  incidence_10 = round((dengue_10 / TOT_P) * 100000,1)
)
grid_weighted <- grid_weighted %>% mutate(
  incidence_average = grid_weighted %>% 
    st_drop_geometry() %>% 
    select(c(incidence_08, incidence_09, incidence_10)) %>% 
    rowMeans()
)

## Adding the movement flows to the tiles and classification based on the profiles
dt_combined
edges
nodes
grid_weighted

# graphe total ?
# graph de la zone d'étude
nodes_nct <- st_drop_geometry(grid_weighted)
edges_nct <- edges %>% subset(start_id %in% nodes_nct$node_id & end_id %in% nodes_nct$node_id)
edges_nct <- edges_nct %>% mutate(
  total_volume = wd.5h30am + wd.1h30pm + wd.9h30pm + we.5h30am + we.1h30pm + we.9h30pm
)

g <- graph_from_data_frame(edges_nct %>% 
                             select(start_id, end_id), directed = TRUE) ## if undirected are the flows summed ?
E(g)$weight <- edges_nct$total_volume # Weighing with the total volumen, to be possibly adapted
V(g)$subdt <- nodes_nct$subdt[match(V(g)$name, nodes_nct$node_id)]

# Computing centrality indices and making a Jenks classification to make it discrete and observe hubs ?
centrality_proximity_tot <- closeness(g, mode="all", weights = 1 / E(g)$weight) # Weight has to be the invert of number of people moving, because it is interpreted as a cost/distance
degree_centrality_in_tot <- degree(g, mode = "in")
degree_centrality_out_tot <- degree(g, mode = "out") 
degree_centrality_all_tot <- degree(g, mode = "all")
reach2 <- ego_size(g, 2)-1 #number of second degree links a node has
centrality_betwenness_flat <- betweenness(g, directed = TRUE, weights = NA) #number of shortest path going through the node
centrality_betwenness_weighted <- betweenness(g, directed = TRUE, weights = 1 / E(g)$weight) #here weights are taken as distance, so the invert is needed
pagerank_scores_flat <- page_rank(g, directed = TRUE, weights = NA)
pagerank_scores_weighted <- page_rank(g, directed = TRUE, weights = E(g)$weight) # Weighted page rank algorithm, where weights means strength of connexion
eigenvector_score_flat <- eigen_centrality(g, directed = TRUE, weights = NA)
eigenvector_score_w_undirected <- eigen_centrality(as.undirected(g, mode = "collapse", edge.attr.comb = "sum"), 
                                               directed = FALSE, 
                                               weights = E(as.undirected(g, mode = "collapse", edge.attr.comb = "sum"))$weight) # Weighted eigenvector algorithm, where weights means strength of connexion

# Making discrete classes for all the centrality indicators, with Jenks method
centralities <- list(
  centrality_proximity_tot,
  degree_centrality_in_tot,
  degree_centrality_out_tot,
  degree_centrality_all_tot,
  reach2,
  centrality_betwenness_flat,
  centrality_betwenness_weighted,
  pagerank_scores_flat$vector,    # PageRank returns a list, we need the 'vector'
  pagerank_scores_weighted$vector, 
  eigenvector_score_flat$vector, 
  eigenvector_score_w_undirected$vector
)

names(centralities) <- c(
  "proximity_tot", "degree_in", "degree_out", "degree_all", "reach2", 
  "betweenness_flat", "betweenness_weighted", 
  "pagerank_flat", "pagerank_weighted", 
  "eigenvector_flat", "eigenvector_weighted"
)

centralities <- as.data.frame(centralities)
jenks_classes <- lapply(centralities, function(x) {
  ci <- classIntervals(x, n = 7, style = "jenks") # discretisation using 7 classes
  cut(x, breaks = ci$brks, include.lowest = TRUE, labels = FALSE)
  }) 
jenks_classes
names(jenks_classes) <- paste0(names(jenks_classes), "_jenks")
jenks_classes <- as.data.frame(jenks_classes)
centralities_discrete <- cbind(centralities, jenks_classes)
# Correcting the rownames (who were set on the node_id attribute)
centralities_discrete$node_id <- row.names(centralities_discrete)
row.names(centralities_discrete) <- seq_len(nrow(centralities_discrete))
# Writing all the commands to attach the centrality indicators as attributs of the graph vertices
i=1
for(i in 1:ncol(centralities_discrete)) {
command <-  paste("V(g)$",
        colnames(centralities_discrete)[i], 
        "<- centralities_discrete$", 
        colnames(centralities_discrete)[i], sep = "")
print(command)
eval(parse(text = command))
}
# Checking that all the attributes have been attached correctly
vertex_attr(g)
setdiff(V(g)$name, V(g)$node_id)
# Attaching it to the nodes df too
nodes_nct <- merge(nodes_nct, centralities_discrete)
# Louvain community detection
#E(g)$weight <- edges_nct$wd.5h30am
E(g)$weight <- edges_nct$total_volume
g_undirected <- as.undirected(g, mode = "collapse", edge.attr.comb = "mean") #Mean, can be tested with sums ? given two flows max, no difference ?
communities_louvain <- cluster_louvain(g_undirected, weights = E(g_undirected)$weight) #weights here mean stronger connexion
V(g_undirected)$community <- communities_louvain$membership
communities_louvain
modularity(g_undirected, V(g_undirected)$community, weights = E(g_undirected)$weight)


#communities NCT; add "../../Community_detection_Louvain_Composante1_NCR.csv" for comp1 ncr version
#flux significatifs ? améliore la modularité ?

# Trying with filtering the main flows
# Selecting only the main flow outgoing for each tile (cf: script Chap 4)
dt_filtered <- dt %>%
  group_by(origin_lon, origin_lat) %>%
  slice_max(wd.5h30am, n = 1, with_ties = FALSE) %>% # TO MODIFY IN CASE NEW MAP NEEDED
  ungroup()
dt_high_flows <- dt %>%
  filter(wd.5h30am > 60) # TO MODIFY IN CASE NEW MAP NEEDED
dt_combined <- bind_rows(dt_filtered, dt_high_flows) %>% # TO RUN IN CASE NEW MAP NEEDED
  distinct()   # Remove duplicates that might exist between the two datasets
edges_filtered <- dt_combined %>% st_drop_geometry() %>%
  select(origin_lon, origin_lat, dest_lon, dest_lat, 
         L3_NAME_s, L3_CODE_s, #subdistrict name of the departure and arrival tile (centroid)
         wd.5h30am) #timestep, to adapt accordingly
nodes_filtered <- unique(
  dt_combined %>% st_drop_geometry() %>%
    select(origin_lon, origin_lat, L3_NAME_s) %>%
    rename(lon = origin_lon, lat = origin_lat, subdt = L3_NAME_s)
)
edges_filtered$start_id <- paste0(edges_filtered$origin_lon, "-", edges_filtered$origin_lat)
edges_filtered$end_id <- paste0(edges_filtered$dest_lon, "-", edges_filtered$dest_lat)
edges_filtered <- edges_filtered %>% subset(start_id %in% nodes_nct$node_id & end_id %in% nodes_nct$node_id)
nodes_filtered$node_id <- paste0(nodes_filtered$lon, "-", nodes_filtered$lat)
nodes_filtered <- nodes_filtered %>% subset(node_id %in% nodes_nct$node_id)
g_filtered <- graph_from_data_frame(edges_filtered %>% 
                             select(start_id, end_id), directed = TRUE)
g_filtered_undirected <- as.undirected(g_filtered, mode = "collapse", edge.attr.comb = "mean") #Mean, can be tested with sums ? given two flows max, no difference ?
communities_louvain_filtered <- cluster_louvain(g_filtered_undirected, weights = E(g_filtered_undirected)$weight) #weights here mean stronger connexion
V(g_filtered_undirected)$community <- communities_louvain_filtered$membership
communities_louvain_filtered
modularity(g_filtered_undirected, V(g_filtered_undirected)$community, weights = E(g_filtered_undirected)$weight)

#Attaching the different communities to the nodes 
setwd("~/Documents/Charbon/REDAC/Figures/Community Detection FB Delhi/")
#NCRComp1 unfiltered
communities_louvain_comp1NCR_unfiltered <- read_csv("Community_detection_Louvain_Composante1_NCR_Unfiltered.csv") %>%
  mutate(
    lon = as.numeric(sub("^(.*?)-.*$", "\\1", name)),
    lat = as.numeric(sub("^.*?-(.*)$", "\\1", name))
  ) %>%  
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(grid_sf) %>%
  subset(node_id %in% grid_weighted$node_id) %>%
  st_drop_geometry() %>%
  select(c(node_id, community))
#NCRComp1 filtered >60 + top1 outgoing for each tile
communities_louvain_comp1NCR_filtered <- read_csv("Community_detection_Louvain_Composante1_NCR.csv") %>%  
  mutate(
  lon = as.numeric(sub("^(.*?)-.*$", "\\1", name)),
  lat = as.numeric(sub("^.*?-(.*)$", "\\1", name))
) %>%  
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_join(grid_sf) %>%
  subset(node_id %in% grid_weighted$node_id) %>%
  st_drop_geometry() %>%
  select(c(node_id, community))
#NCT unfiltered
communities_louvain <- vertex_attr(g_undirected) %>% as.data.frame() %>% select(c(node_id, community))
#NCT filtered >60 + top1 outgoing for each tile
communities_louvain_filtered <-  vertex_attr(g_filtered_undirected) %>% as.data.frame() %>% select(c(name, community)) %>% rename(node_id = name)
#Merging it all
communities <- left_join(communities_louvain, communities_louvain_filtered, by="node_id") %>%
  left_join(communities_louvain_comp1NCR_unfiltered, by="node_id") %>%
  left_join(communities_louvain_comp1NCR_filtered, by="node_id")
colnames(communities) <- c("node_id", "louvain_tot_nct", "louvain_red_nct", "louvain_tot_comp1", "louvain_red_comp1")
communities

# Attaching it to the nodes df too
nodes_nct <- merge(nodes_nct, communities)

## Hclust based on incoming/outgoing profile during the day
edges_nct
# Data formatting and computing certain indcators
total_incoming <- edges_nct %>%
  select(c(end_id, wd.5h30am, wd.1h30pm, wd.9h30pm, we.5h30am, we.1h30pm, we.9h30pm)) %>%
  group_by(end_id) %>%
  summarise(
    wd.5h30am_incoming = sum(wd.5h30am, na.rm = TRUE),
    wd.1h30pm_incoming = sum(wd.1h30pm, na.rm = TRUE),
    wd.9h30pm_incoming = sum(wd.9h30pm, na.rm = TRUE),
    we.5h30am_incoming = sum(we.5h30am, na.rm = TRUE),
    we.1h30pm_incoming = sum(we.1h30pm, na.rm = TRUE),
    we.9h30pm_incoming = sum(we.9h30pm, na.rm = TRUE)
  ) %>%
  rename(node_id = end_id)
total_outgoing <- edges_nct %>%
  select(c(start_id, wd.5h30am, wd.1h30pm, wd.9h30pm, we.5h30am, we.1h30pm, we.9h30pm)) %>%
  group_by(start_id) %>%
  summarise(
    wd.5h30am_outgoing = sum(wd.5h30am, na.rm = TRUE),
    wd.1h30pm_outgoing = sum(wd.1h30pm, na.rm = TRUE),
    wd.9h30pm_outgoing = sum(wd.9h30pm, na.rm = TRUE),
    we.5h30am_outgoing = sum(we.5h30am, na.rm = TRUE),
    we.1h30pm_outgoing = sum(we.1h30pm, na.rm = TRUE),
    we.9h30pm_outgoing = sum(we.9h30pm, na.rm = TRUE)
  ) %>%
  rename(node_id = start_id)
ag_movement <- left_join(total_outgoing, total_incoming, by = "node_id")
ag_movement <- ag_movement %>% mutate_all(~ ifelse(is.na(.), 1, .)) # Replacing the missing values, for the spatial unit that is in none other destination

ag_movement <- ag_movement %>% mutate(
  wd.5h30am_diff = (wd.5h30am_incoming - wd.5h30am_outgoing),
  wd.1h30pm_diff = (wd.1h30pm_incoming - wd.1h30pm_outgoing),
  wd.9h30pm_diff = (wd.9h30pm_incoming - wd.9h30pm_outgoing),
  we.5h30am_diff = (we.5h30am_incoming - we.5h30am_outgoing),
  we.1h30pm_diff = (we.1h30pm_incoming - we.1h30pm_outgoing),
  we.9h30pm_diff = (we.9h30pm_incoming - we.9h30pm_outgoing)
)
ag_movement <- ag_movement %>% mutate(
  wd.5h30am_diff_rat = (wd.5h30am_incoming - wd.5h30am_outgoing)/wd.5h30am_outgoing*100,
  wd.1h30pm_diff_rat = (wd.1h30pm_incoming - wd.1h30pm_outgoing)/wd.5h30am_outgoing*100,
  wd.9h30pm_diff_rat = (wd.9h30pm_incoming - wd.9h30pm_outgoing)/wd.5h30am_outgoing*100,
  we.5h30am_diff_rat = (we.5h30am_incoming - we.5h30am_outgoing)/wd.5h30am_outgoing*100,
  we.1h30pm_diff_rat = (we.1h30pm_incoming - we.1h30pm_outgoing)/wd.5h30am_outgoing*100,
  we.9h30pm_diff_rat = (we.9h30pm_incoming - we.9h30pm_outgoing)/wd.5h30am_outgoing*100
)
ag_movement <- merge(ag_movement, nodes_nct[c("node_id", "TOT_P")])
ag_movement <- ag_movement %>% mutate(
  wd.5h30am_openness = (wd.5h30am_incoming + wd.5h30am_outgoing)/TOT_P*100,
  wd.1h30pm_openness = (wd.1h30pm_incoming + wd.1h30pm_outgoing)/TOT_P*100,
  wd.9h30pm_openness = (wd.9h30pm_incoming + wd.9h30pm_outgoing)/TOT_P*100,
  we.5h30am_openness = (we.5h30am_incoming + we.5h30am_outgoing)/TOT_P*100,
  we.1h30pm_openness = (we.1h30pm_incoming + we.1h30pm_outgoing)/TOT_P*100,
  we.9h30pm_openness = (we.9h30pm_incoming + we.9h30pm_outgoing)/TOT_P*100
)
ag_movement <- ag_movement %>% mutate(
  total_openness = wd.5h30am_openness + wd.1h30pm_openness + wd.9h30pm_openness + we.5h30am_openness + we.1h30pm_openness + we.9h30pm_openness
)
ag_movement <- ag_movement %>% mutate(
  wd.5h30am_openness_incoming = (wd.5h30am_incoming)/TOT_P*100,
  wd.1h30pm_openness_incoming = (wd.1h30pm_incoming)/TOT_P*100,
  wd.9h30pm_openness_incoming = (wd.9h30pm_incoming)/TOT_P*100,
  we.5h30am_openness_incoming = (we.5h30am_incoming)/TOT_P*100,
  we.1h30pm_openness_incoming = (we.1h30pm_incoming)/TOT_P*100,
  we.9h30pm_openness_incoming = (we.9h30pm_incoming)/TOT_P*100
)
# K-means classification to identify different profiles based on movement activity
d <- ag_movement[34:39]
d <- scale(d, center=T,scale=T) 
# Creating the groups (here 6) with k-means methods
groupes.kmeans <- kmeans(d,centers=7,nstart=5)
print(groupes.kmeans)
classes1 <- groupes.kmeans$centers %>%
  as.data.frame()
# create the neighbouring network based on movement
classes1$class <- c(
  paste('Classe 1', " (", groupes.kmeans$size[1], ")"),
  paste('Classe 2', " (", groupes.kmeans$size[2], ")"),
  paste('Classe 3', " (", groupes.kmeans$size[3], ")"),
  paste('Classe 4', " (", groupes.kmeans$size[4], ")"),
  paste('Classe 5', " (", groupes.kmeans$size[5], ")"),
  paste('Classe 6', " (", groupes.kmeans$size[6], ")"),
  paste('Classe 7', " (", groupes.kmeans$size[7], ")")
  #paste('Class 8', " (", groupes.kmeans$size[8], ")")
)
classes1_long <- gather(classes1, variable, value, -class)
classes1_long$variable <- sapply(strsplit(classes1_long$variable, "_"), `[`, 1) #shortening the variable names
classes1_long$variable <- factor(classes1_long$variable, 
                                 levels = c("wd.5h30am", "wd.1h30pm", "wd.9h30pm", 
                                            "we.5h30am", "we.1h30pm", "we.9h30pm"))

plot <- ggplot(subset(classes1_long, class != "Class 3  ( 2 )")) +
  geom_bar(aes(x = variable, y = value, fill = class),
           stat = "identity") +
  facet_wrap(~ class) +
  geom_vline(xintercept=3.5)+
  theme_minimal()+ 
  #scale_colour_wsj("colors6")+
  theme(strip.text = element_text(size=11, face = "bold"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        #panel.grid.major = element_line(color="grey"),
        legend.position = "none",
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=9.5, face="bold"),
        axis.text.x = element_text(size=9.5, face="bold", angle = 45, hjust = 1))+
  xlab("") + 
  ylab("Mouvements entrants / population (en écart-type)")+
  scale_fill_brewer(palette = "Set2")
plot(plot)
# Bringing the class number to each node
ag_movement$kmeans_incoming_openes <- groupes.kmeans$cluster
nodes_nct <- left_join(nodes_nct, ag_movement, by="node_id")

## Clustering of dengue cases
dengue_cases <- dengue_cases %>% st_join(grid_weighted[c("node_id", "geometry")]) 
dengue_cases_grid <- dengue_cases[!is.na(dengue_cases$node_id), ]

#Function needed to extract the results in a table format
extract_knox_info <- function(knox_result, distance) { 
  return(data.frame(
    Distance = distance,
    Statistic = knox_result$statistic,
    Expected_null = knox_result$null.value,
    RR = knox_result$statistic %>% as.numeric()/knox_result$null.value %>% as.numeric(),
    P_Value = knox_result$p.value,
    stringsAsFactors = FALSE
  ))
}
# Formating the dates in days since origin format for the tests
dengue_cases_grid$date_deb <- as.Date(dengue_cases_grid$date_deb)
origin <- min(dengue_cases_grid$date_deb)
dengue_cases_grid$days_since_origin <- as.numeric(difftime(dengue_cases_grid$date_deb, origin, units = "days"))
dengue_08 <-dengue_cases_grid %>% subset(year==2008)
dengue_09 <- dengue_cases_grid %>% subset(year==2009)
dengue_10 <- dengue_cases_grid %>% subset(year==2010)
time_dist <- dist(dengue_10$days_since_origin)
space_dist <- dist(st_coordinates(st_transform(dengue_10,32643))) #okayish crs for meter distance calculation, and keeping the "dist()" function (st_distance returns all the pairs)
#space_dist <- st_distance(dengue_cases_grid) %>% as.numeric()

parallel::detectCores() # to check how many cores are available

knoxtest <- knox(
  dt = time_dist, eps.t = 21,
  ds = space_dist, eps.s = 2000,
  simulate.p.value = TRUE, 
  B = 300, #number of Monte-Carlo
  .parallel = 5, # TO BE ADAPTED BASED ON THE NUMBER OF AVAILABLE CORES ON THE MACHINE !!
  .seed = 1, .verbose = TRUE
)
knoxtest
knoxtest_all_euc_750 <- knoxtest #500m 21j
knoxtest_all_euc_500 <- knoxtest #500m 21j
knoxtest_all_euc_400 <- knoxtest #400m 21j
knoxtest_all_euc_300 <- knoxtest #300m 21j
knoxtest_all_euc_200 <- knoxtest #200m 21j
knoxtest_all_euc_100 <- knoxtest #100m 21j
knoxtest_all_euc_50 <- knoxtest #50m 21j
knoxtest_all_euc_25 <- knoxtest #25m 21j

results_list <- list()
results_list[[1]] <- extract_knox_info(knoxtest_all_euc_500, "500m")
results_list[[2]] <- extract_knox_info(knoxtest_all_euc_400, "400m")
results_list[[3]] <- extract_knox_info(knoxtest_all_euc_300, "300m")
results_list[[4]] <- extract_knox_info(knoxtest_all_euc_200, "200m")
results_list[[5]] <- extract_knox_info(knoxtest_all_euc_100, "100m")
results_list[[6]] <- extract_knox_info(knoxtest_all_euc_50, "50m")
results_list[[7]] <- extract_knox_info(knoxtest_all_euc_25, "25m")

final_results <- do.call(rbind, results_list)
final_results
#final_results_eucdist_08 <- final_results
#final_results_eucdist_09 <- final_results
final_results_eucdist_10 <- final_results
write.csv(final_results, "./Dengue Sp Analysis These Redac/knoxtest_results_euc_distance_10.csv", row.names = FALSE)

#rescaling the weights based on physical max distance, so that the smallest people flow = max dist
max_dist <- subset(dt, dt$L3_NAME_s %in% edges_nct$L3_NAME_s & dt$L3_NAME_n %in% edges_nct$L3_NAME_n) %>% st_transform(32643) %>% st_length() %>% max() %>% as.numeric()
min_dist <- subset(dt, dt$L3_NAME_s %in% edges_nct$L3_NAME_s & dt$L3_NAME_n %in% edges_nct$L3_NAME_n) %>% st_transform(32643) %>% st_length() %>% min() %>% as.numeric()

E(g)$weight %>% median() 
E(g)$weight %>% sd()
threshold = quantile(E(g)$weight)[4] %>% as.numeric()
k = 300/(1/threshold)
dist_matrix <- igraph::distances(g, weights = 1/E(g)$weight*k)
pairwise_matrix <- expand.grid(row = rownames(dist_matrix), col = colnames(dist_matrix))
pairwise_matrix$distance <- as.vector(dist_matrix)

# getting all the combinations to attach the distance
pairwise <- combn(dengue_cases_grid$node_id, 2, simplify = FALSE, .parallel =6)
pairwise <- do.call(rbind, lapply(pairwise, function(x) data.frame(row = x[1], col = x[2], stringsAsFactors = FALSE)))

space_dist <- pairwise %>%
  left_join(pairwise_matrix, by = c("row" = "row", "col" = "col"))

### START OVER FROM HERE TO RUN THE TEST AGAIN
dengue_08_unique <- dengue_08 %>%
  group_by(node_id) %>%
  slice_min(date_deb) %>%  # Keep the row with the earliest date_deb
  slice_min(order_by = OBJECTID, with_ties = FALSE) %>% # In case two cases are reported at the same date in a same tile
  ungroup()
#time_dist <- dist(dengue_cases_grid$days_since_origin)
time_dist <- dist(dengue_08_unique$days_since_origin)
pairwise <- combn(dengue_08_unique$node_id, 2, simplify = FALSE, .parallel =6)
pairwise <- do.call(rbind, lapply(pairwise, function(x) data.frame(row = x[1], col = x[2], stringsAsFactors = FALSE)))
space_dist <- pairwise %>%
  left_join(pairwise_matrix, by = c("row" = "row", "col" = "col"))

## TO ADDITIONNALY PUNDERATE THE DISTANCE WITH THE FORCE OF INFECTION
tile_annual_count_08 <- dengue_08["node_id"] %>% 
  group_by(node_id) %>% 
  count() %>% 
  ungroup() %>% 
  rename(cases = n,
         row = node_id)
space_dist <- left_join(space_dist, tile_annual_count_08, by = c("row")) %>%
  mutate(cases = ifelse(is.na(cases), 0, cases)) %>%
  mutate(dist_foi_log = distance / log(cases + 1.1), #with log function ?
         dist_foi = distance / (sqrt(cases)+0.1)) 


# Formule = 1/nb_people*39600, so that top 25% flows = 300m

knoxtest <- knox(
  dt = time_dist, eps.t = 21,
  ds = space_dist$dist_foi, eps.s =20,
  simulate.p.value = TRUE, B = 999,  #number of Monte-Carlo replication
  .parallel = 12, .seed = 1, .verbose = TRUE
)
knoxtest
plot(knoxtest)
knoxtest_1000_mov <- knoxtest
knoxtest_500_mov <- knoxtest
knoxtest_300_mov <- knoxtest #equivalent 300m
knoxtest_200_mov <- knoxtest #equivalent 200m
knoxtest_100_mov <- knoxtest #equivalent 100m p-value 0.04
knoxtest_75_mov <- knoxtest #equivalent 75m p-value 0.01
knoxtest_50_mov <- knoxtest #equivalent 50m (+- 800 people moving) p-value 0.01
knoxtest_30_mov <- knoxtest #equivalent 30m (+-) p-value 0.01
knoxtest_20_mov <- knoxtest #equivalent 20m (+-) p-value 0.01
knoxtest_15_mov <- knoxtest #equivalent 15m (+-) p-value 0.01
knoxtest_10_mov <- knoxtest #equivalent 10m (+-) p-value 0.01
knoxtest_5_mov <- knoxtest #equivalent 10m (+-) p-value 0.01, RR=1,16 environ
knoxtest_2.5_mov <- knoxtest #equivalent 10m (+-) p-value 0.01, RR=1,16 environ


results_list <- list()

results_list[[1]] <- extract_knox_info(knoxtest_1000_mov, "1000m")
results_list[[2]] <- extract_knox_info(knoxtest_500_mov, "500m")
results_list[[3]] <- extract_knox_info(knoxtest_300_mov, "300m")
results_list[[4]] <- extract_knox_info(knoxtest_200_mov, "200m")
results_list[[5]] <- extract_knox_info(knoxtest_100_mov, "100m")
results_list[[6]] <- extract_knox_info(knoxtest_75_mov, "75m")
results_list[[7]] <- extract_knox_info(knoxtest_50_mov, "50m")
results_list[[8]] <- extract_knox_info(knoxtest_30_mov, "30m")
results_list[[9]] <- extract_knox_info(knoxtest_20_mov, "20m")
results_list[[10]] <- extract_knox_info(knoxtest_15_mov, "15m")
results_list[[11]] <- extract_knox_info(knoxtest_10_mov, "10m")
results_list[[12]] <- extract_knox_info(knoxtest_5_mov, "5m")
results_list[[13]] <- extract_knox_info(knoxtest_2.5_mov, "2.5m")

final_results <- do.call(rbind, results_list)
final_results
write.csv(final_results, "./Dengue Sp Analysis These Redac/knoxtest_results_movement_unique_grid_logfoi_10.csv", row.names = FALSE)

## PLOT THE RR
final_results_eucdist_08
final_results_eucdist_09
final_results_eucdist_10

# Adding a year column
final_results_eucdist_08 <- final_results_eucdist_08 %>% mutate(Year = "2008") 
final_results_eucdist_09 <- final_results_eucdist_09 %>% mutate(Year = "2009")
final_results_eucdist_10 <- final_results_eucdist_10 %>% mutate(Year = "2010")
# Combining the three df
df_combined <- bind_rows(final_results_eucdist_08, final_results_eucdist_09, final_results_eucdist_10)


df_combined <- bind_rows(final_results_movement_unique_08 %>% select(c("Distance", "RR", "P_Value")) %>% mutate(Year = "2008"),
                         final_results_movement_unique_09 %>% select(c("Distance", "RR", "P_Value")) %>% mutate(Year = "2009"),
                         final_results_movement_unique_10 %>% select(c("Distance", "RR", "P_Value")) %>% mutate(Year = "2010")
) %>% na.omit()


df_combined <- bind_rows(final_results_movement_unique_distfoi_08 %>% mutate(Year = "2008"),
                         final_results_movement_unique_distfoi_09 %>% mutate(Year = "2009"),
                         final_results_movement_unique_distfoi_10 %>% mutate(Year = "2010")) 

df_combined$Distance <- as.numeric(gsub("m", "", df_combined$Distance))

# Creating the graph for 
ggplot(df_combined, aes(x = Distance, y = RR, color = Year)) +
  geom_line(size = 1) +
  #geom_point(size = 2) +
  labs(title = "Risk Ratio (RR) en fonction de la distance potentielle d'infection",
       subtitle = "Par rapport aux cas index. P-values indiquées pour chaque point de mesure.",
       x = "Distance",
       y = "Risk Ratio (RR)") +
  geom_hline(yintercept = 1, linetype = "dashed") +
  geom_text(aes(label = round(`P_Value`, 3)), vjust = -0.5, size = 3.5) +
  scale_color_manual(values = c("2008" = "seagreen", "2009" = "royalblue", "2010" = "red")) +
  #scale_y_continuous(limits = c(1, 1.85)) +
  theme_linedraw()+
  theme(legend.title = element_blank())+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) 

## Checking the temporal dynamics
nodes_nct$is_hub_prox <- ifelse(nodes_nct$proximity_tot_jenks == 7, 1, 0)
nodes_nct$is_hub_vol_res <- ifelse(nodes_nct$wd.9h30pm_incoming > 2500, 1, 0)
nodes_nct$is_hub_vol_work <- ifelse(nodes_nct$wd.5h30am_incoming > 2500, 1, 0)

dengue_cases_grid <- merge(dengue_cases_grid, nodes_nct[c("node_id", "is_hub_prox", "is_hub_vol_res", "is_hub_vol_work")])

# Extract year from the date
dengue_cases_grid$year <- format(dengue_cases_grid$date_deb, "%Y")

# Group by year and count the number of cases
cases_per_year <- dengue_cases_grid %>%
  group_by(year) %>%
  summarise(num_cases = n())

daily_cases <- dengue_cases_grid %>%
  group_by(date_deb, year) %>%
  summarise(num_cases = n()) %>%
  ungroup()

# Calculate daily number of cases where is_hub_prox == 1 (hub-infected cases)
daily_hub_cases <- dengue_cases_grid %>%
  filter(is_hub_vol_work == 1) %>%
  group_by(year, date_deb) %>%
  summarise(num_hub_cases = n()) %>%
  ungroup()

# Merge the daily cases with the daily hub-infected cases
daily_cases <- daily_cases %>%
  left_join(daily_hub_cases, by = c("date_deb", "year")) %>%
  # Replace NA values in num_hub_cases with 0 (for days with no hub-infected cases)
  mutate(num_hub_cases = ifelse(is.na(num_hub_cases), 0, num_hub_cases))

# Plot the daily number of cases and the daily hub-infected cases
ggplot(daily_cases, aes(x = date_deb)) +
  # First curve: daily number of cases (in blue)
  geom_line(aes(y = num_cases), color = "blue", size = 1) +
  # Second curve: daily number of hub-infected cases (in red)
  geom_line(aes(y = num_hub_cases), color = "red", size = 1) +
  labs(title = "Daily Dengue Cases and Daily Hub Infected Cases by Year",
       x = "Date",
       y = "Number of Cases") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Daily Hub Infected Cases")) + # Secondary y-axis
  theme_minimal() +  # Clean theme
  facet_wrap(~ year, scales = "free_x", ncol = 1) +  # Facet per year, free x-axis scales
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Tilt x-axis labels


# Arrondir les dates à la semaine
dengue_cases_grid$week <- floor_date(dengue_cases_grid$date_deb, unit = "week")

# Extraire l'année de la date
dengue_cases_grid$year <- format(dengue_cases_grid$date_deb, "%Y")

# Regrouper par semaine et par année pour compter le nombre total de cas de dengue
weekly_cases <- dengue_cases_grid %>%
  group_by(week, year) %>%
  summarise(num_cases = n(), .groups = 'drop')

# Calculer le nombre hebdomadaire de cas où is_hub_prox == 1 (cas infectés des hubs)
weekly_hub_cases <- dengue_cases_grid %>%
  filter(is_hub_vol_work == 1) %>%
  group_by(year, week) %>%
  summarise(num_hub_cases = n(), .groups = 'drop')

# Fusionner les cas hebdomadaires avec les cas infectés des hubs
weekly_cases <- weekly_cases %>%
  left_join(weekly_hub_cases, by = c("week", "year")) %>%
  # Remplacer les valeurs NA dans num_hub_cases par 0 (pour les semaines sans cas infectés des hubs)
  mutate(num_hub_cases = ifelse(is.na(num_hub_cases), 0, num_hub_cases))

# Créer le graphique des cas hebdomadaires et des cas infectés des hubs
ggplot(weekly_cases, aes(x = week)) +
  # Première courbe : nombre hebdomadaire de cas (en bleu)
  geom_line(aes(y = num_cases), color = "blue", size = 1) +
  # Deuxième courbe : nombre hebdomadaire de cas infectés des hubs (en rouge)
  geom_line(aes(y = num_hub_cases), color = "red", size = 1) +
  labs(title = "Cas Hebdomadaires de Dengue et Cas Hebdomadaires de Hubs Infectés par Année",
       x = "Semaine",
       y = "Nombre de Cas") +
  scale_y_continuous(sec.axis = sec_axis(~., name = "Cas Hebdomadaires de Hubs Infectés")) + # Axe secondaire
  theme_minimal() +  # Thème minimal
  facet_wrap(~ year, scales = "free_x", ncol = 1) +  # Facette par année, échelles x libres
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold"))  # Incliner les étiquettes de l'axe x

# Replacing some NA values by 0 (here the tiles that are not part of the first component of the wider regional graph)
nodes_nct$louvain_red_comp1[is.na(nodes_nct$louvain_red_comp1)] <- 0
nodes_nct$louvain_tot_comp1[is.na(nodes_nct$louvain_tot_comp1)] <- 0
nodes_nct$TOT_P.y <- NULL
nodes_nct$area_ratio.y <- NULL
nodes_nct <- nodes_nct %>% rename(TOT_P = TOT_P.x,
                                  area_ratio = area_ratio.x)
nodes_reg$kmeans_incoming_openess[nodes_reg$kmeans_incoming_openess == 3] <- 7 # merging category 3 and 7 to balance a bit the number of units based on similarity between classes
nodes_reg$kmeans_incoming_openess <- relevel(as.factor(nodes_reg$kmeans_incoming_openess), ref = "4")
nodes_reg <- nodes_nct[ #Formating the df for the regression
  c(1, #id 
  2, #offset pop
  4:7, #demography
  10:52, #assets
  64:74, #centrality
  74:85, #discrete centrality
  86:89, #commmunities
  90:126, #movement flows
  127, #kmeans movement flows
  128:130, #is_hub binaries
  55:58 #outcome dengue
)]
nodes_reg_kfiltered <- nodes_reg %>% subset(nodes_reg$kmeans_incoming_openess != 7)
res.pca <- FactoMineR::PCA(nodes_reg[3:121], scale.unit = TRUE, ncp = 5, graph = TRUE)
fviz_pca_ind(res.pca)

model <- glm(dengue_10 ~ kmeans_incoming_openess + 
               #scale(degree_all) + 
               #as.factor(louvain_tot_nct) +
               MARGWORK_P + 
               No_drainage +
               Hh_Good_condition + 
               Tapwater_treated +               
               P_06 +
               P_SC +
               F_RATIO
             , 
             family = quasipoisson,
             data = nodes_reg_kfiltered, offset = log(nodes_reg_kfiltered$TOT_P))
summary(model)
pscl::pR2(model)
car::vif(model)
car::influencePlot(model)
residuals <- residuals(model, type = "pearson")  # or "deviance" for deviance residuals
fitted_values <- fitted(model)
qqnorm(residuals(model, type = "pearson"), main = "Q-Q Plot of Residuals")
qqline(residuals(model, type = "pearson"), col = "red")
AER::dispersiontest(model)
anova(model)
1 - model$deviance / model$null.deviance ## quasi-R2 for quasipoisson
pearson_residuals <- residuals(model, type = "pearson")
residuals(model, type = "pearson")

model_nb <- MASS::glm.nb(dengue_10 ~ kmeans_incoming_openess + 
                     degree_all + 
                     #as.factor(louvain_tot_nct) +
                     MARGWORK_P + 
                     No_drainage +
                     Hh_Good_condition + 
                     Tapwater_treated, 
                     offset(log(nodes_reg$TOT_P)),
                   data = nodes_reg)
summary(model_nb)
pscl::pR2(model_nb)
car::vif(model_nb)
residuals <- residuals(model_nb, type = "pearson")  # or "deviance" for deviance residuals
fitted_values <- fitted(model_nb)
qqnorm(residuals(model_nb, type = "pearson"), main = "Q-Q Plot of Residuals")
qqline(residuals(model_nb, type = "pearson"), col = "red")
AER::dispersiontest(model_nb)
anova(model_nb)

# Example of a Bayesian negative binomial regression
model_bayesian <- brm(
  formula = dengue_10 ~ as.factor(kmeans_incoming_openess) + 
    scale(degree_all) + 
    #as.factor(louvain_tot_nct) +
    MARGWORK_P + No_drainage +
    Hh_Good_condition + Tapwater_treated +
    offset(log(TOT_P)),
  family = negbinomial(),
  data = nodes_reg,
  prior = set_prior("normal(0, 5)", class = "b"),
  iter = 2000,   # Number of iterations
  warmup = 1000, # Number of warmup iterations
  chains = 4     # Number of chains
)
#model_bayesian08 <- model_bayesian
#model_bayesian09 <- model_bayesian
model_bayesian10 <- model_bayesian
summary(model_bayesian)
plot(model_bayesian)

pp_check(model_bayesian, type = "scatter")
pp_check(model_bayesian, type = "intervals")
as_draws(model_bayesian)


## Mapping the classifcation categories
grid_sf <- merge(grid_sf, nodes_reg[c("node_id", "kmeans_incoming_openess")])
#grid_sf <- merge(grid_sf, nodes_nct[c("node_id", "kmeans_incoming_openess_v3")])
palette <- colorFactor(palette = "Set1", domain = grid_sf$kmeans_incoming_openess)
nct <- st_read("/Users/utilisateur/Documents/Charbon/Geo Data/NCT_boundaries.gpkg") %>%
  st_transform(4326)
# Create the leaflet map
leaflet(data = st_transform(grid_sf,4326)) %>%
  addProviderTiles(providers$CartoDB) %>%
  addPolygons(data = nct, color = "black", weight = 4, fill = FALSE) %>%
  addPolygons(
    fillColor = ~palette(kmeans_incoming_openess),  # Color based on kmeans class
    weight = 1, 
    color = "black", 
    fillOpacity = 0.7,
    popup = ~paste("Subdt:", subdt)
  ) %>%
  addLegend(
    pal = palette, 
    values = ~kmeans_incoming_openess, 
    title = "K-means Class",
    opacity = 0.7
  )

grid_sf <- merge(grid_sf, nodes_reg[c("node_id", "louvain_tot_nct")])
palette <- colorFactor(palette = "Set1", domain = grid_sf$louvain_tot_nct)
# Create the leaflet map
leaflet(data = grid_sf) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~palette(louvain_tot_nct),  # Color based on kmeans class
    weight = 1, 
    color = "black", 
    fillOpacity = 0.7,
    popup = ~paste("Subdt:", subdt)
  ) %>%
  addLegend(
    pal = palette, 
    values = ~louvain_tot_nct, 
    title = "Louvain Community",
    opacity = 0.7
  )

## MAPPING ALL THE VARIABLES INCLUDED IN THE MODEL

grid_sf2 <- merge(grid_sf["node_id"], nodes_reg[c("node_id", "degree_all" , "MARGWORK_P", "No_drainage", "Hh_Good_condition", "Tapwater_treated", "P_06", "P_SC", "F_RATIO")])

# Pivot the data into long format for facetting, excluding node_id and geometry
grid_sf_long <- grid_sf2 %>%
  pivot_longer(cols = -c(node_id, geometry), 
               names_to = "variable", 
               values_to = "value")

# Create individual ggplot maps for each variable
plots <- lapply(unique(grid_sf_long$variable), function(var) {
  ggplot(grid_sf_long[grid_sf_long$variable == var, ]) +
    geom_sf(aes(fill = value), color = NA) +   # Fill polygons based on values
    geom_sf(data = nct, fill = NA, color = "black", size = 1) +  # Add nct boundary
    scale_fill_viridis_c(option = "plasma") +  # Continuous color scale
    theme_minimal() +
    labs(title = ifelse(var == "degree_all", 
                              paste(var, "(nb degrés)"), 
                              paste(var, "(%)")),
         fill = "Valeur") + 
    theme(
      axis.text = element_blank(), 
      axis.ticks = element_blank(),
      panel.grid = element_blank()
    )
})
cowplot::plot_grid(plotlist = plots, ncol = 2)

## MAPPING THE DENGUE CASES
dengue_cases <- dengue_cases %>%
  mutate(year = year(date_deb))

ggplot() +
  geom_sf(data = grid_sf, fill = NA, color = "black", size = 0.5) +  # Outline of grid
  geom_sf(data = nct, fill = NA, color = "black", size = 0.5) +  # NCT outline
  geom_sf(data = dengue_cases, aes(color = factor(year)), size = 2) +  # Dengue cases colored by year
  scale_color_manual(values = c("2008" = "red", "2009" = "green", "2010" = "blue")) +  # Set colors for each year
  labs(
    title = "Dengue Cases in 2008, 2009, and 2010",
    color = "Year"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom") +  # Position the legend at the bottom
  coord_sf() +  # Ensure that the map uses the correct coordinate system
  ggspatial::annotation_scale(location = "bl")  # Add a scale bar
  #ggspatial::annotation_north_arrow(location = "tr", style = north_arrow_fancy_orienteering)  # Add a north arrow

color_palette <- colorFactor(palette = c("seagreen", "royalblue", "red"), 
                             levels = c("2008", "2009", "2010"))
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = grid_sf, fill = FALSE, color = "black", weight = 1, 
              opacity = 1, group = "Grid") %>%
  addPolygons(data = nct, fill = FALSE, color = "black", weight = 1, 
              opacity = 1, group = "NCT") %>%
  addCircleMarkers(data = dengue_cases, 
                   fillColor = ~color_palette(factor(year)),
                   radius = 2.5, 
                   fillOpacity = 1, 
                   stroke = TRUE,
                   color = "black",
                   weight = 0.5,
                   group = "Dengue Cases", 
                   popup = ~paste("Date:", date_deb, "<br>", "Year:", year)) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) %>%
  addLegend(
    position = "bottomright",  
    pal = color_palette, 
    values = dengue_cases$year,  
    title = "Année des cas de dengue",
    labFormat = labelFormat(prefix = ""), 
    opacity = 1
  )
