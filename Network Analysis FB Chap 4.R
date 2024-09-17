library(readr)
library(tidyverse)
library(sf)
library(leaflet)
library(leaflet.minicharts)
library(leaflet.extras2)
library(ineq)
library(igraph)
library(tidygraph)
library(ggraph)


dt <- st_read("/Users/utilisateur/Documents/Charbon/Geo Data/Delhi Urban Area + Flows Analysis/Mobility Tiles With UA 2/tiles_with_ua.shp")
nct <- st_read("/Users/utilisateur/Documents/Charbon/Geo Data/NCT_boundaries.gpkg")

dt <- st_transform(dt, 4326)

colnames(dt)[5:10] <- c("wd.5h30am", "wd.1h30pm", "wd.9h30pm", "we.5h30am", "we.1h30pm", "we.9h30pm")

dt <- dt %>%
  mutate(
    origin_lon = map_dbl(geometry, ~st_coordinates(.x)[1, 1]),
    origin_lat = map_dbl(geometry, ~st_coordinates(.x)[1, 2]),
    dest_lon = map_dbl(geometry, ~st_coordinates(.x)[2, 1]),
    dest_lat = map_dbl(geometry, ~st_coordinates(.x)[2, 2])
  )

max_flow <- dt %>% 
  st_drop_geometry() %>% 
  select(5:10) %>% max()

create_flow_map <- function(dt, nct, flow_column, max_flow) {
  # Create a leaflet map
  leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    # Add the NCT outline
    addPolygons(data = nct, color = "black", weight = 4, fill = FALSE) %>%
    addFlows(
      lng0 = dt$origin_lon, lat0 = dt$origin_lat,
      lng1 = dt$dest_lon, lat1 = dt$dest_lat,
      flow = (dt[[flow_column]] / max_flow) * 10, 
      color = 'blue',
      opacity = dt$opacity,
      maxFlow = max_flow,
      maxThickness = 14,
      time = NULL
    ) %>% 
    addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
}

#### RESTART FROM HERE WHEN NEW MAP NEEDED, JUST REPLACE ALL THE FOLLWING "we/wd..."
# Setting the opacity
dt <- dt %>% mutate(opacity = 1 - (1/((wd.9h30pm/50)+1))) # TO MODIFY IN CASE NEW MAP NEEDED

#create_flow_map(dt, nct, "wd.1h30pm", max_flow)

## Reducing the data to make it readable

#Checking the Lorenz curve of people moving to see where the threshold could be to gather the max information with the min flows
lorenz_curve <- Lc(dt$wd.1h30pm)
plot(
  lorenz_curve,
  main = "Courbe de Lorenz (Semaine ?? Week-end ??)",
  xlab = "Part cumulée des flux",
  ylab = "Part cumulée de la population en mouvement",
  col = "blue",
  lwd = 2
) + 
  abline(0, 1, col = "red", lty = 2)+ #line of equirepartition
  abline(h = 0.2, col = "black", lty = 3) #line showig where the 20% of total share is reached (checking the 80/20 Paretto law)

# Observing the cumulated frequence
cumulative_data <- dt %>%
  filter(!is.na(wd.1h30pm)) %>%
  arrange(wd.1h30pm) %>%
  mutate(CumulativeFrequency = cumsum(rep(1, n())),,
         CumulativePercentage = (CumulativeFrequency / n()) * 100) %>%
  select(wd.1h30pm, CumulativePercentage)

ggplot(subset(cumulative_data, wd.1h30pm > 0), aes(x = wd.1h30pm, y = CumulativePercentage)) +
  geom_line(color = "blue") +
  geom_point(color = "blue") +
  labs(
    title = "Fréquence cumulée des flux par nombre de personnes en mouvement (Semaine 5h30-13h30)",
    x = "Nombre de personnes",
    y = "Fréquence cumulée en pourcentage"
  ) +
  scale_x_log10() + 
  theme_minimal()

# Selecting only the main flow outgoing for each tile
dt_filtered <- dt %>%
  group_by(origin_lon, origin_lat) %>%
  slice_max(wd.1h30pm, n = 1, with_ties = FALSE) %>% # TO MODIFY IN CASE NEW MAP NEEDED
  ungroup()

#create_flow_map(dt_filtered, nct, "wd.1h30pm", max_flow)

# Retaining flows above 60 users
dt_high_flows <- dt %>%
  filter(wd.1h30pm > 60) # TO MODIFY IN CASE NEW MAP NEEDED

#Combine `dt_filtered` with `dt_high_flows` using `bind_rows`
dt_combined <- bind_rows(dt_filtered, dt_high_flows) %>% # TO RUN IN CASE NEW MAP NEEDED
  # Remove duplicates that might exist between the two datasets
  distinct()

#Checking the share of the moving population covered with the "combined" filtered dataset
sum(dt_combined$wd.1h30pm)/sum(dt$wd.1h30pm)

#Mapping the combined dataset
#create_flow_map(dt_combined, nct, "wd.1h30pm", max_flow)

# Group by destination latitude and longitude, then summarize by summing `wd.1h30pm`
dt_grouped_by_destination <- aggregate(
  wd.1h30pm ~ dest_lat + dest_lon, # TO MODIFY IN CASE NEW MAP NEEDED
  data = dt, 
  FUN = sum, 
  na.rm = TRUE
)
sd(dt_grouped_by_destination$wd.1h30pm)/mean(dt_grouped_by_destination$wd.1h30pm)
#Filtrering the top destinations 
dt_dest_filtered <- dt_grouped_by_destination %>%
  slice_max(order_by = wd.1h30pm, n = 25) # TO MODIFY IN CASE NEW MAP NEEDED
print(dt_dest_filtered)
create_flow_map(dt_combined, nct, "wd.1h30pm", max_flow)%>% # TO MODIFY IN CASE NEW MAP NEEDED
  addCircleMarkers(
    data = dt_dest_filtered,
    lng = ~dest_lon, lat = ~dest_lat,
    radius = ~sqrt(wd.1h30pm) / 10,  # TO MODIFY IN CASE NEW MAP NEEDED
    color = 'red',
    fillOpacity = 0.7,
    stroke = FALSE,
    popup = ~paste("Incoming people:", wd.1h30pm) # TO MODIFY IN CASE NEW MAP NEEDED
  ) %>% setView(lat = 28.65239, lng = 77.17896, zoom = 10)

#dt_combined_wd.5h30am <- dt_combined
#dt_combined_wd.1h30pm <- dt_combined
#dt_combined_wd.9h30pm <- dt_combined
#dt_combined_we.5h30am <- dt_combined
#dt_combined_we.1h30pm <- dt_combined
#dt_combined_we.9h30pm <- dt_combined

rsd(dt$wd.5h30am) #to try on high flows only
rsd(dt$wd.1h30pm)
rsd(dt$wd.9h30pm)
rsd(dt$we.5h30am)
rsd(dt$we.1h30pm)
rsd(dt$we.9h30pm)

## Mapping the tiles to observe the neighbouring environment
points_sf <- st_as_sf(dt_grouped_by_destination, coords = c("dest_lon", "dest_lat"), crs = 4326)
# Calculer les polygones de Voronoï
voronoi_sf <- st_voronoi(points_sf) %>%
  st_collection_extract(type = "POLYGON") %>%
  st_sf(crs = 4326) %>%
  st_intersection(st_as_sfc(st_bbox(points_sf)))

# Afficher le diagramme de Voronoï sur une carte leaflet
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  #addPolygons(data = voronoi_sf, color = "blue", weight = 1, fill = FALSE) %>%
  #addCircleMarkers(data = points_sf, radius = 3, color = "red", fill = TRUE, fillOpacity = 0.7) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE)) %>% 
  setView(lat = 28.62310, lng = 77.28882, zoom = 14)

## ATTEMPT TO OVERLAP MOVEMENT WITH THE BUILT UP AREAS

# Importing the Delhi built-up continuum of 1975 for the need of the income map
bua75 <- st_read("Documents/Charbon/Geo Data/Delhi Urban Areas/tacheUrbaineDelhi200_1975.shp") %>%
  st_transform(crs= 4326)
bua90 <- st_read("Documents/Charbon/Geo Data/Delhi Urban Areas/tacheUrbaineDelhi200_1990.shp") %>%
  st_transform(crs= 4326)
bua00 <- st_read("Documents/Charbon/Geo Data/Delhi Urban Areas/tacheUrbaineDelhi200_2000.shp") %>%
  st_transform(crs= 4326)
bua15 <- st_read("Documents/Charbon/Geo Data/Delhi Urban Areas/tacheUrbaineDelhi200.shp") %>%
  st_transform(crs= 4326)

create_flow_map(dt_combined, nct, "wd.5h30am", max_flow)%>% # TO MODIFY IN CASE NEW MAP NEEDED
  addCircleMarkers(
    data = dt_dest_filtered,
    lng = ~dest_lon, lat = ~dest_lat,
    radius = ~sqrt(wd.5h30am) / 10,  # TO MODIFY IN CASE NEW MAP NEEDED
    color = 'red',
    fillOpacity = 0.7,
    stroke = FALSE,
    popup = ~paste("Incoming people:", wd.5h30am) # TO MODIFY IN CASE NEW MAP NEEDED
  ) %>% setView(lat = 28.65239, lng = 77.17896, zoom = 10) %>%
  addPolygons(data = bua15, color = "#e31a1c", weight = 1.5, fill = FALSE, opacity = 1)

### ATTEMPT TO MAP IT WITH MAPDECK ###
library(mapdeck)

create_flow_map_mapdeck <- function(dt, nct, flow_column, mapbox_token) {
  # Set the Mapbox token
  set_token(mapbox_token)
  
  # Calculate stroke width based on the number of people moving
  max_flow_value <- max(dt[[flow_column]], na.rm = TRUE)
  dt$stroke_width <- (dt[[flow_column]] / max_flow_value) * 10  # Scale to a range [1, 10]
  
  # Check if destination is within the nct perimeter
  nct_polygon <- st_union(nct$geom)  # Combine all multipolygons into a single polygon
  dt$in_nct <- st_intersects(
    st_as_sf(dt, coords = c("dest_lon", "dest_lat"), crs = st_crs(nct)),
    nct_polygon,
    sparse = FALSE
  )[, 1]  # Returns a logical vector
  
  # Create color columns based on whether the destination is within the nct perimeter
  dt$color <- ifelse(dt$in_nct, "red", "blue")
  # Attempt for a gradient color
  #dt$color <- colorRampPalette(c("blue", "red"))(100)
  #dt$color <- dt$color[ceiling(scales::rescale(dt$wd.5h30am) * 99) + 1]
  
  # Initialize the Mapdeck map
  map <- mapdeck(style = mapdeck_style("dark"), pitch = 45)
  
  # Add 3D vertical arcs for the flows
  map <- map %>%
    add_arc(
      data = dt,
      layer_id = "flow_arcs",
      origin = c("origin_lon", "origin_lat"),    
      destination = c("dest_lon", "dest_lat"),   
      stroke_from = "color",   # Color at the origin, attempt for gradient colour
      stroke_to = "color",     # Color at the destination, attempt for gradient colour
      stroke_width = "stroke_width",  # Dynamic width based on the flow value
      tilt = 30,  # Adjust tilt for the 3D effect
      opacity = 1 - (1/((dt[[flow_column]]/50)+1)),
      auto_highlight = TRUE
    )

  map
}
token = "pk.eyJ1Ijoic2JlbmtpbW91biIsImEiOiJja2N5eW03anAwZW5pMnltdjQ2dndld2VhIn0.S7od4ghFgJA-_x6J0Z_bWA"
create_flow_map_mapdeck(st_drop_geometry(dt), nct, "wd.5h30am", token) 

## Spatiale analysis
# Checking the link between spatial proximity and volume of exchanges
dt <- dt %>% mutate(length = as.numeric(st_length(geometry)))
ggplot(dt, aes(x = length, y = wd.5h30am)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "gam", se = FALSE, color = "red") +
  coord_cartesian(xlim = c(0, 50000))+
  #scale_x_log10()+
  labs(x = "Distance (mètres)", y = "Volume de populaiton échangée",
       title = "Relation entre longueur des flux et population échangée",
       subtitle = "Jours de semaine, pas de temps 5h30-13h30") +
  theme_minimal()
cor(dt$length, dt$wd.5h30am, use = "complete.obs")
cor(dt$length, dt$wd.1h30pm, method = "spearman") #works better than Pearson

# Linear relationship average, son instead we compare the cumulative share of people exchanged with respect to the distance
dt_grouped_by_length <- dt %>%
  mutate(length = as.numeric(length)) %>%  # Ensure length is numeric
  group_by(length) %>%
  summarise(total_flows = sum(wd.5h30am, na.rm = TRUE)) %>%
  arrange(length)

# Calculate the cumulative sum and cumulative share
dt_grouped_by_length <- dt_grouped_by_length %>%
  mutate(cumulative_flows = cumsum(total_flows),
         cumulative_share = 100 * cumulative_flows / sum(total_flows),
         length_km = length / 1000)

ggplot(dt_grouped_by_length, aes(x = length_km, y = cumulative_share)) +
  geom_line(color = "blue", size = 1) +
  geom_vline(xintercept = 10, linetype = "dotted", color = "red") +  # Optional: Add line for 80% mark
  labs(x = "Distance euclidienne (km)", y = "Part du total de la population échangée (%)",
       title = "Part de la population échangée entre tuiles (2km) selon la distance",
       subtitle = "Jours de semaine, pas de temps 5h30-13h30, Février-Mars 2020",
       caption = "Source: FB Data For Good, Auteur: Samuel Benkimoun") +
  coord_cartesian(xlim = c(0, 50)) +  # Zoom in on the range of 0 to 50km, only a long tail after
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0)   
  )
# Checking whether the correlation works for shorter distances 
dt_short_distance <- dt %>% subset(length < 50000)
cor(dt_short_distance$length, dt_short_distance$wd.1h30pm)

# Checking the standard deviation and RSD for each time steps to know about the dispersion
dt$wd.9h30pm %>% sd() / mean(dt$wd.9h30pm) * 100
rsd <- function(number){
  sd <- sd(number, na.rm = TRUE)
  cat("écart-type: ", sd, "\n")
  mean <- mean(number, na.rm = TRUE)
  rsd <- (sd/mean)*100
  cat("coef. variation: ", rsd, "\n")
}
rsd(dt$wd.5h30am) #to try on high flows only
rsd(dt$wd.1h30pm)
rsd(dt$wd.9h30pm)
rsd(dt$we.5h30am)
rsd(dt$we.1h30pm)
rsd(dt$we.9h30pm)

### NETWORK ANALYSIS
dt_combined <-  dt_combined_wd.5h30am
dt_combined %>% head()
dt_combined <- dt_combined %>% mutate(wd.5h30am = 
                                        ifelse(wd.5h30am <= 0, 1, wd.5h30am)) ## TO ADAPT IN CASE OF A CHANGE IN TIMESTEP

# Extracting the coordinates of the edges with admin location and number of people travelling it
edges <- dt_combined %>% st_drop_geometry() %>%
  select(origin_lon, origin_lat, dest_lon, dest_lat, 
         L3_NAME_s, L3_CODE_s, #subdistrict name of the departure and arrival tile (centroid)
         wd.5h30am) #timestep, to adapt accordingly

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

# Generating the graph from the edges
#g <- graph_from_data_frame(edges %>% 
#                                 select(origin_lon, origin_lat, dest_lon, dest_lat),
#                               directed = TRUE)
g <- graph_from_data_frame(edges %>% 
                             select(start_id, end_id), directed = TRUE)

# Adding a wieght to the edges based on the number of people moving
E(g)$weight <- edges$wd.5h30am # To be adapted depending on the timestep
E(g)$weight <- ifelse(E(g)$weight == 0, 1, E(g)$weight)

# Adding the subdistrict information as an attribute to the vertices
V(g)$subdt <- nodes$subdt

# In case some nodes are missing because part of the destinations in the graph, but not in the origines (hence missing in the nodes df) 
# Once done, the graph has to be regenerated
setdiff(V(g)$name, nodes$node_id)
setdiff(nodes$node_id, V(g)$name)
missing_node <- edges %>%
filter(paste0(dest_lon, "-", dest_lat) == setdiff(V(g)$name, nodes$node_id)) %>%
 select(dest_lon, dest_lat) %>%
 distinct()
nodes <- bind_rows(nodes,
                  data.frame(lon = missing_node$dest_lon,
          lat = missing_node$dest_lat,
          subdt = c("Gautam Buddha Nagar"))) #wd.5h30am
          #subdt = c("Faridabad","Gautam Buddha Nagar"))) #wd.9h30pm #we.5h30
V(g)$subdt <- nodes$subdt

## Basic measures on the graph
# order
vcount(g)
# size (number of edges)
ecount(g)
# connected components 
components <- components(g)
components
V(g)$component <- components$membership
# cut points, nodes who are augmenting the number of components if withdrawn
cut_points <- articulation_points(g)
# bridges, same thing but for edges
bridges <- bridges(g)
# graph density in percentage
ecount(g)/(vcount(g)*(vcount(g)-1))*100
# reciprocity
reciprocity(g)


graph_tbl <- as_tbl_graph(g)

## Visualizing the graph with all the components
ggraph::ggraph(graph_tbl, layout = "fr") + 
  geom_edge_link(aes(edge_width = weight), alpha = 0.6) + 
  geom_node_point(aes(color = as.factor(component)), size = 3) +
  geom_node_text(aes(label = subdt), size = 3, repel = TRUE, max.overlaps = 10) +  # Adding the L3 labels
  paletteer::scale_color_paletteer_d("pals::polychrome", name = "Composantes connexes:") +
  labs(title = "Réseau d'échanges de population sous forme de graphe avec composantes",
       subtitle = "Jours de semaine, 5h30-13h30. Algorithme de disposition: Fruchterman-Reingold.",
       edge_width = "Nombre de personnes en mouvement:") +
  theme_graph() +
  theme(legend.position = "right")+  
  annotate("text", x = Inf, y = -Inf, label = "Sources: FB Data For Good, Fev-Mars 2020. Auteur: Samuel Benkimoun. Fait avec igraph, tidygraph et ggraph.", 
                                              hjust = 1, vjust = -0.5, size = 3, color = "black", fontface = "bold")


## Working on a subgraph corresponding to the first component only [528 over 709 vertices]
comp1_vertices <- which(components$membership == 1)
g_comp1 <- induced_subgraph(g, comp1_vertices)

#graph density in percentage
ecount(g_comp1)/(vcount(g_comp1)*(vcount(g_comp1)-1))*100
#diameter, longest shortest path between two pairs
diameter(g_comp1, directed = TRUE, weights = NA)
path_diameter <- get_diameter(g_comp1, directed = TRUE, weights = NA)
#mean distance
mean_distance(g_comp1, directed = TRUE, weights = NA)
#visualising the longest shortest path              
ggraph(g_comp1, layout = "graphopt") + 
   geom_edge_link(aes(),alpha = 0.1) +
   geom_node_point(size = 3, color = "blue", alpha = 0.1) +
   geom_node_point(aes(filter = name %in% V(g_comp1)[path_diameter]$name), size = 5, color = "red") + 
  geom_edge_link(aes(filter = from %in% path_diameter & to %in% path_diameter), edge_color = "red", edge_width = 1.5) +
  geom_node_text(aes(filter = name %in% V(g_comp1)[path_diameter]$name, label = subdt), size = 3, repel = TRUE, max.overlaps = 10) +
   labs(title = "Chemin de diamètre",
        subtitle = "Jours de semaine, 5h30-13h30. Algorithme de disposition: Graphopht") +
  annotate("text", x = Inf, y = -Inf, label = "Sources: FB Data For Good, Fev-Mars 2020. Auteur: Samuel Benkimoun. Fait avec igraph, tidygraph et ggraph.", 
                                                             hjust = 1, vjust = -0.5, size = 3, color = "black", fontface = "bold")+
   theme_graph()

nodes_diameter <- subset(nodes, node_id %in% path_diameter$name)

leaflet(data = nodes_diameter) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolygons(data = nct, color = "white", weight = 4, fill = FALSE) %>%
  addCircleMarkers(
    ~lon, ~lat,  
    color = "red", 
    stroke = FALSE,  
    fillOpacity = 0.7, 
    radius = 5,  
    popup = ~paste0("Subdistrict: ", subdt)  
  ) %>%
  addCircleMarkers(data = nodes_diameter[c(1,nrow(nodes_diameter)),],
                   ~lon, ~lat,  
                   color = "yellow", 
                   stroke = FALSE,  
                   fillOpacity = 1, 
                   radius = 10,  
                   popup = ~paste0("Subdistrict: ", subdt)  
  ) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))

#clustering global and local
# Compute local clustering coefficient
local_clustering <- transitivity(g_comp1, type = "local")
local_clustering
global_transitivity <- transitivity(g_comp1, type = "global")
global_transitivity
V(g_comp1)$clustering <- local_clustering
V(g_comp1)$clustering[is.na(V(g_comp1)$clustering)] <- 0
ggraph(g_comp1, layout = "fr") +
  geom_edge_link(aes(edge_width = weight, alpha = weight)) +
  geom_node_point(aes(color = clustering, alpha = clustering), size = 3) +
  geom_node_text(aes(filter = clustering > 0.80, label = subdt), size = 5, repel = TRUE, max.overlaps = 10) +
  #geom_node_text(aes(filter = clustering > 0.60 & clustering < 0.80, label = subdt), size = 3, repel = TRUE, max.overlaps = 5, color="gray30") +
  scale_color_viridis_c(name = "Coefficient de transitivité (triades):") +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  labs(title = "Transitivité locale des unités spatiales (composante 1)", subtitle = "Jours de semaine, 5h30-13h30. Algorithme de disposition: Fruchterman-Reingold") +
  annotate("text", x = Inf, y = -Inf, label = "Sources: FB Data For Good, Fev-Mars 2020. Auteur: Samuel Benkimoun. Fait avec igraph, tidygraph et ggraph.", 
           hjust = 1, vjust = -0.5, size = 3, color = "black", fontface = "bold")+
  theme_graph()

#Centrality measures: proximity
#E(g_comp1)$weight <- ifelse(is.na(E(g_comp1)$weight) | E(g_comp1)$weight == 0, 1, E(g_comp1)$weight)
centrality_proximity <- closeness(g_comp1, mode="all", weights = 1 / E(g_comp1)$weight) # Weight has to be the invert of number of people moving, because it is interpreted as a cost/distance
#centrality_proximity <- closeness(g_comp1, mode="all", weights = NULL)

V(g_comp1)$proximity <- centrality_proximity
top_10_threshold <- sort(V(g_comp1)$proximity, decreasing = TRUE)[10] %>% as.numeric()

node_labels <- V(g_comp1)$name[V(g_comp1)$proximity >= top_10_threshold]
node_labels_df <- data.frame(
  name = node_labels,
  label = V(g_comp1)$subdt[V(g_comp1)$name %in% node_labels]
)
V(g_comp1)$top_10_label <- ifelse(V(g_comp1)$name %in% node_labels_df$name, 
                                  V(g_comp1)$subdt, NA)
ggraph(g_comp1, layout = "fr") +
  geom_edge_link(aes(edge_width = weight, alpha = weight)) +
  geom_node_point(aes(color = proximity, alpha = proximity), size = 3) +
  geom_node_text(aes(label = top_10_label),
                 size = 4, repel = TRUE, max.overlaps = Inf, color = "black") +
  #geom_node_text(aes(filter = proximity >= top_10_proximity, label = subdt), size = 4, repel = TRUE, max.overlaps = 15) +
  #geom_node_text(aes(filter = clustering > 0.60 & clustering < 0.80, label = subdt), size = 3, repel = TRUE, max.overlaps = 5, color="gray30") +
  scale_color_viridis_c(name = "Degré de centralité de proximité:") +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  labs(title = "Centralité de proximité des unités spatiales (composante 1)", subtitle = "Jours de semaine, 5h30-13h30. Algorithme de disposition: Fruchterman-Reingold") +
  annotate("text", x = Inf, y = -Inf, label = "Sources: FB Data For Good, Fev-Mars 2020. Auteur: Samuel Benkimoun. Fait avec igraph, tidygraph et ggraph.", 
           hjust = 1, vjust = -0.5, size = 3, color = "black", fontface = "bold")+
  theme_graph()


#Centrality measures: eigenvector
centrality_eigenvector <- eigen_centrality(g_comp1, weights = E(g_comp1)$weight)$vector
#centrality_eigenvector <- eigen_centrality(g_comp1, weights = NULL)$vector
V(g_comp1)$eigen_centrality <- centrality_eigenvector

top_10_threshold_eigen <- sort(V(g_comp1)$eigen_centrality, decreasing = TRUE)[10] %>% as.numeric()
node_labels_eigen <- V(g_comp1)$name[V(g_comp1)$eigen_centrality >= top_10_threshold_eigen]
node_labels_df_eigen <- data.frame(
  name = node_labels_eigen,
  label = V(g_comp1)$subdt[V(g_comp1)$name %in% node_labels_eigen]
)

V(g_comp1)$top_10_label_eigen <- ifelse(V(g_comp1)$name %in% node_labels_df_eigen$name, 
                                  V(g_comp1)$subdt, NA)


ggraph(g_comp1, layout = "graphopt") +
  geom_edge_link(aes(edge_width = weight, alpha = weight)) +
  geom_node_point(aes(color = eigen_centrality, alpha = eigen_centrality), size = 3) +
  geom_node_text(aes(label = top_10_label_eigen),
                 size = 4, repel = TRUE, max.overlaps = Inf, color = "black") +
  #geom_node_text(aes(filter = proximity >= top_10_proximity, label = subdt), size = 4, repel = TRUE, max.overlaps = 15) +
  #geom_node_text(aes(filter = clustering > 0.60 & clustering < 0.80, label = subdt), size = 3, repel = TRUE, max.overlaps = 5, color="gray30") +
  scale_color_viridis_c(name = "Degré de centralité de proximité:") +
  scale_alpha_continuous(range = c(0.4, 1), guide = "none") +
  labs(title = "Centralité de vecteur propre des unités spatiales (composante 1)", subtitle = "Jours de semaine, 5h30-13h30. Algorithme de disposition: Fruchterman-Reingold") +
  annotate("text", x = Inf, y = -Inf, label = "Sources: FB Data For Good, Fev-Mars 2020. Auteur: Samuel Benkimoun. Fait avec igraph, tidygraph et ggraph.", 
           hjust = 1, vjust = -0.5, size = 3, color = "black", fontface = "bold")+
  theme_graph()

#Centrality measures: degree, sur gcomp1
degree_centrality <- degree(g_comp1, mode = "out")
V(g_comp1)$degree_centrality <- degree_centrality

ggplot(data.frame(degree = degree_centrality), aes(x = degree)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  scale_y_log10() +
  labs(title = "Distribution des degrés au sein du graphe", subtitle = "Composante 1. Jours de semaine, 5h30-13h30",
       x = "Degré", y = "Log fréquence (nombre de nœuds)") +
  theme_minimal()

nodes_degree <- nodes %>% merge(data_frame(
  node_id = V(g_comp1)$name, 
  #degree = V(g_comp1)$proximity))
  degree = V(g_comp1)$degree_centrality))



colors <- colorNumeric(palette = "inferno", domain = nodes_degree$degree)
leaflet(data = nodes_degree) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  addPolygons(data = nct, color = "white", weight = 4, fill = FALSE) %>%
  addCircleMarkers(
    ~lon, ~lat,  
    color = ~colors(degree),  # Associer la couleur à la centralité de degré
    stroke = FALSE,  
    fillOpacity = 0.7, 
    radius = 5,
    popup = ~paste0("Subdistrict: ", subdt, "<br>Degree: ", degree)  
  ) %>%
  addLegend(
    position = "bottomright",  
    pal = colors, 
    values = ~degree,  
    title = "Degree Centrality"  # Mettre à jour le titre
  ) %>% 
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))

#map bridges
bridges(g_comp1)

gcomp1_tbl <- as_tbl_graph(g_comp1)

bridges_tidy <- gcomp1_tbl %>% 
  activate(edges) %>% 
  filter(edge_is_bridge())

ggraph(gcomp1_tbl, layout = 'fr') +
  geom_edge_link(aes(color = ifelse(edge_is_bridge(), "red", "gray"), width = ifelse(edge_is_bridge(), 2, 1))) +
  geom_node_point(size = 5, color = "black") +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_edge_color_identity() +
  scale_edge_width_identity() +
  theme_void() +
  labs(title = "Graphe avec les Ponts Mise en Évidence")

# COMMUNITY DETECTION
# Using algorithms for direct network (Infomap or Label Propagation)
communities_infomap <- cluster_infomap(g_comp1, e.weights = E(g_comp1)$weight)
V(g_comp1)$community <- membership(communities_infomap) 

communities_label_prop <- cluster_label_prop(g_comp1)
V(g_comp1)$community <- membership(communities_label_prop) 

# Select communities whose size is > 1
community_sizes <- table(V(g_comp1)$community)
large_communities <- names(community_sizes[community_sizes > 1])
g_comp1_filtered <- induced_subgraph(g_comp1, V(g_comp1)[community %in% large_communities])

# Visualisation des communautés
##ggraph(g_comp1, layout = "graphopt") +
ggraph(g_comp1_filtered, layout = "graphopt") +
  geom_edge_link(aes(edge_width = weight, alpha = weight)) +
  geom_node_point(aes(color = factor(community)), size = 4) +
  geom_node_text(aes(label = subdt), size = 3, repel = TRUE, max.overlaps = 10) +  # Adding the L3 labels
  paletteer::scale_color_paletteer_d("pals::polychrome", name = "Communautés:") +
  labs(title = "Détection des communautés (Infomap)",
       subtitle = "Jours de semaine, 5h30-13h30. Algorithme de disposition: Fruchterman-Reingold") +
  annotate("text", x = Inf, y = -Inf, label = "Sources: FB Data For Good, Fev-Mars 2020. Auteur: Samuel Benkimoun. Fait avec igraph, tidygraph et ggraph.", 
           hjust = 1, vjust = -0.5, size = 3, color = "black", fontface = "bold")+
  theme_graph()

# Attempt with an undericted graph of the component 1 to use a Louvain algorithm, Leiden, Cluster Optimal
g_undirected <- as.undirected(g_comp1, mode = "collapse", edge.attr.comb = "mean")

communities_louvain <- cluster_louvain(g_undirected, weights = E(g_undirected)$weight)
V(g_undirected)$community <- communities_louvain$membership

communities_leiden <- cluster_leiden(g_undirected, weights = E(g_undirected)$weight)
V(g_undirected)$community <- communities_leiden$membership

communities_optimal <- cluster_optimal(g_undirected, weights = E(g_undirected)$weight)
V(g_undirected)$community <- communities_optimal$membership


# Select communities whose size is > 1
community_sizes <- table(V(g_undirected)$community)
large_communities <- names(community_sizes[community_sizes > 1])
g_undirected_filtered <- induced_subgraph(g_undirected, V(g_undirected)[community %in% large_communities])

ggraph(g_undirected, layout = "fr") +
#ggraph(g_undirected_filtered, layout = "fr") +
  geom_edge_link(aes(edge_width = weight, alpha = weight)) +
  geom_node_point(aes(color = factor(community)), size = 4) +
  geom_node_text(aes(label = subdt), size = 3, repel = TRUE, max.overlaps = 5) +
  paletteer::scale_color_paletteer_d("pals::polychrome", name = "Communautés:") +
  labs(title = "Détection des communautés (Louvain)",
       subtitle = "Jours de semaine, 5h30-13h30. Algorithme de disposition: Fruchterman-Reingold") +
  annotate("text", x = Inf, y = -Inf, label = "Sources: FB Data For Good, Fev-Mars 2020. Auteur: Samuel Benkimoun. Fait avec igraph, tidygraph et ggraph.",
           hjust = 1, vjust = -0.5, size = 3, color = "black", fontface = "bold") +
  theme_graph()

## Mapping the communities
nodes_community <- nodes %>% merge(data_frame(
  node_id = V(g_undirected_filtered)$name, 
  community = V(g_undirected_filtered)$community))
nodes_community <- nodes_community %>% mutate(community = as.factor(community))
# Defining a color ramp
community_levels <- levels(nodes_community$community)
colors <- paletteer::paletteer_d("pals::polychrome", n = length(community_levels))

leaflet(data = nodes_community) %>%
  addProviderTiles(providers$CartoDB.DarkMatter) %>%
  # Add the NCT outline
  addPolygons(data = nct, color = "white", weight = 4, fill = FALSE) %>%
  addCircleMarkers(
    ~lon, ~lat,  
    color = ~colors[community], 
    stroke = FALSE,  
    fillOpacity = 0.7, 
    radius = 5,  
    popup = ~paste0("Subdistrict: ", subdt, "<br>Community: ", community)  
  ) %>%
  addLegend(
    position = "bottomright",  
    colors = colors, 
    labels = community_levels,  
    title = "Communities" 
  )   %>% 
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
