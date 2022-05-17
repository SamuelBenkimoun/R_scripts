#Importing the libraries and setting directories
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(tmap)
library(readr)
library(tidyr)
library(readxl)
library(igraph)
library(ggplot2)
library(stargazer)
library(ggthemes)
options(scipen=999)
## IMPORTING AND FORMATING THE DATA ##
setwd("../northern_india_nepal_and_pakistan_disease_prevention_map_may_29_2019_movement_between_administrative_regions/")
#Importing the movement data (already agregated in one file)
dt <- read_delim("../Delhi Tiles Til 26 March 2020/Delhi_Tiles_till_2603.csv", ";", trim_ws = TRUE)
#Keeping the dates before the lockdown implementation to get an averaged value week day vs week-end
dt <- dt[2:45]
colnames(dt)[6:ncol(dt)] <- paste(
  weekdays(
    parse_date_time(
      colnames(dt[6:ncol(dt)]),
      orders="ymd HM")
    ),".",
  c("5h30am","1h30pm","9h30pm"), 
  sep = "")
#Averaging the number of people moving by timestep, week-days (wd) and week-end (we) separately
mean <- dt[grep("5h30",colnames(dt))]
mean <- mean[-(grep("samedi|dimanche",colnames(mean)))]
dt$wd.5h30am <- round(rowMeans(mean),0)

mean <- dt[grep("1h30",colnames(dt))]
mean <- mean[-(grep("samedi|dimanche",colnames(mean)))]
dt$wd.1h30pm <- round(rowMeans(mean),0)

mean <- dt[grep("9h30",colnames(dt))]
mean <- mean[-(grep("samedi|dimanche",colnames(mean)))]
dt$wd.9h30pm <- round(rowMeans(mean),0)

mean <- dt[grep("5h30",colnames(dt))]
mean <- mean[grep("samedi|dimanche",colnames(mean))]
dt$we.5h30am <- round(rowMeans(mean),0)

mean <- dt[grep("1h30",colnames(dt))]
mean <- mean[grep("samedi|dimanche",colnames(mean))]
dt$we.1h30pm <- round(rowMeans(mean),0)

mean <- dt[grep("9h30",colnames(dt))]
mean <- mean[grep("samedi|dimanche",colnames(mean))]
dt$we.9h30pm <- round(rowMeans(mean),0)

#Keeping only the averaged values for weekdays and weekends
dt <- dt[-(6:44)]
#Writing the output df in a csv file for safety !
write.csv(dt, "./delhi_tiles_avg_28febto8march.csv")

## RECREATING THE TILES ##
#Creating an sf object from the movement dataset
dt_ua <- st_as_sf(dt, wkt = "Geometry") %>% 
  st_set_crs(4326) %>% 
  st_transform(3857) 
#Importing the L3 boundaries layer to join it to the tiles
subd <- st_read("../SDT_shape_2011/India_L3_Administrative_Boundaries.shp") %>%
  st_transform(3857)
#Isolating all the extremeties ("from" and "to") of the movement flows
start <- dt_ua 
start$Geometry <- st_line_sample(start$Geometry, sample = 0)
start$x <- st_coordinates(start)[,1]
start$y <- st_coordinates(start)[,2]
end <- dt_ua 
end$Geometry <- st_line_sample(end$Geometry, sample = 1)
end$x <- st_coordinates(end)[,1]
end$y <- st_coordinates(end)[,2]
#Generating the point grid with the unique values of the flow extremeties
tiles <- unique(start[c("x", "y")]) %>%
  rbind(unique(end[c("x", "y")])) %>%
  unique() 
#Creating the voronoi polygons from the point grid and joining L3 admin info
tiles <- st_voronoi(st_combine(tiles)) %>%
  st_collection_extract(type="POLYGON")%>%
  st_as_sf() 
plot(tiles)
#Computing the surface and attributing an id key to the tiles
tiles$surf_tile <- round(st_area(tiles),1)
tiles$id_tile <- 1:nrow(tiles)

## LINKING TILES TO THE MOVEMENT DATA ##
#Attaching the tile id to the extremities of the movements flows
start <- st_join(start, tiles["id_tile"])
colnames(start)[14] <- "id_tile_start"
end <- st_join(end, tiles["id_tile"])
colnames(end)[14] <- "id_tile_end"
#Getting both "from" and "to" tile id on a same file 
mob_tiles <- cbind(dt_ua, st_drop_geometry(start[c("id_tile_start")]), st_drop_geometry(end[c("id_tile_end")]))
#Aggregating the total incoming mobility by tile for each time-step and weekday/weekend
ag_tiles <- aggregate(st_drop_geometry(mob_tiles[5:10]), by=list(To=mob_tiles$id_tile_start), FUN = sum) %>%
  left_join(st_drop_geometry(tiles), by = c("To" = "id_tile"))

## ADDING EPIDEMIOLOGICAL DATA INTO THE TILES ##
#Importing the cases for the year 2008 and 2009 in Delhi NCT
dengue_08 <- st_read("../EPIDEMIO_DATA_INDIA/Dengue 2008/Cas_dengue.shp") %>%
  st_transform(3857)
dengue_09 <- st_read("../EPIDEMIO_DATA_INDIA/Dengue 2009/Dengue_2009_utm_date_lite.shp") %>%
  st_transform(3857)
#Counting the number of cases per tile
tiles$d08 <- lengths(st_intersects(tiles, dengue_08))
tiles$d09 <- lengths(st_intersects(tiles, dengue_09))
#Joining the state information to each tile, in order to later filter NCT tiles only
tiles <- st_join(tiles, subd[c("L1_NAME")], k=1, join= st_nearest_feature)

## ADDING THE POPULATION INFORMATION TO THE TILES ##
#Importing the population from the Ward census of 2011
wards_pop <- st_read("../WARD 2011/delhi_wards_pop2011_ok.shp") %>%
  st_transform(3857) %>%
  dplyr::select("id", "tot.pop") %>%
  mutate(surf = st_area(geometry))
#Intersecting the Wards layer, to reattribute the population to each tile proportionally to the overlapping surface 
wards_inter <-  st_intersection(wards_pop, tiles) %>%
  mutate(surf2 = st_area(geometry)) %>%
  mutate(pop2 = tot.pop * (surf2/surf)) %>%
  dplyr::select("id_tile", "pop2") 
wards_inter <- aggregate(wards_inter$pop2, by= list(wards_inter$id_tile), FUN = sum)
colnames(wards_inter) <- c("id_tile","pop_tile")
#Joining the overlapping weighted Ward population data to the tiles
tiles <- merge(tiles,wards_inter)

## ATTACHING THE AVERAGE INCOMING MOBILITY DATA TO THE TILES ##
tc <- ag_tiles[1:7] %>% 
  rename(id_tile = To) %>% 
  merge(x=tiles)
tiles <- tc
rm(tc)

## ATTACHING THE HOUSEHOLD DATA TO THE TILES ##
# Importing the data from the Census 2011 excel tables
setwd("~/Households Equipments SDT DELHI/")
list_csv <- list.files()
hh <- lapply(list.files(), function (x) {
  read_excel(x, skip = 6)
})
# Filtering the information at the most precise scale "Sub-dist" 
hh <- lapply(hh, subset, grepl("Sub-Dist", `9`))
hh <- lapply(hh, subset, `10` == "Total")
hh <- do.call(rbind,hh)
# Filtering the fields of interest with the research question, particularly water related, and formating
hh <- hh[c(5,6,12,72,73,106)]
colnames(hh) <- c("L3_CODE","L3_NAME", "hh_good", "tap_treated", "tap_untreated", "closed_drain")
hh$L3_CODE <- as.numeric(hh$L3_CODE)
# Grouping the tap fields, and computing the surface
hh <- merge(subd,hh) %>%
  dplyr::select(c("L3_CODE","L3_NAME", "hh_good", "tap_treated", "tap_untreated", "closed_drain")) %>%
  mutate(tap = tap_treated + tap_untreated) %>%
  mutate(surf = st_area(geometry))
# Distributing the household data to the files proportionally to the overlapping surface
hh_inter <-  st_intersection(hh, tiles) %>%
  mutate(surf2 = st_area(geometry)) %>%
  mutate(hh_good2 = hh_good * (surf2/surf_tile)) %>%
  mutate(tap2 = tap * (surf2/surf_tile)) %>%
  mutate(drain2 = closed_drain * (surf2/surf_tile)) %>%
  dplyr::select("id_tile", "hh_good2", "tap2", "drain2") 
hh_inter <- aggregate(hh_inter[2:4], by= list(hh_inter$id_tile), FUN = sum) %>%
  st_drop_geometry()
colnames(hh_inter) <- c("id_tile", "hh_good", "tap", "drain") 
tiles <- merge(tiles, hh_inter)

## FILTERING NCT AREA AND MINIMUM MOVEMENT ##
tiles$min_mob <- apply(st_drop_geometry(tiles[11:16]), 1, FUN = min)
tiles$max_mob <- apply(st_drop_geometry(tiles[11:16]), 1, FUN = max) 
tiles <- st_join(tiles, subd["L3_NAME"], k=1, join= st_intersects, largest=TRUE)
nct <- subset(tiles, L1_NAME == "NCT Of Delhi") %>%
  subset(min_mob > 10)

## BUILDING THE GRAPH FOR CENTRALITY COMPUTATION ## 
# Attaching the Sub-district of belonging (largest overlap) to help locate the tiles
tiles <- st_join(tiles, subd["L3_NAME"], k=1, join= st_intersects, largest=TRUE)
# Building the graph with the movement network between tiles
mtg <- left_join(mob_tiles, st_drop_geometry(tiles[c("id_tile","L3_NAME", "L1_NAME")]), by = c("id_tile_start" = "id_tile")) %>%
  left_join(st_drop_geometry(tiles[c("id_tile","L3_NAME", "L1_NAME")]), by = c("id_tile_end" = "id_tile"))%>%
  na.omit()
# Labeling the edges with a name that comprises the subdistrict name and tile id, for convenience 
mtg$start <-  paste(mtg$id_tile_start, mtg$L3_NAME.x, sep = "_")
mtg$end <- paste(mtg$id_tile_end, mtg$L3_NAME.y, sep = "_")
mtg <- st_drop_geometry(mtg)
graph <- graph_from_data_frame(cbind(mtg[17:18], mtg[5:10]), directed = TRUE)
#graph <- simplify(graph, remove.multiple = F, remove.loops = T) 
plot(graph, edge.arrow.size=.4,vertex.label=NA, layout= layout_in_circle)
# Creating a dataframe to stock the centrality indices values 
tiles_index <- data_frame()
tiles_index <- as.data.frame(cbind(unique(mtg$start),degree(graph))) %>%
  cbind(as_tibble(betweenness(graph, directed = TRUE))) %>%
  cbind(as_tibble(closeness(graph)))%>%
  cbind(as_tibble(eigen_centrality(graph)$vector))
rownames(tiles_index) <- 1:nrow(tiles_index)
#Attributing a weight for the computation of centrality indices, here the value of the movement flow on weekdays mornings
weight = E(graph)$wd.5h30am
weight[weight == 0] <- 0.01
E(graph)$weight <- weight
tiles_index <- cbind(tiles_index, as_tibble(betweenness(graph, weight=weight, directed = TRUE))) %>%
  cbind(as_tibble(closeness(graph, weight=weight)))
colnames(tiles_index) <- c("id_tile","degree","betw","eigen","close","betw_w", "close_w")  
tiles_index$id_tile <- as.numeric(gsub(".*?([0-9]+).*", "\\1", tiles_index$id_tile)) 

## GROUPING THE TILES WITH THEIR CENTRALITY INDICES##
nct <- merge(nct, tiles_index)
nct$degree <- as.numeric(nct$degree)
nct <- subset(nct, !(L3_NAME %in% c("Ghaziabad","Sonipat","Dadri")))

## MAKING A K-MEANS CLASSIFICATION ON THE INCOMING MOVEMENT VALUES PER TILES ##
# Gathering the movement fields 
d <- st_drop_geometry(nct[c(10:15)])
# Possibly expressing the movement values in a relative way as compared to the cell's maximum value
#d <- st_drop_geometry(nct[c(10:15,17)])
#d <- sweep(d[, -(7)], 1, d[, 7], "/") 
# Scaling the values to express it in standard deviation
d <- scale(d, center=T,scale=T) 
# Creating the groups (here 6) with k-means methods
groupes.kmeans <- kmeans(d,centers=6,nstart=5)
print(groupes.kmeans)

## CREATING A PLOT TO DISPLAY THE AVERAGE PROFILE OF EACH CALSS ##
# Renaming the groups for clarity when plotting (making appear the number of individuals for each group)
classes1 <- groupes.kmeans$centers %>%
  as.data.frame()
classes1$class <- c(
  paste('Class 1', " (", groupes.kmeans$size[1], ")"),
  paste('Class 2', " (", groupes.kmeans$size[2], ")"),
  paste('Class 3', " (", groupes.kmeans$size[3], ")"),
  paste('Class 4', " (", groupes.kmeans$size[4], ")"),
  paste('Class 5', " (", groupes.kmeans$size[5], ")"),
  paste('Class 6', " (", groupes.kmeans$size[6], ")")
  )
# Converting the dataframe in long format for plotting purpose
classes1_long <- gather(classes1, variable, value, -class)
# Re-ordering the variables for it to appear in chronological order in the plot
classes1_long$variable <- factor(classes1_long$variable, levels=c("wd.5h30am","wd.1h30pm","wd.9h30pm","m", "we.5h30am","we.1h30pm","we.9h30pm"))
# Plotting the classes with the average profile of each group
plot <- ggplot(classes1_long) +
  geom_bar(aes(x = variable, y = value, fill = class),
           stat = "identity") +
  facet_wrap(~ class) +
  geom_vline(xintercept=3.5)+
  theme_wsj()+ 
  #scale_colour_wsj("colors6")+
  theme(strip.text = element_text(size=11, face = "bold"),
        panel.background = element_rect(fill = "white"),
        plot.background = element_rect(fill = "white"),
        #panel.grid.major = element_line(color="grey"),
        legend.position = "none",
        axis.title.y = element_text(size=14, face="bold"),
        axis.text.y = element_text(size=9.5, face="bold"),
        axis.text.x = element_text(size=9.5, face="bold"))+
  xlab("") + 
  ylab("Incoming movement (in s.d.)")+
  scale_fill_brewer(palette = "Set2")
plot(plot)
# Exporting in vectorial format for any modification to bring externally to the plot 
ggsave(file="../plot_classes_tiles.svg", plot=plot)

## MODEL TO ESTABLISH THE INFLUENCE OF THE MOVEMENT CLASSES AND CENTRALITY INDICES ON DENGUE INCIDENCE ##
# Attaching the group belonging to each tile
nct$class_mob <- as.factor(groupes.kmeans$cluster)
# Fitting a glm of poisson family on both years of dengue data
fit08 <- glm(data = nct, d08 ~ class_mob, family = poisson, offset = log(pop_tile)) 
fit09 <- glm(data = nct, d09 ~ class_mob, family = poisson, offset = log(pop_tile))
# Generating the latex code to format the regression tables
stargazer::stargazer(fit08)
stargazer::stargazer(fit09)

## EXPORTING THE FILE WITH THE CLASSES FOR GIS MAPPING
st_write(st_as_sf(nct, wkt = "geometry"), "../Tiles_with_mob_kmeans/Tiles_with_mob_class.gpkg")
