#Importing the libraries and setting directories
library(sf)
library(dplyr)
library(purrr)
library(lubridate)
library(tmap)
library(readr)
library(tidyr)
librar(readxl)
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
