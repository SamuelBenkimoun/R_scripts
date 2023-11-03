library(dplyr)
library(readxl)
library(jsonlite)
library(sf)

#Function for distance calculation to amenities using Google Maps API
amenities <- function(sample, type, key, distance) {
  for(i in 1:nrow(sample)) {
    #in case some new row has to be created
    row_name= paste("closest",gsub('%20','_',type), sep = "_")
    row_name2= paste(substr(distance, start = 1, stop = 1), "time", gsub('%20','_',type), sep = "_")
    row_name3= paste(substr(distance, start = 1, stop = 1), "dist", gsub('%20','_',type), sep = "_")
    coords = c(sample[[i,"latitude"]], sample[[i,"longitude"]])
    url <- paste("https://maps.googleapis.com/maps/api/place/findplacefromtext/json?",
                 "&input=",
                 type,
                 "&inputtype=textquery",
                 "&locationbias=point:",
                 coords[1], 
                 ",",
                 coords[2],
                 "&fields=name,place_id",
                 "&key=",
                 key,
                 sep = "")
    res <- fromJSON(url)
    #sample[i, row_name] <- res$candidates$name
    sample[i,row_name] <- res$candidates$name
    
    url2 <- paste("https://maps.googleapis.com/maps/api/distancematrix/json?",
                  "&origins=",
                  coords[1], 
                  ",",
                  coords[2],
                  "&destinations=place_id:",
                  res$candidates$place_id,
                  "&mode=",
                  distance,
                  "&key=",
                  key,
                  sep = ""
    )
    res2 <- fromJSON(url2)                
    sample[i,row_name2] <- res2$rows$elements[[1]][1,2][[2]]
    sample[i,row_name3]<- res2$rows$elements[[1]][1,1][[2]]
    print(i)
    print(sample[i,row_name])
    i = i+1
    Sys.sleep(1)
  }
  return(sample)
}

#Function to return the closest fiven amenities to each household using an Open Street Map dataset
osm_amenities <- function(df, path, type){
  row_name=paste("closest",type, sep = "_")
  row_name2=paste("closest",type,"id", sep = "_")
  row_name3=paste("closest",type,"type", sep = "_")
  row_name4=paste("euc_distance",type,sep = "_")
  osm <- st_read(path)
  osm.sp <- as(osm, "Spatial")
  df.sp <- df %>% 
    st_as_sf(coords = c("longitude", "latitude"), crs=4326) %>%
    as("Spatial")
  dist <- geosphere::dist2Line(p= df.sp, line= osm.sp)
  df <- df %>% mutate(!!row_name:= st_drop_geometry(osm[dist[,4],])$name) %>%
    mutate(!!row_name2:= st_drop_geometry(osm[dist[,4],])$osm_id) %>%
    mutate(!!row_name3:= st_drop_geometry(osm[dist[,4],]$fclass)) %>%
    mutate(!!row_name4:= round(dist[,1], 2))
  return(df)
  
  #optional part to draw the lines from the household to the closest amenity
  #sample_21 <- cbind(df, dist)
  #lines = sprintf("LINESTRING(%s %s, %s %s)", sample_21$longitude, sample_21$latitude, sample_21$lon, sample_21$lat)
  #path2 = paste("./CHALLINEQ/lines_", type,".csv", sep="")
  #write.csv(lines, path2)
}

#Saving the GoogleMaps API Key 
key <-  "API_key"

#Importing the survey data from the Excel file
Challineq_data <- read_excel("CHALLINEQ/ChallineqColonyMerged_Sept5th.xlsx", 
                                            col_types = c("numeric", "text", "text", 
                                                          "text", "text", "text", "text", "numeric", 
                                                          "text", "numeric", "numeric", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "numeric", 
                                                          "text", "text", "text", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "numeric", "numeric", 
                                                          "numeric", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "text", 
                                                          "text", "text", "text", "text", "numeric", 
                                                          "numeric", "numeric", "text", "text", 
                                                          "text", "numeric", "text", "numeric", 
                                                          "numeric", "text", "numeric", "text", 
                                                          "numeric", "text", "numeric", "numeric", 
                                                          "text", "numeric", "numeric", "text", 
                                                          "numeric", "text", "numeric", "text", 
                                                          "numeric", "numeric", "text", "numeric", 
                                                          "text", "numeric", "text", "numeric", 
                                                          "numeric", "text", "numeric", "numeric", 
                                                          "text", "text"))

sample <- subset(Challineq_data, coord_corrected != "NA")

##SUBWAY STATION
type="subway%20station"
distance = "walking" #has to be either "driving", "walking", "transit" or "bicycling"
#Resetting the subway station in case the location has been updated meanwhile
sample$closest_subway_station <- "NA"
sample$w_time_subway_station <- "NA" %>%
  as.numeric()
sample$w_dist_subway_station <- "NA" %>%
  as.numeric()
sample <- amenities(sample, "subway%20station", key, distance)

##ATM
type="atm"
distance = "walking" #has to be either "driving", "walking", "transit" or "bicycling"
#Resetting the subway station in case the location has been updated meanwhile
sample$closest_atm <- "NA"
sample$w_time_atm <- "NA" %>%
  as.numeric()
sample$w_dist_atm <- "NA" %>%
  as.numeric()
sample <- amenities(sample, type, key, distance)

##MOSQUE
type="mosque"
distance = "walking" #has to be either "driving", "walking", "transit" or "bicycling"
#Resetting the subway station in case the location has been updated meanwhile
sample$closest_mosque <- "NA"
sample$w_time_mosque <- "NA" %>%
  as.numeric()
sample$w_dist_mosque <- "NA" %>%
  as.numeric()
sample <- amenities(sample, type, key, distance)

##MANDIR
type="hindu%20temple"
distance = "walking" #has to be either "driving", "walking", "transit" or "bicycling"
#Resetting the subway station in case the location has been updated meanwhile
colnames(sample)
colnames(sample)[136:138] <- c("closest_hindu_temple", "w_time_hindu_temple", "w_dist_hindu_temple")
sample$closest_hindu_temple <- "NA"
sample$w_time_hindu_temple <- "NA" %>%
  as.numeric()
sample$w_dist_hindu_temple <- "NA" %>%
  as.numeric()
sample <- amenities(sample, type, key, distance)

#saving temporary files to not lose the result of API requests
writexl::write_xlsx(sample, "./CHALLINEQ/sample_temp.xlsx")

#OPEN STREET MAP DISTANCES

## DISTANCE TO THE CLOSEST WATERWAY
type = "waterway"
path <- "./CHALLINEQ/selected_ww.gpkg"
sample$closest_waterway <- "NA"
sample$closest_waterway_id <- "NA"
sample$closest_waterway_type <- "NA"
sample$euc_distance_waterway <- NA %>%
  as.numeric()
sample <- osm_amenities(df = sample, path = path, type = type)

## PARK
type = "park"
path <- "~/CHALLINEQ/selected_forest-nature-parks-recreationalground.gpkg"
sample$closest_park <- "NA"
sample$closest_park_id <- "NA"
sample$closest_park_type <- "NA"
sample$euc_distance_park <- NA %>%
  as.numeric()
sample <- osm_amenities(df = sample, path = path, type = type)

## MAIN ROAD
type = "main_road"
path <- "~/CHALLINEQ/selected_roads.gpkg"
sample$closest_main_road <- "NA"
sample$closest_main_road_id <- "NA"
sample$closest_main_road_type <- "NA"
sample$euc_distance_main_road <- NA %>%
  as.numeric()
sample <- osm_amenities(df = sample, path = path, type = type)

##JOINING THE AQI VALUES ??

## Merging the datasets and writing a new file
sample2 <- Challineq_data[is.na(Challineq_data$coord_corrected),]
sample2 <- subset(Challineq_data, is.na(Challineq_data$coord_corrected))
colnames(sample2) <- colnames(sample)
writexl::write_xlsx(rbind(sample,sample2), path = "./CHALLINEQ/Challineq_loc_corrected_Nov1.xlsx")


