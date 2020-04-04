#Osm network distance computation
library(httr)
library(sf)
library(stringr)
library(osrm)

# reading the data
Delhi_Tiles_till_2603 <- read_delim("Delhi Tiles Til 26 March 2020/Delhi_Tiles_till_2603.csv", 
                                    +     ";", escape_double = FALSE, col_types = cols(X1 = col_skip()), 
                                    +     trim_ws = TRUE)
dt <- st_as_sf(Delhi_Tiles_till_2603, wkt = "Geometry")
st_set_crs(dt, 4326)

# pasting x,y for both departure and arrival point of the mobility flows
dt_xy <- do.call(rbind, st_geometry(st_line_sample(dt, sample=0))) %>% as_tibble() %>% setNames(c("lon1","lat1"))
dt_xy2 <- do.call(rbind, st_geometry(st_line_sample(dt, sample=1))) %>% as_tibble() %>% setNames(c("lon2","lat2"))
dt <- cbind(st_drop_geometry(dt), dt_xy)
dt <- cbind(dt, dt_xy2)
dt$distance <- 0

# Using osrmRoute, number of requests limited

#for (i in 1:(nrow(dt))) {
#  dt[i,]$distance <- osrmRoute(src=c(dt[i,]$lon1, dt[i,]$lat1), dst = c(dt[i,]$lon2, dt[i,]$lat2), overview = FALSE)[2]
#}

# Iterating on the whole dataset
for (i in 1:(nrow(dt))) {
print(i)  
# creating the URL to request on the OSRM demo server hosted by Mapbox, usage is limited to reaonable and non-commercial
req <- paste(
  "http://router.project-osrm.org/route/v1/driving/", 
  dt[i,]$lon1, ",", 
  dt[i,]$lat1, ";", 
  dt[i,]$lon2, ",", 
  # overview parameter is set to false, because the details of routing are not needed here and would make the request heavier
  dt[i,]$lat2, "?overview=false", 
  sep="")
dist <- GET(url=req)
# extracting the distance variable from the body of the answer
dist <- content(dist, "text")
dist <- str_match(dist, 'distance(.*?)w')[,2]
dist <- parse_number(dist)
dt[i,]$distance <-dist
}
