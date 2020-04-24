#Osm network distance computation
library(httr)
library(sf)
library(stringr)
library(osrm)
library(reshape2)
library(matrixStats)

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

dt$distance <- as.numeric(dt$distance)
#in case there are NA values that would block mean calculation
dt <- subset(dt, !is.na(dt$distance))

#create a new dataframe to analyse the evolution of distance traveled by timestep, weighted by the number of people moving
dm = data.frame(matrix(vector(), 0, 4,
                       dimnames=list(c(), c("date", "mean_distance", "median_distance", "standard_deviation"))),
                stringsAsFactors=F)
j <- 1
for (i in 5:(ncol(dt)-5)) {
  dm[j,]$date <- colnames(dt[i])
  dm[j,]$mean_distance <-  weighted.mean(dt$distance, dt[[i]])
  dm[j,]$median_distance <-  weightedMedian(dt$distance, dt[[i]])
  sdt <- aggregate(dt[[i]] ~ dist, data = dt, sum)
  dm[j,]$standard_deviation <- sd(sdt[[2]])
  j <- j+1
}

rownames(dm) <- c(1:nrow(dm))
write.csv(dm, "./Delhi Tiles Til 26 March 2020/distances.csv")

# plotting the graph of distance evolution
ggplot(dm, 
       aes(x=date, 
           y=median_distance, 
           group=1))+
  labs(x="Date",
       y="Mean of traveled distance (m)")+ 
  ggtitle("Delhi NCR median of distance traveled")+
  geom_text(label=as.integer(dm$median_distance),
            colour="black",
            fontface = "italic",
            check_overlap = TRUE, 
            nudge_y = +1, 
            size=3.2
            )+
  scale_x_discrete(expand=c(0,0),
                   breaks=c("2020.02.25.0530","2020.03.02.0530","2020.03.10.0530","2020.03.17.0530", "2020.03.22.0530"))+
  geom_line()

