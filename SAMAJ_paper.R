library(readr)
library(tidyr)
library(lubridate)
library(magrittr)
library(ggplot2)
library(ggdark)
library(dplyr)
library(reshape2)
library(sf)


#Disabling the scientific notation
options(scipen=999)
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

# Setting working directory where to get a more recent dataset to compute the FB users ratio in a post-lockdown context
setwd("~/FB India Mobilities Between Tiles 2021 /Delhi Coronavirus Disease Prevention Map Mar 21 2020 Mobility Between Tiles")
# Read the csv data
list_csv <- list.files()
data_csv <- lapply(list_csv, read.csv, header=TRUE, sep=",")
# Calculating the penetration rate, 30 nov 2021 3rd timestep
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
