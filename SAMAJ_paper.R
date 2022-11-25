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
