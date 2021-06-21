#Importing the needed libraries
library(magrittr)
library(purrr)
library(dplyr)
library(readxl)
library(readr)
library(sf)
library(stplanr)
library(tidyr)
library(igraph)
library(ggplot2)
library(formattable)
library(lubridate)
#Preventing the scientific notation for large numbers
options(scipen=999)
#Reading the census files related to households
setwd("~/Census_2011_NCR/Households/Census 2011 Households NCT-UP-HR Subdistricts")
list_csv <- list.files()
hh <- lapply(list.files(), function (x) {
  read_excel(x, skip = 6)
  })
#Keeping only the sub-districts and total value
hh <- lapply(hh, subset, grepl("Sub-Dist", `9`))
hh <- lapply(hh, subset, `10` == "Total")
#Building a unique table with all the information
hh <- do.call(rbind,hh)
hh <- hh[c(5,12)]
colnames(hh) <- c("Subdistt","HH_GCONDITION")

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

#mobility_areas <- c(unique(Delhi_Tiles_til_2603$Starting.Region.Name), "Chanakya Puri", "Darya Ganj", "Connaught Place", "Gandhi Nagar", "Karol Bagh", "Pahar Ganj", "Sadar Bazar", "Seelampur", "Seemapwi", "Modinagar", "Hapur", "Garhmukteshwar")
# Given that some areas are finally not in the mobility dataset (50km radius approx), selection of the common areas with the Census data
Delhi_wider_urban_area_70km <- read_csv("~/Delhi_wider_urban_area_70km.csv")
mobility_areas <- Delhi_wider_urban_area_70km$L3_NAME
mobility_areas <- mobility_areas[!(mobility_areas %in% c("Tijara", "Hathin", "Nuh", "Kosli", "Matenhail","Gohana","Samalkha", "Ganaur","Baraut"))]
data_csv <- subset(data_csv, Name %in% mobility_areas) %>%
  subset(!as.numeric(Subdistt) == 402)
data_csv <- merge(data_csv, rel[,c(3,10,86,87,88,89)]) %>%
  merge(hh)

#Importing the goegraphical layer and computing the area for density computation
subd <- st_read("../SDT_shape_2011/India_L3_Administrative_Boundaries.shp")
subd$area <- st_area(subd)
subd <- st_drop_geometry(subd[c("L3_CODE","area")])
colnames(subd) <- c("Subdistt", "AREA")
# Getting the subdistrict ID in numeric format to avoid  the "00" preceding, useful to join later with a geographical layer and map
data_csv <- transform(data_csv, Subdistt = as.numeric(Subdistt)) %>%
  merge(subd) %>%
  transform(DENSITY = TOT_P/AREA)

#Scaling the values
d.cr <- scale(data_csv[,-c(1, 2, 3, 7, 15, 16, 19, 21, 24)], center=T,scale=T)
#Applying a k-means classification with 8 classes
groupes.kmeans <- kmeans(d.cr,centers=8,nstart=5)
print(groupes.kmeans)
data_csv$class_k8 <- groupes.kmeans$cluster

# keeping class centers
classes1 <- groupes.kmeans$centers %>%
  as.data.frame()
# plotting the group profiles
colnames(classes1)<- c("P_06", "P_SC", "P_LIT", "MAIN_CL_P" ,"MAIN_AL_P", "MAIN_HH_P", "MAIN_OT_P", "MARGWORK_P", "NON_WORK_P", "URB_RATE", "HINDUS", "MUSLIMS", "F_RATIO", "DENSITY", "HH_GCONDITION")
classes1$class <- c(
  paste('Class 1', " (", groupes.kmeans$size[1], ")"),
  paste('Class 2', " (", groupes.kmeans$size[2], ")"),
  paste('Class 3', " (", groupes.kmeans$size[3], ")"),
  paste('Class 4', " (", groupes.kmeans$size[4], ")"),
  paste('Class 5', " (", groupes.kmeans$size[5], ")"),
  paste('Class 6', " (", groupes.kmeans$size[6], ")"),
  paste('Class 7', " (", groupes.kmeans$size[7], ")"),
  paste('Class 8', " (", groupes.kmeans$size[8], ")")
)
#Setting the relevant names to the categories (for the class centers files in order to plot it)
classes1$class <- c(
  paste('Rural intermediary localities', " (", groupes.kmeans$size[1], ")"),
  paste('Urban intermediary to upscale', " (", groupes.kmeans$size[2], ")"),
  paste('Urban business centers', " (", groupes.kmeans$size[3], ")"),
  paste('Urban lower-end areas (M +)', " (", groupes.kmeans$size[4], ")"),
  paste('Rural lower-end localities', " (", groupes.kmeans$size[5], ")"),
  paste('Urban very dense lower-end areas', " (", groupes.kmeans$size[6], ")"), 
  paste('Suburban intermediary areas', " (", groupes.kmeans$size[7], ")"),
  paste('Urban dense areas (SC +)', " (", groupes.kmeans$size[8], ")")
)
#Setting the relevant names to the categories (for all the subdistricts)
df <- data.frame (class_k8  = c(2,6,4,5,8,1,7,3),
                  category = c("Urban intermediary to upscale", 
                               "Urban very dense lower-end areas",
                               'Urban lower-end areas (M +)',
                               'Rural lower-end localities',
                               'Urban dense areas (SC +)',
                               'Rural intermediary localities',
                               'Suburban intermediary areas',
                               'Urban business centers'
                               ))
data_csv <- merge(data_csv, df)
# Writing the resulting table in a csv file
write_csv(data_csv, "../NCR50_Subdt_Classif_8classes.csv")

#Plotting the category centers to get a view on each category
ggplot(gather(classes1, variable, value, -class)) +
  geom_bar(aes(x = variable, y = value, fill = class),
           stat = "identity") +
  facet_wrap(~ class) +
  coord_flip() +
  theme(strip.text = element_text(size=10, face = "bold"), legend.position = "none")
#theme_bw()

sdt <- st_read("../SDT_shape_2011/India_L3_Administrative_Boundaries.shp")
colnames(sdt)[7] <- c("Subdistt")
sdt <- merge(sdt, data_csv[c(1,2,25)]) %>%
  subset(category %in% data_csv$category)
#Get the average surface of the subdistricts in km2 square:
mean(st_area(sdt$geometry)/1000000)

#Importing mobility data corresponding to the lockdown implementation period (until 26 March 2020)
Delhi_Tiles_til_2603 <- read_delim("~/Delhi Tiles Til 26 March 2020/Delhi_Tiles_till_2603.csv", ";", escape_double = FALSE, trim_ws = TRUE)
#Preparing the data to plot distributions and curve of total people moving
dd <- Delhi_Tiles_til_2603[7:100]
dd <- dd[grep("1330", colnames(dd))]
#Keeping the timesteps at 1:30pm
colnames(dd) <- gsub('.1330', '', colnames(dd)) 
dd <- gather(dd, Date, People)
dd$Date <- gsub('\\.', "/", dd$Date) %>% 
  ymd()
#Drawing the curve of cumulated trips registered (in thousands) and the number of people by mobility flow
ggplot(dd, aes(x = Date, y = jitter(People))) +
  geom_point(alpha=0.05, color="#3933FF")+
  geom_line(data = cd, size = 1.3, aes(x = Date, y = Total_movements/1000))+
  geom_vline(xintercept = as.Date("2020-03-10"), linetype="dotted", color = "violet", size=1.2)+
  geom_vline(xintercept = as.Date("2020-03-22"), linetype="dotted", color = "orange", size=1.2)+
  geom_vline(xintercept = as.Date("2020-03-24"), linetype="dotted", color = "red", size=1.2)+
  labs(x = "", y = "Total Movements")+
  theme(text = element_text(size=20))

#Subsetting the needed time-steps from the mobility data
Delhi_Tiles_til_2603 <- Delhi_Tiles_til_2603[c(2,3,4,11,99)]

#Importing the rest of mobility data for the continuation of the observation window (May and June 2020) and subsetting the dates and fields needed
Delhi_Tiles_May <- read_csv("~/Delhi Tiles 2km from May 2020/Delhi Mvt Point/Delhi Coronavirus Disease Prevention Map Mar 21 2020 Id  Movement between Tiles__2020-05-08 0800.csv")
Delhi_Tiles_May <-  Delhi_Tiles_May[c("geometry", "start_polygon_name", "end_polygon_name", "n_crisis")]
colnames(Delhi_Tiles_May)[1] <- c("Geometry")
colnames(Delhi_Tiles_May)[4] <- c("2020.05.08.1330")

Delhi_Tiles_May2 <- read_csv("~/Delhi Tiles 2km from May 2020/Delhi Mvt Point/Delhi Coronavirus Disease Prevention Map Mar 21 2020 Id  Movement between Tiles__2020-05-26 0800.csv")
Delhi_Tiles_May2 <-  Delhi_Tiles_May2[c("geometry", "start_polygon_name", "end_polygon_name", "n_crisis")]
colnames(Delhi_Tiles_May2)[1] <- c("Geometry")
colnames(Delhi_Tiles_May2)[4] <- c("2020.05.26.1330")

Delhi_Tiles_May3 <- read_csv("~/Delhi Tiles 2km from May 2020/Delhi Mvt Point/Delhi Coronavirus Disease Prevention Map Mar 21 2020 Id  Movement between Tiles__2020-06-11 0800.csv")
Delhi_Tiles_May3 <-  Delhi_Tiles_May3[c("geometry", "start_polygon_name", "end_polygon_name", "n_crisis")]
colnames(Delhi_Tiles_May3)[1] <- c("Geometry")
colnames(Delhi_Tiles_May3)[4] <- c("2020.06.11.1330")

#Building the common mobility file where each mobility flow has its continuation of values for the time-steps we retained and category information
mob_tiles <- merge(Delhi_Tiles_til_2603, Delhi_Tiles_May) %>%
  merge(Delhi_Tiles_May2) %>%
  merge(Delhi_Tiles_May3)
mob_tiles <- st_as_sf(mob_tiles, wkt = "Geometry")
#Keeping the origin of the flows to join the category information
start <- mob_tiles
start$Geometry <- st_line_sample(mob_tiles$Geometry, sample = 0) 
#joining the L3 level from the Census 2011
start <- st_set_crs(start, 4326) %>% 
  st_join(sdt)
#Keeping the destination of the flows to join the category information
end <- mob_tiles
end$Geometry <- st_line_sample(mob_tiles$Geometry, sample = 1) 
#joining the L3 level from the Census 2011
end <- st_set_crs(end, 4326) %>% 
  st_join(sdt)
#Joining both start and end information for category and sub-district name as per Census 2011 
mob_tiles$start <- start$L3_NAME
mob_tiles$end <- end$L3_NAME
mob_tiles$st_c <- start$L3_CODE
mob_tiles$en_c <- end$L3_CODE
mob_tiles$start_cat <- start$category
mob_tiles$end_cat <- end$category
# Dropping the geometry to keep  a simple table with only the timesteps of interest
#mob_tiles <- st_drop_geometry(mob_tiles[c(1,2,12,100,102,103,104,105,106,107,108,109)])
mob_tiles <- cbind(st_drop_geometry(mob_tiles[11:14]), st_drop_geometry(mob_tiles[5:9]))

# Filtering the mobility flows staying continuously over 10 people in order to avoid artificial "0" 
mt_plot <- mob_tiles %>% subset(`2020.02.26.1330`>=10 & `2020.03.26.1330`>=10 & `2020.05.08.1330`>=10 & `2020.05.26.1330`>=10 & `2020.06.11.1330`>=10)
# Getting the table of individuals by categories
table(mt_plot$start_cat)
# Getting the mean of mobility by flow for each categories
aggregate(mt_plot$`2020.02.26.1330`, by = list(Category=mt_plot$start_cat), FUN = mean)
# Transforming the table with index values from the 26th feb 2020
mt_plot <- cbind(mt_plot[1:4] , (sweep(mt_plot[, -c(1:4)], 1, mt_plot[, 5], "/"))*100)
# Rounding the values to 0,1
mt_plot[5:9] <- round(mt_plot[5:9], 1)
colnames(mt_plot) <- c("From", "To", "Category_From", "Category_To", "26 Feb 2020 1:30pm", "26 Mar 2020 1:30pm", "8 May 2020 1:30pm", "26 May 2020 1:30pm", "11 June 2020 1:30pm")
# Converting the data to the long format for plotting convenience
mt_long <- gather(mt_plot[-c(1,2,4)], variable, value, -Category_From)  
mt_long$variable <-  factor(mt_long$variable, levels=c('26 Feb 2020 1:30pm','26 Mar 2020 1:30pm','8 May 2020 1:30pm','26 May 2020 1:30pm', '11 June 2020 1:30pm'))

## Box-plots
ggplot(data=mt_long, aes(x=Category_From, y=value, colour=Category_From))+
  #geom_boxplot()+
  geom_jitter(width = 0.15)+ 
  geom_boxplot(alpha=0.90)+  
  theme(axis.text.x = element_text(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.text.y = element_text(size = "10"), plot.title = element_text(size=18, face="bold", hjust = 0.5), legend.position="left", strip.text = element_text(size = 12, face = "bold.italic"))+
  ggtitle("Outgoing mobility flows variation in Delhi NCR")+
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  scale_y_continuous(breaks = seq(0,150,20), limits = c(0,150)) + 
  labs(x = "")+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=1, binwidth = 0.015)+
  facet_wrap(~ variable )

##To get the outliers by category and date
categories <- unique(mt_plot$Category_From)
i = 1
j = 6
#creating an empty dataframe
df_out <- setNames(data.frame(matrix(ncol = 10, nrow = 0)), append(colnames(mt_plot), "date_out"))
#initiating the loop to fill the df with the outliers
for (i in 1:length(categories)){
  for (j in 6:9){
  outliers <- boxplot.stats(mt_plot[mt_plot$Category_From == categories[i],j])$out
  o <- mt_plot[mt_plot$`26 Mar 2020 1:30pm` %in% outliers & mt_plot$Category_From == categories[i],]
  if (nrow(o)!=0){
    o$date_out <- colnames(mt_plot)[j]
    df_out <- rbind(df_out, o)
    }
  j = j+1
  }
  j=6
  i = i+1 
}
write.csv(df_out, "./outliers_mob-tiles_cat8.csv")

## Violin-plots
ggplot(data=subset(mt_long, variable != "26 Feb 2020 1:30pm"), aes(x=Category_From, y=value, colour=Category_From))+
  geom_violin()+
  theme(axis.text.x = element_text(), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.text.y = element_text(size = "10"), plot.title = element_text(size=18, face="bold", hjust = 0.5), legend.position="left", strip.text = element_text(size = 12, face = "bold.italic"))+
  ggtitle("Outgoing mobility flows variation in Delhi NCR")+
  scale_x_discrete(labels = NULL, breaks = NULL) + 
  scale_y_continuous(breaks = seq(0,150,20), limits = c(0,150)) + 
  labs(x = "")+
  #geom_dotplot(binaxis='y', stackdir='center', dotsize=2, binwidth = 0.015)+
  facet_wrap(~ variable )

## Regression curves plots
mt_long <- gather(mt_plot[-c(1,2)], variable, value, -c("Category_From", "Category_To"))  
mt_long$variable <-  factor(mt_long$variable, levels=c('26 Feb 2020 1:30pm','26 Mar 2020 1:30pm','8 May 2020 1:30pm','26 May 2020 1:30pm', '11 June 2020 1:30pm'))
levels(mt_long$variable) <- c("26/02/2020","26/03/2020","08/05/2020","26/05/2020","11/06/2020")
mt_long$variable <- as.Date(mt_long$variable, format='%d/%m/%y')
ggplot(data=mt_long, aes(x=jitter(as.numeric(variable)), y=value, colour = Category_To))+
  geom_point()+
  geom_smooth(se = FALSE)+
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x=element_blank(), axis.title.x=element_blank(), axis.text.y = element_text(size = "10"), plot.title = element_text(size=18, face="bold", hjust = 0.5), legend.position="left", strip.text = element_text(size = 12, face = "bold.italic"))+
  ggtitle("Outgoing mobility flows variation in Delhi NCR")+
  scale_y_continuous(breaks = seq(0,150,20), limits = c(0,150)) + 
  facet_wrap(~ Category_From)

## Wrapped OD Matrix plots
mt_pivot <- aggregate(mt_long[4], by = list(From=mt_long$Category_From, To=mt_long$Category_To, Date=mt_long$variable), FUN = mean)
ggplot(data=subset(mt_pivot, Date != "2020-02-26"), aes(x=To, y=From, fill = value, group = 1))+
  geom_tile()+
  geom_text(aes(x=To, y=From, label = round(value, 1), fontface = "bold"), color = "white", size = 3.5) +
  theme(axis.text.x = element_text(angle = 90), axis.ticks.x=element_blank(), axis.text.y = element_text(size = "10"), plot.title = element_text(size=18, face="bold", hjust = 0.5), legend.position="left", strip.text = element_text(size = 12, face = "bold.italic"))+
  ggtitle("Mobility matrix among categories in Delhi NCR")+
  scale_fill_viridis(discrete=FALSE, option = "viridis")+ 
  guides( fill = FALSE)+
  facet_wrap(~ Date)

## Overall table of mobility evolution summarized by category
mt_cat <-  aggregate(mt_plot[5:9], by=list(Category = mt_plot$start_cat), FUN = sum)
mt_cat <- cbind(pivot$From , (sweep(mt_cat[, -(1)], 1, mt_cat[, 2], "/"))*100)
colnames(mt_cat) <- c("From", "26 Feb 2020 1:30pm", "26 Mar 2020 1:30pm", "8 May 2020 1:30pm", "26 May 2020 1:30pm", "11 June 2020 1:30pm")
mt_cat[2:6] <- round(mt_cat[2:6], 1)
formattable(mt_cat,
            list(
              `26 Feb 2020 1:30pm` = color_bar("#90EE90"),
              `26 Mar 2020 1:30pm` = color_bar("#FA614B66", fun = function(x) (x/100)),
              `8 May 2020 1:30pm` = color_bar("#FA614B66", fun = function(x) (x/100)),
              `26 May 2020 1:30pm` = color_bar("#FA614B66", fun = function(x) (x/100)),
              `11 June 2020 1:30pm` = color_bar("#FA614B66", fun = function(x) (x/100))
            ))


#Network analysis by categories 
network <- aggregate(mob_tiles[5:9], by=list(From=mob_tiles$start_cat, To=mob_tiles$end_cat), FUN = sum)
size = as.vector(t(sqrt(aggregate(network[3], by= list(To=network$From), FUN = sum)[2])/6))
network <- subset(network, network$`2020.03.26.1330` >= 100) 
network <- cbind(network[1:2] , (sweep(network[, -c(1:2)], 1, network[, 3], "/"))*100)
#Drawing the network graph by category
plot(graph_from_data_frame(d=network[1:2]),
     vertices = unique(network$From),
     edge.arrow.size=0 ,
     edge.arrow.width=0,
     directed=F,
     #vertex.size= size,
     vertex.label.color="black",
     vertex.label.family="Times",
     vertex.label.font=2,
     edge.width=network[[6]]/10,
     #main = "ok",
     col.main = "white"
     #layout=layout_as_tree, 
)
#title(main = colnames(network[3]), colour = "white")
#df[df=="" | df==12] <- NA

plot(graph_from_data_frame(d=network[1:2]), 
     vertices = unique(network$From),
     #directed=F,
     #vertex.size= size,
     #edge=network$`2020.02.26.1330`,  
     edge.arrow.size=0.2, 
     edge.arrow.width=0.8, 
     #edge.width=log(network$`2020.02.26.1330`)^2/6,
     #edge.color="white",
     vertex.label.color="black",
     vertex.label.family="Times",
     vertex.label.font=2
)

write_csv(network, "./network_NCR50_8class.csv")
