library(readr)
library(tidyr)
library(lubridate)
library(magrittr)
library(dplyr)
library(reshape2)


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

#Setting the working directory where the other csv dataset is
setwd("~/FB India Mobilities Between Tiles 2021 /Delhi Coronavirus Disease Prevention Map Mar 21 2020 Mobility Between Tiles")
#List all the file and read the csv data
list_csv <- list.files()
data_csv <- lapply(list_csv, read.csv, header=TRUE, sep=",")
# Separate the spatial information in another list, in order to extract all movement flow combinations on the observed time perio
data_csv2 <- lapply(data_csv, subset, select=c(GEOMETRY, start_polygon_name, end_polygon_name, start_polygon_id, end_polygon_id, length_km))
# Pasting all the list content vertically in one df
data_csv2 <- do.call(rbind,data_csv2)
# Identify lines with duplicates (i.e. redudant mobility combination)
duplicates <- which(duplicated(data_csv2))
# Remove them from the df, to have a reference df with all the locations
data_csv2 <- data_csv2[-duplicates,]
# In another list, we separate the information about number of people at each time step
data_csv <- lapply(data_csv, subset, select=c(GEOMETRY, n_baseline))
# To keep only the part after the underscore (date and time) in the filename
#list_csv <- sub("^[^_]*_", "", list_csv)
# We rename the population row by the time step date (if contained in the filename)
for (i in 1:length(data_csv)){
  colnames(data_csv[[i]])[2] <- substr(list_csv[i], 1, nchar(list_csv[i]) - 4)
  }
# Alternative loop in case the time step date in containted in the content of the file itself)
# for (i in 1:length(data_csv2)){colnames(data_csv2[[i]])[3] <- as.character(data_csv2[[i]]$Date.Time[[1]])}
# Loop to left join population information at each time step to the reference spatial df
for (i in 1:length(data_csv)){
  data_csv2 <- left_join(data_csv2, as.data.frame(data_csv[i]), by= c("GEOMETRY"="GEOMETRY"))
  }
# Replace all the NA values by 0
data_csv2[is.na(data_csv2)] = 0
# Rename the fields of dates
colnames(data_csv2)[7:27] <- sub("^[^_]*_", "", colnames(data_csv2[7:27]))
# To verify the spatial resolution
min(subset(data_csv2, length_km != 0)$length_km)



