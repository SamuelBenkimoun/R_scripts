library(readxl)
library(dplyr)
library(data.table)
Challineq_data <- read_excel("CHALLINEQ/Challineq_loc_corrected_Nov1st.xlsx")
View(Challineq_loc_corrected_Nov1st)

# Importing the raw pollution data from the Kobo file (already filtered to keep only certain columns)
ts_pol <-  read_excel("CHALLINEQ/Challineq_Time-stamps-pollution-levels.xlsx", col_types = c("text", "text", "date","numeric", "numeric", "numeric", "text"))
##Renaming the columns from the raw questionnaires
colnames(ts_pol) <- c("ID", "address", "time_of_visit", "US_index", "PM2_5", "TVOC", "uuid")
##Converting the identifier to numeric to merge it to the rest of the data 
ts_pol$ID <- as.numeric(ts_pol$ID)

# Merging the two datasets to have the correct timesteps and pollution measurements
#cm <- merge(Challineq_data[1:137], ts_pol[c("ID", "time_of_visit" ,"PM2_5")])
cm <- left_join(x=Challineq_data[1:137], y=ts_pol[c("ID", "time_of_visit" ,"PM2_5")], by = "ID")
#Retrieving the pollution files from the US Embassy station
setwd("~/CHALLINEQ/Pollution levels")
data_pollution <- lapply(list.files(), read.csv, header=TRUE, sep=",") %>%
  do.call(what = rbind)
##In the pollution data file, renaming the column to merge easily
colnames(data_pollution)[3] <- "time_of_visit"
##Converting to the date format matching the date information in Challineq file
data_pollution$visit_date <- as.POSIXct(data_pollution$visit_date, format = "%Y-%m-%d %I:%M %p", tz="UTC")
##Merging the two tables, merging the temporally nearest information from the pollution data
house <- data.table(ID = cm$ID, time_of_visit = cm$time_of_visit, PM2_5 = cm$PM2_5) %>%
  setkey(time_of_visit)
pol <- data.table(PM2_5_US = data_pollution$Raw.Conc., time_of_visit = data_pollution$time_of_visit) %>%
  setkey(time_of_visit)
comb <- pol[house, roll = "nearest" ]
##Removing the intermediary tables
rm(house, pol)

# Reordering the combined file before merging it with the survey data
colorder <- c("ID", "time_of_visit", "PM2_5", "PM2_5_US")
comb <- comb[,..colorder]
comb <- comb %>% group_by(ID) %>%
  slice_max(order_by = time_of_visit, n=1) %>%
  ungroup()
comb <- comb[!duplicated(comb2),]

# Final merging with the survey data
Challineq_data <- left_join(x=Challineq_data[1:137], y=comb2, by = "ID")

# Writing the file
writexl::write_xlsx(Challineq_data, "../Challineq_with_pm_28Nov2023.xlsx")
