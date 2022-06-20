#Importing the required libraries
library(readxl)
library(dplyr)
#Importing the data file with the survey results
survey <- read_excel("CHALLINEQ/Challineq_Geo_21apr2022.xlsx")
#Keeping only a certain number of variables of interest
survey <- survey[c("ID", "garden","shared_green","shared_green_price","income","house_independant",
           "house_status", "common_entry", "odour","incident","children_number","SC","ST","OBC","General",
           "Other","religion","colony", "euc_distance_park")]
#Reformat certain variables into binary values
survey$garden <- ifelse(survey$garden=="Yes",1,0) 
survey$shared_green <- ifelse(survey$shared_green=="Yes",1,0)
survey$house_independant <- ifelse(survey$house_independant=="Yes",1,0) 
survey$odour <- ifelse(survey$odour=="Yes",1,0) 
survey$incident <- ifelse(survey$incident=="Yes",1,0)  
survey$common_entry <- ifelse(survey$common_entry=="Common entry",1,0)
#Reformat certain variables into numeric values
survey$shared_green_price <- as.numeric(survey$shared_green_price)
survey$children_number <- as.numeric(survey$children_number)
survey[11:15] <- sapply(survey[11:15],as.numeric)
#Reformat certain variables as factors
survey$income <- as.factor(survey$income)
survey$income <- factor(survey$income, levels = c("Less than 2,000 Rs/month", 
                                 "2,000 to 6,000", 
                                 "6,000 to 10,000",
                                 "10,000 to 20,000",
                                 "20,000 to 30,000",
                                 "30,000 to 40,000",
                                 "40,000 to 50,000",
                                 "50,000 to 60,000",
                                 "60,000 to 70,000",
                                 "70,000 to 110,000",
                                 "110,000 and above"))
survey$colony <- as.factor(survey$colony)
#Subset dataset removing some non-attributed values 
survey <- subset(survey, house_status != "NA")
                                
                                 
