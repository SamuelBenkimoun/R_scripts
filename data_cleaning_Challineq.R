library(readxl)
library(sf)
library(lubridate)
# Importing and formatting the survey data
surv <- read_excel("CHALLINEQ/Challineq_consolidated.xlsx", 
                   col_types = c("date", "date", "text", 
                                 "text", "text", "text", "text", "numeric", 
                                 "text", "numeric", "numeric", "numeric", 
                                 "numeric", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "numeric", 
                                 "text", "numeric", "text", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text", "numeric", "text", "text", 
                                 "text", "text", "text", "numeric", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "numeric", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "text", "text", "text", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "numeric", "numeric", 
                                 "text", "text", "text", "date", 
                                 "text", "numeric", "text", "numeric", 
                                 "text", "text", "numeric", "numeric", 
                                 "numeric", "text", "text"))
colnames(surv)[c(10,11)] <- c("latitude", "longitude")
#Importing Delhi boundaries
delhi_bounds <- st_read("./Delhi_bounds_corrected.gpkg")

#Flagging potential errors in the database
surv$flag <- "none"

#For missing price
surv$rent <- as.numeric(surv$rent)
surv$acquisition_price <- as.numeric(surv$acquisition_price)
surv$modification_price <- as.numeric(surv$modification_price)
surv$flag[is.na(surv$acquisition_price) & is.na(surv$rent) & is.na(surv$modification_price),] <- "missing price"

#For coordinates missing or out of bounds
surv$longitude[is.na(surv$longitude)] <- 0
surv$latitude[is.na(surv$latitude)] <- 0
surv_g <- st_as_sf(surv, coords = c("longitude","latitude")) %>%
  st_set_crs(4326)
surv$within_delhi <- st_within(surv_g,delhi_bounds, sparse = FALSE)[,1]
surv$flag[surv$within_delhi=="FALSE"] <- "out of NCT bounds" 
surv$flag[surv$longitude==0] <- "no coordinates"
surv$flag[surv$longitude==0] <- "no coordinates"
surv$flag[is.na(surv$ID)] <- "no ID"

#Duplicated ID
surv$duplicated <- duplicated(surv$ID)

#Colony misspelling
colonies <- c("AGCR_Encl","Ashok Nagar","Ber_Sarai","Bodhela", "Chowkhandi","Connaught_Place","Dabri","Dilshad_Garden", "Dwarka", "Gandhi_Vihar_North","Hastal","Hauz_Khas","Jhilmil_Colony","Jorbagh","Kalkaji","Katwarya_Sarai","Khayala","Khichripur","Kondly","Madangir","Madipur","Mahavit_Encl","Malivya_Nagar","Mayur_Vihar","Naraina","New_Kondli","North_Rajendra_Nagar","Okhla","Old_Delhi","Palam_Village","Panchscheel_Enclave","Patparganj","Prasad_nagar","Punjabi_Bagh","Rajouri_Garden","Rohini","Sabzi_Mandi_Malka_Ganj","Saket","Samaypur","Shakti_Nagar","Shalimarbagh","Sultanpuri","Timarpur","Trilok_Puri","Karol_Bagh","Vasant_Kunj")
surv$spelling <- "not ok"
surv$spelling[(surv$colony %in% colonies)] <- "ok"

#Incoherent dates
surv$time_start <- gsub(".* ", "",surv$start)
surv$time_visit <- gsub(".* ", "",surv$visit_date)

#Writing the file with no scientific notation
withr::with_options(c(scipen = 999), write.table(surv, "./CHALLINEQ/Challineq_consolidated_flagged.csv", sep = ","))
