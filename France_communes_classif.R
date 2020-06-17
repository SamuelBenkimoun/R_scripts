# Constituting the Communes social database
library(dplyr)
library(magrittr)
library(sf)
library(readr)
library(readxl)
library(tidyverse)
#Importing the layer with communal limits
setwd("~/GEOFLA_2-2_COMMUNE_SHP_LAMB93_FXX_2016-06-28/GEOFLA/1_DONNEES_LIVRAISON_2016-06-00236/GEOFLA_2-2_SHP_LAMB93_FR-ED161/COMMUNE")
shp <- st_read("./COMMUNE.shp")
shp <- subset(shp, select = c("INSEE_COM", "NOM_COM", "SUPERFICIE", "POPULATION", "NOM_DEPT", "NOM_REG"))
#Mobility table for France
fr_mob <- fr_mob
#Joining communes with their Facebook ID
fr_locations <- fr_mob %>% 
  subset(select = c("Geometry", "Starting.Region.Name", "Starting.Location")) %>% 
  st_as_sf(wkt = "Geometry")
fr_locations$Geometry <- st_line_sample(fr_locations$Geometry, sample=0)
fr_locations <- fr_locations[!duplicated(fr_locations$Starting.Location),] %>%
  st_set_crs(4326) %>%
  st_transform(2154) %>%
  st_join(shp, st_within) %>% 
  subset(NOM_COM != "NA")
# Joining revenue attributes
setwd("~/")
communes_revenus <- read_excel("INSEE/communes_revenus.xlsx") 
communes_revenus <- subset(communes_revenus, select = c("CODGEO", "Q217"))
fr_locations <- left_join(fr_locations, communes_revenus, by=c("INSEE_COM" = "CODGEO"))
FILO2017_DEC_Pauvres_COM <- read_excel("INSEE/FILO2017_DEC_Pauvres_COM.xlsx", sheet = "ENSEMBLE", col_types = c("text",  "text", "numeric", "numeric", "numeric", "numeric", "numeric"), skip = 5)
FILO2017_DEC_Pauvres_COM <- subset(FILO2017_DEC_Pauvres_COM, select = c("CODGEO", "TP6017"))
fr_locations <- left_join(fr_locations, FILO2017_DEC_Pauvres_COM, by=c("INSEE_COM" = "CODGEO"))

# Joining population structure/ prof categories data
cc_evol_struct_pop_2016 <- read_excel("INSEE/cc-evol-struct-pop-2016.xlsx", skip = 5)
cc_evol_struct_pop_2016 <- subset(cc_evol_struct_pop_2016, select = c("CODGEO", "P16_POP0014", "P16_POP1529",	"P16_POP3044","P16_POP4559","P16_POP6074", "P16_POP7589",	"P16_POP90P", "C16_POP15P_CS1",	"C16_POP15P_CS2",	"C16_POP15P_CS3",	"C16_POP15P_CS4",	"C16_POP15P_CS5", "C16_POP15P_CS6",	"C16_POP15P_CS7"))
cc_evol_struct_pop_2016 <- transform(cc_evol_struct_pop_2016, P16_POP1559 = rowSums(cc_evol_struct_pop_2016[3:5]))
cc_evol_struct_pop_2016 <- transform(cc_evol_struct_pop_2016, P16_POP60plus = rowSums(cc_evol_struct_pop_2016[6:8]))
cc_evol_struct_pop_2016 <- cbind(cc_evol_struct_pop_2016[1:2], cc_evol_struct_pop_2016[16:17], cc_evol_struct_pop_2016[9:15])
fr_locations <- left_join(fr_locations, cc_evol_struct_pop_2016, by=c("INSEE_COM" = "CODGEO"))

# Joining companies attributes
TD_CLAP2015_NA05_TREF_NBSAL <- read_csv("~/INSEE/TD_CLAP2015_NA05_TREF_NBSAL.csv", skip = 5)
TD_CLAP2015_NA05_TREF_NBSAL <- subset(TD_CLAP2015_NA05_TREF_NBSAL, select = c("CODGEO", "EFF_TOT", "EFF_AZ", "EFF_BE",	"EFF_FZ",	"EFF_GU",	"EFF_OQ"))
fr_locations <- left_join(fr_locations, TD_CLAP2015_NA05_TREF_NBSAL, by=c("INSEE_COM" = "CODGEO"))

# Joining the family structure data
cc_coupl_fam_men_2016 <- read_excel("INSEE/cc-coupl-fam-men-2016.xlsx", skip = 5)
cc_coupl_fam_men_2016 <- subset(cc_coupl_fam_men_2016, select = c("CODGEO", "C16_MEN", "C16_MENPSEUL", "C16_FAM", "C16_COUPAENF", "C16_FAMMONO", "C16_HMONO", "C16_FMONO", "C16_COUPSENF","C16_NE24F3", "C16_NE24F4P"))
cc_coupl_fam_men_2016 <- transform(cc_coupl_fam_men_2016 , C16_MENPSEUL = C16_MENPSEUL/C16_MEN)
cc_coupl_fam_men_2016 <- transform(cc_coupl_fam_men_2016 , C16_NE24F3P=rowSums(cc_coupl_fam_men_2016[10:11])/C16_FAM)
cc_coupl_fam_men_2016 <- cbind(cc_coupl_fam_men_2016[1], cc_coupl_fam_men_2016[3], cc_coupl_fam_men_2016[5:9]/cc_coupl_fam_men_2016$C16_FAM, cc_coupl_fam_men_2016[12])
fr_locations <- left_join(fr_locations, cc_coupl_fam_men_2016, by=c("INSEE_COM" = "CODGEO"))

# Joining the housing data
cc_logement_2016 <- read_excel("INSEE/cc_logement-2016.xlsx", skip = 5)
cc_logement_2016 <- subset(cc_logement_2016, select = c("CODGEO", "P16_LOG", "P16_RP", "P16_RSECOCC","P16_LOGVAC","P16_MAISON","P16_APPART", "P16_RP_1P", "P16_RP_2P","P16_RP_ACHTOT","P16_RP_ACH19","P16_RP_ACH45","P16_RP_ACH70", "P16_RP_ACH13","P16_PMEN","P16_PMEN_ANEM0002","P16_PMEN_ANEM0204","P16_RP_LOC","P16_RP_LOCHLMV","P16_RP_VOIT1P","P16_RP_VOIT1","P16_RP_VOIT2P","P16_RP_HABFOR"))
cc_logement_2016$P16_RP_AVANT70 = 0
cc_logement_2016 <- transform(cc_logement_2016, P16_RP_AVANT70=(rowSums(cc_logement_2016[,11:13])/cc_logement_2016[,10]))
cc_logement_2016 <- transform(cc_logement_2016, P16_RP_ACH13=cc_logement_2016[,14]/cc_logement_2016[,10])
##cc_logement_2016 <- cbind(cc_logement_2016$CODGEO, cc_logement_2016[3:9]/cc_logement_2016$P16_LOG, cc_logement_2016$P16_RP_AVANT70, cc_logement_2016$P16_RP_ACH13, cc_logement_2016[16:17]/cc_logement_2016$P16_PMEN, cc_logement_2016[18:22]/cc_logement_2016$P16_RP)
cc_logement_2016 <- cbind(cc_logement_2016[1], cc_logement_2016[,3:9]/cc_logement_2016[,2], cc_logement_2016[24], cc_logement_2016[14], cc_logement_2016[,16:17]/cc_logement_2016[,15], cc_logement_2016[,18:22]/cc_logement_2016[,3])
fr_locations <- left_join(fr_locations, cc_logement_2016, by=c("INSEE_COM" = "CODGEO"))

# Joining tourism attributes
base_cc_tourisme_2020 <- read_excel("base-cc-tourisme-2020.xlsx", skip = 5)
base_cc_tourisme_2020 <- subset(base_cc_tourisme_2020, select = c("CODGEO", "HTCH20"))
fr_locations <- left_join(fr_locations, base_cc_tourisme_2020, by=c("INSEE_COM" = "CODGEO"))

# treating the communes with arrondissements apart
write.csv(fr_locations[grep("ARRONDISSEMENT", fr_locations$NOM_COM), ], "./temp_for_cah/fr_locations_arrondissements.csv")
fr_locations <- fr_locations[-grep("ARRONDISSEMENT", fr_locations$NOM_COM), ] %>%
  rbind(fr_ville_arrondissements)

fr_loc_backup <- fr_locations
fr_locations <- fr_loc_backup 
# Finalizing ratio data
fr_locations <- st_drop_geometry(fr_locations)
fr_locations <- fr_locations %>%
  transform(C16_POP15P_CS1=C16_POP15P_CS1/(P16_POP1559+P16_POP60plus))%>%
  transform(C16_POP15P_CS2=C16_POP15P_CS2/(P16_POP1559+P16_POP60plus))%>%
  transform(C16_POP15P_CS3=C16_POP15P_CS3/(P16_POP1559+P16_POP60plus))%>%
  transform(C16_POP15P_CS4=C16_POP15P_CS4/(P16_POP1559+P16_POP60plus))%>%
  transform(C16_POP15P_CS5=C16_POP15P_CS5/(P16_POP1559+P16_POP60plus))%>%
  transform(C16_POP15P_CS6=C16_POP15P_CS6/(P16_POP1559+P16_POP60plus))%>%
  transform(C16_POP15P_CS7=C16_POP15P_CS7/(P16_POP1559+P16_POP60plus))
fr_locations <- fr_locations %>%
  transform(EFF_AZ=EFF_AZ/EFF_TOT) %>%
  transform(EFF_BE=EFF_BE/EFF_TOT) %>%
  transform(EFF_FZ=EFF_FZ/EFF_TOT) %>%
  transform(EFF_GU=EFF_GU/EFF_TOT) %>%
  transform(EFF_OQ=EFF_OQ/EFF_TOT) %>%
  transform(EFF_TOTR=EFF_TOT/POPULATION)
fr_locations <- fr_locations %>%  
  transform(P16_POP0014=P16_POP0014/POPULATION) %>% 
  transform(P16_POP1559=P16_POP1559/POPULATION) %>%
  transform(P16_POP60plus=P16_POP60plus/POPULATION) %>%
  transform(P16_PMEN_ANEM0004=P16_PMEN_ANEM0002+P16_PMEN_ANEM0204)%>%
  transform(P16_RP_M2P=P16_RP_1P+P16_RP_2P)%>%
  transform(HTCH20=HTCH20/POPULATION)%>%
  transform(DENSITE=POPULATION/SUPERFICIE)
  
# Variables selection
fr_locations.sub <- subset(fr_locations, select=c("INSEE_COM",	"NOM_COM",	"POPULATION",	"DENSITE"	,"Q217",	"P16_POP0014",	"P16_POP1559",	"P16_POP60plus",	"C16_POP15P_CS1",	"C16_POP15P_CS2",	"C16_POP15P_CS3",	"C16_POP15P_CS4",	"C16_POP15P_CS5",	"C16_POP15P_CS6",	"C16_POP15P_CS7", "EFF_TOT", "EFF_AZ", "EFF_BE",	"EFF_FZ",	"EFF_GU",	"EFF_OQ", "EFF_TOTR",	"C16_MENPSEUL",	"C16_COUPAENF",	"C16_FAMMONO",	"C16_COUPSENF",	"C16_NE24F3P",	"P16_RSECOCC",	"P16_MAISON",	"P16_APPART",	"P16_RP_M2P",	"P16_RP_AVANT70",	"P16_RP_ACH13",	"P16_PMEN_ANEM0004",	"P16_RP_LOC",	"P16_RP_LOCHLMV",	"P16_RP_VOIT1P",	"HTCH20"))
# Removing potential duplicates and a non numeric column
fr_loc.cr <- fr_locations.sub[ !duplicated(fr_locations.sub[,c("INSEE_COM")]),] %>% 
  subset(!(INSEE_COM %in% c(55189, 55139))) %>%
  subset(select=-c(NOM_COM))
# Join the Facebook mobility attributes
tspiv <- aggregate(fr.mob[36:44], by=list(Starting.Location=fr.mob$Starting.Location), FUN=sum)
tepiv <- aggregate(fr.mob[36:44], by=list(Ending.Location=fr.mob$Ending.Location), FUN=sum)
tspiv <- transform(tspiv, sumtot.mob_start = rowSums(tspiv[2:10]))
tepiv <- transform(tepiv, sumtot.mob_end = rowSums(tepiv[2:10]))
fr_locations <- merge(fr_locations, tspiv[, c("Starting.Location", "sumtot.mob_start")])
fr_locations <- left_join(fr_locations, tepiv[, c("Ending.Location", "sumtot.mob_end")], by = c("Starting.Location"="Ending.Location"))


# Setting the row names with INSEE code
row.names(fr_loc.cr) <- fr_loc.cr[,1]
fr_loc.cr <- fr_loc.cr[,-1] %>%
  #na.omit() %>%
  scale(center=T,scale=T) %>%
  as.data.frame()

# Distance matrix
d.fr_loc <- dist(fr_loc.cr)
# CAH
cah.ward <- hclust(d.fr_loc,method="ward.D2")
plot(cah.ward)

cah.ward <- fr_loc.cr[,-c(1, 4, 14, 17, 18, 21, 23, 24)] %>%
  scale(center=T,scale=T) %>%
  dist()%>%
  hclust(method="ward.D2")
plot(cah.ward)
groupes.cah <-cutree(cah.ward,k=8)

inertie <- sort(cah.ward$height, decreasing = TRUE)
plot(inertie[1:20], type = "s", xlab = "Nombre de classes", ylab = "Inertie")


fr_loc.cr$class = 0
#groupes.kmeans <- kmeans(na.omit(fr_loc.cr),centers=4,nstart=5)
#fr_loc.crsub <- na.omit(fr_loc.cr[,-c(1, 11, 14, 17, 18, 21, 23, 24, 25, 29)])
fr_loc.crsub <- fr_loc.cr[,c(1, 2, 3, 7, 8, 9, 10, 11, 12, 13, 20, 26)]%>%
  na.omit()
#fr_loc.crsub <- subset(fr_loc.crsub, !(rownames(fr_loc.crsub) %in% c(75056)))

groupes.kmeans <- kmeans(fr_loc.crsub,centers=8,nstart=5)
print(groupes.kmeans)
fr_loc.crsub$class <- groupes.kmeans$cluster
write_csv(as.data.frame(cbind(rownames(fr_loc.crsub), fr_loc.crsub$class)), "./K-means/clusters8class.csv")
# subsetting to rerun the classification without the two very distinct categories
#fr_loc.crsub <- subset(fr_loc.crsub, class %in% c(1,2,3,4,5,8)) %>%
#  scale(center=T,scale=T) %>%
#  as.data.frame()
#groupes.kmeans <- kmeans(fr_loc.crsub[2:11],centers=6,nstart=5)
#print(groupes.kmeans)


# keeping class centers
classes1 <- groupes.kmeans$centers %>%
  as.data.frame()
# plotting the group profiles
colnames(classes1)<- c("POPULATION", "DENSITY", "MEDIAN_INCOME", "CSP_AGRICULTURE", "CSP_ARTISAN/TRADE", "CSP_EXECUTIVE", "CSP_INTERMEDIARY_JOBS", "CSP_SKILLED_EMPLOYEES", "CSP_LABORERS", "CSP_RETIRED", "LOCAL_JOBS/INHAB", "SECOND_HOME")
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
classes1$class <- c(
  paste('Wealthy suburbs', " (", groupes.kmeans$size[1], ")"),
  paste('Peri-urban localities', " (", groupes.kmeans$size[2], ")"), 
  paste('Heliotropic localities', " (", groupes.kmeans$size[3], ")"),
  paste('Working-class localities', " (", groupes.kmeans$size[4], ")"),
  paste('Shrinking localities', " (", groupes.kmeans$size[5], ")"),
  paste('Rural areas', " (", groupes.kmeans$size[6], ")"),
  paste('Major urban centres', " (", groupes.kmeans$size[7], ")"),
  paste('Employment hubs', " (", groupes.kmeans$size[8], ")")
  )

ggplot(classes1long) +
  geom_bar(aes(x = variable, y = value, fill = class),
           fill = "dark red",
           stat = "identity") +
  #scale_fill_grey() +
  facet_wrap(~ class) +
  coord_flip() +
  theme(strip.text = element_text(size=10, face = "bold"), legend.position = "none")
  #theme_bw()
