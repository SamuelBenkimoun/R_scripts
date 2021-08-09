library(readxl)
library(sf)
library(tmap)
library(readr)
#Demo data formatting
#Importing the census tables related to religion
#Setting the working directory
setwd("~/Census_2011_NCR/Religion")
#Listing the files in the working directory
list_xls <- list.files()
#Importing the files from the list
rel <- lapply(list_xls, read_excel)
#Keeping the sub-district information only and building a unique table
rel <- lapply(rel, subset, grepl("Sub-District", Name))
#Binding all the tables in one
rel <- do.call(rbind,rel)
#Computing the rate of each religion, initializing the new attributes at 0 
i = 1
rel$r_hindu <- 0
rel$r_muslim <- 0
rel$r_sikh <- 0 
#Iterating to compute the ratios (change addressing, then round)
for (i in 1:(nrow(rel)-4)){
  rel[i, 86] <- rel[i+1, "TOT_P"]/ rel[i, "TOT_P"] * 100
  rel[i, 87] <- rel[i+2, "TOT_P"]/ rel[i, "TOT_P"] * 100
  rel[i, 88] <- rel[i+4, "TOT_P"]/ rel[i, "TOT_P"] * 100
  i = i+27
}
#Filtering the data to keep only the total numbers
rel <- subset(rel, Religion == "Total" & TRU == "Total")
#Writing the file
write_csv(rel, "../../test_religion.csv")
#Getting the coefficient of correlation for both variables
cor(rel$r_hindu, rel$r_sikh, method = "pearson")
#fitting a linear regression to explain the sikh ratio by hindu and muslim ratio in the population 
lm <- lm(formula=r_sikh~r_hindu+r_muslim, data=rel)
#reading the results of the regression
summary(lm)
#Importing a spatial layer with the subdistricts geometries 
subd <- st_read("../SDT_shape_2011/India_L3_Administrative_Boundaries.shp")
#Renaming the join variable (subdistrict code) to be in accordance in both the tables
colnames(rel)
colnames(subd)
colnames(rel)[3] <- "L3_CODE"
rel$L3_CODE <- as.numeric(rel$L3_CODE)
#Joining the two tables
rel_geom <- merge(rel,subd)
#Inspecting the class of the variable
class(rel_geom)
#Transforming it into a "sf" object i.e. with a geometry
rel_geom <- st_as_sf(rel_geom, sf_column_name = "geometry")
class(rel_geom)
#Mapping the ratio of sikh by subdistricts
tm_shape(rel_geom)+
  tmap_options(check.and.fix = TRUE)+
  tm_polygons("r_sikh") +
  tm_facets(by = "L1_NAME")
