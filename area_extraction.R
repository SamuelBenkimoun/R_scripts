# Reding the files
dt <- st_as_sf(Delhi_Tiles_till_2603, wkt = "Geometry")
states <- st_read("./India States/Indian_States.shp")
# Isolating departure and arrival points of mobility flows
x1 <- dt
x1$Geometry <- st_line_sample(dt$Geometry, sample = 0)
x2 <- dt
x2$Geometry <- st_line_sample(dt$Geometry, sample = 1)
# joining State of arrival and Departure
x1 <- st_set_crs(x1, 4326)
x1 <- st_join(x1, states)
x1 <- st_drop_geometry(x1)
x2 <- st_set_crs(x2, 4326)
x2 <- st_join(x2, states)
x2 <- st_drop_geometry(x2)
x3 <- cbind(x1, x2$st_nm)
x3 <- cbind(dt$Geometry, x3)
# keeping Delhi mobilities only
x3 <- subset(x3, x3$st_nm == "NCT of Delhi" & x3$`x2$st_nm`== "NCT of Delhi")
# refactor colnames
names(x3) <- gsub(x = names(x3), pattern = "\\X", replacement = "") 
names(x3) <- gsub(x = names(x3), pattern = "\\.", replacement = "-") 
# writing results in single files for each timesteps
for (i in 7:ncol(x3)-2){
  timestep <- cbind(x3[1], x3[3:4], x3[i])
  colnames(timestep)[4] <- "moving_people"
  write.csv(timestep, paste("./",colnames(x3[i]), ".csv", sep=""))
}
