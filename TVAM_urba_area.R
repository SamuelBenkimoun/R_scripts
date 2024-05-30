library(sf)
library(purrr)
library(units)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)
filepath = "~/Documents/Charbon/Geo Data/Delhi Urban Areas/"
setwd(filepath)
#function to compute areas in km after reprojecting in 7760 (WGS 84 / Delhi) for more accurate measures
areakm <- function(x){
  x$areakm = 0
  x$areakm <- st_transform(x, crs = 7760) %>%
    st_area()/1000000 
}
#function to get the overlapping surface between the NCT and the built-up continuum
intersectkm <- function(x,y){
  y$areawithinnctkm = 0
  inter <- st_intersection(
    st_transform(x, crs = 7760),
    st_transform(y, crs = 7760),
  )
  y$areaintersectkm <- areakm(inter)
}

#Importing built-up areas from shapefiles
dua75 <- st_read("tacheUrbaineDelhi200_1975.shp")
dua90 <- st_read("tacheUrbaineDelhi200_1990.shp")
dua00 <- st_read("tacheUrbaineDelhi200_2000.shp")
dua14 <- st_read("tacheUrbaineDelhi200.shp")
#Importing the National Capital Territory area
nct <- st_read("../NCT_boundaries.gpkg") 
#Computing the total areas
dua75$areakm <- areakm(dua75)
dua90$areakm <- areakm(dua90)
dua00$areakm <- areakm(dua00)
dua14$areakm <- areakm(dua14)
nct$areakm <- areakm(nct)
#Computing the intersecting areas
dua75$areawithinnctkm <- intersectkm(nct, dua75)
dua90$areawithinnctkm <- intersectkm(nct, dua90)
dua00$areawithinnctkm <- intersectkm(nct, dua00)
dua14$areawithinnctkm <- intersectkm(nct, dua14)
#Computing the disjoint areas
dua75 <- mutate(dua75, areaoutofnctkm = areakm - areawithinnctkm)
dua90 <- mutate(dua90, areaoutofnctkm = areakm - areawithinnctkm)
dua00 <- mutate(dua00, areaoutofnctkm = areakm - areawithinnctkm)
dua14 <- mutate(dua14, areaoutofnctkm = areakm - areawithinnctkm)

## Dataframe to track the evolution of the urban area
df <- data.frame("année" = c(1975, 1990, 2000, 2014), 
           "surface" = c(dua75$areakm, dua90$areakm, dua00$areakm, dua14$areakm))
# Difference of total built-up surface between two time-steps
df$difference <- 0
for (i in 2:nrow(df)) {
  df$difference[i] <- df$surface[i] - df$surface[i-1]
}
# Annual average evolution in km
df$diffmoyenne <- 0
for (i in 2:nrow(df)) {
  df$diffannuelle[i] <- df$difference[i] / (df$année[i]-df$année[i-1])
}
# Annual average evolution rate in %
df$TVAM <- 0
for (i in 2:nrow(df)) {
  df$TVAM[i] <- (df$surface[i] / df$surface[i-1])^(1/(df$année[i]-df$année[i-1])) %>% 
    units::drop_units() -1
  df2$TVAM[i] <- df2$TVAM[i]*100
}
# Formatting the table for LateX
stargazer(drop_units(df), summary=F, title="Evolution de la tâche urbaine à Delhi")

## Dataframe to compare the NCT and built-up continuum surface
df2 <- data.frame("année" = c(1975, 1990, 2000, 2014), 
                 "surfaceNCT" = c(nct$areakm, nct$areakm, nct$areakm, nct$areakm),
                 "BUAwithin" = c(dua75$areawithinnctkm, dua90$areawithinnctkm, dua00$areawithinnctkm, dua14$areawithinnctkm))
# Dropping the metric units to have plain numbers
df2[2:3] <- df2[2:3] %>% units::drop_units()
# Getting the percentage of NCT surface that is within the built-up corridor
df2 <- mutate(df2, prctgbuilt = BUAwithin/surfaceNCT *100)
# Annual average evolution rate in %
df2$TVAM <- 0
for (i in 2:nrow(df2)) {
  df2$TVAM[i] <- (df2$BUAwithin[i] / df2$BUAwithin[i-1])^(1/(df2$année[i]-df2$année[i-1])) -1
  df2$TVAM[i] <- df2$TVAM[i]*100
}
# Formatting the table for LateX
stargazer(drop_units(df), summary=F, title="Evolution de la tâche urbaine intra-NCT")

## Dataframe to compare the NCT and built-up continuum surface
df3 <- data.frame("année" = c(1975, 1990, 2000, 2014),
                  "surface" = c(dua75$areakm, dua90$areakm, dua00$areakm, dua14$areakm),
                  "BUAbeyond" = c(dua75$areaoutofnctkm, dua90$areaoutofnctkm, dua00$areaoutofnctkm, dua14$areaoutofnctkm))
# Dropping the metric units to have plain numbers
df3[2:3] <- df3[2:3] %>% units::drop_units()
# Getting the percentage of NCT surface that is within the built-up corridor
df3 <- mutate(df3, prctgbeyond = BUAbeyond/surface *100)
df3 <- mutate(df3, prctwithin = (1-(BUAbeyond/surface)) *100)
# Annual average evolution rate in %
df3$TVAM <- 0
for (i in 2:nrow(df3)) {
  df3$TVAM[i] <- (df3$BUAbeyond[i] / df3$BUAbeyond[i-1])^(1/(df3$année[i]-df3$année[i-1])) -1
  df3$TVAM[i] <- df3$TVAM[i]*100
}
# Formatting the table for LateX
stargazer(drop_units(df), summary=F, title="Evolution de la tâche urbaine intra-NCT")

###Plotting the evolution of the BUA within and beyond NCT
df3 <- df3 %>%
  rename(TVAM2 = TVAM)
df_combined <- left_join(df2, df3, by = "année") 

# Putting the dataframe in long format
df_long <- df_combined %>%
  select(année, TVAM, TVAM2) %>%
  tidyr::pivot_longer(cols = c(TVAM, TVAM2), names_to = "Type", values_to = "Value")

# Plotting with grouped bars and curves for within NCT and beyond NCT
ggplot() +
  # Adding grouped bars
  geom_bar(data = filter(df_long, année != 1975), aes(x = as.factor(année), y = Value*100, fill = Type), 
           stat = "identity", position = "dodge", width = 0.5) +
  # Labelling the bars with values
  geom_text(data = filter(df_long, année != 1975), aes(x = as.factor(année), y = Value, label = sprintf("%.2f%%", Value), group = Type), 
            position = position_dodge(width = 0.5), vjust = -0.5,
            fontface = "bold", colour = "gray10") +
  # Adding the curves
  geom_line(data = df_combined, aes(x = as.factor(année), y = BUAwithin, group = 1, color = "BUAwithin"), size = 1) +
  geom_point(data = df_combined, aes(x = as.factor(année), y = BUAwithin, group = 1, color = "BUAwithin"), size = 2) +
  geom_line(data = df_combined, aes(x = as.factor(année), y = BUAbeyond, group = 1, color = "BUAbeyond"), size = 1) +
  geom_point(data = df_combined, aes(x = as.factor(année), y = BUAbeyond, group = 1, color = "BUAbeyond"), size = 2) +
  # Editing the labels
  labs(x = "", 
       y = expression(bold("Surface totale (en km"^2*")")), 
       fill = "Taux de variation annuel moyen (en %)", 
       color = "Tache urbaine", 
       title = "Evolution et répartition de la tache urbaine dans l'agglomération de Delhi: \n à l'intérieur et à l'extérieur de la NCT") +
  # Editing the legend
  scale_fill_manual(values = c("TVAM" = "#3C5480", "TVAM2" = "#FCD882"), 
                    labels = c("TVAM" = "Intramuros", "TVAM2" = "Hors-NCT")) +
  scale_color_manual(values = c("BUAwithin" = "#3C5480", "BUAbeyond" = "#FCD882"),
                     labels = c("BUAwithin" = "Total des surfaces bâties intramuros", "BUAbeyond" = "Total des surfaces bâties hors-NCT")) +
  # Theme elements
  theme_minimal(base_family = "Avenir Next") +
  theme(
  plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
  plot.caption = element_text(hjust = 1), 
  axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11), # Rotating the x axis labels
  panel.grid.major = element_line(color = "gray80", size = 0.5), 
  panel.grid.minor = element_line(color = "gray90", size = 0.25) 
) +
  # Caption
  labs(caption = "Source: Global Human Settlement Built-up Grid 2016; Auteur: Samuel Benkimoun, 2024.") # Texte en bas indiquant la source

