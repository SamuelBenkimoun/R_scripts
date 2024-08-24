#CHALLINEQ data base maps and local indicators
library(readxl)
library(dplyr)
library(sf)
library(leaflet)
library(leaflegend)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(webshot)
library(viridis)
library(ggridges)
library(ggplot2)

# Reading the survey data and the coordinates
cd <- read_excel("Documents/Charbon/Challineq/Challineq_with_pm_28Nov2023 (1).xlsx")
cd_sf <- st_as_sf(cd, coords = c("longitude", "latitude"), crs = 4326)

# Importing the Delhi NCT outline for mapping
nct <- st_read("Documents/Charbon/Geo Data/NCT_boundaries.gpkg")


# Create convex hulls for each group of 'colony'
convex <- cd_sf %>%
  group_by(colony) %>%
  summarise(geometry = st_union(geometry)) %>%
  st_convex_hull()
plot(convex)

# Adding a number id to each colony for the map legend
convex <- convex %>%
  mutate(colony_id = as.numeric(factor(colony)))

# Keeping the centroids of the convex enveloppes for labelling purpose when mapping
convex_centroids <- st_centroid(convex) %>% st_coordinates() %>%
  as.data.frame()

# Create a color palette based on the 'colony' attribute
custom_palette <- colorRampPalette(RColorBrewer::brewer.pal(9, "Set1"))(length(convex$colony))
pal <- colorFactor(palette = custom_palette, domain = unique(convex$colony))

# Create the leaflet map for colonies location layout
m <- leaflet(cd) %>%
  # Base map
  addProviderTiles(providers$CartoDB.Positron) %>%
  # Centering view on the following coordinates and zooming in.
  setView(lat = 28.612926, lng = 77.229724, zoom = 11) %>%
  # Add the NCT outline
  addPolygons(data = nct, color = "black", weight = 4, fill = FALSE) %>%
  # Add markers with colors based on 'colony'
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~pal(colony),
    popup = ~paste("ID:", ID, "<br>Colony:", colony),
    radius = 0.5
  ) %>%
  # Add a legend for the 'colony' colors
  addLegend(
    "bottomright",
    colors = custom_palette,
    labels = paste0(unique(convex$colony_id), ": ", unique(convex$colony)),
    title = "Colonies",
    opacity = 1
  ) %>%
  addPolygons(
    data = convex,
    fillColor = ~pal(colony),
    fillOpacity = 0.3,
    color = "black",
    weight = 1,
    popup = ~paste("Colony:", colony),
    label = ~colony_id,
    labelOptions = labelOptions(noHide = TRUE, direction = "center", textOnly = TRUE, 
                                buffer = 20, textMargin = 10,
                                style = list("color" = "black", "font-weight" = "bold","font-size" = "15px", 
                                             # Adding a white buffer around the text
                                "text-shadow" = "-1px -1px 0 white, 1px -1px 0 white, -1px 1px 0 white, 1px 1px 0 white"
                                )
                                ),
  ) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))

m

# Save the widget
saveWidget(m, "./challineq_areas.html", selfcontained = TRUE)

# Use webshot to capture the map as an image
webshot("./challineq_areas.html", file = "./challineq_areas.png", vwidth = 1200, vheight = 900, cliprect = "viewport")


## MAKING THE DIFFERENT MAPS BY ATTRIBUTE OF THE HOUSEHOLDS
### INCOME
## Create the map using the function, mapping 'income' attribute
# Review the levels of the income variable
cd_sf$income <- factor(cd_sf$income, exclude=NULL, levels = c(
  "Less than 2,000 Rs/month",
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

# Create a new variable 'income_merged' to reduce the number of levels
cd_sf$income_merged <- as.character(cd_sf$income) # Converting it into character to be able to modify it with values which are not part of the actual factor levels
cd_sf$income_merged[cd_sf$income_merged %in% c("30,000 to 40,000", "40,000 to 50,000")] <- "30,000 to 50,000"
cd_sf$income_merged[cd_sf$income_merged %in% c("50,000 to 60,000", "60,000 to 70,000")] <- "50,000 to 70,000"
cd_sf <- mutate(cd_sf, income_merged = as.factor(income_merged))
cd_sf$income_merged <- factor(cd_sf$income_merged, exclude=NULL, levels = c(
  "Less than 2,000 Rs/month",
  "2,000 to 6,000",
  "6,000 to 10,000",
  "10,000 to 20,000",
  "20,000 to 30,000",
  "30,000 to 50,000",
  "50,000 to 70,000",
  "70,000 to 110,000",
  "110,000 and above"))

# Get the bounding box of the convex hulls
bbox <- st_bbox(convex)

# Create a colorFactor palette including NA
palette_income <- leaflet::colorFactor(
  palette = c(magma(9, direction = 1)),
  domain = levels(cd_sf$income_merged),
  na.color = "gainsboro",
  #levels = levels(cd_sf$income_merged),
  ordered = TRUE
)

# Create Leaflet map for income
leaflet() %>%
  #addProviderTiles(providers$Esri.WorldImagery) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = nct, color = "black", weight = 4, fill = FALSE) %>%
  setView(lng = mean(st_coordinates(cd_sf)[, "X"]), 
          lat = mean(st_coordinates(cd_sf)[, "Y"]), 
          zoom = 11) %>%
  fitBounds(lng1 = min(st_coordinates(convex)[, "X"]), 
            lat1 = min(st_coordinates(convex)[, "Y"]), 
            lng2 = max(st_coordinates(convex)[, "X"]), 
            lat2 = max(st_coordinates(convex)[, "Y"])) %>%
  addPolygons(data = convex, 
              fillColor = NULL, 
              color = "black",
              weight = 2, 
              fillOpacity = 0) %>%
  addLabelOnlyMarkers(data = convex, 
                      lat = convex_centroids$Y,  # Accessing Y coordinates
                      lng = convex_centroids$X,  # Accessing X coordinates
                      label = ~colony,  # Label based on colony attribute
                      labelOptions = labelOptions(noHide = TRUE, opacity = 0.8)) %>%
  addCircleMarkers(data = cd_sf, 
                   radius = 2.5, 
                   color = "black",
                   weight = 0.1,
                   fillColor = ~palette_income(income_merged),  # Use palette_income directly
                   fillOpacity = 0.7,
                   popup = ~paste("Colony:", colony, "<br>", "Income:", income_merged)) %>%
  addLegendFactor(pal = palette_income, 
                  values = factor(cd_sf$income_merged, exclude=NULL, levels = c(
                    "Less than 2,000 Rs/month",
                    "2,000 to 6,000",
                    "6,000 to 10,000",
                    "10,000 to 20,000",
                    "20,000 to 30,000",
                    "30,000 to 50,000",
                    "50,000 to 70,000",
                    "70,000 to 110,000",
                    "110,000 and above")),
                  title = "Levels of monthly income by household (in rupees):",
                  position = 'topright',
                  shape = 'circle') %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))

# Transforming the count into percentages by areas
colonies_percentage <-  cd_sf %>%
  group_by(colony, income_merged) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total) * 100) %>%
  st_drop_geometry()

# Facetted plot of the income distribution by areas
ggplot(colonies_percentage, aes(x = income_merged, y = percentage, fill = income_merged)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = palette_income(levels(cd_sf$income_merged))) +
  labs(title = "Income Distribution by Surveyed Areas",
       x = "Income Level",
       y = "Percentage of Total",
       fill = "Income Level") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.text.x = element_blank(),
        strip.text = element_text(size = 9.5)) +
  facet_wrap(~ colony, 
             scales = "free_y",
             #scales = "fixed"
             )

### RELIGION
# Setting the religion attribute as a factor and with the right levels
sub_religion <- cd_sf %>% subset(religion %in% c("Hindu", "Muslim", "Christian", "Sikh", "Jain", "Buddhist"))
sub_religion <- mutate(sub_religion, religion = as.factor(religion))
sub_religion$religion <- factor(sub_religion$religion, levels = c("Buddhist", "Christian", "Jain", "Sikh", "Muslim", "Hindu"))
# Create a colorFactor palette including NA
palette_rel <- leaflet::colorFactor(
  palette = c("orange", "seagreen", "darkblue", "indianred", "mediumpurple", "yellow"),
  domain = levels(sub_religion$religion),
  #na.color = "gainsboro",
  #levels = levels(cd_sf$income_merged),
  ordered = TRUE
)

# Create Leaflet map for religion
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = nct, color = "black", weight = 4, fill = FALSE) %>%
  setView(lng = mean(st_coordinates(cd_sf)[, "X"]), 
          lat = mean(st_coordinates(cd_sf)[, "Y"]), 
          zoom = 11) %>%
  fitBounds(lng1 = min(st_coordinates(convex)[, "X"]), 
            lat1 = min(st_coordinates(convex)[, "Y"]), 
            lng2 = max(st_coordinates(convex)[, "X"]), 
            lat2 = max(st_coordinates(convex)[, "Y"])) %>%
  addPolygons(data = convex, 
              fillColor = NULL, 
              color = "black",
              weight = 2, 
              fillOpacity = 0) %>%
  addCircleMarkers(data = sub_religion, 
                   radius = 2.5, 
                   color = "black",
                   weight = 0.1,
                   fillColor = ~palette_rel(religion),  # Use palette_income directly
                   fillOpacity = 0.7,
                   popup = ~paste("Colony:", colony, "<br>", "Religion:", religion)) %>%
  addLegendFactor(pal = palette_rel, 
                  values = factor(sub_religion$religion, exclude=NULL, levels = c("Buddhist", "Christian", "Jain", "Sikh", "Muslim", "Hindu")),
                  title = "Main religion by household:",
                  position = 'topright') %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))

# Transforming the count into percentages by areas
religion_percentage <-  sub_religion %>%
  group_by(colony, religion) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total) * 100) %>%
  st_drop_geometry()

# Facetted plot of the religion distribution by areas
ggplot(religion_percentage, aes(x = religion, y = percentage, fill = religion)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = palette_rel(levels(sub_religion$religion))) +
  labs(title = "Main Religion in the Housheold by Surveyed Areas",
       x = "Religion",
       y = "Percentage of Total",
       fill = "Religion:") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.x = element_blank(),
    strip.text = element_text(size = 9.5)) +
  facet_wrap(~ colony, 
             scales = "free_y",
             #scales = "fixed"
  )


### CASTE

# Setting the religion attribute as a factor and with the right levels
sub_caste <- cd_sf %>% subset(caste %in% c("Scheduled Tribe (ST)", "Scheduled Caste (SC)", "Other Backward Class (OBC)", "General", "NA"))
sub_caste <- mutate(sub_caste, caste = as.factor(caste))
sub_caste$caste <- factor(sub_caste$caste, levels = c("Scheduled Tribe (ST)", "Scheduled Caste (SC)", "Other Backward Class (OBC)", "General"))
# Create a colorFactor palette including NA
palette_caste <- leaflet::colorFactor(
  palette = c("purple", "mediumblue", "indianred", "navajowhite"),
  domain = levels(sub_caste$caste),
  na.color = "gainsboro",
  ordered = TRUE
)

# Create Leaflet map for religion
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = nct, color = "black", weight = 4, fill = FALSE) %>%
  setView(lng = mean(st_coordinates(cd_sf)[, "X"]), 
          lat = mean(st_coordinates(cd_sf)[, "Y"]), 
          zoom = 11) %>%
  fitBounds(lng1 = min(st_coordinates(convex)[, "X"]), 
            lat1 = min(st_coordinates(convex)[, "Y"]), 
            lng2 = max(st_coordinates(convex)[, "X"]), 
            lat2 = max(st_coordinates(convex)[, "Y"])) %>%
  addPolygons(data = convex, 
              fillColor = NULL, 
              color = "black",
              weight = 2, 
              fillOpacity = 0) %>%
  addCircleMarkers(data = sub_caste, 
                   radius = 2.5, 
                   color = "black",
                   weight = 0.1,
                   fillColor = ~palette_caste(caste),  # Use palette_income directly
                   fillOpacity = 0.7,
                   popup = ~paste("Colony:", colony, "<br>", "Caste:", caste)) %>%
  addLegendFactor(pal = palette_caste, 
                  values = factor(sub_caste$caste, exclude=NULL, levels = c("Scheduled Tribe (ST)", "Scheduled Caste (SC)", "Other Backward Class (OBC)", "General")),
                  title = "Caste of the Household Head:",
                  position = 'topright') %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))


# Transforming the count into percentages by areas
caste_percentage <-  sub_caste %>%
  group_by(colony, caste) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total) * 100) %>%
  st_drop_geometry()

# Facetted plot of the caste distribution by areas
ggplot(caste_percentage, aes(x = caste, y = percentage, fill = caste)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = palette_caste(levels(sub_caste$caste))) +
  labs(title = "Main caste in the Housheold by Surveyed Areas",
       x = "caste",
       y = "Percentage of Total",
       fill = "Caste:") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.x = element_blank(),
    strip.text = element_text(size = 9.5)) +
  facet_wrap(~ colony, 
             scales = "free_y",
             #scales = "fixed"
  )

### SHARED GREEN-SPACE
table(cd_sf$shared_green)
table(cd_sf$garden)

# Create a colorFactor palette including NA
palette_green_space <- leaflet::colorFactor(
  palette = c("indianred", "darkgreen"),
  domain = c("No", "Yes"),
  na.color = "gainsboro",
  ordered = TRUE
)

# Computing the percentage of declared access to a shared green space by colony
green_space_percentage <- cd_sf %>%
  group_by(colony, shared_green) %>%
  summarize(count = n()) %>%
  mutate(total = sum(count)) %>%
  ungroup() %>%
  mutate(percentage = (count / total) * 100) %>%
  st_drop_geometry()

# Adding the access to green space percentage to the convex enveloppes of the colonies for mapping
convex <- merge(convex, 
                subset(green_space_percentage, shared_green == "Yes"), 
                by = "colony", all.x = TRUE)
palette_convex <- colorNumeric(palette = "Greens", domain = convex$percentage)

# Mapping the enveloppes and households with regards to the declared access to a shared greens space
leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = nct, color = "black", weight = 4, fill = FALSE) %>%
  setView(lng = mean(st_coordinates(cd_sf)[, "X"]), 
          lat = mean(st_coordinates(cd_sf)[, "Y"]), 
          zoom = 11) %>%
  fitBounds(lng1 = min(st_coordinates(convex)[, "X"]), 
            lat1 = min(st_coordinates(convex)[, "Y"]), 
            lng2 = max(st_coordinates(convex)[, "X"]), 
            lat2 = max(st_coordinates(convex)[, "Y"])) %>%
  addPolygons(data = convex, 
              fillColor = ~palette_convex(percentage),  # Use the calculated percentage to color
              color = "black",
              weight = 2, 
              fillOpacity = 0.6) %>%
  addCircleMarkers(data = cd_sf, 
                   radius = 2.5, 
                   color = "black",
                   weight = 0.1,
                   fillColor = ~palette_green_space(shared_green),  # Use palette_green_space directly
                   fillOpacity = 0.7,
                   popup = ~paste("Colony:", colony, "<br>", "Green Space:", shared_green)) %>%
  addLegendFactor(pal = palette_green_space, 
                  values = factor(cd_sf$shared_green, exclude=NULL, levels = c("Yes", "No")),
                  title = "Household declaring an access to a shared green space:",
                  position = 'topright',
                  shape = 'circle') %>%
  addLegendNumeric(pal = palette_convex,
                   values = convex$percentage,
                   title = "Percentage of households declaring access by surveyed area:",
                   position = 'topright') %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))

# Facetted plot of the green-space access by areas
ggplot(green_space_percentage, aes(x = shared_green, y = percentage, fill = shared_green)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  scale_fill_manual(values = palette_green_space(levels(as.factor(cd_sf$shared_green)))) +
  labs(title = "Declared Access to A Shared Green-Space by Surveyed Areas",
       x = "Access",
       y = "Percentage of Total",
       fill = "Access:") +
  theme_minimal() +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1), 
    axis.text.x = element_blank(),
    strip.text = element_text(size = 9.5)) +
  facet_wrap(~ colony, 
             scales = "free_y",
             #scales = "fixed"
  )

## COMPUTING INDICATORS RELATED TO THE VARIABLES
## Income

# Plotting with grouped bars and curves for within NCT and beyond NCT

#ggplot(cd_sf %>% filter (!is.na(income_merged)), aes(x = income_merged)) +
ggplot(cd_sf %>% filter (!is.na(income)), aes(x = income_french)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Distribution of Income Levels",
       x = "Declared Income Levels (1 dollar = 83,6 rupees [nominal] / 20,2 rupees [PPP]",
       y = "Frequency") +
  theme_minimal(base_family = "Avenir Next") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18), 
    plot.caption = element_text(hjust = 1), 
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold", size = 11), # Rotating the x axis labels
    panel.grid.major = element_line(color = "gray80", size = 0.5), 
    panel.grid.minor = element_line(color = "gray90", size = 0.25) 
  ) +
  # Caption
  labs(caption = "Source: CHALLINEQ dataset 2020-2022; Auteur: Samuel Benkimoun, 2024.") # Text box to indicate the source and author


#ANOVA (just in case)

# Isolating the data about income and area where the households are located
data_income <- cd_sf %>% st_drop_geometry() %>% 
  select(colony, income) %>% 
  filter(!is.na(income))
# Recode the income levels by ranking, where 1 is the smaller income bracket, and 9 the highest
data_income <- data_income %>%
  mutate(income_numeric = as.numeric(income))
# Checking the average income bracket
data_income$income_numeric %>% mean()
# Testing if the variable follows a normal distribution:
stats::shapiro.test(data_income$income_numeric) # here it doesn't, so the ANOVA might not be the preferred analysis
# Running the ANOVA and checking the results
anova_result <- stats::aov(income_numeric ~ colony, data = data_income)
summary(anova_result)

# Kruskal test, non parametric alternative to the ANOVA test, to compare the average delcared income between the different areas and see whether there is significant difference between them
kruskal <- kruskal.test(income_numeric ~ colony, data = data_income)
kruskal
# Post-hoc Dunn test to check the pairs that significantly defer from one another
dunn <- rstatix::dunn_test(income_numeric ~ colony, data = data_income, p.adjust.method = "bonferroni") %>% 
  arrange(p.adj)
View(dunn)

# Having a look at the mean and standard deviation by area in order to map it, and get indications about spatial inequalities and heterogeneity
data_income %>% 
  group_by(colony) %>% 
  summarise(mean=mean(income_numeric), sd=sd(income_numeric)) %>% 
  print(n=41) %>% View()

plot(merge(convex, data_income)["income_numeric"])


# Test Moran Index
cd_sf_filtered_income <- cd_sf %>% filter(!is.na(income_merged))
# Créer une matrice de voisinage (k plus proches voisins)
nb <- knn2nb(knearneigh(st_coordinates(cd_sf_filtered_income), k = 4))
# Convertir en liste de poids
lw <- nb2listw(nb, style = "W")
# Calculer Moran's I
moran_test <- moran.test(as.numeric(cd_sf_filtered_income$income_merged), lw)
# Afficher les résultats
print(moran_test)

# Correlation test (Spearman rankings) between income level et formal educaiton level ?
cd$formal_education <- factor(cd$formal_education, levels = c("No school", 
                                                              "Primary school - 1st to 5th", 
                                                              "High school - 6th to 8th", 
                                                              "High secondary - 9th to 12th", 
                                                              "Post secondary - college and above"), 
                              ordered = TRUE, exclude=NULL)
cd_cor <- cd[!is.na(cd$formal_education) & !is.na(cd$income), ]
cor(as.numeric(cd_cor$formal_education), as.numeric(cd_cor$income), method = "spearman", use = "complete.obs") %>% print()
rm(cd_cor)
