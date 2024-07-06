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

# Create the map using the function, mapping 'income' attribute

# Create a new variable 'income_merged' to reduce the number of levels
cd_sf$income_merged <- cd_sf$income
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

ord <- factor(aa$city, levels = c('Charlottesville', 'Richmond', 'Charlotte', 'Raleigh'))

# Create a colorFactor palette including NA
palette_income <- leaflet::colorFactor(
  palette = c(magma(9, direction = 1)),
  domain = levels(cd_sf$income_merged),
  na.color = "gainsboro",
  #levels = levels(cd_sf$income_merged),
  ordered = TRUE
)


# Create Leaflet map
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
  addCircleMarkers(data = cd_sf, 
                   radius = 2.5, 
                   color = "black",
                   weight = 0.1,
                   fillColor = ~palette_income(income_merged),  # Use palette_income directly
                   fillOpacity = 0.7,
                   popup = ~paste("Colony:", colony, "<br>", "Income:", income_merged)) %>%
  #addLegend(position = "bottomright", 
  #          pal = palette_income, 
  #          values = c(levels(cd_sf$income_merged), NA),
  #          title = "Income Levels",
  #          opacity = 1) %>%
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
                  position = 'topright') %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))
