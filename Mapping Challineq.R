#CHALLINEQ data base maps and local indicators
library(readxl)
library(dplyr)
library(sf)
library(leaflet)
library(RColorBrewer)
library(mapview)
library(htmlwidgets)
library(webshot)


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
                                style = list("color" = "black", "font-weight" = "bold",
                                             # Adding a white buffer around the text
                                "text-shadow" = "-1px -1px 0 white, 1px -1px 0 white, -1px 1px 0 white, 1px 1px 0 white"
                                )
                                ),
  ) %>%
  addScaleBar(position = "bottomleft", options = scaleBarOptions(metric = TRUE, imperial = FALSE))

# Define HTML content for the title
#title_html <- '<h6 style="text-align: center;">Location of the surveyed areas</h3>'
# Add the title using addControl
#m <- m %>% addControl(html = title_html, position = "bottomleft")
m
# Save the widget
saveWidget(m, "./challineq_areas.html", selfcontained = TRUE)

# Use webshot to capture the map as an image
webshot("./challineq_areas.html", file = "./challineq_areas.png", vwidth = 1200, vheight = 900, cliprect = "viewport")
