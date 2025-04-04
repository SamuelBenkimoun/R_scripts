library(readxl)
library(tidygeocoder)
library(giscoR)
library(ggplot2)
library(ggthemes)
library(ggrepel)
library(ggdark)
# Loading the survey data on the students' travelling preferences
dest <- read_excel("CartoStatsEnqueteDestinations.xlsx", 
                                            col_types = c("text", "numeric"))

dest <- tibble::add_column(dest, Origine = "France", .before = 1)
# Geocoding origin and destination
dest <- dest %>% 
  tidygeocoder::geocode(Origine, method='osm', lat = latitude_og , long = longitude_og)
dest <- dest %>% 
  tidygeocoder::geocode(Destination, method='osm', lat = latitude_dt , long = longitude_dt)
# Fixing approximately Germany odd coordinates after geocoding
dest[1,]$latitude_dt <- 50.9
dest[1,]$longitude_dt <- 10.3
# Creating the linestring geometries
dest$wkt <- paste("LINESTRING(", dest$longitude_og," ",dest$latitude_og,", ",dest$longitude_dt," ",dest$latitude_dt, ")", sep = "")
dsf <- st_as_sf(dest, wkt = "wkt", crs = st_crs(4326))
dsf3035 <- st_transform(dsf, crs = st_crs(3035))
# European countries to map
nuts0 <- gisco_get_nuts(
     year = "2021", epsg = "4326", resolution = "3",
     nuts_level = "0") %>%
  subset(geo != "TR") %>%
  subset(geo != "IS")
# Mapping
p1 <- ggplot() +
  geom_sf(
    data = nuts0,
    fill = "#063140",
    color = "white",
    size = .2,
    alpha = .35
  ) +
  geom_sf(
    data = dsf,
    arrow = arrow(length = unit(0.15, "cm"),type = "closed"),
    lineend = 'butt',
    linejoin = 'round',
    aes(
      linewidth = `Nombre de PREF_VACANCES`/10,
      alpha = `Nombre de PREF_VACANCES`
    ),
    fill = "gold",
    color = "gold"
  )+
  #geom_sf_label(data = dsf, aes(label=`Nombre de PREF_VACANCES`))+
  #ggrepel::geom_label_repel(data = dsf,aes(label=`Nombre de PREF_VACANCES`, geometry = wkt),stat = "sf_coordinates",min.segment.length = 0)+
  coord_sf(xlim = c(-23, 42), ylim = c(30, 73), expand = FALSE)+
  scale_linewidth(
    name = "Nombre d'étudiant-e-s ayant déclaré leur préférence",
    range = c(.5, 2.5),
    breaks = c(.5,1.25,2.5),
    labels = c("1: Slovaquie","14: Espagne","28: Norvège")
  ) +
  scale_alpha(
    guide = 'none',
    range = c(.20, .95)
  ) +
  dark_theme_linedraw()+
  theme(legend.position = "bottom")+
  #theme(legend.background = element_rect(fill="lightgray", linewidth =0.5, linetype="solid"))+
  theme(panel.grid.major = element_line(colour = "lightgray", linetype = "dashed"))+
  theme(plot.title = element_text(face = "bold", hjust = 0.5, size = 20), plot.subtitle = element_text(hjust = 0.5, size = 14))+
  ggtitle("Destination priviliégiée pour les vacances", subtitle = "Enquête mobilité L1 Géo à Paris 1 2023-2024")+
  labs(caption = "Source: Enquête mobilité 2023-2024, Réalisation: Equipe pédagogique C-S")+
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0.5))
  #+annotate("text",x=-1,y=-3.1,label="Source: Enquête mobilité 2023-2024, Réalisation: Equipe pédagogique C-S")
p1
