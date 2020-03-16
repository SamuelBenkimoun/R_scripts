library(readr)
library(sf)
semaine_type_5km <- read_delim("semaine_type_5km.csv","\t", escape_double = FALSE, trim_ws = TRUE)
semaine <- st_as_sf(semaine_type_5km, wkt = "Geometry")
semaine_starting <- semaine
semaine_starting$Geometry <- st_line_sample(semaine$Geometry, sample = 0)
semaine_ending <- semaine
semaine_ending$Geometry <- st_line_sample(semaine$Geometry, sample = 1)
