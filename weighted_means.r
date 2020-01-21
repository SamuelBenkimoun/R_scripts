library(sp)
library(raster)
library(sf)
dispensaries <- st_read("./Malaria/dispensaries_cases_3857.shp",
crs=3857)
capital_va <- raster("./capital_va.tif")
projection(capital_va) <- (CRS("+init=epsg:3857"))
capital_va_extract <- extract(capital_va, dispensaries, buffer=1500,
fun=mean, weights=TRUE, normalizeWeights=TRUE, sp=TRUE)
x <- data.frame("id" = capital_va_extract@data[["Dispensa_1"]],
"capital_va" = capital_va_extract@data[["capital_va"]])
View(x)
