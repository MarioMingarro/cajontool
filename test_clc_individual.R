library(terra)
library(sf)
library(dplyr)
library(exactextractr)

# 1. Carga de datos
clc_original <- rast("C:/A_TRABAJO/DATA/CLC_copernicus/CLC2006_100m.tif")
grid         <- st_read("C:/A_TRABAJO/ALEJANDRA/Grid/Export_Output.shp")
r_template   <- rast("C:/A_TRABAJO/ALEJANDRA/1km/climate/pr_01_1x1.tif")

# Sincronizar CRS (Muy importante: el template manda)
grid <- st_transform(grid, crs(r_template))
clc_original <- project(clc_original, r_template, method = "near") # Asegura alineación

# 2. Reclasificación (Artificial: Índices 1-11)
m_art <- matrix(c(1, 11, 1), ncol = 3, byrow = TRUE)
clc_art <- classify(clc_original, m_art, others = 0)

# 3. Cálculo de porcentaje
# Usamos el grid para extraer del raster de 100m
art_pct <- exact_extract(clc_art, grid, function(values, coverage_fraction) {
  sum(values * coverage_fraction, na.rm = TRUE) / sum(coverage_fraction, na.rm = TRUE) * 100
})

# 4. TRUCO PARA ALINEACIÓN PERFECTA:
# En lugar de rasterize(), vamos a usar la posición espacial de los centroides
grid$pct_art <- art_pct
centroides <- st_centroid(grid)
coords <- st_coordinates(centroides)

# Creamos un raster vacío idéntico al template
r_final <- rast(r_template)
values(r_final) <- NA # Limpiamos valores previos

# Asignamos los valores por coordenadas (esto garantiza que caigan en la celda correcta)
cells <- cellFromXY(r_final, coords)
r_final[cells] <- grid$pct_art

# 5. Guardar
writeRaster(r_final, "C:/A_TRABAJO/ALEJANDRA/CLC/Pct_Artificial_Perfect_Match.tif", overwrite = TRUE)

message("--- Proceso finalizado: Píxeles alineados con el template ---")
