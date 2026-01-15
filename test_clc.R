library(terra)
library(sf)
library(dplyr)
library(exactextractr)
library(tidyr)

# 1. Carga de datos
clc_original <- rast("C:/A_TRABAJO/DATA/CLC_copernicus/CLC2006_100m.tif")
grid         <- st_read("C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_1km_grid_final.shp")
r_template   <- rast("C:/A_TRABAJO/ALEJANDRA/1km/climate/pr_01_1x1.tif")

grid <- st_transform(grid, crs(clc_original))
clc_original <- crop(clc_original, grid)

# 2. Nueva Matriz de Reclasificación (Basada en tus ÍNDICES 1-48)
# Formato: [Inicio_Indice, Fin_Indice, Nueva_Clase]
reclass_matrix <- matrix(c(
  1,  11, 1,  # Artificial (Cubre 111 a 142)
  12, 22, 2,  # Agrícola (Cubre 211 a 244)
  23, 34, 3,  # Bosques y Semi-natural (Cubre 311 a 335)
  35, 39, 4,  # Humedales (Cubre 411 a 423)
  40, 44, 5   # Masas de agua (Cubre 511 a 523)
), ncol = 3, byrow = TRUE)

# 3. Reclasificación general
message("Reclasificando todas las categorías...")
clc_lvl1 <- classify(clc_original, reclass_matrix, others = NA)

# 4. Extracción de porcentajes para TODAS las clases a la vez
message("Calculando porcentajes para todas las clases...")

pct_list <- exact_extract(clc_lvl1, grid, function(values, coverage_fraction) {
  # Creamos el dataframe asegurando nombres claros
  df <- data.frame(id_clase = values, frac = coverage_fraction) %>%
    filter(!is.na(id_clase)) %>%
    group_by(id_clase) %>%
    summarise(sum_frac = sum(frac), .groups = "drop") %>%
    mutate(pct = (sum_frac / sum(sum_frac)) * 100)
  return(df)
}, summarize_df = FALSE)

# 5. Organizar resultados
# Usamos el poly_id que creamos al principio en el grid
names(pct_list) <- grid$poly_id

df_final <- bind_rows(pct_list, .id = "poly_id") %>%
  mutate(poly_id = as.integer(poly_id))

# 6. Generar los Rásters de salida
# Definimos las clases que queremos extraer (las que pusimos en la matriz: 1, 2, 3, 4, 5)
clases_a_procesar <- c(1, 2, 3, 4, 5)
nombres_clases <- c("Artificial", "Agricola", "Forestal", "Humedales", "Agua")

for (i in clases_a_procesar) {
  nombre_label <- nombres_clases[i]
  message("Generando ráster para clase: ", nombre_label)

  # CORRECCIÓN AQUÍ: Filtramos por 'id_clase' que es el nombre que pusimos arriba
  capa_clase <- df_final %>%
    filter(id_clase == i) %>%
    select(poly_id, pct)

  # Si la clase no existe en ninguna celda, saltamos o creamos raster de ceros
  if(nrow(capa_clase) == 0) {
    message("Advertencia: No se encontraron píxeles para la clase ", nombre_label)
    next
  }

  # Unir al grid
  grid_data <- grid %>%
    select(poly_id) %>%
    left_join(capa_clase, by = "poly_id") %>%
    mutate(pct = replace_na(pct, 0))

  # Convertir a ráster
  r_clase <- rasterize(vect(grid_data), r_template, field = "pct", fun = "max")

  # Guardar
  file_out <- paste0("C:/A_TRABAJO/ALEJANDRA/CLC/Pct_Lvl1_", i, "_", nombre_label, ".tif")
  writeRaster(r_clase, file_out, overwrite = TRUE)
}

reclass_lvl1 <- matrix(c(
  0,  11, 1,  # Artificial
  12, 22, 2,  # Agrícola
  23, 34, 3,  # Forestal y Semi-natural
  35, 39, 4,  # Humedales
  40, 44, 5   # Masas de agua
), ncol = 3, byrow = TRUE)

reclass_lvl2 <- matrix(c(
  0,   2,  11, # Zonas urbanas (111-112)
  3,   6,  12, # Zonas industriales, comerciales y de transportes (121-124)
  7,   9,  13, # Zonas de extracción minera, escombreras y construcción (131-133)
  10,  11, 14, # Zonas verdes urbanas y de recreo (141-142)
  12,  14, 15, # Tierras de labor (211-213)
  15,  17, 16, # Cultivos permanentes (221-223)
  18,  18, 17, # Prados y pastizales (231)
  19,  22, 18, # Zonas agrícolas heterogéneas (241-244)
  23,  25, 19, # Bosques (311-313)
  26,  29, 20, # Áreas con vegetación arbustiva y/o herbácea (321-324)
  30,  34, 21, # Áreas abiertas con poca o sin vegetación (331-335)
  35,  36, 22, # Humedales continentales (411-412)
  37,  39, 23, # Humedales marítimos (421-423)
  40,  41, 24, # Aguas continentales (511-512)
  42,  44, 25  # Aguas marítimas (521-523)
), ncol = 3, byrow = TRUE)

reclass_lvl3_custom <- matrix(c(
  23, 23, 22, # Bosques de frondosas (311 -> ID 22)
  25, 25, 22, # Bosques mixtos (313 -> ID 22)
  24, 24, 23, # Bosques de coníferas (312 -> ID 23)
  31, 31, 24, # Roquedo (332 -> ID 24)
  32, 32, 25, # Zonas con vegetación dispersa (333 -> ID 25)
  33, 33, 26, # Zonas quemadas (334 -> ID 26)
  16, 16, 27  # Frutales (222 -> ID 27)
), ncol = 3, byrow = TRUE)
