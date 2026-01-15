library(terra)

# 1. Carga de datos
clc_original <- rast("C:/A_TRABAJO/DATA/CLC_copernicus/CLC2006_100m.tif")
r_template   <- rast("C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_1km_grid_raster.tif")

# 2. Definición de categorías (Mantenemos tu lógica de reclasificación)
# Usamos el ajuste de 0 a 11 para incluir la clase 1
categorias <- list(
  "1_Artificial" = c(0, 11),
  "2_Agricola"   = c(11, 22),
  "3_Bosque"     = c(22, 34),
  "4_Humedal"    = c(34, 39),
  "5_Agua"       = c(39, 44)
)

# 3. Preparación inicial
clc_original <- crop(clc_original, r_template)

# Bucle para procesar cada categoría
for (nom_cat in names(categorias)) {

  cat("\nProcesando categoría:", nom_cat)

  # Límites de la categoría actual
  limite_inf <- categorias[[nom_cat]][1]
  limite_sup <- categorias[[nom_cat]][2]

  # A. Crear máscara binaria (1 para la clase actual, 0 para las demás)
  # Esto es lo que permite calcular el porcentaje después
  m_binaria <- matrix(c(limite_inf, limite_sup, 1), ncol = 3, byrow = TRUE)
  clc_binario <- classify(clc_original, m_binaria, others = 0)

  # B. Calcular porcentaje de área (0 a 1)
  # method = "average" cuenta cuántos 1s hay en la superficie de 1km
  r_pct <- resample(clc_binario, r_template, method = "average")
  r_pct <- r_pct * 100

  # C. Garantizar alineación e inyectar valores
  r_final <- r_template
  values(r_final) <- values(r_pct)
  r_final <- mask(r_final, r_template) # Mantener NAs del grid (mar, etc)

  # D. Guardar cada categoría por separado
  output_path <- file.path("C:/A_TRABAJO/ALEJANDRA/1km/landuse/", paste0("Pct_CLC_06_L1_", nom_cat, "_1x1.tif"))

  writeRaster(r_final,
              output_path,
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")

  cat(" -> Guardado en:", output_path)
}

message("\n--- Proceso finalizado: Todas las categorías exportadas ---")

############### L2

library(terra)

# 1. CARGA DE DATOS
clc_original <- rast("C:/A_TRABAJO/DATA/CLC_copernicus/CLC2006_100m.tif")
r_template   <- rast("C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_1km_grid_raster.tif")

# 2. DEFINICIÓN DE LA RECLASIFICACIÓN NIVEL 2
# Usamos tu matriz para agrupar el CLC original en las nuevas clases (11 a 25)
reclass_lvl2 <- matrix(c(
  0,   2,  11, # Zonas urbanas (111-112)
  2,   6,  12, # Zonas industriales, comerciales... (121-124)
  6,   9,  13, # Minería, escombreras... (131-133)
  9,   11, 14, # Zonas verdes urbanas... (141-142)
  11,  14, 21, # Tierras de labor (211-213)
  14,  17, 22, # Cultivos permanentes (221-223)
  17,  18, 23, # Prados y pastizales (231)
  18,  22, 24, # Zonas agrícolas heterogéneas (241-244)
  22,  25, 31, # Bosques (311-313)
  25,  29, 32, # Arbustiva/herbácea (321-324)
  29,  34, 33, # Áreas abiertas sin vegetación (331-335)
  34,  36, 41, # Humedales continentales (411-412)
  36,  39, 42, # Humedales marítimos (421-423)
  39,  41, 51, # Aguas continentales (511-512)
  41,  44, 52  # Aguas marítimas (521-523)
), ncol = 3, byrow = TRUE)

# Nombres para los archivos de salida
nombres_clases <- c(
  "11_Urbano", "12_Industrial", "13_Minero", "14_VerdeUrbano",
  "21_Labor", "22_Permante", "23_Pastizal", "24_AgriHetero",
  "34_Bosque", "32_Arbustivo", "33_Abierto", "41_HumedalCont",
  "42_HumedalMar", "51_AguaCont", "52_AguaMar"
)

# 3. PROCESAMIENTO
clc_crop <- crop(clc_original, r_template)

# Clasificamos el CLC original en las 15 nuevas clases
cat("\nReclasificando a Nivel 2...")
clc_reclass <- classify(clc_crop, reclass_lvl2, others = NA)

# 4. BUCLE PARA EXTRAER PORCENTAJES DE CADA CLASE (11 a 25)
for (i in 1:nrow(reclass_lvl2)) {

  id_clase <- reclass_lvl2[i, 3]
  nombre   <- nombres_clases[i]

  cat("\nCalculando porcentaje para:", nombre)

  # A. Crear máscara binaria para la clase actual
  # Si el píxel es igual al id_clase, ponemos 1, si no 0.
  clc_binario <- clc_reclass == id_clase

  # B. Promedio de área (Porcentaje)
  r_pct <- resample(clc_binario, r_template, method = "average")
  r_pct <- r_pct * 100

  # C. Inyectar en estructura del grid y aplicar máscara
  r_final <- r_template
  values(r_final) <- values(r_pct)
  r_final <- mask(r_final, r_template)

  # D. Guardar
  output_name <- paste0("Pct_CLC_06_L2_", nombre, "_1x1.tif")
  writeRaster(r_final,
              file.path("C:/A_TRABAJO/ALEJANDRA/1km/landuse/", output_name),
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")
}

message("\n--- Proceso Nivel 2 Finalizado ---")


#########l3
library(terra)

# 1. CARGA DE DATOS
clc_original <- rast("C:/A_TRABAJO/DATA/CLC_copernicus/CLC2006_100m.tif")
r_template   <- rast("C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_1km_grid_raster.tif")

# 2. DEFINICIÓN DE LA RECLASIFICACIÓN (Nivel 3 específico)
# Usamos decimales para que classify capture exactamente los enteros
reclass_lvl3 <- matrix(c(
  22.5, 23.5, 22, # 311 Bosques de frondosas -> ID 22
  24.5, 25.5, 22, # 313 Bosques mixtos    -> ID 22
  23.5, 24.5, 23, # 312 Bosques de coníferas -> ID 23
  30.5, 31.5, 24, # 332 Roquedo -> ID 24
  31.5, 32.5, 25, # 333 Zonas con vegetación dispersa -> ID 25
  32.5, 33.5, 26, # 334 Zonas quemadas -> ID 26
  15.5, 16.5, 27  # 222 Frutales -> ID 27
), ncol = 3, byrow = TRUE)

# Nombres asociados a cada ID ÚNICO (usamos una lista para mapear ID -> Nombre)
diccionario_nombres <- list(
  "22" = "311_3_Bosque_Frond_Mix",
  "23" = "312_Bosque_Conif",
  "24" = "332_Roquedo",
  "25" = "333_Veg_Dispersa",
  "26" = "334_Quemado",
  "27" = "222_Frutales"
)

# 3. PROCESAMIENTO
clc_crop <- crop(clc_original, r_template)

cat("\nReclasificando...")
# 'others = NA' es vital para que solo queden las clases que nos interesan
clc_reclass <- classify(clc_crop, reclass_lvl3, others = NA)

# 4. BUCLE SOBRE LOS IDs ÚNICOS
# Obtenemos los IDs que realmente existen en nuestra matriz (22, 23, 24, 25, 26, 27)
ids_unicos <- sort(unique(reclass_lvl3[, 3]))

for (id in ids_unicos) {

  nombre <- diccionario_nombres[[as.character(id)]]
  cat("\nCalculando porcentaje para:", nombre)

  # A. Crear máscara binaria
  # Si el ID es 22, este paso detectará tanto las frondosas como los mixtos a la vez
  clc_binario <- clc_reclass == id

  # Importante: convertir NAs a 0 para que el promedio de área sea correcto
  clc_binario <- classify(clc_binario, matrix(c(NA, 0), ncol=2), others=NULL)

  # B. Promedio de área (Resample con method = "average")
  r_pct <- resample(clc_binario, r_template, method = "average")
  r_pct <- r_pct * 100

  # C. Inyectar en estructura del grid y aplicar máscara de Iberia
  r_final <- r_template
  values(r_final) <- values(r_pct)
  r_final <- mask(r_final, r_template)

  # D. Guardar
  output_name <- paste0("Pct_CLC_06_L3_", nombre, "_1x1.tif")
  writeRaster(r_final,
              file.path("C:/A_TRABAJO/ALEJANDRA/1km/landuse/", output_name),
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")
}

message("\n--- Proceso Finalizado: Clases unificadas correctamente ---")
