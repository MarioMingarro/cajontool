library(terra)


clc_original <- rast("C:/A_TRABAJO/DATA/CLC_copernicus/CLC2006_100m.tif")
r_template   <- rast("C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_10km_grid_raster.tif")


categorias <- list(
  "1_Artificial" = c(0, 11),
  "2_Agricola"   = c(11, 22),
  "3_Bosque"     = c(22, 34),
  "4_Humedal"    = c(34, 39),
  "5_Agua"       = c(39, 44)
)

clc_original <- crop(clc_original, r_template)
for (nom_cat in names(categorias)) {
  cat("\nProcesando categoría:", nom_cat)
  limite_inf <- categorias[[nom_cat]][1]
  limite_sup <- categorias[[nom_cat]][2]
  m_binaria <- matrix(c(limite_inf, limite_sup, 1), ncol = 3, byrow = TRUE)
  clc_binario <- classify(clc_original, m_binaria, others = 0)
  r_pct <- resample(clc_binario, r_template, method = "average")
  r_pct <- r_pct * 100
  r_final <- r_template
  values(r_final) <- values(r_pct)
  r_final <- mask(r_final, r_template)
  output_path <- file.path("C:/A_TRABAJO/ALEJANDRA/10km/landuse/2006/", paste0("Pct_CLC_06_L1_", nom_cat, "_10x10.tif"))
  writeRaster(r_final,
              output_path,
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")
}

# L2 ----

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


nombres_clases <- c(
  "11_Urbano", "12_Industrial", "13_Minero", "14_VerdeUrbano",
  "21_Labor", "22_Permante", "23_Pastizal", "24_AgriHetero",
  "34_Bosque", "32_Arbustivo", "33_Abierto", "41_HumedalCont",
  "42_HumedalMar", "51_AguaCont", "52_AguaMar"
)

clc_crop <- crop(clc_original, r_template)
clc_reclass <- classify(clc_crop, reclass_lvl2, others = NA)

for (i in 1:nrow(reclass_lvl2)) {

  id_clase <- reclass_lvl2[i, 3]
  nombre   <- nombres_clases[i]
  cat("\nCalculando porcentaje para:", nombre)
  clc_binario <- clc_reclass == id_clase
  r_pct <- resample(clc_binario, r_template, method = "average")
  r_pct <- r_pct * 100
  r_final <- r_template
  values(r_final) <- values(r_pct)
  r_final <- mask(r_final, r_template)
  output_path <- file.path("C:/A_TRABAJO/ALEJANDRA/10km/landuse/2006/", paste0("Pct_CLC_06_L2_", nombre, "_10x10.tif"))
  writeRaster(r_final,
              output_path,
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")
}


#L3 -----

reclass_lvl3 <- matrix(c(
  22.5, 23.5, 22, # 311 Bosques de frondosas -> ID 22
  24.5, 25.5, 22, # 313 Bosques mixtos    -> ID 22
  23.5, 24.5, 23, # 312 Bosques de coníferas -> ID 23
  30.5, 31.5, 24, # 332 Roquedo -> ID 24
  31.5, 32.5, 25, # 333 Zonas con vegetación dispersa -> ID 25
  32.5, 33.5, 26, # 334 Zonas quemadas -> ID 26
  15.5, 16.5, 27  # 222 Frutales -> ID 27
), ncol = 3, byrow = TRUE)


diccionario_nombres <- list(
  "22" = "311_3_Bosque_Frond_Mix",
  "23" = "312_Bosque_Conif",
  "24" = "332_Roquedo",
  "25" = "333_Veg_Dispersa",
  "26" = "334_Quemado",
  "27" = "222_Frutales"
)


clc_crop <- crop(clc_original, r_template)

clc_reclass <- classify(clc_crop, reclass_lvl3, others = NA) # 'others = NA' solo las clases que interesan

ids_unicos <- sort(unique(reclass_lvl3[, 3]))
for (id in ids_unicos) {
  nombre <- diccionario_nombres[[as.character(id)]]
  cat("\nCalculando porcentaje para:", nombre)
  clc_binario <- clc_reclass == id
  clc_binario <- classify(clc_binario, matrix(c(NA, 0), ncol=2), others=NULL)
  r_pct <- resample(clc_binario, r_template, method = "average")
  r_pct <- r_pct * 100
  r_final <- r_template
  values(r_final) <- values(r_pct)
  r_final <- mask(r_final, r_template)
  output_path <- file.path("C:/A_TRABAJO/ALEJANDRA/10km/landuse/2006/", paste0("Pct_CLC_06_L3_", nombre, "_10x10.tif"))
  writeRaster(r_final,
              output_path,
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")
}
