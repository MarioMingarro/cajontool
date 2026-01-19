library(terra)

input_folder  <- "D:/A_MNCN/A_ALEJANDRA/SDM_PLANTS/V_SOILGRIDS/RESULTS_0-10/LAEA89"
output_folder <- "C:/A_TRABAJO/ALEJANDRA/1km/soil_12"
grid_path     <- "C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_1km_grid_raster.tif"

if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)

grid_raster  <- rast(grid_path)
grid_ext     <- ext(grid_raster)

archivos_tif <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

for (f in archivos_tif) {
  nombre_base <- tools::file_path_sans_ext(basename(f))
  cat("\nProcesando:", nombre_base)
  r_actual <- rast(f)
  r_actual <- crop(r_actual, grid_ext)

  r_filled <- focal(r_actual, w = 9, fun = mean, na.policy = "only", na.rm = TRUE)


  r_ajustado <- resample(r_filled, grid_raster, method = "bilinear")

  z_table <- zonal(r_ajustado, grid_raster, fun = "mean", na.rm = TRUE)

  r_zonal <- classify(grid_raster, rcl = as.matrix(z_table))
  r_zonal <- mask(r_zonal, grid_raster)

  writeRaster(r_zonal,
              filename = file.path(output_folder, paste0(nombre_base, "_1x1.tif")),
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")

  tmpFiles(remove = TRUE)
}
