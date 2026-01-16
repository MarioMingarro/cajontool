library(terra)

input_folder  <- "D:/A_DATA/MDT/PENINSULA_IBERICA/topography/k"
output_folder <- "C:/A_TRABAJO/ALEJANDRA/1km/1_estaticas/topography2"
grid_path     <- "C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_1km_grid_raster.tif"
#D:/A_MNCN/A_ALEJANDRA/SDM_PLANTS/V_SOILGRIDS/RESULTS_0-10/LAEA89

if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)


grid_raster <- rast(grid_path)
grid_ext    <- ext(grid_raster)
archivos_tif <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)

for (f in archivos_tif) {
  nombre_base <- tools::file_path_sans_ext(basename(f))
  cat("\nProcesando:", nombre_base)
  r_actual <- rast(f)
  r_ajustado <- crop(r_actual, grid_ext)
  r_ajustado <- resample(r_actual, grid_raster, method = "mean")
  z_table <- zonal(r_ajustado, grid_raster, fun = "mean", na.rm = TRUE)
  r_zonal <- classify(grid_raster, rcl = as.matrix(z_table))
  writeRaster(r_zonal,
              filename = file.path(output_folder, paste0(nombre_base, "_sd_1x1.tif")),
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")
}




