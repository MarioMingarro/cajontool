library(terra)

input_folder  <- "C:/A_TRABAJO/ALEJANDRA/1km/2_dinamicas/2006/clima"
output_folder <- "C:/A_TRABAJO/ALEJANDRA/10km/2_dinamicas/2006/clima"
grid_path     <- "C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_10km_grid_raster.tif"

if (!dir.exists(output_folder)) dir.create(output_folder, recursive = TRUE)


grid_raster <- rast(grid_path)
grid_ext    <- ext(grid_raster)
archivos_tif <- list.files(input_folder, pattern = "\\.tif$", full.names = TRUE)
#archivos_tif <- archivos_tif[[2]]
for (f in archivos_tif) {
  nombre_base <- tools::file_path_sans_ext(basename(f))
  cat("\nProcesando:", nombre_base)
  r_actual <- rast(f)
  r_ajustado <- crop(r_actual, grid_ext)
  r_ajustado <- resample(r_actual, grid_raster, method = "mean")
  z_table <- zonal(r_ajustado, grid_raster, fun = "mean", na.rm = TRUE)
  r_zonal <- classify(grid_raster, rcl = as.matrix(z_table))
  writeRaster(r_zonal,
              filename = file.path(output_folder, paste0(nombre_base, "_10x10.tif")),
              overwrite = TRUE,
              datatype = "FLT4S",
              gdal = "COMPRESS=DEFLATE")
}




