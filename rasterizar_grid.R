library(sf)
library(terra)

grid_shp <- st_read("C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_10km_grid_final.shp")

res_deseada <- 10000

ext_shp <- ext(grid_shp)
raster_ref <- rast(ext_shp, resolution = res_deseada, crs = crs(grid_shp))

grid_rasterizado <- rasterize(grid_shp, raster_ref, field = "id_final")

writeRaster(grid_rasterizado, "C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_10km_grid_raster.tif", overwrite=TRUE)

