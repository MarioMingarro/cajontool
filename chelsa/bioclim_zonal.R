library(terra)

base_path      <- "C:/A_TRABAJO/DATA/CHELSA/MONTHLY_1980_2022"
out_clim_path  <- "C:/A_TRABAJO/ALEJANDRA/1km/climate2"
path_plantilla <- "C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_1km_grid_raster.tif"

if (!dir.exists(out_clim_path)) dir.create(out_clim_path, recursive = TRUE)

grid_ref <- rast(path_plantilla)

iberia_wgs84 <- project(grid_ref, "EPSG:4326")


get_climatology_iberia <- function(subfolder, prefix, is_temp = FALSE) {
  path <- file.path(base_path, subfolder)
  files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)
  months <- as.numeric(gsub(".*_([0-9]{1,3})_[0-9]{4}_.*", "\\1", basename(files)))

  cat("\n--- Procesando:", subfolder, "---\n")
  climatology_list <- list()

  for (m in 1:12) {
    cat("\nMes", m, ": ")
    m_files <- files[months == m]
    monthly_stack <- list()

    for (f in seq_along(m_files)) {
      r_crop <- try({
        r <- rast(m_files[f])
        crop(r, iberia_wgs84, snap = "near")
      }, silent = TRUE)

      if (!inherits(r_crop, "try-error")) {
        monthly_stack[[length(monthly_stack) + 1]] <- r_crop
      }
    }
    s <- rast(monthly_stack)
    m_avg_wgs84 <- mean(s, na.rm = TRUE)

    if (is_temp) m_avg_wgs84 <- m_avg_wgs84 - 273.15

    m_avg_laea <- project(m_avg_wgs84, grid_ref, method = "bilinear")

    stats_tabla <- zonal(m_avg_laea, grid_ref, fun = "mean", na.rm = TRUE)

    # Mapeamos los valores promedio de vuelta a las celdas del grid
    m_zonal_raster <- subst(grid_ref, stats_tabla[[1]], stats_tabla[[2]])

    # Guardar
    out_name <- file.path(out_clim_path, paste0(prefix, "_", sprintf("%02d", m), ".tif"))
    writeRaster(m_zonal_raster, out_name, overwrite=TRUE)

    climatology_list[[m]] <- m_zonal_raster
    cat(" OK.")
    tmpFiles(remove = TRUE)
  }
  return(rast(climatology_list))
}


pr_clim   <- get_climatology_iberia("PR", "pr_clim", is_temp = FALSE)
tmax_clim <- get_climatology_iberia("TMAX", "tmax_clim", is_temp = TRUE)
tmin_clim <- get_climatology_iberia("TMIN", "tmin_clim", is_temp = TRUE)


biovars_terra <- function(prec, tmin, tmax, filename = "", ...) {
  s <- c(prec, tmin, tmax)

  calc_bio <- function(x) {
    if (all(is.na(x))) return(rep(NA, 19))
    p <- x[1:12]; tn <- x[13:24]; tx <- x[25:36]; ta <- (tn + tx) / 2

    window_3m <- function(v) {
      v_ext <- c(v, v[1:2])
      return(v_ext[1:12] + v_ext[2:13] + v_ext[3:14])
    }

    p_q <- window_3m(p); t_q <- window_3m(ta) / 3
    res <- rep(NA, 19)

    # Temperatura (BIO1 - BIO11)
    res[1]  <- mean(ta)                          # BIO1
    res[2]  <- mean(tx - tn)                     # BIO2
    res[5]  <- max(tx)                           # BIO5
    res[6]  <- min(tn)                           # BIO6
    res[7]  <- res[5] - res[6]                   # BIO7
    res[3]  <- (res[2] / res[7]) * 100           # BIO3
    res[4]  <- sd(ta) * 100                      # BIO4
    res[10] <- max(t_q)                          # BIO10
    res[11] <- min(t_q)                          # BIO11
    # Compuestas
    idx_wet <- which.max(p_q); res[8] <- t_q[idx_wet] # BIO8
    idx_dry <- which.min(p_q); res[9] <- t_q[idx_dry] # BIO9

    # Precipitación (BIO12 - BIO19)
    res[12] <- sum(p)                            # BIO12
    res[13] <- max(p)                            # BIO13
    res[14] <- min(p)                            # BIO14
    res[15] <- (sd(p + 1) / (mean(p) + 1)) * 100 # BIO15
    res[16] <- max(p_q)                          # BIO16
    res[17] <- min(p_q)                          # BIO17
    # Compuestas
    idx_hot  <- which.max(t_q); res[18] <- p_q[idx_hot]  # BIO18
    idx_cold <- which.min(t_q); res[19] <- p_q[idx_cold] # BIO19
    return(res)
  }

  return(app(s, calc_bio, filename = filename, ...))
}



n_cores <- 10

bioclim_grid <- biovars_terra(
  prec = pr_clim,
  tmin = tmin_clim,
  tmax = tmax_clim,
  cores = n_cores,
  wopt = list(datatype = "FLT4S")
)

nombres_bio <- paste0("BIO", 1:19)
names(bioclim_grid) <- nombres_bio

if (!dir.exists(out_indiv_path)) dir.create(out_indiv_path, recursive = TRUE)

for (i in 1:19) {
  file_out <- file.path(out_clim_path, paste0(nombres_bio[i], ".tif"))
  writeRaster(bioclim_grid[[i]],
              filename = file_out,
              overwrite = TRUE,
              wopt = list(datatype = "FLT4S"))
  cat("\nListo:", nombres_bio[i])
}

