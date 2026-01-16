library(terra)
library(parallel)


base_path     <- "C:/A_TRABAJO/DATA/CHELSA/MONTHLY_1980_2022"
out_clim_path <- "C:/A_TRABAJO/ALEJANDRA/1km/dynamic/climate/2006"
shp_path      <- "C:/A_TRABAJO/ALEJANDRA/Grid/Iberia_1km_grid_final.shp"

year_start <- 2000
year_end   <- 2006

if (!dir.exists(out_clim_path)) dir.create(out_clim_path, recursive = TRUE)

iberia_laea  <- vect(shp_path)
wgs84_crs    <- "EPSG:4326"
iberia_wgs84 <- project(iberia_laea, wgs84_crs)

get_climatology_iberia <- function(subfolder, prefix, is_temp = FALSE) {
  path <- file.path(base_path, subfolder)
  files <- list.files(path, pattern = "\\.tif$", full.names = TRUE)

  months <- as.numeric(gsub(".*_([0-9]{1,2})_([0-9]{4})_.*", "\\1", basename(files)))
  years  <- as.numeric(gsub(".*_([0-9]{1,2})_([0-9]{4})_.*", "\\2", basename(files)))

  idx_years <- which(years >= year_start & years <= year_end)
  files  <- files[idx_years]
  months <- months[idx_years]

  cat("\n--- Procesando:", subfolder, "(Años:", year_start, "-", year_end, ") ---")
  climatology_list <- list()

  for (m in 1:12) {
    cat("\nMes", m, ": ")
    m_files <- files[months == m]

    if(length(m_files) == 0) {
      cat("No hay archivos para el mes", m)
      next
    }

    monthly_stack <- list()
    for (f in seq_along(m_files)) {
      r_crop <- try({
        r <- rast(m_files[f])
        crop(r, iberia_wgs84, snap = "near")
      }, silent = TRUE)

      if (inherits(r_crop, "try-error")) {
        cat("\n[!] ERROR EN:", basename(m_files[f]), "- Saltado.")
      } else {
        monthly_stack[[length(monthly_stack) + 1]] <- r_crop
      }
    }
    s <- rast(monthly_stack)
    m_avg <- mean(s, na.rm = TRUE)
    m_avg <- mask(m_avg, iberia_wgs84)
    if (is_temp) { m_avg <- m_avg - 273.15 }
    out_name <- file.path(out_clim_path, paste0(prefix, "_", sprintf("%02d", m), ".tif"))
    m_avg <- project(m_avg, "EPSG:3035")
    writeRaster(m_avg, out_name, overwrite=TRUE)

    climatology_list[[m]] <- m_avg
    tmpFiles(remove = TRUE)
  }
  return(rast(climatology_list))
}

# --- EJECUCIÓN ---
pr_clim   <- get_climatology_iberia("PR", "pr_", is_temp = FALSE)
tmax_clim <- get_climatology_iberia("TMAX", "tx_", is_temp = TRUE)
tmin_clim <- get_climatology_iberia("TMIN", "tn_", is_temp = TRUE)


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
    res[1]  <- mean(ta)                          # BIO1: Media Anual
    res[2]  <- mean(tx - tn)                     # BIO2: Rango Diurno
    res[5]  <- max(tx)                           # BIO5: Max mes cálido
    res[6]  <- min(tn)                           # BIO6: Min mes frío
    res[7]  <- res[5] - res[6]                   # BIO7: Rango Anual
    res[3]  <- (res[2] / res[7]) * 100           # BIO3: Isotermalidad
    res[4]  <- sd(ta) * 100                      # BIO4: Estacionalidad Tª

    res[10] <- max(t_q)                          # BIO10: Tª Trim. Cálido
    res[11] <- min(t_q)                          # BIO11: Tª Trim. Frío

    idx_wet <- which.max(p_q); res[8] <- t_q[idx_wet] # BIO8: Tª Trim. Húmedo
    idx_dry <- which.min(p_q); res[9] <- t_q[idx_dry] # BIO9: Tª Trim. Seco

    # Precipitación (BIO12 - BIO19)
    res[12] <- sum(p)                            # BIO12: Prec. Anual
    res[13] <- max(p)                            # BIO13: Mes más húmedo
    res[14] <- min(p)                            # BIO14: Mes más seco
    res[15] <- (sd(p + 1) / (mean(p) + 1)) * 100 # BIO15: Estacionalidad Prec.

    res[16] <- max(p_q)                          # BIO16: Trimestre más húmedo
    res[17] <- min(p_q)                          # BIO17: Trimestre más seco

    idx_hot  <- which.max(t_q); res[18] <- p_q[idx_hot]  # BIO18: Prec. Trim. Cálido
    idx_cold <- which.min(t_q); res[19] <- p_q[idx_cold] # BIO19: Prec. Trim. Frío
    return(res)
  }

  return(app(s, calc_bio, filename = filename, ...))
}


cat("\n--- Calculando Biovars (Procesamiento en paralelo) ---\n")
n_cores <- 10
bioclim_wgs84 <- biovars_terra(
  prec = pr_clim, tmin = tmin_clim, tmax = tmax_clim,
  cores = n_cores,
  wopt = list(datatype = "FLT4S")
)

cat("\n--- Proyectando a ETRS89 LAEA ---")
bioclim_laea <- project(bioclim_wgs84, crs(iberia_laea), method = "bilinear")

nombres_bio <- paste0("BIO", 1:19)
names(bioclim_laea) <- nombres_bio


cat("\n--- Guardando 19 archivos individuales ---")


out_indiv_path <- "C:/A_TRABAJO/ALEJANDRA/1km/dynamic/climate/2006"

for (i in 1:19) {
  file_out <- file.path(out_indiv_path, paste0(nombres_bio[i], ".tif"))
  writeRaster(bioclim_laea[[i]],
              filename = file_out,
              overwrite = TRUE,
              wopt = list(datatype = "FLT4S"))

  cat("\nGuardado:", nombres_bio[i])
}

