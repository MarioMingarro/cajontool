# =========================================================================
# SCRIPT COMPLETO: CLIMATOLOGÍA E IBERIA BIOCLIM (CHELSA V2.1)
# =========================================================================
library(terra)
library(parallel)

# --- 1. CONFIGURACIÓN DE RUTAS ---
base_path     <- "C:/A_TRABAJO/DATA/CHELSA/MONTHLY_1980_2022"
out_clim_path <- "C:/A_TRABAJO/DATA/CHELSA/CLIMATOLOGIAS_IBERIA"
shp_path      <- "C:/A_TRABAJO/ALEJANDRA/Grid/Europe limits/Iberia_limits.shp"
output_final  <- "C:/A_TRABAJO/DATA/CHELSA/IBERIA_BIOCLIM_1980_2021_LAEA.tif"

if (!dir.exists(out_clim_path)) dir.create(out_clim_path, recursive = TRUE)

# --- 2. PREPARACIÓN DE LÍMITES Y PROYECCIONES ---
# Cargar límite original (LAEA) y proyectar a WGS84 para el recorte eficiente
iberia_laea  <- vect(shp_path)
# Usamos un CRS estándar WGS84 (el de CHELSA)
wgs84_crs    <- "EPSG:4326"
iberia_wgs84 <- project(iberia_laea, wgs84_crs)

# =========================================================================
# 3. FUNCIÓN: CLIMATOLOGÍA MENSUAL CON RECORTE Y SEGURIDAD
# =========================================================================
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
      # Intentar cargar y recortar (protección contra archivos corruptos)
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

    # Promediar años del mes 'm'
    s <- rast(monthly_stack)
    m_avg <- mean(s, na.rm = TRUE)
    m_avg <- mask(m_avg, iberia_wgs84) # Aplicar máscara de la costa


    if (is_temp) {
      m_avg <- m_avg - 273.15
    } else {
      m_avg <- m_avg
    }

    # Guardar archivo intermedio
    out_name <- file.path(out_clim_path, paste0(prefix, "_", sprintf("%02d", m), ".tif"))
    writeRaster(m_avg, out_name, overwrite=TRUE)
    climatology_list[[m]] <- m_avg
    cat(" OK.")

    # Limpieza de temporales para evitar saturación de disco/RAM
    tmpFiles(remove = TRUE)
  }
  return(rast(climatology_list))
}

# =========================================================================
# 4. FUNCIÓN: BIOCLIM VARIABLE CALCULATOR (TERRA OPTIMIZED)
# =========================================================================
biovars_terra <- function(prec, tmin, tmax, filename = "", ...) {
  s <- c(prec, tmin, tmax) # stack de 36 capas

  calc_bio <- function(x) {
    if (all(is.na(x))) return(rep(NA, 19))
    p <- x[1:12]; tn <- x[13:24]; tx <- x[25:36]; ta <- (tn + tx) / 2
    window_3m <- function(v) {
      v_ext <- c(v, v[1:2])
      return(v_ext[1:12] + v_ext[2:13] + v_ext[3:14])
    }
    p_q <- window_3m(p); t_q <- window_3m(ta) / 3
    res <- rep(NA, 19)
    res[1]<-mean(ta); res[2]<-mean(tx-tn); res[5]<-max(tx); res[6]<-min(tn)
    res[7]<-res[5]-res[6]; res[3]<-(res[2]/res[7])*100; res[4]<-sd(ta)*100
    res[12]<-sum(p); res[13]<-max(p); res[14]<-min(p); res[15]<-(sd(p+1)/mean(p+1))*100
    res[16]<-max(p_q); res[17]<-min(p_q)
    idx_wet<-which.max(p_q); res[8]<-t_q[idx_wet]
    idx_dry<-which.min(p_q); res[9]<-t_q[idx_dry]
    res[10]<-max(t_q); res[11]<-min(t_q)
    idx_hot<-which.max(t_q); res[18]<-p_q[idx_hot]
    idx_cold<-which.min(t_q); res[19]<-p_q[idx_cold]
    return(res)
  }

  return(app(s, calc_bio, filename = filename, ...))
}

# =========================================================================
# 5. EJECUCIÓN DEL FLUJO DE TRABAJO
# =========================================================================

# A. Generar Climatologías mensuales (12 tifs por variable)
pr_clim   <- get_climatology_iberia("PR", "pr_clim", is_temp = FALSE)
tmax_clim <- get_climatology_iberia("TMAX", "tmax_clim", is_temp = TRUE)
tmin_clim <- get_climatology_iberia("TMIN", "tmin_clim", is_temp = TRUE)

# B. Calcular 19 Biovars en WGS84
cat("\n--- Calculando Biovars (Procesamiento en paralelo) ---\n")
n_cores <-10
bioclim_wgs84 <- biovars_terra(
  prec = pr_clim, tmin = tmin_clim, tmax = tmax_clim,
  cores = n_cores,
  wopt = list(datatype = "FLT4S")
)

# 1. Primero proyectamos el stack completo (más eficiente)
cat("\n--- Proyectando a ETRS89 LAEA ---")
bioclim_laea <- project(bioclim_wgs84, crs(iberia_laea), method = "bilinear")

# 2. Definimos los nombres de las capas
nombres_bio <- paste0("BIO", 1:19)
names(bioclim_laea) <- nombres_bio

# 3. Guardamos cada capa por separado en un bucle
cat("\n--- Guardando 19 archivos individuales ---")

# Creamos una carpeta específica para las capas individuales si no existe
out_indiv_path <- "C:/A_TRABAJO/DATA/CHELSA/BIOCLIMAS_INDIVIDUALES"
if (!dir.exists(out_indiv_path)) dir.create(out_indiv_path, recursive = TRUE)

for (i in 1:19) {
  # Construir la ruta de salida para cada BIO
  file_out <- file.path(out_indiv_path, paste0(nombres_bio[i], ".tif"))

  # Guardar la capa i del stack
  writeRaster(bioclim_laea[[i]],
              filename = file_out,
              overwrite = TRUE,
              wopt = list(datatype = "FLT4S"))

  cat("\nGuardado:", nombres_bio[i])
}

cat("\n\n¡PROCESO FINALIZADO! Los 19 archivos están en:", out_indiv_path)
