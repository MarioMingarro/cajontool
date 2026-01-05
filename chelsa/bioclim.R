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
wgs84_crs    <- "EPSG:4324"
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

    # Ajuste de escala CHELSA (0.1) y Kelvin a Celsius
    if (is_temp) {
      m_avg <- (m_avg * 0.1) - 273.15
    } else {
      m_avg <- m_avg * 0.1
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
n_cores <- max(1, parallel::detectCores() - 2)
bioclim_wgs84 <- biovars_terra(
  prec = pr_clim, tmin = tmin_clim, tmax = tmax_clim,
  cores = n_cores,
  wopt = list(datatype = "FLT4S")
)

# C. Proyectar resultado final a LAEA89 y guardar
cat("\n--- Proyectando a ETRS89 LAEA y guardando final ---\n")
bioclim_laea <- project(bioclim_wgs84, crs(iberia_laea), method = "bilinear")
writeRaster(bioclim_laea, output_final, overwrite = TRUE, wopt = list(datatype = "FLT4S"))

# D. Visualización rápida
names(bioclim_laea) <- paste0("BIO", 1:19)
plot(bioclim_laea[[c(1, 12)]], main = c("BIO1 (Temp Media)", "BIO12 (Prec Anual)"))

cat("\n¡PROCESO FINALIZADO CON ÉXITO!\nArchivo guardado en:", output_final)
