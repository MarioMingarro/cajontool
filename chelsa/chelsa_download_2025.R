
dest_dir <- "D:/A_DATA/CHELSA/MONTHLY_1980_2022/PR"
if (!dir.exists(dest_dir)) {
  dir.create(dest_dir, recursive = TRUE)
}


años <- 1980:2021
meses <- sprintf("%02d", 1:12)
variable <- "pr"
version <- "V.2.1"

for (anio in años) {
  for (mes in meses) {
    file_name <- paste0("CHELSA_", variable, "_", mes, "_", anio, "_", version, ".tif")
    url <- paste0("https://os.unil.cloud.switch.ch/chelsa02/chelsa/global/monthly/",
                  variable, "/", anio, "/", file_name)

    dest_file <- file.path(dest_dir, file_name)
    if (!file.exists(dest_file)) {
      message(paste("Descargando:", file_name))
      tryCatch({
        download.file(url, dest_file, mode = "wb", quiet = FALSE)
      }, error = function(e) {
        message(paste("Error al descargar:", url))
      })
    } else {
      message(paste("El archivo ya existe, saltando:", file_name))
    }
  }
}
