if (!require("fs")) install.packages("fs")
if (!require("dplyr")) install.packages("dplyr")
if (!require("tidyr")) install.packages("tidyr")


ruta_raiz <- "C:/A_TRABAJO/ALEJANDRA/10km"
archivos <- fs::dir_info(ruta_raiz, recurse = TRUE)

tabla_proyecto <- archivos %>%
  dplyr::select(path, type, size) %>%
  dplyr::mutate(
    rel_path = fs::path_rel(path, start = ruta_raiz),
    nivel = stringr::str_count(rel_path, "/")
  ) %>%
  tidyr::separate(rel_path, into = paste0("Nivel_", 1:5), sep = "/",
                  remove = FALSE, fill = "right")



writexl::write_xlsx(tabla_proyecto, "C:/A_TRABAJO/ALEJANDRA/Estructura_Proyecto_10km.xlsx")
fs::dir_tree(ruta_raiz, recurse = 5)
