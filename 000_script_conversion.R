
# Funcion para cargar
cargar_base_package <- function(input_folder, selected_file, selected_sep, selected_dec ) {

  # Construir la ruta completa al archivo CSV
  input_path <- file.path(input_folder, selected_file)

  # Leer el archivo CSV desde la ruta completa
  base_cargada <- read.csv(file = input_path, header = TRUE, sep = selected_sep, dec = selected_dec)

  # Obtener el nombre del objeto sin la extensión
  nombre_en_R <- tools::file_path_sans_ext(selected_file)

  # Asignar el objeto a un nombre en el entorno global
  assign(nombre_en_R, base_cargada)

  # Utilizar usethis::use_data para registrar el objeto en el paquete
  nueva_sentencia <- paste0("usethis::use_data(", nombre_en_R, ", overwrite = TRUE)")
  eval(parse(text = nueva_sentencia))

  # Limpiar objetos innecesarios
  rm(base_cargada)
  rm(nombre_en_R)
}

# # # Cargamos la SEMANA01_BASE01_PESOS.csv
with(
  list(
    input_folder = "./data-raw",
    selected_file = "SEMANA01_BASE01_PESOS.csv",
    selected_sep = ";",
    selected_dec = ","
  ),
  cargar_base_package(input_folder, selected_file, selected_sep, selected_dec)
)
#####################################################################################



# # # Cargamos la SEMANA01_BASE02_PESOS.csv
with(
 list(
    input_folder = "./data-raw",
    selected_file = "SEMANA01_BASE02_ALTURA.csv",
    selected_sep = ";",
    selected_dec = "."
  ),
  cargar_base_package(input_folder, selected_file, selected_sep, selected_dec)
)
#####################################################################################


rm(cargar_base_package)
