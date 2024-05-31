
with({
vector_paths <- list.files("./data-raw", full.names = T)

for(x in 1:length(vector_paths)){

  the_path <- vector_paths[x]
  base_cargada <- openxlsx::read.xlsx(xlsxFile = the_path, sheet = 1)
  # Obtener el nombre del objeto sin la extensión
  nombre_en_R <- tools::file_path_sans_ext(basename(the_path))

  # Asignar el objeto a un nombre en el entorno global
  assign(nombre_en_R, base_cargada)

  # Utilizar usethis::use_data para registrar el objeto en el paquete
  nueva_sentencia <- paste0("usethis::use_data(", nombre_en_R, ", overwrite = TRUE)")
  eval(parse(text = nueva_sentencia))

  rm(base_cargada)
  rm(nombre_en_R)
}
})
