
# https://bookdown.org/yihui/rmarkdown-cookbook/chunk-styling.html

library("devtools")

#openxlsx::read.xlsx()

devtools::load_all()

devtools::document()

devtools::check()

devtools::install()

###############################################

# Desinstalar Rscience|
remove.packages("Rscience.Diplo")

# Instalar el paquete para instalar
install.packages("remotes")

# Activar la libreria para instalar
library("remotes")

# Instalar Rscience desde la librería desde GitHub
remotes::install_github("deliaspanigo/Rscience.Diplo", force = T)

