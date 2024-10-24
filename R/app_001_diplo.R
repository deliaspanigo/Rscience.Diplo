# Instalar y cargar los paquetes necesarios
#if (!require("shiny")) install.packages("shiny")
#if (!require("shinydashboard")) install.packages("shinydashboard")


# https://ubco-biology.github.io/BIOL202/pdf-latex-is-not-found.html
#install.packages("tinytex", dependencies = TRUE)
#library("tinytex")
#tinytex::install_tinytex()

app_001_diplo <- function(){

  library(shiny)
  library(shinydashboard)
  library(DT)
  library(openxlsx)
  library(shinyjs)
  library(markdown)
  library(rmarkdown)
  library(Rscience.Diplo)
  #ruta_css <- system.file("www", "estilos.css", package = "miPaquete")

#includeCSS(ruta_css)

  # Definir la interfaz de usuario
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Rscience"),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      "Diplo - 0.0.1",
      shiny::br(),
      shiny::br(),
      shinydashboard::menuItem(text = "Inicio", tabName = "tab_clase99", icon = shiny::icon("th")),
      br(),
      shinydashboard::menuItem(text = "Semana 01", tabName = "tab_clase01_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 02-P01", tabName = "tab_clase02_p01", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Semana 02-P02", tabName = "tab_clase02_p02", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 03", tabName = "tab_clase03_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 04 - Probabilidad", tabName = "tab_clase04_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 05***", tabName = "tab_clase05_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 06***", tabName = "tab_clase06_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 07***", tabName = "tab_clase07_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 08 - ANOVA 1 Factor", tabName = "tab_clase08_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 09 - Anova 1 Factor con Bloque", tabName = "tab_clase09_p01", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Semana 09 - Kruskal-Wallis", tabName = "tab_clase09_p02", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Semana 09 - Friedman", tabName = "tab_clase09_p03", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 10 - Correlación", tabName = "tab_clase10_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 11 - RLS", tabName = "tab_clase11_p01", icon = shiny::icon("th")),

      shinydashboard::menuItem(text = "Semana 12 - Chi Cuadrado", tabName = "tab_clase12_p01", icon = shiny::icon("th")),
      shinydashboard::menuItem(
        text = "Control",
        shinydashboard::menuSubItem(
          shiny::sliderInput("obs", "Número de observaciones:", min = 1, max = 1000, value = 500)
        )
      )
    )
  ),
  shinydashboard::dashboardBody(
    shinydashboard::tabItems(


      shinydashboard::tabItem(tabName = "tab_clase99",
                              module_diplo_001_clase99_ui("space_clase99")), # Final - tab_clase99


      shinydashboard::tabItem(tabName = "tab_clase01_p01",
                              module_diplo_001_clase01_p01_ui("space_clase01_p01")), # Final - tab_clase01

      shinydashboard::tabItem(tabName = "tab_clase02_p01",
                              module_diplo_001_clase02_p01_ui("space_clase02_p01")), # Final - tab_clase02_p01

      shinydashboard::tabItem(tabName = "tab_clase02_p02",
                              module_diplo_001_clase02_p02_ui("space_clase02_p02")), # Final - tab_clase02_p02

      shinydashboard::tabItem(tabName = "tab_clase03_p01",
                              module_diplo_001_clase03_p01_ui("space_clase03_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase04_p01",
                              module_diplo_001_clase04_p01_ui("space_clase04_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase05_p01",
                              module_diplo_001_clase05_p01_ui("space_clase05_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase06_p01",
                              module_diplo_001_clase06_p01_ui("space_clase06_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase07_p01",
                              module_diplo_001_clase07_p01_ui("space_clase07_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase08_p01",
                              module_diplo_001_clase08_p01_ui("space_clase08_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase09_p01",
                              module_diplo_001_clase09_p01_ui("space_clase09_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase09_p02",
                              module_diplo_001_clase09_p02_ui("space_clase09_p02")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase09_p03",
                              module_diplo_001_clase09_p03_ui("space_clase09_p03")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase10_p01",
                              module_diplo_001_clase10_p01_ui("space_clase10_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase11_p01",
                              module_diplo_001_clase11_p01_ui("space_clase11_p01")), # Final - tab_clase03_p01

      shinydashboard::tabItem(tabName = "tab_clase12_p01",
                              module_diplo_001_clase12_p01_ui("space_clase12_p01")) # Final - tab_clase03_p01
    )
    )
    )


# addResourcePath("tmpuser", getwd())

# Definir la lógica del servidor
server <- function(input, output) {


  module_diplo_001_clase99_server("space_clase99")

  # Server - Clase 01
  module_diplo_001_clase01_p01_serverA("space_clase01_p01")
  module_diplo_001_clase01_p01_serverB("space_clase01_p01")

  # Server - Clase 02_p01
  module_diplo_001_clase02_p01_serverA("space_clase02_p01")
  module_diplo_001_clase02_p01_serverB("space_clase02_p01")


  # Server - Clase 02_p02
  module_diplo_001_clase02_p02_serverA("space_clase02_p02")
  module_diplo_001_clase02_p02_serverB("space_clase02_p02")


  # Server - Clase 03_p01
  module_diplo_001_clase03_p01_serverA("space_clase03_p01")
  module_diplo_001_clase03_p01_serverB("space_clase03_p01")

  # Server - Clase 04_p01
  module_diplo_001_clase04_p01_serverA("space_clase04_p01")
  module_diplo_001_clase04_p01_serverB("space_clase04_p01")

  # Server - Clase 05_p01
  module_diplo_001_clase05_p01_serverA("space_clase05_p01")
  module_diplo_001_clase05_p01_serverB("space_clase05_p01")

  # Server - Clase 06_p01
  module_diplo_001_clase06_p01_serverA("space_clase06_p01")
  module_diplo_001_clase06_p01_serverB("space_clase06_p01")

  # Server - Clase 07_p01
  module_diplo_001_clase07_p01_serverA("space_clase07_p01")
  module_diplo_001_clase07_p01_serverB("space_clase07_p01")

  # Server - Clase 08_p01
  module_diplo_001_clase08_p01_serverA("space_clase08_p01")
  module_diplo_001_clase08_p01_serverB("space_clase08_p01")

  # Server - Clase 09_p01
  module_diplo_001_clase09_p01_serverA("space_clase09_p01")
  module_diplo_001_clase09_p01_serverB("space_clase09_p01")

  # Server - Clase 09_p02
  module_diplo_001_clase09_p02_serverA("space_clase09_p02")
  module_diplo_001_clase09_p02_serverB("space_clase09_p02")

  # Server - Clase 09_p03
  module_diplo_001_clase09_p03_serverA("space_clase09_p03")
  module_diplo_001_clase09_p03_serverB("space_clase09_p03")

  # Server - Clase 10_p01
  module_diplo_001_clase10_p01_serverA("space_clase10_p01")
  module_diplo_001_clase10_p01_serverB("space_clase10_p01")

  # Server - Clase 11_p01
  module_diplo_001_clase11_p01_serverA("space_clase11_p01")
  module_diplo_001_clase11_p01_serverB("space_clase11_p01")

  # Server - Clase 12_p01
  module_diplo_001_clase12_p01_serverA("space_clase12_p01")
  module_diplo_001_clase12_p01_serverB("space_clase12_p01")
  #module_diplo_001_clase01_serverC("space_clase01")
} #--- Fin server

# Ejecutar la aplicación
shiny::shinyApp(ui = ui, server = server, options = base::list(launch.browser = TRUE))


}



