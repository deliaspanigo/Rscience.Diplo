# Instalar y cargar los paquetes necesarios
#if (!require("shiny")) install.packages("shiny")
#if (!require("shinydashboard")) install.packages("shinydashboard")


app_001_diplo <- function(){

  library(shiny)
  library(shinydashboard)
  library(DT)
  library(openxlsx)
  library(shinyjs)
  library(rmarkdown)

  # Definir la interfaz de usuario
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Mi primer dashboard"),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      "Diplo - 0.0.1",
      shiny::br(),
      shiny::br(),
      shinydashboard::menuItem(text = "Clase 99", tabName = "tab_clase99", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 01", tabName = "tab_clase01", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 02", tabName = "tab_clase02", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 03", tabName = "tab_clase03", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 04", tabName = "tab_clase04", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 05", tabName = "tab_clase05", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 06", tabName = "tab_clase06", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 07", tabName = "tab_clase07", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 08", tabName = "tab_clase08", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 09", tabName = "tab_clase09", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 10", tabName = "tab_clase10", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 11", tabName = "tab_clase11", icon = shiny::icon("th")),
      shinydashboard::menuItem(text = "Clase 12", tabName = "tab_clase12", icon = shiny::icon("th")),
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


      shinydashboard::tabItem(tabName = "tab_clase01",
                              module_diplo_001_clase01_ui("space_clase01")) # Final - tab_clase01
    )
    )
    )


# addResourcePath("tmpuser", getwd())

# Definir la lógica del servidor
server <- function(input, output) {


  module_diplo_001_clase99_server("space_clase99")

  module_diplo_001_clase01_server("space_clase01")
} #--- Fin server

# Ejecutar la aplicación
shiny::shinyApp(ui = ui, server = server, options = base::list(launch.browser = TRUE))


}



