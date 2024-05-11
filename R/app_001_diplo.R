# Instalar y cargar los paquetes necesarios
#if (!require("shiny")) install.packages("shiny")
#if (!require("shinydashboard")) install.packages("shinydashboard")


app_001_diplo <- function(){

  library(shiny)
  library(shinydashboard)
  library(DT)

# Definir la interfaz de usuario
ui <- shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = "Mi primer dashboard"),

  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      "Diplo - 0.0.1",
      shiny::br(),
      shiny::br(),
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
      shinydashboard::tabItem(tabName = "tab_clase01",
          shiny::h1("Clase 01"),
          shiny::fluidRow(
            shiny::column(12,
            shinydashboard::box(
              title = "Seleccionar base de datos",
              status = "primary",
              id = "my_box01",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,# Colapsado por defecto
              width = 12,

              shiny::selectInput(inputId = "data_source",
                          label = "Fuente de datos",
                          choices = c("R examples" = "r_examples",
                                      "CSV files"  = "csv_file",
                                      "Diplo UNC"  = "diplo_file")),
              shiny::br(),
              shiny::br(),

              shiny::conditionalPanel(condition = "input.data_source == 'r_examples'",
                shiny::selectInput(inputId = "r_database",
                                     label = "Bases de R",
                                   choices = c("01 - mtcars" = "mtcars",
                                               "02 - iris" =  "iris"))),


              shiny::br(),
              shiny::br(),

          DT::DTOutput("table_database"),
          shiny::br(),
          shiny::br()

    ),
    shiny::br(),
    shiny::br(),
    shiny::fluidRow(
      shiny::column(12,
                    shinydashboard::box(
                      title = "Estadísticas",
                      status = "primary",
                      id = "my_box02",
                      solidHeader = TRUE,
                      collapsible = TRUE,
                      collapsed = FALSE,
                      closable = FALSE,# Colapsado por defecto
                      width = 12,
                      shiny::uiOutput("var_selector"),
                      shiny::br(),
                      shiny::br(),
                      shiny::actionButton("render", "Renderizar R Markdown"),
                      shiny::uiOutput("rmd_output")#,
                     # shiny::plotOutput("distPlot")

                    )
  )
)
)
)
)
)
)
)


# addResourcePath("tmpuser", getwd())

# Definir la lógica del servidor
server <- function(input, output) {

  # Inicializacion de objetos
  database <- shiny::reactiveVal()
  vector_var_names <- shiny::reactiveVal()

  # Fuente de datos y creacion de database
  shiny::observeEvent(input$"data_source", {

      if(input$data_source == "r_examples"){

        # database
        shiny::observeEvent(input$"r_database", {
        database(base::eval(base::parse(text = input$"r_database")))
        })




      }
  })


  shiny::observeEvent(database(),{
    vector_var_names(base::colnames(database()))
  })


  # vector var names


  output$table_database <- DT::renderDT({

  req(database())



    # Vector con nombres de elementos a ver
    selected_objs <- c("df_selected_vars")


    # Usar lapply para mostrar los elementos deseados

    mi_tabla <- database()
    #https://rstudio.github.io/DT/functions.html
    vector_pos <- 1:nrow(mi_tabla)
    vector_color <- rep(NA, length(vector_pos))
    vector_color[c(T, F)] <- "lightblue"#'red'#
    vector_color[c(F, T)] <- "lightgreen"#'blue'#
    vector_color <- vector_color[vector_pos]

    datatable(
      mi_tabla,
      rownames = FALSE,
      options = list(

        headerCallback = DT::JS(
          "function(thead) {",
          "  $(thead).css('font-size', '2em');",
          "}"
        ),
        columnDefs = list(list(className = 'dt-center', targets = "_all")),
        #pageLength = 5,
        #dom = "t",
        scrollX = TRUE,
        searching = FALSE,
        scrollCollapse = TRUE,  # Permitir colapsar el scroll
        fixedColumns = list(leftColumns = 3),  # Fijar las primeras 3 columnas
        #lengthMenu = list(c(-1), c("All")), # Todas las filas
        style = list(
          'font-size' = '20px'  # Tamaño de letra para el nombre de las columnas
        )
      )

    ) %>%formatStyle(
      colnames(mi_tabla),
      backgroundColor = styleRow(vector_pos, vector_color),#,
      target = 'row',
      fontSize = "26px"
    )
  })




  shiny::observeEvent(database(),{

    output$var_selector <- shiny::renderUI({

      req(database(), vector_var_names())
      vector_opciones <- c("Selecciona una..." = "", vector_var_names())

      shiny::selectInput(inputId = "selected_var", label = "Selecciona una variable",
                  choices = vector_opciones,
                  selected = vector_opciones[2])

    })
  })


  shiny::observeEvent(input$render, {
    # Renderizar el archivo .Rmd a HTML
    #rmarkdown::render("report.Rmd", output_file = "report.html")
    #aver <- system.file("vignettes", "report.Rmd")
    #rmarkdown::render("report.Rmd", output_file = "report.html")
    #htmltools::includeMarkdown(base::system.file("vignettes", "report.Rmd", package = "Rscience.Diplo"))
    # Incluir el HTML en la interfaz de usuario
    output$rmd_output <- shiny::renderUI({


      rmarkdown::render(base::system.file("extdata", "report.Rmd", package = "Rscience.Diplo"), output_format = "html_document")

      html_file_path <- base::system.file("extdata", "report.html", package = "Rscience.Diplo")

      html_content <- base::readLines(html_file_path)

      # Include the HTML content in the Shiny app
      htmltools::HTML(base::paste(html_content, collapse = "\n"))

      #htmltools::includeHTML(html_file_path)

      #htmltools::includeHTML("report.html")
     # tags$iframe(src="tmpuser/report.html", height = "100%", width = "100%")
    #  tags$iframe(src="tmpuser/report.html", height = 600, width = 600)
      #tags$iframe(htmltools::includeHTML("report.html"))
    })
  })


  # output$distPlot <- shiny::renderPlot({
  #   req(database(), input$selected_var)
  #   hist(database()[,input$selected_var])
  #   #hist(rnorm(input$obs))
  # })
}

# Ejecutar la aplicación
shiny::shinyApp(ui = ui, server = server, options = base::list(launch.browser = TRUE))


}



