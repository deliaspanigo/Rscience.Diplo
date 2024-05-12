


# # # 01) UI - Selection for 'database'
module_diplo_001_clase01_ui <- function(id){
  ns <- shiny::NS(id)



  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      shiny::h1("Clase 01"),
      shiny::fluidRow(
        shiny::column(12,
                      shinydashboard::box(
                        title = "Seleccionar base de datos",
                        status = "primary",
                        id = ns("my_box01"),
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = FALSE,
                        closable = FALSE,# Colapsado por defecto
                        width = 12,

                        shiny::selectInput(inputId = ns("data_source"),
                                           label = "Fuente de datos",
                                           choices = c("R examples" = "r_examples",
                                                       "CSV files"  = "csv_file",
                                                       "Diplo UNC"  = "diplo_file")),
                        shiny::br(),
                        shiny::br(),

                        shiny::conditionalPanel(condition = "input.data_source == 'r_examples'",ns = ns,
                                                shiny::selectInput(inputId = ns("r_database"),
                                                                   label = "Bases de R",
                                                                   choices = c("01 - mtcars" = "mtcars",
                                                                               "02 - iris" =  "iris"))),


                        shiny::br(),
                        shiny::br(),

                        DT::DTOutput(ns("table_database")),
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
                                        id = ns("my_box02"),
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = FALSE,
                                        closable = FALSE,# Colapsado por defecto
                                        width = 12,
                                        shiny::uiOutput(ns("var_selector")),
                                        shiny::br(),
                                        shiny::br(),
                                        shiny::actionButton(ns("render"), "Renderizar R Markdown"),
                                        shiny::uiOutput(ns("rmd_output"))#,
                                        # shiny::plotOutput("distPlot")

                                      )
                        )
                      )
        )
      )

  ) # End div
}










#
#
#
# module_cpiC001_s01_varselection_ui <- function(id){
#
#   ns <- shiny::NS(id)
#
#   div(
#     uiOutput(ns("vars_selection"))
#   )
# }
#



module_diplo_001_clase01_server <- function(id){



  moduleServer(
    id,
    function(input, output, session) {

      ENV_SET_PACK <- TRUE

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

          ns <- shiny::NS(id)

          req(database(), vector_var_names())

          vector_opciones <- 1:ncol(database())
          names(vector_opciones) <- colnames(database())
          vector_opciones <- c("Selecciona una..." = "", vector_opciones)

          shiny::selectInput(inputId = ns("selected_pos_var"), label = "Selecciona una variable",
                             choices = vector_opciones,
                             selected = vector_opciones[2])

        })
      })


      selected_var_pos <- reactive({
        req(input$selected_pos_var)
        as.numeric(as.character(input$selected_pos_var))
      })


      shiny::observeEvent(input$render, {
        # Renderizar el archivo .Rmd a HTML
        #rmarkdown::render("report.Rmd", output_file = "report.html")
        #aver <- system.file("vignettes", "report.Rmd")
        #rmarkdown::render("report.Rmd", output_file = "report.html")
        #htmltools::includeMarkdown(base::system.file("vignettes", "report.Rmd", package = "Rscience.Diplo"))
        # Incluir el HTML en la interfaz de usuario
        output$rmd_output <- shiny::renderUI({

          print(ENV_SET_PACK)

          if(ENV_SET_PACK){
            rmd_file_path_master <- base::system.file("extdata", "report_clase01_master.Rmd", package = "Rscience.Diplo")
            rmd_file_path_mod    <- base::system.file("extdata", "report_clase01_mod.Rmd",    package = "Rscience.Diplo")
            html_file_mod        <- base::system.file("extdata", "report_clase01_mod.html",   package = "Rscience.Diplo")
          } else

          if(!ENV_SET_PACK){
            rmd_file_path_master <- file.path(getwd(), "inst", "extdata", "report_clase01_master.Rmd")
            rmd_file_path_mod    <- file.path(getwd(), "inst", "extdata", "report_clase01_mod.Rmd")
            html_file_path_mod   <- file.path(getwd(), "inst", "extdata", "report_clase01_mod.html")
          }


          # Leer el archivo y modificar
          lines_rmd_mod <- readLines(con = rmd_file_path_master)
          lines_rmd_mod <- gsub(pattern = "_database_", replacement = "database()", x = lines_rmd_mod)
          lines_rmd_mod <- gsub(pattern = "_seleceted_pos_var_", replacement = selected_var_pos(), x = lines_rmd_mod)

          # Guardar los cambios
          writeLines(text = lines_rmd_mod, con = rmd_file_path_mod)

          # Render .Rmd
          rmarkdown::render(rmd_file_path_mod, output_format = "html_document")

          # HTML
          html_content_mod <- base::readLines(html_file_path_mod)
          htmltools::HTML(base::paste(html_content_mod, collapse = "\n"))


          #htmltools::includeHTML(html_file_path)

          #htmltools::includeHTML("report.html")
          # tags$iframe(src="tmpuser/report.html", height = "100%", width = "100%")
          #  tags$iframe(src="tmpuser/report.html", height = 600, width = 600)
          #tags$iframe(htmltools::includeHTML("report.html"))
        })
      })


      #return(output_list)
    })
}



