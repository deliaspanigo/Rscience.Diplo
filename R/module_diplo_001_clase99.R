


# # # 01) UI - Selection for 'database'
module_diplo_001_clase99_ui <- function(id){
  ns <- shiny::NS(id)



  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      shiny::h1("Clase 99 - SPECIAL!"),
      shiny::fluidRow(
        shiny::column(12,
                      shinydashboard::box(
                        title = "Seleccionar base de datos",
                        status = "primary",
                        id = ns("my_box01"),
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        collapsed = TRUE, #FALSE,
                        closable = FALSE,# Colapsado por defecto
                        width = 12,

                        fluidRow(column(3,
                                        shiny::selectInput(inputId = ns("data_source"),
                                                           label = "Fuente de datos",
                                                           choices = c("R examples" = "r_source",
                                                                       "CSV files"  = "csv_source",
                                                                       "Diplo UNC"  = "diplo_source"))
                        )),
                        shiny::br(),
                        shiny::br(),

                        shiny::conditionalPanel(condition = "input.data_source == 'r_source'",ns = ns,
                                                fluidRow(column(3,
                                                                shiny::selectInput(inputId = ns("r_database"),
                                                                                   label = "Bases de R",
                                                                                   choices = c("01 - mtcars" = "mtcars",
                                                                                               "02 - iris" =  "iris")))
                                                )),



                        shiny::conditionalPanel(condition = "input.data_source == 'csv_source'", ns = ns,
                                                fluidRow(
                                                  column(4, fileInput(inputId = ns("csv_file_path"), "Elija un archivo CSV",
                                                                      accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
                                                  )),
                                                fluidRow(
                                                  column(1, radioButtons(inputId = ns("header"),
                                                                         label = "header",
                                                                         choices = c("TRUE" = TRUE, "FALSE" = FALSE), selected = TRUE)),
                                                  column(2, radioButtons(inputId = ns("sep"),
                                                                         label = "Separador de columnas",
                                                                         choices = c("semicolon (;)" = ";", "comma (,)" = ","),
                                                                         selected = ";")),
                                                  column(2, radioButtons(inputId = ns("dec"),
                                                                         label = "Decimal",
                                                                         choices = c("Period (.)" = ".", "Comma (,)" = ","),
                                                                         selected = ".")),
                                                  column(2, radioButtons(inputId = ns("quote"),
                                                                         label = "Comillas",
                                                                         choices = c("Double quotes (\")" = "\"",
                                                                                     "Simple quotes (')" = "'"),
                                                                         selected = "\""))
                                                ),
                                                tags$hr()
                        ),

                        shiny::br(),
                        shiny::br(),
                        DT::dataTableOutput(ns("table_database2")),
                        DT::DTOutput(ns("table_database")),
                        shiny::br(),
                        shiny::br()

                      ),
                      shiny::br(),
                      shiny::br(),
                      shiny::fluidRow(
                        shiny::column(12,
                                      shinydashboard::box(
                                        title = "Estadísticas y Código de R",
                                        status = "primary",
                                        id = ns("my_box02"),
                                        solidHeader = TRUE,
                                        collapsible = TRUE,
                                        collapsed = FALSE,
                                        closable = FALSE,# Colapsado por defecto
                                        width = 12,
                                        fluidRow(column(3, shiny::uiOutput(ns("var_selector")))),
                                        shiny::br(),
                                        shiny::br(),
                                        ##############################
                                        h2("Generacion de Reportes"), br(),
                                        actionButton(ns("render_report_button"), "Render Report", width = "100%"),
                                        downloadButton(outputId = ns('download_button_pdf'),
                                                       label = "Download PDF",
                                                       width = "100%", disabled = TRUE),
                                        downloadButton(outputId = ns('download_button_html'),
                                                       label = "Download HTML",
                                                       width = "100%", disabled = TRUE),
                                        downloadButton(outputId = ns('download_button_word'),
                                                       label = "Download WORD",
                                                       width = "100%", disabled = TRUE),
                                        downloadButton(outputId = ns('download_button_zip'),
                                                       label = "Download All (ZIP)",
                                                       width = "100%", disabled = TRUE)

                                      )
                        )
                      ), br(), br(), br(),
                      fluidRow(
                        column(3, textInput(ns("pdfurl"), "PDF URL", value = "https://cran.r-project.org/doc/contrib/rdebuts_es.pdf"))
                      ),
                      uiOutput(ns("magia"))
        )
      )

  ) # End div
}












module_diplo_001_clase99_server <- function(id){

  moduleServer(
    id,
    function(input, output, session) {

      # ns para el server!
      ns <- session$ns

      # Inicializacion de objetos
      database <- shiny::reactiveVal()
      vector_var_names  <- shiny::reactiveVal()



      # Fuente de datos y creacion de database
      shiny::observeEvent(input$"data_source", {

        database(NULL)

        if(input$data_source == "r_source"){

          # database
          shiny::observeEvent(input$"r_database", {
            database(NULL)
            database(base::eval(base::parse(text = input$"r_database")))
          })




        } else

          if(input$data_source == "csv_source"){


            # database
            shiny::observeEvent(input$"csv_file_path", {
              req(input$csv_file_path)

              database(NULL)

              database(read.csv(file = input$csv_file_path$datapath,
                                header = as.logical(as.character(input$header)),
                                sep = input$sep,
                                dec = input$dec,
                                stringsAsFactors = FALSE )
              )


            })




          }



      })


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


      output$table_database2 <- renderDataTable({

        req(database())

        database()


      })


      # vector var names - selector
      shiny::observeEvent(database(),{
        req(database())

        vector_var_names(base::colnames(database()))



        output$var_selector <- shiny::renderUI({

          ns <- shiny::NS(id)

          req(database(), vector_var_names())

          vector_pos <- 1:ncol(database())
          vector_letters <- openxlsx::int2col(vector_pos)
          vector_colnames <- colnames(database())


          # Determinar la cantidad máxima de dígitos
          max_digits <- max(nchar(vector_pos))
          max_digits <- max(max_digits, 2)
          vector_order <- sprintf(paste0("%0", max_digits, "d"), vector_pos)

          # Para el usuario
          vector_names <- paste0(vector_order, " - ", vector_letters, " - ", vector_colnames)

          # Vector de opcion interno (nombre de columnas)
          vector_options <- vector_colnames
          names(vector_options) <- vector_names
          vector_options <- c("Selecciona una..." = "", vector_options)

          shiny::selectInput(inputId = ns("selected_var_name"), label = "Selecciona una variable",
                             choices = vector_options,
                             selected = vector_options[2])

        })






      })


      selected_var_name <- reactive({
        req(database(), input$selected_var_name)
        input$selected_var_name

      })

      selected_var_pos <- reactive({
        req(database(), input$selected_var_name, selected_var_name())

        vector_var_names <- colnames(database())
        dt_pos <- vector_var_names == selected_var_name()

        vector_pos <- 1:length(vector_var_names)

        value_pos <- vector_pos[dt_pos]
        value_pos

      })





      control_general <- reactive({

        req(database(), selected_var_name(), selected_var_pos())

        validate(
          need(!is.null(database()), 'Error 001: Problemas en la base de datos. Vuelva a cargar el archivo'),
          need(!is.null(selected_var_name()), 'Error 002: Problemas con la variable seleccionada. Vuelva a cargar el archivo.'),
          need(!is.null(selected_var_pos()), 'Error 002: Problemas con la variable seleccionada. Vuelva a cargar el archivo.')

        )

        validate(
          need((ncol(database())>= 1), 'Error 003: La base de datos debe contener al menos una columna.'),
          need((nrow(database())>= 2), 'Error 004: La base de datos debe contener al menos una fila.')
        )

        validate(
          need(sum(colnames(database()) == selected_var_name()) == 1, 'Error 005: El nombre de variable seleccionado no pertenece a la base.')
        )

        validate(
          need(ncol(database()) > selected_var_pos(), 'Error 005: La posición de variable no pertenece a la base de datos.')
        )


        vector_vr <- database()[,selected_var_pos()]

        validate(
          need((sum(is.na(vector_vr))==0), 'Error 005: La columna seleccionada posee al menos una celda sin datos. \n
              En la diplomatura solo veremos código aplicable a bases de datos sin celdas vacías.')
        )

        validate(
          need((is.numeric(vector_vr)), 'Error 006: La variable seleccionada debe ser numérica. Verifique las siguientes opciones:\n
               1) Se equivocó al seleccionar variable.\n
               2) No abrió la base de datos csv como archivo de texto para tomar noción sobre \n
               si el archivo tiene como primera fila al nombre de columnas, cual es el separador de columna \n
               y del separador decimal.\n
               3) Mal indicado el argumento header en el menú de carga del archivo csv.\n
               4) Mal indicado el separador de columna en el menú de carga del archivo csv.\n
               5) Mal indicado el separador decimal en el menú de carga del archivo csv.\n'
          )
        )


        return(TRUE)

      })
      ################################################################################

      the_time <- reactiveVal()
      report_loc <- reactiveVal()
      my_output_temp_folder <- reactiveVal()
      report_output_path_pdf <- reactiveVal()
      report_output_path_html <- reactiveVal()
      report_output_path_word <- reactiveVal()
      report_output_path_zip <- reactiveVal()


      observeEvent(input$render_report_button, {
        execution_time <- format(Sys.time(), "%Y_%m_%d_%H_%M_%S")
        input_file_rmd <- 'report_clase01_master.Rmd'
        input_path_rmd <- normalizePath(input_file_rmd)

        p1_name <- tools::file_path_sans_ext(input_file_rmd)
        p2_name <- tools::file_ext(input_file_rmd)

        output_file_rmd  <- paste0(p1_name, "_", execution_time, ".", p2_name)
        output_file_pdf  <- paste0(tools::file_path_sans_ext(output_file_rmd), ".pdf")
        output_file_html <- paste0(tools::file_path_sans_ext(output_file_rmd), ".html")
        output_file_word <- paste0(tools::file_path_sans_ext(output_file_rmd), ".docx")
        output_file_zip  <- paste0(tools::file_path_sans_ext(output_file_rmd), ".zip")

        #new_temp_folder <- tempdir()
        new_temp_folder <- normalizePath("super_folder")
        output_path_rmd  <- file.path(new_temp_folder, output_file_rmd)
        output_path_pdf  <- file.path(new_temp_folder, output_file_pdf)
        output_path_html <- file.path(new_temp_folder, output_file_html)
        output_path_word <- file.path(new_temp_folder, output_file_word)
        output_path_zip  <- file.path(new_temp_folder, output_file_zip)

        file.copy(input_path_rmd, output_path_rmd, overwrite = TRUE)

        library(rmarkdown)
        render(output_path_rmd, pdf_document(),  output_file = output_path_pdf)
        render(output_path_rmd, html_document(), output_file = output_path_html)
        render(output_path_rmd, word_document(), output_file = output_path_word)

        the_time(execution_time)
        my_output_temp_folder(new_temp_folder)
        report_output_path_pdf(output_path_pdf)
        report_output_path_html(output_path_html)
        report_output_path_word(output_path_word)
        report_output_path_zip(output_path_zip)

        report_loc(list.files(new_temp_folder, full.names = TRUE))
      })

      control_user_pdf <- reactive({
        validate(
          need(!is.null(input$render_report_button), ""),
          need(input$render_report_button != 0, "Render PDF not executed.")
        )

        aver <- input$render_report_button > 0 && !is.null(report_output_path_pdf())
        validate(
          need(aver, "Loading..."),
          need(file.exists(report_output_path_pdf()), "Loading...")
        )
        return(TRUE)
      })

      observe({

        ns <- session$ns

        if (!is.null(report_output_path_pdf())) {
          shinyjs::enable("download_button_pdf")
          shinyjs::enable("download_button_html")
          shinyjs::enable("download_button_word")
          shinyjs::enable("download_button_zip")
        } else {
          shinyjs::disable("download_button_pdf")
          shinyjs::disable("download_button_html")
          shinyjs::disable("download_button_word")
          shinyjs::disable("download_button_zip")
        }
      })

      output$pdfviewer_online <- renderText({
        return(paste('<iframe style="height:600px; width:100%" src="', input$pdfurl, '"></iframe>', sep = ""))
      })

      output$pdfviewer_local <- renderText({
        my_path <- file.path(getwd(), "resources", "pdf")
        addResourcePath(prefix = "my_pdf_resource", directoryPath = my_path)

        my_local_file <- "my_pdf_resource/fileAAA.pdf"
        return(paste('<iframe style="height:600px; width:100%" src="', my_local_file, '"></iframe>', sep = ""))
      })

      output$pdfviewer_temporal <- renderText({
        req(control_user_pdf())

        my_path <- my_output_temp_folder()
        addResourcePath(prefix = "my_output_temp_folder", directoryPath = my_path)

        my_file <- basename(report_output_path_pdf())
        my_local_file <- file.path("my_output_temp_folder", my_file)

        return(paste('<iframe style="height:600px; width:100%" src="', my_local_file, '"></iframe>', sep = ""))
      })

      output$htmlviewer_temporal <- renderText({
        req(control_user_pdf())

        my_path <- my_output_temp_folder()
        addResourcePath(prefix = "my_output_temp_folder", directoryPath = my_path)

        my_file <- basename(report_output_path_html())
        my_local_file <- file.path("my_output_temp_folder", my_file)

        armado_v <- paste('<div style="height: 1000vh; width: 200vh; overflow: hidden;"><iframe style="height: 1000vh; width:200vh; border: none;" src="', my_local_file, '"></iframe></div>', sep = "")
        print(armado_v)
        return(armado_v)
      })

      output$magia <- renderUI({

        ns <- NS(id)

        div(
          fluidRow(
            column(4, h2("Web pdf file"), htmlOutput(ns('pdfviewer_online'))),
            column(4, h2("Local file with addResourcePath()"), htmlOutput(ns('pdfviewer_local'))),
            column(4, h2("Temporal()"), shinycssloaders::withSpinner(htmlOutput(ns('pdfviewer_temporal'))))
          ), br(), br(),
          fluidRow(
            column(12, h2("EL HTML"), shinycssloaders::withSpinner(htmlOutput(ns("htmlviewer_temporal"))))
          )
        )
      })

      output$download_button_pdf <- downloadHandler(
        filename = function() {
          basename(report_output_path_pdf())
        },
        content = function(file) {
          file.copy(report_output_path_pdf(), file, overwrite = TRUE)
        }
      )

      output$download_button_html <- downloadHandler(
        filename = function() {
          basename(report_output_path_html())
        },
        content = function(file) {
          file.copy(report_output_path_html(), file, overwrite = TRUE)
        }
      )

      output$download_button_word <- downloadHandler(
        filename = function() {
          basename(report_output_path_word())
        },
        content = function(file) {
          file.copy(report_output_path_word(), file, overwrite = TRUE)
        }
      )

      output$download_button_zip <- downloadHandler(
        filename = function() {
          basename(report_output_path_zip())
        },
        content = function(file) {
          files <- c(report_output_path_pdf(), report_output_path_html(), report_output_path_word())
          zip(file, files)
        }
      )


    }) # Fin Module
}



