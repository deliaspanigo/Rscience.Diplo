


# # # 01) UI - Selection for 'database'
module_diplo_000_select_database_Rexample_ui <- function(id){
  ns <- shiny::NS(id)

  vector_opt <- c("Select one..." = "",
                  "01 - mtcars" = "mtcars",
                  "02 - iris" = "iris")


  div(shinyjs::useShinyjs(), id = ns("input-panel"),

      # h2("Initial user election - database"),
      fluidRow(
        column(6,

               # # # R examples selector
               div(shinyjs::useShinyjs(), id = ns("input-example"),
                   selectInput(inputId = ns("file_example"),
                               label = "R Examples",
                               choices = vector_opt)

               ) # Div
        ),
        # # # Action buttons
        column(4, br(), br(), uiOutput(ns("action_buttons")))

      ), # End fluidRow
      br(), br(), br(),
      textOutput(ns("calling_help"))#,
      # # # Visualization for database
      #uiOutput(ns("show_all_database"))

  ) # End div
}













module_cpiC001_s01_varselection_ui <- function(id){

  ns <- shiny::NS(id)

  div(
    uiOutput(ns("vars_selection"))
  )
}




module_cpiC001_s01_varselection_server <- function(id, input_general){
  moduleServer(
    id,
    function(input, output, session) {

      # # # Very importan objects from input_general
      # # Vector var names from database
      vector_var_names_database <- reactive({
        req(input_general())

        input_general()$vector_var_names_database
      })


      # # Info about source database
      intro_source_database <- reactive({
        req(input_general())

        input_general()$intro_source_database
      })


      # # # Control user 01
      control_user_01 <- reactive({


        validate(
          need(!is.null(input_general), "Error 01: Module t test s01 - input_general can not be NULL."),
          need(!is.null(vector_var_names_database()), "Error 10: Module t test s01 - vector_var_names_database() can not be NULL."),
          need(!is.null(intro_source_database()), "Error 11: Module t test s01 - intro_source_database() can not be NULL.")
        )


        return(TRUE)
      })



      # # # Initial values
      action_button_load <- reactiveVal(FALSE)
      action_button_show <- reactiveVal(FALSE)

      # # Colours for actions buttons
      hardcorded_initial_color <- "orange" #"#F4A020"
      color_button_load <- reactiveVal(hardcorded_initial_color)
      color_button_show <- reactiveVal(hardcorded_initial_color)



      # # # Action buttons
      output$action_buttons <- renderUI({

        ns <- shiny::NS(id)


        # # # Style for load button
        standard_style_button_load <- "color: white; background-color: _color_;width: 150px; height: 100px; font-size: 30px;"
        output_style_button_load <- gsub(pattern = "_color_",
                                         replacement = color_button_load(),
                                         x = standard_style_button_load)

        # # # Style for reset button
        output_style_button_reset <- "color: white; background-color: orange;width: 150px; height: 100px; font-size: 30px;"

        # # # UI content
        div(
          fluidRow(
            actionButton(ns("action_load"), label = "RUN", style = output_style_button_load),
            actionButton(ns("action_reset_all"), "RESET ALL", style = output_style_button_reset)
          )
        )
      })



      # # # Intro source database
      output$intro_source_database <- renderTable({



        df_output <- as.data.frame(intro_source_database())
        colnames(df_output) <- names(intro_source_database())
        df_output



      })



      # # # Var selection for t Test - 2 independent samplesy
      output$vars_selection <- renderUI({

        ns <- shiny::NS(id)



        set_options <- setup_var_info(all_var_names = vector_var_names_database())
        set_options <- c("Var selection..." = "", set_options)




        div(shinyjs::useShinyjs(), id = ns("input-var-selection"),
            # fluidRow(
            #   column(12, h1("t Test  for 2 independent samples"))
            # ),
            # fluidRow(
            #   column(6,
            #          tableOutput(ns("intro_source_database")))
            # ),
            # br(), br(), br(),


            fluidRow(
              column(4,
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("vr_var_name"), label = "Response Variable (RV)",
                                          choices = set_options ,
                                          selected = set_options[1])
                       )),
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("factor_var_name"), label = "Factor",
                                          choices = set_options,
                                          selected = set_options[1])
                       ))),
              column(4,
                     fluidRow(
                       column(12,
                              selectInput(inputId = ns("alpha_value"), label = "Alpha value",
                                          choices = c(0.10, 0.05, 0.01),
                                          #choices = c(0.05),
                                          selected = 0.05)
                       ))),
              column(4, uiOutput(ns("action_buttons"))),
            ),
            fluidRow(
              column(12, textOutput(ns("calling_help")))
            )
        )

        #     column(8,
        #             fluidRow(
        #       selectInput(inputId = ns("vr_var_name"), label = "Response Variable",
        #                   choices = set_options ,
        #                   selected = set_options[1])
        #             ),
        #             fluidRow(
        #       selectInput(inputId = ns("factor_var_name"), label = "Factor",
        #                   choices = set_options,
        #                   selected = set_options[1])
        #             )
        #       ),
        #       column(4,
        #             fluidRow(
        #       selectInput(inputId = ns("alpha_value"), label = "Alpha value",
        #                   choices = c(0.10, 0.05, 0.01),
        #                   selected = 0.05)
        #          )
        #       )
        #       ,
        #   column(4, br(), br(), uiOutput(ns("action_buttons"))
        #          )
        #   ),
        # br(),
        # textOutput(ns("calling_help"))
        # )



      })



      # # # Control user 02
      control_user_02 <- reactive({

        req(control_user_01())

        validate(
          need(!is.null(input$vr_var_name), "Error 09: Module t test s01 - input$vr_var_name can not be NULL."),
          need(!is.null(input$factor_var_name), "Error 10: Module t test s01 - input$factor_var_name can not be NULL."),
          need(!is.null(input$alpha_value), "Error 11: Module t test s01 - input$alpha_value can not be NULL.")
        )

        validate(
          need(is.vector(input$vr_var_name), "Error 12: Module t test s01 - input$vr_var_name must be a vector."),
          need(is.vector(input$factor_var_name), "Error 13: Module t test s01 - input$vr_var_name must be a vector."),
          need(is.vector(input$alpha_value), "Error 14: Module t test s01 - input$alpha_value must be a vector.")
        )

        validate(
          need(length(input$vr_var_name) == 1, "Error 15: Module t test s01 - input$vr_var_name has length 1."),
          need(length(input$factor_var_name) == 1, "Error 16: Module t test s01 - input$factor_var_name has length 1."),
          need(length(input$alpha_value) == 1, "Error 17: Module t test s01 - input$alpha_value has length 1.")
        )


        validate(
          need(input$vr_var_name != "", "Select a response variable."),
          need(input$factor_var_name != "", "Select a factor."),
        )

        validate(
          need(input$vr_var_name != input$factor_var_name, "Selected variables can not be equal.")
        )



        return(TRUE)
      })


      # # # General control user
      control_user_99 <- reactive({

        req(control_user_02())
        control_user_02()

      })


      # #
      # Reset All --------------------------------------------------------------
      observeEvent(input$action_reset_all, {

        shinyjs::reset("input-var-selection")
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)


      })


      # # # Reset if change file source or any options
      # # VR selection
      observeEvent(input$vr_var_name, {

        # Not show yet
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)

      })


      # # Factor selection
      observeEvent(input$factor_var_name, {

        # Not show yet
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)

      })

      observeEvent(input$alpha_value, {

        # Not show yet
        color_button_load(hardcorded_initial_color)
        color_button_show(hardcorded_initial_color)
        action_button_load(FALSE)
        action_button_show(FALSE)

      })

      observeEvent(input$action_load, {


        req(control_user_99())
        action_button_load(TRUE)
        color_button_load("green")
      })



      observeEvent(input$action_load, {

        req(!action_button_load())

        color_button_load("red")
        action_button_load(FALSE)

      })




      observeEvent(input$action_load, {

        req(action_button_load(), control_user_99())
        action_button_show(TRUE)

      })



      # # # Final objects
      vr_var_name <- reactive({

        req(action_button_show())

        output_value <- input$vr_var_name
        return(output_value)
      })


      factor_var_name <- reactive({
        req(action_button_show())

        output_value <- input$factor_var_name
        return(output_value)
      })


      alpha_value <- reactive({
        req(action_button_show())
        output_value <- as.numeric(as.character(input$alpha_value))
        output_value
      })


      output$calling_help <- renderText({

        req(control_user_99())
        ""

      })


      # # # Output list
      output_list <- reactive({

        req(action_button_show())


        the_list <- list(vr_var_name(), factor_var_name(), alpha_value(), intro_source_database())

        names(the_list) <- c("vr_var_name", "factor_var_name", "alpha_value", "intro_source_database")
        the_list
      })


      return(output_list)
    })
}



