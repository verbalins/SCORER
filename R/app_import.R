mod_import_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    # UI for the import tab
    # Handles:
    # - Data import
    # - Import handling of inputs, outputs, and objectives
    shiny::fluidPage(
      shiny::fluidRow(
        # Data selection and import
        shinydashboard::box(title="Upload data", #width = NULL,
                            shiny::helpText("Select a file exported from OptimizeBrowser, or which contains similar headers."),
                            shiny::fileInput(ns("fileupload"), "Choose CSV File",
                                             multiple = FALSE,
                                             accept = c("text/csv",
                                                        "text/comma-separated-values",
                                                        ".csv"))
        )
      ),
      shiny::fluidRow(shinydashboard::box(title="Select Parameters", #width = NULL,
                                          shiny::helpText("Select the type of parameters for importing. Parameters not selected in either category will be excluded."),
                                          shiny::uiOutput(ns("data_objectives")),
                                          shiny::uiOutput(ns("data_inputs")),
                                          shiny::uiOutput(ns("data_outputs")),
                                          shiny::checkboxInput(ns("distancemetric"), "Add distance metric"),
                                          shiny::actionButton(ns("importData"), "Import")

      )
      )
    )
  )
}

mod_import_server <- function(id) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ### Data upload logic ---------------------------------------------
      uploaded_data <- shiny::reactiveValues(data = FMC)#data=NULL)

      current_data <- reactive({
        uploaded_data$data
      })

      data_parameters <- shiny::reactive({
        if (nrow(current_data())==0 || is.null(current_data())){
          return(list(header = "",
                      inputs = "",
                      outputs = "",
                      objectives = ""))
        } else {
          return(list(header = colnames(current_data()),
                      inputs = current_data()$inputs,
                      outputs = current_data()$outputs,
                      objectives = current_data()$objectives))
        }
      })

      output$data_inputs <- shiny::renderUI({
        ns <- session$ns
        shiny::selectInput(ns("data_inputs"), "Inputs", data_parameters()$header, data_parameters()$inputs, multiple = TRUE)
      })

      output$data_outputs <- shiny::renderUI({
        ns <- session$ns
        shiny::selectInput(ns("data_outputs"), "Outputs", data_parameters()$header, data_parameters()$outputs, multiple = TRUE)
      })

      output$data_objectives <- shiny::renderUI({
        ns <- session$ns
        shiny::selectInput(ns("data_objectives"), "Objectives", data_parameters()$header, data_parameters()$objectives, multiple = TRUE)
      })

      shiny::observeEvent(input$fileupload, {
        ns <- session$ns
        new_data <- input$fileupload
        if (!is.null(new_data)) {
          # Start by getting the column names if available
          uploaded_data$data <- NULL

          uploaded_data$data <- loaddataset(input$fileupload$datapath)
          uploaded_data$data$opt_name <- tools::file_path_sans_ext(input$fileupload$name)
          updateSelectInput(session, ns("data_objectives"), selected = unique(uploaded_data$data$objectives))
        }
      })

      shiny::observeEvent(input$importData, {
        if (!is.null(input$fileupload)) {
          sel <- c(input$data_inputs, input$data_objectives, input$data_outputs)
          uploaded_data$data <- uploaded_data$data %>% dplyr::select("Iteration", dplyr::all_of(sel), "Rank")

          if (input$distancemetric) {
            uploaded_data$data <- uploaded_data$data %>% addDistances(parallelCores = 10)
          }

          if(!is.null(input$data_inputs)) {
            uploaded_data$data$inputs <- input$data_inputs }
          if(!is.null(input$data_objectives)) {
            uploaded_data$data$objectives <- input$data_objectives }
          if(!is.null(input$data_outputs)) {
            uploaded_data$data$outputs <- input$data_outputs
          } else {
            uploaded_data$data$outputs <- uploaded_data$data$parameters[!(uploaded_data$data$parameters %in% c(uploaded_data$data$inputs,
                                                                                                               uploaded_data$data$objectives))]
            shiny::updateSelectInput(session,"data_outputs",selected = uploaded_data$data$outputs)
          }
        }
      })

      return(shiny::reactive(shiny::reactiveValues(
        data = current_data())))
    })
}
