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

mod_import_server <- function(id, r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      ### Data upload logic ---------------------------------------------
      data_parameters <- shiny::reactive({
        if (nrow(r$data)==0 || is.null(r$data)){
          return(list(header = "",
                      inputs = "",
                      outputs = "",
                      objectives = ""))
        } else {
          return(list(header = grep("Rank|Distance|Cluster|Iteration", colnames(r$data), invert = TRUE, value = TRUE),
                      inputs = r$data$inputs,
                      outputs = r$data$outputs,
                      objectives = r$data$objectives))
        }
      })

      output$data_inputs <- shiny::renderUI({
        shiny::selectInput(ns("data_inputs"), "Inputs",
                           data_parameters()$header, isolate(data_parameters()$inputs), multiple = TRUE)
      })

      output$data_outputs <- shiny::renderUI({
        shiny::selectInput(ns("data_outputs"), "Outputs",
                           data_parameters()$header, isolate(data_parameters()$outputs), multiple = TRUE)
      })

      output$data_objectives <- shiny::renderUI({
        shiny::selectInput(ns("data_objectives"), "Objectives",
                           data_parameters()$header, isolate(data_parameters()$objectives), multiple = TRUE)
      })

      shiny::observeEvent(input$fileupload, {
        new_data <- input$fileupload
        if (!is.null(new_data)) {
          # Start by getting the column names if available
          r$data <- NULL
          r$filepath <- input$fileupload$name
          r$data <- loaddataset(input$fileupload$datapath)
          r$data$opt_name <- tools::file_path_sans_ext(input$fileupload$name)
          shiny::updateSelectInput(session, "data_objectives", selected = isolate(unique(r$data$objectives)))
        }
      })

      shiny::observeEvent(input$importData, {
        if (!is.null(input$fileupload)) {
          sel <- c(input$data_inputs, input$data_objectives, input$data_outputs)
          r$data <- r$data %>% dplyr::select("Iteration", dplyr::all_of(sel), "Rank")

          if (input$distancemetric) {
            r$data <- r$data %>% addDistances(parallelCores = 10)
          }

          if(!is.null(input$data_inputs)) {
            r$data$inputs <- input$data_inputs }

          if(!is.null(input$data_objectives)) {
            attr(r$data, "objectives") <- attr(r$data, "objectives")[input$data_objectives]
          }

          if(!is.null(input$data_outputs)) {
            r$data$outputs <- input$data_outputs
          } else {
            r$data$outputs <- r$data$parameters[!(r$data$parameters %in% c(r$data$inputs,r$data$objectives))]
            shiny::updateSelectInput(session, "data_outputs", selected = isolate(unique(r$data$outputs)))
          }

          r$filtered_data <- r$data
        }
      })
    })
}
