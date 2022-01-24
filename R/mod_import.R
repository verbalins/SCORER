# UI for the import tab
# Handles:
# - Data import
# - Import handling of inputs, outputs, and objectives

mod_import_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      tags$head(tags$style(".shiny-output-error{color: red;}")),
      shiny::fluidRow(
        # Data selection and import
        shinydashboard::box(title = "Upload data",
          shiny::helpText("Select a file exported from OptimizeBrowser,
                          or which contains similar headers."),
          shiny::fileInput(ns("fileupload"), "Choose CSV File",
                           multiple = FALSE,
                           accept = c("text/csv",
                                      "text/comma-separated-values",
                                      ".csv")),
          shiny::textOutput(ns("upload_status"))
        )
      ),
      shiny::fluidRow(
        shinydashboard::box(title = "Select Parameters", collapsible = TRUE,
          shiny::helpText("Select the type of parameters for importing.
                          Parameters not selected in either category will
                          be excluded."),
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
        if (nrow(r$data) == 0 || is.null(r$data)) {
          return(list(header = "",
                      inputs = "",
                      outputs = "",
                      objectives = ""))
        } else {
          return(list(header = grep("Rank|Distance|Cluster|Iteration",
                                    colnames(r$data),
                                    invert = TRUE,
                                    value = TRUE),
                      inputs = r$data$inputs,
                      outputs = r$data$outputs,
                      objectives = r$data$objective_names))
        }
      })

      output$data_inputs <- shiny::renderUI({
        shiny::selectInput(ns("data_inputs"), "Inputs",
                           data_parameters()$header,
                           isolate(data_parameters()$inputs),
                           multiple = TRUE)
      })

      output$data_outputs <- shiny::renderUI({
        shiny::selectInput(ns("data_outputs"), "Outputs",
                           data_parameters()$header,
                           isolate(data_parameters()$outputs),
                           multiple = TRUE)
      })

      output$data_objectives <- shiny::renderUI({
        shiny::selectInput(ns("data_objectives"), "Objectives",
                           data_parameters()$header,
                           isolate(data_parameters()$objectives),
                           multiple = TRUE)
      })

      shiny::observeEvent(input$fileupload, {
        new_data <- input$fileupload
        if (!is.null(new_data)) {
          # Start by getting the column names if available
          r$data <- NULL
          output$upload_status <- NULL
          r$filepath <- input$fileupload$name
          if (is.character(try(r$data <- load_dataset(input$fileupload$datapath), silent = TRUE))) {
            shiny::showNotification("Please include objective information in the OptBrowser export", type = "error")
            output$upload_status <- shiny::renderText(shiny::validate(
              shiny::need(!is.null(r$data), "Error while parsing, include objective information in the file!")))
            return()
          }
          r$data$opt_name <- tools::file_path_sans_ext(input$fileupload$name)
          shiny::updateSelectInput(session, "data_objectives",
                                   selected = isolate(unique(r$data$objective_names)))
        }
      })

      shiny::observeEvent(input$importData, {
        if (!is.null(input$data_inputs)) {
          r$data$inputs <- input$data_inputs
        }

        if (!is.null(input$data_objectives)) {
          r$data$objectives <- r$data$objectives[input$data_objectives]
        }

        if (!is.null(input$data_outputs)) {
          r$data$outputs <- input$data_outputs
        } else {
          r$data$outputs <- r$data$parameters[!(r$data$parameters %in% c(r$data$inputs,
                                                                         r$data$objective_names,
                                                                         "Rank",
                                                                         "Iteration",
                                                                         "Error",
                                                                         "ConstraintViolation"))]
          shiny::updateSelectInput(session,
                                   "data_outputs",
                                   selected = shiny::isolate(unique(r$data$outputs)))
        }

        sel <- c(input$data_inputs, input$data_objectives, input$data_outputs)
        r$data <- r$data %>%
          dplyr::select("Iteration", dplyr::all_of(sel), dplyr::any_of(c("Rank",
                                                                       "Cluster",
                                                                       "Distance")))

        if (input$distancemetric) {
          r$data <- r$data %>% add_distances(parallel_cores = 10)
        }

        r$filtered_data <- r$data
      })
    })
}
