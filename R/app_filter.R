# UI for the filter tab
# Handles:
# - Data filtering
mod_filter_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        # Data filtering, specific filters for certain values, initial screening
        shinydashboard::box(title = "Data summary", width = NULL,
                            shiny::helpText("Use this table to filter the data you want to analyze. It will persist through the other tabs."),
                            shiny::helpText("Filter the table and then use the Apply Filter button at the bottom."),
                            shinycssloaders::withSpinner(DT::dataTableOutput(ns("datatable"))),
                            shiny::actionButton(ns("applyfilter"), "Apply Filter",
                                                icon = shiny::icon("check-circle"),
                                                class = "btn-success"),
                            shiny::actionButton(ns("resetfilter"), "Reset Filter",
                                                icon = shiny::icon("ban"),
                                                class = "btn-danger")
        )
      )
    )
  )
}

mod_filter_server <- function(id, r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ### Data filter logic ---------------------------------------------
      output$datatable <- DT::renderDataTable({
        table <- r$data
        if (is.null(table))
          return(NULL)

        DT::datatable(table,
                      filter = "top",
                      options = list(scrollX = TRUE,
                                     scrollY = 600,
                                     pageLength = 50),
                      rownames = FALSE)
      })

      shiny::observeEvent(input$applyfilter, {
        r$filtered_data <- r$data[input$datatable_rows_all, ]
        r$filters <- input$datatable_search_columns
      })

      shiny::observeEvent(input$resetfilter, {
        r$filtered_data <- r$data
        r$filters <- NULL
      })
    })
}
