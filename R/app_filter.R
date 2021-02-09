# UI for the filter tab
# Handles:
# - Data filtering
mod_filter_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fluidPage(
      shiny::fluidRow(
        # Data filtering, specific filters for certain values, initial screening
        shinydashboard::box(title="Data summary", width = NULL,
                            shiny::helpText("Use this table to filter the data you want to analyze. It will persist through the other tabs."),
                            shiny::helpText("Filter the table and then use the Apply Filter button at the bottom."),
                            shinycssloaders::withSpinner(DT::dataTableOutput(ns("datatable"))),
                            shiny::actionButton(ns("applyfilter"),"Apply Filter",
                                                icon = shiny::icon("check-circle"),
                                                class = "btn-success"),
                            shiny::actionButton(ns("resetfilter"),"Reset Filter",
                                                icon = shiny::icon("ban"),
                                                class = "btn-danger")
        )
      )
    )
  )
}

mod_filter_server <- function(id, current_data) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      ### Data filter logic ---------------------------------------------
      df_filtered <- shiny::reactive({
        if(is.null(nrow(current_data()))){
          return(NULL)
        }
        current_data()[input$datatable_rows_all,]
      })

      output$data_name <- shiny::renderText({ current_data()$opt_name })
      output$datatable <- DT::renderDataTable({
        table <- current_data()
        if (is.null(table))
          return(NULL)

        DT::datatable(table, filter="top", options = list(scrollX = TRUE, scrollY = 600, pageLength = 50), rownames = FALSE)
      })

      output$filterslider <- renderUI({
        ns <- session$ns
        minVal <- min(df_filtered()[,input$filters])
        maxVal <- max(df_filtered()[,input$filters])
        shiny::sliderInput(ns("filterslider"), "", minVal, max=maxVal,value=c(minVal, maxVal))
      })

      shiny::observeEvent(input$applyfilter, {
        #ranges$data <- NULL
        #selected_points$data <- NULL
      })
      return(shiny::reactive({
        shiny::reactiveValues(data = df_filtered(), filters = input$datatable_search_columns)
      }))
    })
}
