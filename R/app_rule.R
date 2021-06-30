# UI for the rule extraction tab
mod_rule_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fillPage(
      # Decision trees
      # FPM, through python and R
      shiny::fluidRow(
        shiny::column(3,
          shinydashboard::box(width = NULL,
            #shiny::checkboxInput("test", "test"),
            shiny::tabsetPanel(id = ns("ruleTab"), type = "tabs",
              shiny::tabPanel("FPM",
                 # Minimum significance
                 shiny::numericInput(ns("minsig"),
                                     "Minimum Significance",
                                     0.5, 0.1, 1.0, 0.1),
                 shiny::numericInput(ns("fpmlevel"),
                                     "Rule levels",
                                     1, 1, 4, 1)),
              shiny::tabPanel("Hierarchical")
            ),
            shiny::actionButton(ns("rulebutton"), "Create Rules"),
            shiny::textOutput(ns("rule_error"), inline = TRUE)
          ),
          shinydashboard::box(width = NULL,
            shiny::tabsetPanel(type = "tabs",
             shiny::tabPanel("RFPM",
                             shinycssloaders::withSpinner(
                               DT::DTOutput(ns("ruletable")))
             ),
             shiny::tabPanel("PyFPM",
                             shinycssloaders::withSpinner(
                               DT::DTOutput(ns("pyruletable"))))
            )
          )
        ),
        shiny::column(9,
          shinydashboard::box(width = NULL,
            tags$style(type = "text/css",
                       "#scatter3d {height: calc(100vh - 80px) !important;}"),
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(
                ns("ruleplot"), height = "100%", width = "100%"))
          )
        )
      )
    )
  )
}

mod_rule_server <- function(id, rval) {
  shiny::moduleServer(id,
    function(input, output, session) {
      ns <- session$ns
      "%>%" <- magrittr::"%>%"

      output$ruletable <- DT::renderDT(data.frame())
      output$pyruletable <- DT::renderDT(data.frame())

      shiny::observeEvent(input$rulebutton, {
        if (nrow(rval$df_selected()$unsel) == 0) {
          # Can't go forward without selecting from the Visualization tab
          return()
        }

        sel <- rval$df_selected()$sel %>%
          dplyr::select(.$inputs) %>%
          as.data.frame()

        unsel <- rval$df_selected()$unsel %>%
          dplyr::select(.$inputs) %>%
          as.data.frame()

        rval$rules_r <- fpm(rval$filtered_data,
                           maxLevel = input$fpmlevel,
                           minSig = input$minsig,
                           selectedData = rval$df_selected()$sel$Iteration) %>%
          dplyr::select(Rule, Significance, Unsignificance, Ratio)

        reticulate::source_python("../py/FPM.py")

        rval$rules_py <- ExportRules(
          FPM(minimumSig = input$minsig,
              parameterNames = colnames(sel),
              selectedData = sel,
              unselectedData = unsel,
              useEquality = TRUE)) %>%
          dplyr::mutate(Rule = paste(Parameter, Sign, round(Value, 0)),
                        Significance = SEL,
                        Unsignificance = UNSEL) %>%
          dplyr::select(Rule, Significance, Unsignificance, Ratio)

        rval$minsig <- input$minsig
        rval$fpmlevel <- input$fpmlevel

        output$ruletable <- DT::renderDT(dt_rules(rval$rules_r))
        output$pyruletable <- DT::renderDT(dt_rules(rules$rules_py))
      })

      dt_rules <- function(data) {
        DT::datatable(data,
                      options = list(searching = FALSE), rownames = FALSE) %>%
          DT::formatPercentage(columns = c("Significance",
                                           "Unsignificance",
                                           "Ratio"),
                               digits = 2)
      }

      rule_sel <- shiny::reactive({
        shiny::req(input$ruletable_rows_selected)
        sel <- rval$filtered_data
        if (!is.null(input$ruletable_rows_selected)) {
          selected_rows <- input$ruletable_rows_selected
          rules_str <- paste(unlist(rval$rules_r[selected_rows, "Rule"]),
                             collapse = " & ")
          sel <- sel %>% dplyr::filter(eval(str2expression(rules_str)))
        }
        unsel <- rval$filtered_data %>%
          dplyr::filter(!(Iteration %in% sel$Iteration))
        return(list("sel" = sel, "unsel" = unsel))
      })

      output$rule_error <- shiny::renderText({
        shiny::validate(
          shiny::need(nrow(rval$df_selected()$unsel) != 0,
                      "Please select data first")
        )
      })

      output$ruleplot <- plotly::renderPlotly({
        suppressWarnings(
          SCORER::plot3d(rule_sel()$sel,
                         x = rval$plotdims()$x,
                         y = rval$plotdims()$y,
                         z = rval$plotdims()$z,
                         color = rval$plotdims()$color,
                         unselected_data = rule_sel()$unsel,
                         height = 600)
        )
      })
  })
}
