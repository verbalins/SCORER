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
            shiny::tabsetPanel(id = ns("methodTab"), type = "tabs",
              shiny::tabPanel("FPM",
                 # Minimum significance
                 shiny::numericInput(ns("minsig"),
                                     "Minimum Significance",
                                     0.5, 0.1, 1.0, 0.1),
                 shiny::numericInput(ns("fpmlevel"),
                                     "Rule levels", 1, 1, 4, 1),
                 shiny::checkboxInput(ns("fpmonlymostsig"),
                                     "Use only most significant?",
                                     value = TRUE),
                 shiny::checkboxInput(ns("fpmequal"),
                                      "Use equality rules?",
                                      value = TRUE),
                 shiny::hr(),
                 shiny::selectInput(ns("pointsel"),
                                    "Reference point or manual selection?",
                                    choices = c("From Visualization tab",
                                                "Reference point"),
                                    selected = ""),
                 shiny::uiOutput(ns("referencepoint")),
                 shiny::hr(),
                 shiny::uiOutput(ns("fpmapplyrules"))
              ),
              shiny::tabPanel("SBI",
                  shiny::uiOutput(ns("clusterSelection")),
                  shiny::actionButton(ns("sbirulebutton"),
                                      "Create Rules"),
              )
            ),
          ),
          shinydashboard::box(
            width = NULL,
            #height = 800,
            title = "Rules",
            shiny::tabsetPanel(
              id = ns("fpmTab"),
              type = "tabs",
              shiny::tabPanel(
                "RFPM",
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("FPMruletable")))
              ),
              shiny::tabPanel("PyFPM",
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("Pyruletable")))
              ),
              shiny::tabPanel("SBI",
                shinycssloaders::withSpinner(
                  DT::DTOutput(ns("SBIruletable")))
              )
            ),
          ),
        ),
        shiny::column(9,
          shinydashboard::box(
            width = NULL,
            plotly::plotlyOutput(ns("ruleplot"),
                                 height = "100%",
                                 width = "100%")
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

      shiny::observeEvent(rval$filtered_data, {
        #output$ruleplot <- NULL
        #output$refplot <- NULL
      })

      output$clusterSelection <- shiny::renderUI({
        shiny::tagList(
          shiny::validate(shiny::need("Cluster" %in% names(rval$filtered_data),
                                      "No Clusters assigned")),
          shiny::selectInput(ns("clusterSelection"),
                             "Select the cluster of interest",
                             choices = sort(unique(rval$filtered_data$Cluster))),
          shiny::numericInput(ns("cp"),
                              "Pruning parameter:", 0.01, 0.001, 1, 0.001),
          shiny::numericInput(ns("maxdepth"),
                              "Maximum depth of tree:", 5, 1, 50, 1)
        )
      })

      output$fpmapplyrules <- shiny::renderUI({
        if (input$pointsel == "Reference point") {

          shiny::tagList(
            shiny::actionButton(ns("fpmrulebutton"),
                                "Create Rules")
          )
        } else {
          shiny::tagList(
            shiny::validate(
              shiny::need(nrow(rval$df_selected()$unsel) != 0,
                          "Please select data first on the Visualization tab")),
            shiny::actionButton(ns("fpmrulebutton"),
                                "Create Rules")
          )
        }
      })

      output$referencepoint <- shiny::renderUI({
        if (input$pointsel == "Reference point") {
          shiny::tagList(
            shiny::textInput(ns("referenceValues"),
                             paste("Reference point values separated by comma",
                                   paste0("(",
                                          paste0(rval$filtered_data$objectives,
                                                 collapse = ", "),
                                          ")")
                                   ),
                             ifelse(is.null(rval$reference_point),
                                    "50000, 0.9, 10",
                                    paste0(as.character(unlist(rval$reference_point)),
                                           collapse = ", "))),
            shiny::numericInput(ns("kNN"), "kNN threshold", 0.2),
            shiny::actionButton(ns("refAssign"), "Assign point")
          )
        } else {
          return()
        }
      })

      shiny::observeEvent(input$refAssign, {
        # TODO: Implement a better explanation for which parameter is which
        reference_point <- as.list(
          stats::setNames(as.numeric(stringi::stri_trim(
            stringr::str_split(input$referenceValues,
                               ",",
                               simplify = TRUE))),
            rval$filtered_data$objectives))

        rval$reference_point <- reference_point
        rval$kNN <- input$kNN

        selected <- SCORER::assign_reference_point(shiny::isolate(rval$filtered_data),
                                                   reference_point,
                                                   shiny::isolate(input$kNN))
        rval$selected_data <- selected
      })

      shiny::observeEvent(input$fpmrulebutton, {
        output$FPMruletable <- NULL

        if (grepl("Reference", input$pointsel)) {
          sel <- rval$filtered_data %>%
            dplyr::filter(Iteration %in% rval$selected_data) %>%
            dplyr::select(.$inputs) %>%
            as.data.frame()

          unsel <- rval$filtered_data %>%
            dplyr::filter(!(Iteration %in% rval$selected_data)) %>%
            dplyr::select(.$inputs) %>%
            as.data.frame()

          rval$rule_type <- "Reference"
        } else {
          sel <- rval$df_selected()$sel %>%
            dplyr::select(.$inputs) %>%
            as.data.frame()

          unsel <- rval$df_selected()$unsel %>%
            dplyr::select(.$inputs) %>%
            as.data.frame()

          rval$selected_data <- rval$df_selected()$sel$Iteration
          rval$rule_type <- "Manual"
        }

        if (input$fpmTab == "RFPM") {
          rval$rules_r <- fpm(rval$filtered_data,
                             selected_data = rval$selected_data,
                             max_level = input$fpmlevel,
                             min_sig = input$minsig,
                             use_equality = input$fpmequal,
                             only_most_significant = input$fpmonlymostsig) %>%
            dplyr::select(Rule, Significance, Unsignificance, Ratio)

          output$FPMruletable <- DT::renderDT(dt_rules(rval$rules_r))
        } else {
          reticulate::source_python("../py/FPM.py")

          rval$rules_py <- ExportRules(
            FPM(minimumSig = input$minsig,
                parameterNames = colnames(sel),
                selected_data = sel,
                unselected_data = unsel,
                useEquality = TRUE)) %>%
            dplyr::mutate(Rule = paste(Parameter, Sign, round(Value, 0)),
                          Significance = SEL,
                          Unsignificance = UNSEL) %>%
            dplyr::select(Rule, Significance, Unsignificance, Ratio)

          output$Pyruletable <- DT::renderDT(dt_rules(rval$rules_py))
        }

        rval$minsig <- input$minsig
        rval$fpmlevel <- input$fpmlevel
        rval$fpmequality <- input$fpmequal
        rval$fpmonlysig <- input$fpmonlymostsig
      })

      shiny::observeEvent(input$sbirulebutton, {
        # Analyze rules for a specific cluster

        # Filter on selected cluster

        # Divide into subclusters?
        # Apply distance metric to each cluster and its pareto solutions
        # Find rules for each subcluster

      })

      rule_sel <- shiny::reactive({
        #req(input$FPMruletable_rows_selected)
        sel <- rval$filtered_data

        if (!is.null(input$FPMruletable_rows_selected)) {
          selected_rows <- input$FPMruletable_rows_selected
          rules_str <- paste(unlist(rval$rules_r[selected_rows, "Rule"]),
                             collapse = " & ")
          sel <- sel %>% dplyr::filter(eval(str2expression(rules_str)))
        }

        unsel <- rval$filtered_data %>%
          dplyr::filter(!(Iteration %in% sel$Iteration))

        return(list("sel" = sel, "unsel" = unsel))
      })

      output$ruleplot <- plotly::renderPlotly({
        shiny::validate(
          shiny::need(!is.null(rval$plotdims()$x),
                      "Go to visualization tab first"))

        p <- SCORER::plot3d(.data = rule_sel()$sel,
                            x = rval$plotdims()$x,
                            y = rval$plotdims()$y,
                            z = rval$plotdims()$z,
                            color = rval$plotdims()$color,
                            unselected_data = rule_sel()$unsel,
                            height = 800)
        if (grepl("Reference", input$pointsel) &
            !is.null(rval$reference_point)) {
          p <- p %>%
            plotly::add_markers(data = data.frame(Iteration = 99999,
                                                  Rank = 0,
                                                  Cluster = 0,
                                                  Distance = 0,
                                                  rval$reference_point),
                                marker = list(color = I("red")),
                                size = I(40),
                                name = "Reference") %>%
            plotly::add_markers(data = rval$filtered_data %>%
                                  dplyr::filter(Iteration %in% rval$selected_data),
                                text = ~Iteration,
                                size = 1.5,
                                marker = list(color = I("green")),
                                name = "kNN")
        }
        p
      })
  })
}
