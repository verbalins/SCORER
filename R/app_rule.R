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
                 shiny::hr(),
                 shiny::selectInput(ns("pointsel"),
                                    "Reference point or manual selection?",
                                    choices = c("Reference point",
                                                "From Visualization tab"),
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
            height = 900,
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
                      shinydashboard::box(width = NULL,
                          shiny::tabsetPanel(
                            id = ns("plotTab"),
                            type = "hidden",
                            shiny::tabPanelBody(
                              "ReferencePoint",
                              plotly::plotlyOutput(ns("refplot"),
                                                   height = "100%",
                                                   width = "100%")
                            ),
                            shiny::tabPanelBody("Rules",
                              plotly::plotlyOutput(ns("ruleplot"),
                                                   height = "100%",
                                                   width = "100%")
                            )

                          )
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

      shiny::observeEvent(input$pointsel, {
        shiny::updateTabsetPanel(session, "plotTab",
                                 ifelse(grepl("Reference", input$pointsel),
                                        "ReferencePoint",
                                        "Rules"))
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
                             "Reference point values separated by comma",
                             "50000, 0.9, 10"),
            shiny::numericInput(ns("kNN"), "kNN threshold", 0.2),
            shiny::actionButton(ns("refAssign"), "Assign point")
          )
        } else {
          return()
        }
      })

      shiny::observeEvent(input$refAssign, {
        reference_point <- as.list(
          setNames(as.numeric(stringi::stri_trim(
            stringr::str_split(input$referenceValues,
                               ",",
                               simplify = TRUE))),
            rval$filtered_data$objectives))
        rval$reference_point <- reference_point

        scaled <- rval$filtered_data %>%
          dplyr::filter(Rank == 1) %>%
          dplyr::select(Iteration, names(reference_point)) %>%
          dplyr::bind_rows(data.frame(Iteration = 99999, reference_point)) %>%
          dplyr::mutate(dplyr::across(rval$filtered_data$objectives,
                                      scales::rescale,
                                      to = c(0, 1)))

        scaled_ref <- scaled %>%
          dplyr::slice(nrow(.)) %>%
          dplyr::select(rval$filtered_data$objectives) %>%
          as.numeric(.)

        names(scaled_ref) <- names(reference_point)

        selected <- scaled %>%
          dplyr::filter(Iteration != 99999) %>%
          dplyr::mutate(
            dplyr::across(names(scaled_ref),
                          ~ (.x - scaled_ref[[dplyr::cur_column()]]) ^ 2)) %>%
          dplyr::mutate(Distance =
                          sqrt(rowSums(dplyr::across(names(scaled_ref))))) %>%
          #dplyr::distinct(across(names(reference_point)), .keep_all = TRUE) %>%
          dplyr::arrange(Distance) %>%
          head(n = input$kNN * nrow(.)) %>%
          dplyr::pull(Iteration)

        rval$selected_data <- selected

        output$refplot <- plotly::renderPlotly({
          plotly::plot_ly(x = stats::as.formula(paste0("~", rval$plotdims()$x)),
                          y = stats::as.formula(paste0("~", rval$plotdims()$y)),
                          z = stats::as.formula(paste0("~", rval$plotdims()$z)),
                          hoverinfo = "text",
                          size = 1,
                          opacity = 1,
                          height = 800) %>%
          plotly::add_markers(data = rval$filtered_data %>%
                                dplyr::filter(!(Iteration %in% selected),
                                              Rank == 1),
                              text = ~Iteration,
                              marker = list(color = I("blue")),
                              name = "Pareto") %>%
          plotly::add_markers(data = data.frame(reference_point),
                              marker = list(color = I("red")),
                              size = 1.5,
                              name = "Reference") %>%
          plotly::add_markers(data = rval$filtered_data %>%
                                dplyr::filter(Iteration %in% selected),
                              text = ~Iteration,
                              size = 1.5,
                              marker = list(color = I("green")),
                              name = "kNN")
        })
      })

      shiny::observeEvent(input$fpmrulebutton, {

        if (grepl("Reference", input$pointsel)) {
          sel <- rval$filtered_data %>%
            dplyr::filter(Iteration %in% rval$selected_data) %>%
            dplyr::select(.$inputs) %>%
            as.data.frame()

          unsel <- rval$filtered_data %>%
            dplyr::filter(!(Iteration %in% rval$selected_data)) %>%
            dplyr::select(.$inputs) %>%
            as.data.frame()
        } else {
          sel <- rval$df_selected()$sel %>%
            dplyr::select(.$inputs) %>%
            as.data.frame()

          unsel <- rval$df_selected()$unsel %>%
            dplyr::select(.$inputs) %>%
            as.data.frame()

          rval$selected_data <- rval$df_selected()$sel$Iteration
        }

        if (input$fpmTab == "RFPM") {
          rval$rules_r <- fpm(rval$filtered_data,
                             selected_data = rval$selected_data,
                             max_level = input$fpmlevel,
                             min_sig = input$minsig) %>%
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
      })

      shiny::observeEvent(input$sbirulebutton, {
        # Analyze rules for a specific cluster

        # Filter on selected cluster

        # Divide into subclusters?
        # Apply distance metric to each cluster and its pareto solutions
        # Find rules for each subcluster

      })

      dt_rules <- function(data) {
        DT::datatable(data,
                      options = list(searching = FALSE),
                      rownames = FALSE) %>%
          DT::formatPercentage(columns = c("Significance",
                                           "Unsignificance",
                                           "Ratio"),
                               digits = 2)
      }

      rule_sel <- shiny::reactive({
        req(input$FPMruletable_rows_selected)
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
        p <- SCORER::plot3d(rule_sel()$sel,
                            x = rval$plotdims()$x,
                            y = rval$plotdims()$y,
                            z = rval$plotdims()$z,
                            color = rval$plotdims()$color,
                            unselected_data = rule_sel()$unsel,
                            height = 800)
        if (grepl("Reference", input$pointsel)) {
          p <- p %>%
            plotly::add_markers(data = data.frame(Iteration = 99999,
                                                  Rank = 0,
                                                  Cluster = 0,
                                                  Distance = 0,
                                                  rval$reference_point),
                                marker = list(color = I("red")),
                                size = I(40),
                                name = "Reference")
        }

        p
      })
  })
}
