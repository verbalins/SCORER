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
                        shiny::tabsetPanel(id=ns("methodTab"), type = "tabs",
                          shiny::tabPanel("FPM",
                             # Minimum significance
                             shiny::numericInput(ns("minsig"), "Minimum Significance", 0.5, 0.1, 1.0, 0.1),
                             shiny::numericInput(ns("fpmlevel"), "Rule levels", 1, 1, 4, 1),
                             shiny::hr(),
                             shiny::selectInput(ns("pointsel"), "Reference point or manual selection?", choices = c("Reference point", "From Visualization tab"), selected = ""),
                             shiny::uiOutput(ns("referencepoint")),
                             shiny::hr(),
                             shiny::actionButton(ns("fpmrulebutton"), "Create Rules"),
                             shiny::textOutput(ns("ruleError"), inline = TRUE)
                          ),
                          shiny::tabPanel("SBI",
                              shiny::uiOutput(ns("clusterSelection")),
                              shiny::actionButton(ns("sbirulebutton"), "Create Rules"),
                          )
                        ),

                      ),
                      shinydashboard::box(width = NULL, height= 900, title = "Rules",
                                          shiny::tabsetPanel(id = ns("fpmTab"), type="tabs",
                                                             shiny::tabPanel("RFPM",
                                                                             shinycssloaders::withSpinner(DT::DTOutput(ns("FPMruletable")))
                                                             ),
                                                             shiny::tabPanel("PyFPM",
                                                                             shinycssloaders::withSpinner(DT::DTOutput(ns("Pyruletable")))
                                                                             ),
                                                             shiny::tabPanel("SBI",
                                                                             shinycssloaders::withSpinner(DT::DTOutput(ns("SBIruletable")))
                                                             )
                                          ),
                      ),

        ),
        shiny::column(9,
                      shinydashboard::box(width = NULL,
                          shiny::tabsetPanel(id = ns("plotTab"), type="tabs",
                             shiny::tabPanel("ReferencePoint",
                                             #tags$style(type = "text/css", "#refplot {height: calc(100vh - 80px) !important;}"),
                                             #shinycssloaders::withSpinner(plotly::plotlyOutput(ns("refplot"), height = "100%", width = "100%"))
                                             plotly::plotlyOutput(ns("refplot"), height = "100%", width = "100%")
                                             ),
                             shiny::tabPanel("Rules",
                                             #tags$style(type = "text/css", "#ruleplot {height: calc(100vh - 80px) !important;}"),
                                             #shinycssloaders::withSpinner(plotly::plotlyOutput(ns("ruleplot"), height = "100%", width = "100%"))
                                             plotly::plotlyOutput(ns("ruleplot"), height = "100%", width = "100%")
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

      shiny::observeEvent(rVal$filtered_data, {
        #output$ruleplot <- NULL
        #output$refplot <- NULL
      })

      output$clusterSelection <- shiny::renderUI({
        browser()
        shiny::tagList(
          shiny::validate(shiny::need("Cluster" %in% names(rVal$filtered_data), "No Clusters assigned")),
          shiny::selectInput(ns("clusterSelection"), "Select the cluster of interest", choices = sort(unique(rVal$filtered_data$Cluster))),
          shiny::numericInput(ns("cp"), "Pruning parameter:", 0.01, 0.001, 1, 0.001),
          shiny::numericInput(ns("maxdepth"), "Maximum depth of tree:", 5, 1, 50, 1)
        )
      })

      output$referencepoint <- shiny::renderUI({
        if (input$pointsel == "Reference point") {
          shiny::tagList(
            shiny::textInput(ns("referenceValues"), "Reference point values separated by comma",  "50000, 0.9, 10"),
            shiny::numericInput(ns("kNN"), "kNN threshold", 0.2),
            shiny::actionButton(ns("refAssign"), "Assign point")
          )
        } else {
          return()
        }
      })

      shiny::observeEvent(input$refAssign, {
        #browser()
        reference_point <- as.list(setNames(as.numeric(stringi::stri_trim(stringr::str_split(input$referenceValues, ",", simplify = TRUE))), rVal$filtered_data$objectives))
        rVal$reference_point <- reference_point

        scaled <- rVal$filtered_data %>%
          dplyr::filter(Rank == 1) %>%
          dplyr::select(Iteration, names(reference_point)) %>%
          dplyr::bind_rows(data.frame(Iteration = 99999, reference_point)) %>%
          dplyr::mutate(dplyr::across(rVal$filtered_data$objectives, scales::rescale, to =c(0,1)))

        scaled_ref <- scaled %>% dplyr::slice(nrow(.)) %>% dplyr::select(rVal$filtered_data$objectives) %>% as.numeric(.)
        names(scaled_ref) <- names(reference_point)

        selected <- scaled %>%
          dplyr::filter(Iteration != 99999) %>%
          dplyr::mutate(dplyr::across(names(scaled_ref), ~ (.x - scaled_ref[[dplyr::cur_column()]]) ^ 2)) %>%
          dplyr::mutate(Distance = sqrt(rowSums(dplyr::across(names(scaled_ref))))) %>%
          #dplyr::distinct(across(names(reference_point)), .keep_all = TRUE) %>%
          dplyr::arrange(Distance) %>%
          head(n = input$kNN * nrow(.)) %>%
          dplyr::pull(Iteration)

        rVal$selectedData <- selected

        output$refplot <- plotly::renderPlotly({
          plotly::plot_ly(x=stats::as.formula(paste0("~", rVal$plotdims()$x)),
                          y=stats::as.formula(paste0("~", rVal$plotdims()$y)),
                          z=stats::as.formula(paste0("~", rVal$plotdims()$z)), hoverinfo="text", size = 1, opacity = 1, height = 800) %>%
          plotly::add_markers(data=rVal$filtered_data %>% dplyr::filter(!(Iteration %in% selected), Rank == 1), text= ~Iteration, marker = list(color = I("blue")), name = "Pareto") %>%
          plotly::add_markers(data=data.frame(reference_point), marker = list(color = I("red")), size=1.5, name = "Reference") %>%
          plotly::add_markers(data=rVal$filtered_data %>% dplyr::filter(Iteration %in% selected), text= ~Iteration, size=1.5, marker = list(color = I("green")), name = "kNN")
        })
      })

      shiny::observeEvent(input$fpmrulebutton, {
        if (nrow(rVal$df_selected()$unsel) == 0 & length(rVal$selectedData) == 0) {
          return() # Can't go forward without selecting from the Visualization tab
        }

        if (!grepl("From",input$pointsel)) {
          sel <- rVal$filtered_data %>% dplyr::filter(Iteration %in% rVal$selectedData) %>% dplyr::select(.$inputs) %>% as.data.frame()
          unsel <- rVal$filtered_data %>% dplyr::filter(!(Iteration %in% rVal$selectedData)) %>% dplyr::select(.$inputs) %>% as.data.frame()
        } else {
          sel <- rVal$df_selected()$sel %>% dplyr::select(.$inputs) %>% as.data.frame()
          unsel <- rVal$df_selected()$unsel %>% dplyr::select(.$inputs) %>% as.data.frame()
          rVal$selectedData <- rVal$df_selected()$sel$Iteration
        }

        if (input$fpmTab == "RFPM") {
          rVal$rulesR <- fpm(rVal$filtered_data,
                             maxLevel = input$fpmlevel,
                             minSig = input$minsig,
                             selectedData = rVal$selectedData) %>%
            dplyr::select(Rule, Significance, Unsignificance, Ratio)

          output$FPMruletable <- DT::renderDT(DT_rules(rVal$rulesR))
          #output$ruleplot <- plotly::renderPlotly({})
        } else {
          reticulate::source_python('../py/FPM.py')

          rVal$rulesPy <- ExportRules(
            FPM(minimumSig=input$minsig,
                parameterNames=colnames(sel),
                selectedData=sel,
                unselectedData=unsel,
                useEquality=TRUE)) %>%
            dplyr::mutate(Rule = paste(Parameter, Sign, round(Value,0)),
                          Significance = SEL,
                          Unsignificance = UNSEL) %>%
            dplyr::select(Rule, Significance, Unsignificance, Ratio)

          output$Pyruletable <- DT::renderDT(DT_rules(rVal$rulesPy))
        }

        rVal$minsig <- input$minsig
        rVal$fpmlevel <- input$fpmlevel
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
                      options = list(searching = FALSE), rownames = FALSE) %>%
          DT::formatPercentage(columns = c("Significance",
                                           "Unsignificance",
                                           "Ratio"),
                               digits = 2)
      }

      rule_sel <- shiny::reactive({
        req(input$FPMruletable_rows_selected)
        sel <- rVal$filtered_data
        if (!is.null(input$FPMruletable_rows_selected)) {
          selected_rows <- input$FPMruletable_rows_selected
          rules_str <- paste(unlist(rVal$rulesR[selected_rows,"Rule"]), collapse = " & ")
          sel <- sel %>% dplyr::filter(eval(str2expression(rules_str)))
        }
        unsel <- rval$filtered_data %>%
          dplyr::filter(!(Iteration %in% sel$Iteration))
        return(list("sel" = sel, "unsel" = unsel))
      })

      output$rule_error <- shiny::renderText({
        shiny::validate(
          shiny::need(nrow(rVal$df_selected()$unsel) != 0 | input$methodTab == "SBI", "Please select data first on the Visualization tab")
        )
      })

      output$ruleplot <- plotly::renderPlotly({
        p <- SCORER::plot3d(rule_sel()$sel,
                            x=rVal$plotdims()$x,
                            y=rVal$plotdims()$y,
                            z=rVal$plotdims()$z,
                            color=rVal$plotdims()$color,
                            unselected_data = rule_sel()$unsel,
                            height = 800)
        if (!grepl("From",input$pointsel)) {
          p <- p %>%
            plotly::add_markers(data=data.frame(Iteration=99999, Rank = 0, Cluster = 0, Distance=0, rVal$reference_point),
                                  marker = list(color = I("red")), size=I(40), name = "Reference")
        }
        p
      })
  })
}
