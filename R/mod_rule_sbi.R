# UI for the rule extraction tab
mod_rule_sbi_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fillPage(
      # Decision trees
      # FPM through R
      shiny::fluidRow(
        shiny::column(3,
          shinydashboard::box(width = NULL,
            shiny::uiOutput(ns("clusterSelection")),
            shiny::br(),
            shiny::uiOutput(ns("rule_settings")),
            shiny::br(),
            shiny::actionButton(ns("sbirulebutton"),
                                "Create Rules"),
            shiny::br(),
            shiny::numericInput(ns("maxdist"),
                                "Maximum distance for inclusion (d):", 0.05, 0.01, 1, 0.01),
            shiny::br(),
            shinycssloaders::withSpinner(
              DT::DTOutput(ns("SBIruletable")))
          ),
        ),
        shiny::column(9, # Plot area
          shinydashboard::box(
            shinycssloaders::withSpinner(
              plotly::plotlyOutput(ns("sbi_plot"),
                                   height = "100%",
                                   width = "100%")),
            width = NULL
          )
        )
      )
    )
  )
}

mod_rule_sbi_server <- function(id, rval) {
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
                             "Select the cluster of interest:",
                             choices = sort(unique(rval$filtered_data$Cluster))),
          shiny::selectInput(ns("sbi_obj_select"),
                             "Which objectives should be analyzed?",
                             choices = unique(rval$filtered_data$objective_names),
                             selected = rval$filtered_data$objective_names[1:2],
                             multiple = TRUE),
          shiny::actionButton(ns("apply_sbi"),
                              "Assign Distance")
        )
      })

      output$rule_settings <- shiny::renderUI({
        shiny::tagList(
          shiny::numericInput(ns("cp"),
                              "Pruning parameter:", 0.01, 0.001, 1, 0.001),
          shiny::numericInput(ns("maxdepth"),
                              "Maximum depth of tree:", 5, 1, 50, 1)
        )
      })

      # Functions when pressing "Assign Distance"
      shiny::observeEvent(input$apply_sbi, {
        shiny::validate(
          shiny::need(input$clusterSelection, "No cluster selected"))

        rval$new_distance <- NULL
        rval$single_cluster <- NULL
        plot_color("Rank")
        selected_points$data <- NULL
        if (!is.null(input$SBIruletable)) {
          DT::dataTableProxy("SBIruletable") %>% DT::selectRows(NULL)
          output$SBIruletable <- NULL
        }

        plot_color("Distance")

        plotting_objectives <- shiny::isolate(input$sbi_obj_select)

        rval$single_cluster <- rval$filtered_data %>%
          dplyr::select(-rval$filtered_data$objective_names[!(rval$filtered_data$objective_names %in%
                                                                plotting_objectives)]) %>%
          dplyr::filter(Cluster == input$clusterSelection) %>%
          ndsecr()

        rval$sbi_arguments <- setNames(
          as.list(rval$single_cluster$objective_names),
          c("x", "y", "z")[1:length(rval$single_cluster$objective_names)])
      })

      plot_color <- shiny::reactiveVal("Rank")

      shiny::observeEvent(input$sbirulebutton, {
        shiny::validate(shiny::need(
          selected_points$data, "Select pareto solutions first"
        ))
        # Analyze rules for a specific cluster

        # Let the user select the interesting pareto solutions
        # Apply distance metric to each cluster and its pareto solutions
        # Find rules for each subcluster
        if (!is.null(selected_points$data)) {
          plot_color("Distance")

          rval$rules_r_sbi <- NULL

          output$SBIruletable <- DT::renderDT(rval$rules_r_sbi)

          # Create new distances to the selected points
          rval$new_distance <- rval$single_cluster %>%
            add_distances(pareto_solutions = selected_points$data,
                          parallel_cores = 0)

          # Plot the selected objectives
          plotting_objectives <- shiny::isolate(input$sbi_obj_select)

          arguments <- setNames(as.list(plotting_objectives),
                                c("x", "y", "z")[1:length(plotting_objectives)])

          # Create the rules for the selected solutions.
          form <- as.formula(paste("Distance ~ ",
                                   paste(rval$new_distance$inputs,
                                         collapse = "+"),
                                   collapse=" "))

          fit_rpart <- rpart::rpart(formula = form,
                                    data = rval$new_distance,
                                    control = rpart::rpart.control(cp=input$cp,
                                                                   maxdepth=input$maxdepth),
                                    model = TRUE)

          tree <- partykit::as.party(fit_rpart)

          prediction <- predict(tree, newdata = rval$new_distance, type = "node")

          new_distance_cluster <- rval$new_distance %>%
            dplyr::mutate(Cluster = prediction)

          rval$rules_r_sbi <-
            rpart.plot::rpart.rules(fit_rpart, eq = " == ", when = "") %>%
            tibble::as_tibble(.name_repair = "unique") %>%
            tidyr::unite(-Distance,
                         col = "Rule",
                         sep = " ",
                         na.rm = TRUE) %>%
            dplyr::mutate(Rule = stringr::str_squish(Rule))

          output$SBIruletable <- DT::renderDT(
            DT::datatable(rval$rules_r_sbi,
                          options = list(searching = FALSE,
                                         scrollY = 300,
                                         scrollCollapse = TRUE,
                                         scrollX = TRUE),
                          rownames = FALSE))
        }
      })

      selected_points <- shiny::reactiveValues("data" = NULL)

      shiny::observeEvent(plotly::event_data("plotly_selected", source = "rule_sbi"), {
          selected <- plotly::event_data("plotly_selected", source = "rule_sbi")

          if (is.null(selected)) {
            selected_points$data <- NULL
          } else {
            selected_points$data <- selected$customdata
          }
        })

      # Reactive values which continuously updates with correct values
      # depending on user actions
      rule_sel <- shiny::reactive({
        shiny::validate(
          shiny::need(input$clusterSelection,
                      "No cluster selected"))

        if (!is.null(rval$new_distance)) {
          rule_data <- rval$new_distance
        } else if (!is.null(rval$single_cluster)) {
          rule_data <- rval$single_cluster
        } else {
          rule_data <- rval$filtered_data %>%
            dplyr::filter(Cluster == input$clusterSelection)
        }
        sel <- rule_data

        if (!is.null(input$SBIruletable_rows_selected)) {
          selected_rows <- input$SBIruletable_rows_selected

          rules_str <- unlist(rval$rules_r_sbi[selected_rows, "Rule"])
          rval$selected_rules_sbi <- rules_str

          sel <-
            rule_data %>% dplyr::filter(eval(str2expression(rules_str)),
                                        Distance <= input$maxdist)
        }

        unsel <- rule_data %>%
          dplyr::filter(!(Iteration %in% sel$Iteration))

        return(list("sel" = sel, "unsel" = unsel))
      })

      output$sbi_plot <- plotly::renderPlotly({
        shiny::validate(
          shiny::need(input$clusterSelection, "No cluster selected"),
          shiny::need(!is.null(rval$plotdims()$x), "Go to visualization tab first")
        )

        p <- SCORER::plotnd(
          .data = rule_sel()$sel,
          x = rval$plotdims()$x,
          y = rval$plotdims()$y,
          z = rval$plotdims()$z,
          unselected_data = rule_sel()$unsel,
          color = plot_color(),
          height = 800,
          source = "rule_sbi")
        p
      })

      output$SBIruletable <- NULL
  })
}
