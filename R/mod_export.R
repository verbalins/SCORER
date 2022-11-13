# UI for the export tab
# Handles:
# - Download of data at different states
# - Export rules
# - Export code for reproducibility
mod_export_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::fillPage(
      shinydashboard::box(title = "Download data",
                          shiny::helpText("Select the data to download:"),
                          shiny::radioButtons(ns("download_select"),
                                              label = "",
                                              choices = c("Imported",
                                                          "Filtered",
                                                          "Selected",
                                                          "Rules",
                                                          "Code")),
                          shiny::downloadButton(ns("export_data"), "Download")
      ),
      shinydashboard::box(title = "Generated Code",
                          #shiny::tags$head(shiny::tags$style("#generate_r_code{overflow: scroll;}")),
                          shiny::htmlOutput(ns("generate_r_code")))
    )
  )
}

mod_export_server <- function(id, r) {
  shiny::moduleServer(
    id,
    function(input, output, session) {
      output$export_data <- shiny::downloadHandler(
        filename = function() {
          opt_name <- r$filtered_data$opt_name
          paste0(opt_name, "-",
                 input$download_select,
                 ifelse(input$download_select == "Code", ".R", ".csv"))
        },
        content = function(file) {
          if (input$download_select == "Code") {
            get_code() %>%
              stringr::str_replace_all(pattern = "<br>",
                                       replacement = "\n") %>%
              readr::write_lines(file = file)
          } else {
            data <- switch(input$download_select,
                           "Imported" = r$data,
                           "Filtered" = r$filtered_data,
                           "Selected" = r$df_selected()$sel,
                           "Rules" = r$rules_r_fpm)
            write.csv2(as.data.frame(data), file, row.names = FALSE)
          }
        }
      )

      # Get code for the analysis
      get_code <- shiny::reactive({
        load_data <- paste0("# Loading the dataset and assigning objectives, inputs, and outputs.<br>",
                            "df <- SCORER::load_dataset(file = \"",
                            r$filepath,
                            "\",<br>",
                            pastevector("objectives", r$data$objective_names),
                            ",<br>",
                            pastevector("inputs", r$data$inputs),
                            ",<br>",
                            pastevector("outputs", r$data$outputs),
                            ")<br>")

        import_data <- paste0("# Selecting relevant data.<br>",
                              "df_imported <- df %>% <br>",
                              "dplyr::select(",
                              paste("Iteration",
                                    paste(r$data$objective_names, collapse = ", "),
                                    paste(r$data$inputs, collapse = ", "),
                                    paste(r$data$outputs, collapse = ", "),
                                    "dplyr::starts_with(c('Rank', 'Distance'))",
                                    collapse = ", ",
                                    sep = ", "),
                              ")<br>")

        if (is.null(r$filters)) {
          filter_data <- paste0("df_filtered <- df_imported #No filters applied <br>")
        } else {
          filters <- unlist(r$filters)
          names(filters) <- colnames(r$data)
          filters <- filters[filters != ""]
          filter_col <- mapply(SCORER::createfilter, filters, names(filters))
          filter_data <- paste0("# Filtering data for certain criteria. <br>",
                                "df_filtered <- df_imported %>% <br>",
                                "dplyr::filter(",
                                paste0(filter_col,
                                       collapse = ", "),
                                ")<br>")
        }

        if (is.null(r$cluster$method)) {
          clustered_data <- paste0("df_clustered <- df_filtered # No clusters applied <br>")
        } else {
          if (r$cluster$method == "fastcluster::hclust.vector") {
            clustered_data <- paste0("cluster <- ",
                                     r$cluster$method,
                                     "(as.data.frame(df_filtered), ",
                                     r$cluster$params,
                                     ") %>% cutree(",
                                     r$cluster$params2,
                                     ") <br>")
          } else if (r$cluster$method == "stats::kmeans") {
            clustered_data <- paste0("cluster <- (",
                                     r$cluster$method,
                                     "(df_filtered %>% dplyr::select(",
                                     paste0(r$cluster$dep, collapse = ", "),
                                     "), ",
                                     r$cluster$params,
                                     "))$cluster <br>")
          } else if (r$cluster$method == "cluster::pam") {
            clustered_data <- paste0("cluster <- ",
                                     r$cluster$method,
                                     "(df_filtered, ",
                                     r$cluster$params,
                                     ") <br>")
          } else if (r$cluster$method == "dbscan::dbscan" |
                     r$cluster$method == "dbscan::hdbscan") {
            clustered_data <- paste0("cluster <- (",
                                     r$cluster$method,
                                     "(df_filtered, ",
                                     r$cluster$params,
                                     "))$cluster <br>")
          } else if (r$cluster$method == "dbscan::optics") {
            clustered_data <- paste0("cluster <- (",
                                     r$cluster$method,
                                     "(df_filtered, ",
                                     r$cluster$params,
                                     ") %>% dbscan::extractXi(",
                                     r$cluster$params2,
                                     "))$cluster <br>")
          } else if (r$cluster$method == "rpart::rpart") {
            rprt <- paste0("rprt <- ",
                           r$cluster$method,
                           "(",
                           r$cluster$form,
                           ", ",
                           r$cluster$params,
                           ", data = df_filtered) <br>")
            clustered_data <- paste0(rprt,
                                     "tree_pred <- partykit::as.party(rprt) %>% ",
                                     "predict(type = 'node') <br>",
                                     "cluster <- as.numeric(forcats::fct_recode(",
                                     "factor(tree_pred), ",
                              "!!!as.list(setNames(as.character(unique(tree_pred)),
                              seq_len(length(unique(tree_pred)))))))",
                              " <br>")
          } else {
            clustered_data <- paste0("# ", r$cluster$method, " not implemented ")
          }

          clustered_data <- paste0("# Applying clustering to the filtered data.<br>",
                                   clustered_data,
                                   "df_clustered <- df_filtered %>% dplyr::mutate(Cluster = cluster) <br>")
        }

        if (nrow(r$df_selected()$unsel) == 0) {
          selected_data <- "df_selected <- df_clustered # No selected solutions <br> "
        } else {
          selected_data <- paste0("# Selection from the visualization tab. <br>",
                                  "df_selected <- df_clustered %>% <br>",
                                  "dplyr::filter(Iteration %in% ",
                                  paste0("c(",
                                         paste0(r$df_selected()$sel$Iteration,
                                                collapse = ", "),
                                         ")"),
                                  ") <br>")
        }

        # Rules will be dependent on weather we have assigned a reference point
        #  or selected iterations manually
        if (!is.null(r$rules_r_fpm)) {
          if (r$rule_type == "Reference") {
            refpointassign <- paste0(paste(names(r$reference_point),
                                           paste(r$reference_point),
                                           sep = " = "),
                                     collapse = ", ")
            refpoint <- paste0("# Selecting relevant data.<br>",
                               paste("nearest_solutions <- SCORER::assign_reference_point(df_clustered",
                                     paste0("list(", refpointassign, ")"),
                                     paste0("knn = ", r$kNN, ") <br>"),
                                     sep = ", <br>")
                              )

            rules <- paste(refpoint,
                           paste("rules <- SCORER::fpm(df_clustered",
                             paste0("max_level = ", r$fpmlevel),
                             paste0("min_sig = ", r$minsig),
                             paste0("use_equality = ", r$fpmequality),
                             paste0("only_most_significant = ", r$fpmonlysig),
                             "selected_data = nearest_solutions)",
                             sep = ", <br>"),
                           sep = "<br>")

          } else {
            rules <- paste0("# Extracting and applying rules from the data.<br>",
                            paste("rules <- SCORER::fpm(df_clustered",
                                  paste0("max_level = ", r$fpmlevel),
                                  paste0("min_sig = ", r$minsig),
                                  paste0("use_equality = ", r$fpmequality),
                                  paste0("only_most_significant = ", r$fpmonlysig),
                                  "selected_data = df_selected$Iteration)<br>",
                                  sep = ", <br>"))

            # Add the selected rules from the rule page.
            if (!is.null(r$selected_rules_fpm)) {
              rules <- paste(rules,
                             paste("selected_solutions <- df_filtered %>% ",
                                   paste0("dplyr::filter(eval(str2expression(\"", r$selected_rules_fpm, "\")))<br>"),
                                   sep = " <br>"),
                             sep = " <br>")
              rules <- paste(rules,
                             "# Plotting the data.",
                             paste("SCORER::plotnd(.data = selected_solutions",
                              paste0("x = '", r$plotdims()$x, "'"),
                              paste0("y = '", r$plotdims()$y, "'"),
                              paste0("z = '", r$plotdims()$z, "'"),
                              paste0("color = '", r$plotdims()$color, "'"),
                              paste("unselected_data = df_filtered %>%",
                                  "dplyr::filter(!(Iteration %in% selected_solutions$Iteration))", sep = "<br>"),
                              paste0("height = 800)"),
                              sep = ", <br>"),
                            sep = " <br>")
            }
          }
        } else {
          rules <- "# No rules used"
        }

        preamble <- paste0("# Code to replicate the analysis done in the web tool:<br>",
                           "library(SCORER)<br>",
                           "library(dplyr)<br>",
                           "set.seed(", r$randomseed, ")<br>")
        paste(preamble,
              load_data,
              import_data,
              filter_data,
              clustered_data,
              selected_data,
              rules, sep = "<br>")
      })

      output$generate_r_code <- shiny::renderText({
        get_code()
      })
    }
)}
