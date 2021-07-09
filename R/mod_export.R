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
                           "Rules" = r$rules_r)
            write.csv2(as.data.frame(data), file, row.names = FALSE)
          }
        }
      )

      # Get code for the analysis
      get_code <- shiny::reactive({
        load_data <- paste0("df <- SCORER::loaddataset(file = \"",
                            r$filepath,
                            "\",<br>",
                            pastevector("objectives", r$data$objectives),
                            ",<br>",
                            pastevector("inputs", r$data$inputs),
                            ",<br>",
                            pastevector("outputs", r$data$outputs),
                            ")<br>")

        import_data <- paste0("df_imported <- df %>% <br>",
                              "dplyr::select(",
                              paste("Iteration",
                                    paste(r$data$objectives, collapse = ", "),
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
          filter_col <- mapply(createfilter, filters, names(filters))
          filter_data <- paste0("df_filtered <- df_imported %>% <br>",
                                "dplyr::filter(",
                                paste0(filter_col,
                                       collapse = ", "),
                                ")<br>")
        }

        if (is.null(r)) {
          clustered_data <- paste0("df_clustered <- df_filtered # Not implemented <br>")
        } else {

        }

        if (nrow(r$df_selected()$unsel) == 0) {
          selected_data <- "df_selected <- df_clustered # No selected solutions <br> "
        } else {
          selected_data <- paste("df_selected <- df_clustered %>% <br>",
                                 "dplyr::filter(Iteration %in%",
                                 paste0("c(",
                                        paste0(r$df_selected()$sel$Iteration,
                                               collapse = ", "),
                                        ")"),
                                 ") <br>")
        }

        # Rules will be dependent on weather we have assigned a reference point
        #  or selected iterations manually
        if (!is.null(r$rules_r)) {
          if (r$rule_type == "Reference") {
            refpointassign <- paste0(paste(names(r$reference_point),
                                           paste(r$reference_point),
                                           sep = " = "),
                                     collapse = ", ")
            refpoint <- paste("nearest_solutions <- SCORER::assign_reference_point(df_clustered",
                              paste0("list(", refpointassign, ")"),
                              paste0("kNN = ", r$kNN, ") <br>"),
                              sep = ", <br>")
            rules <- paste(refpoint,
                           paste("rules <- SCORER::fpm(df_clustered",
                             paste0("max_level = ", r$fpmlevel),
                             paste0("min_sig = ", r$minsig),
                             paste0("use_equality = ", r$fpmequality),
                             paste0("only_most_significant = ", r$fpmonlysig),
                             "selected_data = nearest_solutions)",
                             sep = ", <br>"),
                           sep ="<br>")

          } else {
            rules <- paste("rules <- SCORER::fpm(df_clustered",
                           paste0("max_level = ", r$fpmlevel),
                           paste0("min_sig = ", r$minsig),
                           paste0("use_equality = ", r$fpmequality),
                           paste0("only_most_significant = ", r$fpmonlysig),
                           "selected_data = df_selected$Iteration)",
                           sep = ", <br>")
          }
        } else {
          rules <- "# No rules used"
        }

        preamble <- paste0("library(SCORER)<br>",
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
