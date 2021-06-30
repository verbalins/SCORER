#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Targets:
#   Identify interesting points
#   See correlations between parameters
# Prerequisites:
#   Parcoords for inputs and objectives
#       DONE
#   2D-plots for comparison of objectives
#       DONE
#   Filter on both parcoords and scatter plot
#       Parcoords should retain the filters
#       Scatter plots should just update
#

# Views:
#   Clustering
#       Prepare the data
#           Filter
#       Specify the parameters to cluster by
#       Choose the clustering method
#       Choose the number of clusters by evaluating a few metrics
#       Apply the clustering to the dataset
#   Visualization
#       Mostly as now, filtering will stick
#   Rule extraction
#       Extract rules from the selected data in the previous screens

# Input:
#   Dataset with clusters applied.
#       TODO: Set on this screen or another?
#
seed <- 42
set.seed(seed)
options(shiny.maxRequestSize = 30 * 1024 ^ 2) # 30MB
options(shiny.error = browser)
#options(shiny.reactlog = TRUE)

debug <- FALSE

export_ui <- shiny::fillPage(
    shinydashboard::box(title = "Download data",
        shiny::helpText("Specify the data to download."),
        shiny::radioButtons("downloadSelect",
                            label = "",
                            choices = c("Imported",
                                        "Filtered",
                                        "Selected",
                                        "Rules",
                                        "Code")),
        shiny::downloadButton("export_data", "Download")
    ),
    shinydashboard::box(title = "Generated Code",
                        shiny::htmlOutput("generate_r_code"))
)

# Sidebar UI
sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id = "tabs",
        shinydashboard::menuItem("Import Data",
                                 tabName = "import",
                                 icon = shiny::icon("table")),
        shinydashboard::menuItem("Filter Data",
                                 tabName = "filter",
                                 icon = shiny::icon("filter")),
        shinydashboard::menuItem("Clustering",
                                 tabName = "cluster",
                                 icon = shiny::icon("microscope")),
        shinydashboard::menuItem("Visualization",
                                 tabName = "viz",
                                 icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem("Rule Extraction",
                                 tabName = "rule",
                                 icon = shiny::icon("sitemap")),
        shinydashboard::menuItem("Export Data",
                                 tabName = "export",
                                 icon = shiny::icon("file-export"))
    )
)

# Body UI
body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "import",
                                mod_import_ui("import")),
        shinydashboard::tabItem(tabName = "filter",
                                mod_filter_ui("filter")),
        shinydashboard::tabItem(tabName = "cluster",
                                mod_cluster_ui("cluster")),
        shinydashboard::tabItem(tabName = "viz",
                                mod_visualization_ui("visualization")),
        shinydashboard::tabItem(tabName = "rule",
                                mod_rule_ui("rule")),
        shinydashboard::tabItem(tabName = "export",
                                export_ui)
    )
)

dash <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "RulExtR"),
    sidebar,
    body
)

# Define server logic
server <- function(input, output, session) {
    # reactiveValues used to hold the data in the app,
    #  communication between modules.
    # TODO: Default values set, change for prod
    r <- shiny::reactiveValues(data = SCORER::FMC, filtered_data = SCORER::FMC)

    ### Data import logic --------------------------------------------
    mod_import_server("import", r)
    # Reset filters on Visualization tab when current_data changes
    # shiny::observe({
    #     current_data
    #     ranges$data <- NULL
    #     selected_points$data <- NULL
    # })

    ### Data filter logic --------------------------------------------
    mod_filter_server("filter", r)

    ### Cluster logic ------------------------------------------------
    mod_cluster_server("cluster", r)

    ### Visualization logic ------------------------------------------
    mod_visualization_server("visualization", r)

    ### Rule logic ---------------------------------------------------
    mod_rule_server("rule", r)

    ### Data export logic --------------------------------------------
    output$export_data <- shiny::downloadHandler(
        filename = function() {
            opt_name <- df_filtered()$data$opt_name
            paste0(opt_name, "-",
                   input$downloadSelect,
                   ifelse(input$downloadSelect == "Code", ".R", ".csv"))
        },
        content = function(file) {
            if (input$downloadSelect == "Code") {
                get_code() %>%
                    stringr::str_replace_all(pattern = "<br>",
                                             replacement = "\n") %>%
                    readr::write_lines(file = file)
            } else {
                data <- switch(input$downloadSelect,
                               "Imported" = r$data,
                               "Filtered" = r$filtered_data,
                               "Selected" = r$df_selected(),
                               "Rules" = r$R)
                write.csv2(as.data.frame(data), file, row.names = FALSE)
            }
        }
    )

    # Get code for the analysis
    get_code <- shiny::reactive({
        pastevector  <- function(x, y) {
          paste0(x,
                 " = c(",
                 paste0("\"", y, "\"", collapse = ", "),
                 ")")
        }

        createfilter <- function(x, y) {
            val <- unlist(stringr::str_split(x, " ... "))
            paste0("dplyr::between(",
                   paste(y, val[1], val[2], sep = ", "),
                   ")")
        }

        load_data <- paste0("df <- SCORER::loaddataset(file=\"",
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

        filters <- unlist(r$filters)
        names(filters) <- colnames(r$data)
        filters <- filters[filters != ""]
        filter_col <- mapply(createfilter, filters, names(filters))
        if (length(filter_col) == 0) {
            filter_data <- paste0("df_filtered <- df_imported #No filters applied <br>")
        } else {
            filter_data <- paste0("df_filtered <- df_imported %>% <br>",
                                  "dplyr::filter(",
                                  paste0(filter_col,
                                         collapse = ", "),
                                  ")<br>")
        }

        clustered_data <- paste0("df_clustered <- df_filtered # Not implemented <br>")

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

        rules <- paste("rules <- SCORER::fpm(df_clustered",
                       paste0("max_level = ", r$fpmlevel),
                       paste0("min_sig = ", r$dataminsig),
                       "selected_data = df_selected$Iteration)", sep = ", <br>")

        preamble <- paste0("library(SCORER)<br>",
                           "set.seed(",
                           seed,
                           ")<br>")
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

# Run the application
shiny::shinyApp(ui = dash, server = server)
