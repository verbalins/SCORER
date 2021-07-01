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
#options(shiny.error = browser)
options(shiny.reactlog = TRUE)
options(display.mode = "showcase")

debug <- FALSE

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
                                mod_export_ui("export"))
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
    r <- shiny::reactiveValues(data = SCORER::FMC,
                               filtered_data = SCORER::FMC,
                               randomseed = seed)

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
    mod_export_server("export", r)
}

# Run the application
shiny::shinyApp(ui = dash, server = server)
