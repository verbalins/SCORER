#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny reactiveValues
#' @noRd
app_server <- function( input, output, session ) {
  # Your application server logic
  # reactiveValues used to hold the data in the app,
  #  communication between modules.
  # TODO: Default values set, change for prod
  seed <- 42
  set.seed(seed)
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
