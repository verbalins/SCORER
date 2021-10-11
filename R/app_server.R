#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shiny reactiveValues
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  # reactiveValues used to hold the data in the app,
  #  communication between modules.
  # TODO: Default values set, change for prod
  seed <- 42
  set.seed(seed)
  r <- shiny::reactiveValues(randomseed = seed)

  # React for parameters to [SCORER::run_app]
  if (!is.null(golem::get_golem_options("dataset"))) {
    if (is.character(golem::get_golem_options("dataset"))) {
      df <- SCORER::load_dataset(golem::get_golem_options("dataset"))
    } else {
      df <- golem::get_golem_options("dataset")
    }
    r$data <- df
    r$filtered_data <- df
  } else if (golem::app_dev()) {
    # Pre-load for development
    r$data  <- SCORER::FMC
    r$filtered_data <- SCORER::FMC
  }


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
  mod_rule_fpm_server("fpm", r)
  mod_rule_sbi_server("sbi", r)

  ### Data export logic --------------------------------------------
  mod_export_server("export", r)

  if (golem::app_dev()) {
    session$onSessionEnded(stopApp)
  }
}
