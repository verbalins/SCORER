#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @importFrom shinydashboard dashboardSidebar
#' @importFrom shinydashboard sidebarMenu
#' @importFrom shinydashboard menuItem
#' @importFrom shinydashboard dashboardBody
#' @importFrom shinydashboard tabItems
#' @importFrom shinydashboard tabItem
#' @importFrom shinydashboard dashboardPage
#' @importFrom shinydashboard dashboardHeader
#' @noRd
app_ui <- function(request) {
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
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    shinydashboard::dashboardPage(
      shinydashboard::dashboardHeader(title = "RulExtR"),
      sidebar,
      body
    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){

  add_resource_path(
    "www", app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "SCORER"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
