app_options <- function() {
  requireNamespace(c("shiny", "shinydashboard",
                     "shinycssloaders", "shinyjs", "dbscan",
                     "factoextra", "fastcluster", "DT", "gtools"))
  seed <- 42
  set.seed(seed)
  options(shiny.maxRequestSize = 30 * 1024 ^ 2) # 30MB
  #options(shiny.error = browser)
  #options(shiny.reactlog = TRUE)
  #options(shiny.autoreload = TRUE)
  options(shiny.host = '0.0.0.0')
  options(shiny.port = 8080)
  debug <- FALSE
}
app_options()
