#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  port = 8080,
  maxRequestSize = 30 * 1024 ^ 2,
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern,
      options = list(port = port,
                     maxRequestSize = maxRequestSize)
    ),
    golem_opts = list(...)
  )
}
