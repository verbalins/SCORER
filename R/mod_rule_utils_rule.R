dt_rules <- function(data) {
  DT::datatable(data,
                options = list(searching = FALSE,
                               scrollY = 300,
                               scrollCollapse = TRUE,
                               scrollX = TRUE),
                rownames = FALSE) %>%
    DT::formatPercentage(columns = c("Significance",
                                     "Unsignificance",
                                     "Ratio"),
                         digits = 2)
}
