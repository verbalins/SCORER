# Creates a list to be used as input to the parallel coordinates chart
create_dimensions_list <- function(.data) {
  obj <- .data$objective_names
  inputs <- .data$inputs
  params <- colnames(.data)
  dim_list <- vector("list")
  for (i in seq_along(params)) {
    if (params[i] %in% obj) {
      dim_list[[i]] <- list(range = range(.data[[params[i]]]),
                            label = params[i],
                            values = stats::formula(paste0("~", params[i])))
    }
  }
  for (i in seq_along(params)) {
    if (params[i] %in% inputs) {
      dim_list[[length(dim_list) + 1]] <- list(
        range = range(.data[[params[i]]]),
        label = params[i],
        values = stats::formula(paste0("~", params[i])))
    }
  }
  return(dim_list[lengths(dim_list) != 0])
}
