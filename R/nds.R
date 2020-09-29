#' Perform non-dominated sorting
#'
#' @param optresults an OptResults class
#' @param optGoals objectives from optresults or an character/numeric vector
#'   defining the objectives to subset
#'
#' @return optresults with ranks
#' @export
#'
ndsecr <- function(.data, objectives = attr(.data, "objectives")) {
  if (is.numeric(objectives) || is.character(objectives)) {
    # Named or numeric vector for the objective subsets
    if (!is.null(attr(.data, "objectives"))) {
      objectives <- attr(.data, "objectives")[objectives]
    } else {
      temp_obj <- grepl("Max|max", objectives)
      names(temp_obj) <- objectives
      objectives <- temp_obj
    }
  }
  cat("Performing NDS with objectives:", "\n", names(objectives), "\n")

  normalizedData <- normalizeValues(.data[,names(objectives)], objectives)
  optData <- as.matrix(normalizedData)

  if ("Rank" %in% names(.data)) {
    cat("Column Rank is already present, renaming to OldRank.", "\n")
    .data <- .data %>% dplyr::rename(OldRank = Rank)
  }
  optresults <- dplyr::bind_cols(.data, ecr::doNondominatedSorting(t(optData))) %>%
    dplyr::rename(Rank = ranks) %>%
    dplyr::select(-dom.counter)

  return(optresults)
}


#' Normalize values in the optimization data.
#'
#' @param optData an OptResults class
#' @param optGoals Optimization goals
#'
#' @return Normalized values
normalizeValues <- function(optData, optGoals) {
  # Make sure that the values are normalized and handled depending on maximize or minimize

  # Normalize and invert if necessary
  # Start with invert
  for (i in seq(length(optGoals))) {
    if (optGoals[i]) {
      optData[,i] <- 1/optData[,i]
    }
  }
  # Normalize
  optData <- BBmisc::normalize(optData, method = "range", margin = 2)
}
