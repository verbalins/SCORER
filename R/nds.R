#' Perform non-dominated sorting
#'
#' @param optresults an OptResults class
#' @param objectives objectives from optresults or an character/numeric vector
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
  limits <- sapply(.data[,names(objectives)], function(x) {c(min(x), max(x))})
  normalizedData <- normalizeValues(.data[,names(objectives)], objectives)
  optData <- as.matrix(normalizedData)

  if ("Rank" %in% names(.data)) {
    cat("Column Rank is already present, renaming to OldRank.", "\n")
    .data <- .data %>% dplyr::select(-dplyr::matches("OldRank")) %>% dplyr::rename(OldRank = Rank)
  }
  optresults <- dplyr::bind_cols(.data, ecr::doNondominatedSorting(t(optData))) %>%
    dplyr::rename(Rank = ranks) %>%
    dplyr::select(-dom.counter)

  return(optresults)
}


#' Normalize values in the optimization data.
#'
#' @param .data an OptResults class
#' @param objectives Optimization goals
#' @param limits Named character vector with min and max values for each objective
#'
#' @return Normalized values
normalizeValues <- function(.data, objectives = attr(.data, "objectives"), limits=NULL) {
  # Make sure that the values are normalized and handled depending on maximize or minimize
  if (is.null(objectives)) {
    objectives <- attr(.data, "objectives")
  }

  # Normalize and invert if necessary
  # Start with inverting maximization goals
  # for (i in seq(length(objectives))) {
  #   if (objectives[i]) {
  #     .data[,i] <- 1/.data[,i]
  #   }
  # }

  if(is.null(limits)){
    # Normalize by standard range
    for (obj_name in names(objectives)) {
      .data[,obj_name] <- BBmisc::normalize(.data[,obj_name], method = "range", margin = 2)
    }
  } else {
    # Normalize by custom limits
    for (obj_name in names(objectives)) {
      .data[,obj_name] <- BBmisc::normalize(.data[,obj_name], method = "range", margin = 2, range = c(limits[,obj_name][1],limits[,obj_name][2]))
    }
  }
  .data
}

# denormalizeValues <- function(.data, objectives, limits) {
#   for (obj_name in names(objectives)) {
#     max_ <- max(.data[,obj_name])
#     min_ <- min(.data[,obj_name])
#     a <- (limits[2,obj_name]-limits[1,obj_name])/(max_ - min_)
#     b <- limits[2,obj_name] - a * max_
#     .data[,obj_name] <- a * .data[,obj_name] + b
#   }
#
#   .data
# }
