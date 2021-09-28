#' Adding the distance-based metric
#'
#' Apply the distance-based metric, with a new column Distance
#' Find the minimum RSME to a Pareto-optimal solution
#' Pareto-optimal solutions have Rank 1, placed in Y
#' @param .data The optimization data to use, should be imported with [load_dataset()]
#'
#' @param pareto_solutions Iteration number to use as interpolant, default NULL
#' @param parallel_cores Number of cores to use for calculation. Default 0, a positive value will enable parallelization.
#'
#' @importFrom foreach %dopar%
#' @export
add_distances <- function(.data, pareto_solutions = NULL, parallel_cores = 0) {
  objectives = attr(.data, "objectives")[.data$objectives %in% names(.data)]
  limits <- sapply(.data[, names(objectives)], function(x) { c(min(x), max(x)) })
  .data[, names(objectives)] <- normalize_values(.data[, names(objectives)],
                                               objectives)

  # Extract the interpolant, i.e., the non-dominated solutions.
  # If pareto_solutions are supplied, only use those as interpolants
  if (!is.null(pareto_solutions)) {
    .data <- .data %>%
      dplyr::mutate(Rank = replace(Rank, !(Iteration %in% pareto_solutions) & Rank == 1, 2))
  }

  interpolant <- .data %>%
    dplyr::filter(Rank == 1) %>%
    dplyr::select(names(objectives))

  if (parallel_cores > 0) {
    # The dominated solutions
    cont <- .data %>%
      dplyr::filter(Rank > 1)

    # For progress bar
    progcombine <- function() {
      pb <- utils::txtProgressBar(min = 1,
                                  max = nrow(cont) - 1,
                                  style = 3)
      count <- 0
      function(...) {
        count <<- count + length(list(...)) - 1
        utils::setTxtProgressBar(pb, count)
        utils::flush.console()
        c(...)
      }
    }

    doParallel::registerDoParallel(parallel_cores)

    res <- foreach::foreach(i = iterators::icount(nrow(cont)),
                            .combine = progcombine()) %dopar% {
      euclidean_distance_vector(cont[i, names(objectives)], interpolant, objectives)
    }
    doParallel::stopImplicitCluster()

    # Combine the data again
    cont <- cont %>% dplyr::mutate(Distance = res)
    .data <- .data %>%
      dplyr::filter(Rank == 1) %>%
      dplyr::mutate(Distance = 0) %>%
      rbind(., cont) %>%
      dplyr::arrange(Iteration)

  } else {
    .data <- .data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Distance =
                      (Rank != 1) * euclidean_distance_vector(dplyr::cur_data(),
                                                              interpolant,
                                                              objectives)) %>%
      dplyr::ungroup()
  }

  .data[, names(objectives)] <- .data[, names(objectives)] %>% normalize_values(objectives, limits)
  .data
}

# Private functions
# Solutions is a data.frame supplying the current value to evaluate against all values in interpolant
euclidean_distance_vector <- function(solutions, interpolant, objectives) {
  solutions <- solutions %>% dplyr::select(names(objectives))
  solutions <- solutions[rep(seq_len(nrow(solutions)), times=nrow(interpolant)),]
  min(sqrt(rowSums((solutions-interpolant)^2)))
}
