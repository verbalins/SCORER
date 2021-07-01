# Apply the distance-based metric, with a new column Distance
#   Find the minimum RSME to a Pareto-optimal solution
#     Pareto-optimal solutions have Rank 1, placed in Y
#' @importFrom foreach %dopar%
add_distances <- function(.data, parallel_cores = 0) {
  objectives <- attr(.data, "objectives")
  limits <- sapply(.data[, names(objectives)], \(x) {c(min(x), max(x))})
  .data[, names(objectives)] <- normalizeValues(.data[, names(objectives)],
                                               objectives)

  # Extract the interpolant, i.e., the non-dominated solutions.
  interpolant <- .data %>%
    dplyr::filter(Rank == 1) %>%
    dplyr::select(names(attr(.data, "objectives")))

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
      euclidean_distance_vector(cont[i, names(objectives)], interpolant)
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
                                                              interpolant)) %>%
      dplyr::ungroup()
  }

  .data[, names(objectives)] <- .data[, names(objectives)] %>% normalizeValues(objectives, limits)
  .data
}

# Private functions
# Solutions is a data.frame supplying the current value to evaluate against all values in interpolant
euclidean_distance_vector <- function(solutions, interpolant) {
  solutions <- solutions %>% dplyr::select(names(attr(interpolant, "objectives")))
  solutions <- solutions[rep(seq_len(nrow(solutions)), times = nrow(interpolant)), ]
  min(sqrt(rowSums((solutions - interpolant) ** 2)))
}
