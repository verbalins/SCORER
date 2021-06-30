# Apply the distance-based metric, with a new column Distance
#   Find the minimum RSME to a Pareto-optimal solution
#     Pareto-optimal solutions have Rank 1, placed in Y
#' @importFrom foreach %dopar%
#' @export
addDistances <- function(.data, paretoSolutions = NULL, parallelCores = 6) {
  objectives = attr(.data, "objectives")[.data$objectives %in% names(.data)]
  limits <- sapply(.data[,names(objectives)], function(x) {c(min(x), max(x))})
  .data[,names(objectives)] <- normalizeValues(.data[,names(objectives)], objectives) # Normalize objectives

  # Extract the interpolant, i.e., the non-dominated solutions.
  # If paretosolutions are supplied, only use those as interpolants
  if (!is.null(paretoSolutions)) {
      .data <- .data %>%
        dplyr::mutate(Rank = replace(Rank, !(Iteration %in% paretoSolutions) & Rank == 1, 2))
  }

  interpolant <- .data %>%
    dplyr::filter(Rank == 1) %>%
    dplyr::select(names(objectives))

  if (parallelCores > 0){
    # The dominated solutions
    cont <- .data %>%
      dplyr::filter(Rank > 1)

    # For progress bar
    progcombine <- function(){
      pb <- utils::txtProgressBar(min=1, max=nrow(cont)-1,style=3)
      count <- 0
      function(...) {
        count <<- count + length(list(...)) - 1
        utils::setTxtProgressBar(pb,count)
        utils::flush.console()
        c(...)
      }
    }

    cl <- parallel::makeCluster(parallelCores)
    doParallel::registerDoParallel(cl)

    res <- foreach::foreach(i=iterators::icount(nrow(cont)), .combine=progcombine(), .packages = c("dplyr")) %dopar% {
      euclidean_distance_vector(cont[i,names(objectives)], interpolant, objectives)
    }

    doParallel::stopImplicitCluster()
    parallel::stopCluster(cl)

    # Combine the data again
    cont <- cont %>% dplyr::mutate(Distance = res)
    .data <- .data %>%
      dplyr::filter(Rank == 1) %>%
      dplyr::mutate(Distance = 0) %>%
      rbind(., cont) %>%
      dplyr::arrange(Iteration)

  } else { # Causes the class to be reset, needs to be fixed TODO
    .data <- .data %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Distance = (Rank != 1) * euclidean_distance_vector(dplyr::cur_data(), interpolant, objectives)) %>%
      dplyr::ungroup()
  }

  .data[,names(objectives)] <- .data[,names(objectives)] %>% normalizeValues(objectives, limits) # Return original values
  .data
}

# Private functions
# Solutions is a data.frame supplying the current value to evaluate against all values in interpolant
euclidean_distance_vector <- function(solutions, interpolant, objectives) {
  solutions <- solutions %>% dplyr::select(names(objectives))
  solutions <- solutions[rep(seq_len(nrow(solutions)), times=nrow(interpolant)),]
  min(sqrt(rowSums((solutions-interpolant)^2)))
}
