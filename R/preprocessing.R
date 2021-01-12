# Apply the distance-based metric, with a new column Distance
#   Find the minimum RSME to a Pareto-optimal solution
#     Pareto-optimal solutions have Rank 1, placed in Y
addDistances <- function(.data, parallelCores = 0) {
  objectives = attr(.data, "objectives")
  limits <- sapply(.data[,names(objectives)], function(x) {c(min(x), max(x))})
  .data[,names(objectives)] <- normalizeValues(.data[,names(objectives)], objectives)

  # Extract the interpolant, i.e., the non-dominated solutions.
  interpolant <- .data %>%
    dplyr::filter(Rank == 1) %>%
    dplyr::select(names(attr(.data, "objectives")))

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

    doParallel::registerDoParallel(parallelCores)
    "%dopar%"<- foreach::"%dopar%"
    res <- foreach::foreach(i=iterators::icount(nrow(cont)), .combine=progcombine()) %dopar% {
      euclidean_distance_vector(cont[i,names(objectives)], interpolant)
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
      dplyr::mutate(Distance = (Rank != 1) * euclidean_distance_vector(dplyr::cur_data(), interpolant)) %>%
      dplyr::ungroup()
  }

  .data[,names(objectives)] <- .data[,names(objectives)] %>% normalizeValues(objectives, limits)
  .data
}

# Private functions
euclidean_distance_vector <- function(x,y) {
  x <- x %>% dplyr::select(names(attr(y, "objectives")))
  x <- x[rep(seq_len(nrow(x)), times=nrow(y)),]
  min(sqrt(rowSums((x-y)**2)))
}
