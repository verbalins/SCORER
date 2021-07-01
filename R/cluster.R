# Determine number of clusters
#df <- FSMJ %>% addDistances(parallelcores = 8)
#t <- df %>% dplyr::select(Distance,c(names(attr(.,"objectives"))))
# wss <- (nrow(t)-1)*sum(apply(t,2,var))
# for (i in 2:15) wss[i] <- sum(kmeans(t,
#                                      centers=i)$withinss)
# plot(1:15, wss, type="b", xlab="Number of Clusters",
#      ylab="Within groups sum of squares")

workspace <- function() {
  df <- FSMJ %>% add_distances(parallel_cores = 10)
  df <- df %>% partitioning(parameters = c("Distance",names(attr(., "objectives"))))
  #t %>% head(1000) %>% NbClust::NbClust(data = ., diss = NULL, distance = "euclidean",
  #                                      min.nc = 2, max.nc = 15, method = "kmeans")

  plotly::plot_ly(x = ~maxOut,
                  y = ~minLT_Plant,
                  z = ~minLeanBuffer,
                  #symbol = ~Cluster,
                  #color = ~minWaitingParts,
                  color = ~Cluster,
                  data = df,
                  type = "scatter3d",
                  mode = "markers",
                  size = 4,
                  opacity = 0.7)

  df %>%
    head(1000) %>%
    dplyr::select(Distance, Cluster, c(attr(., "inputs"), names(attr(., "objectives")))) %>%
    plotly::plot_ly(type = "parcoords", line = list(color = ~Cluster),
                    dimensions = create_dimensions_list(.))
}

tidy_clust <- function(.dat, nrClusters = 10) {
  kclusts <- tibble::tibble(k = 1:10) %>%
    dplyr::mutate(
      kclust = purrr::map(k, ~kmeans(.dat, .x)),
      tidied = purrr::map(kclust, broom::tidy),
      glanced = purrr::map(kclust, broom::glance),
      augmented = purrr::map(kclust, broom::augment, .dat)
    )

  clusters <- kclusts %>% tidyr::unnest(cols = c(tidied))
  assignments <- kclusts %>% tidyr::unnest(cols = c(augmented))
  clusterings <- kclusts %>% tidyr::unnest(cols = c(glanced))

  ggplot2::ggplot(clusterings, ggplot2::aes(k, tot.withinss)) +
    ggplot2::geom_line() +
    ggplot2::geom_point()

  ggplot2::ggplot(assignments, ggplot2::aes(x = minLeanBuffer, y = maxOut)) +
    ggplot2::geom_point(ggplot2::aes(color = .cluster), alpha = 0.8) +
    ggplot2::facet_wrap(~ k)
}

partitioning <- function(.data, nrClusters = 5, parameters = c(names(attr(.data, "objectives")))) {
  t <- .data %>% dplyr::select(parameters)

  # TODO: Help with determining cluster size
  fit <- kmeans(t, nrClusters, nstart = 10)

  .data <- .data %>% dplyr::mutate(Cluster = fit$cluster)
}
