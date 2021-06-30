# plotting

plot_parcoords <- function(.data,
                           selection = c(Distance,
                                         attr(.data, "inputs"),
                                         names(attr(.data, "objectives")))) {
  .data %>%
    dplyr::select(selection) %>%
    plotly::plot_ly(type = "parcoords",
                    line = list(color = ~Distance),
                    dimensions = create_dimensions_list(.)
    ) %>%
    plotly::toWebGL()
}

# Creates a list to be used as input to the parallel coordinates chart
create_dimensions_list <- function(.data) {
  obj <- names(attr(.data, "objectives"))
  inputs <- attr(.data, "inputs")
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

# plot tree
plot_tree <- function(tree, save) {
  if (save) {
    setEPS()
    postscript(file = paste0(deparse(substitute(tree)), ".eps"))
    rattle::fancyRpartPlot(tree, type = 5, sub = "")
    dev.off()
  }
  else {
    rattle::fancyRpartPlot(tree, type = 5, sub = deparse(substitute(tree)))
  }
}

plot_pca <- function(.data) {
  pca  <- prcomp(FSMJ_dist %>% dplyr::select(Distance, attr(., "inputs")))
  fit <- fastcluster::hclust(dist(pca$x[, 1:3]))
  groups <- cutree(fit, k = 4)

  plotly::plot_ly() %>%
    plotly::add_trace(
      x = pca$x[, 1],
      y = pca$x[, 2],
      z = pca$x[, 3],
      color = groups,
      size = 4,
      opacity = 0.7,
      type = "scatter3d",
      mode = "markers") %>%
    plotly::toWebGL()
}

plot_pca_princomp <- function(.data) {
  scores <- .data$scores
  x <- scores[, 1]
  y <- scores[, 2]
  z <- scores[, 3]

  loads <- .data$loadings

  scale.loads <- 5

  p <- plotly::plot_ly() %>%
    plotly::add_trace(x = x, y = y, z = z,
                      type = "scatter3d",
                      mode = "markers",
                      marker = list(color = y,
                                    colorscale = c("#FFE1A1", "#683531"),
                                    opacity = 0.7))
  for (k in seq_len(nrow(loads))) {
    x <- c(0, loads[k, 1]) * scale.loads
    y <- c(0, loads[k, 2]) * scale.loads
    z <- c(0, loads[k, 3]) * scale.loads
    p <- p %>% plotly::add_trace(x = x, y = y, z = z,
                                 type = "scatter3d", mode = "lines",
                                 line = list(width = 8),
                                 opacity = 1)
  }
  print(p %>% plotly::toWebGL())
}

plot_splom <- function(.data) {
  .data %>%
    plotly::plot_ly(type = "splom", dimensions = list(
      list(label = "maxOut", values = ~maxOut),
      list(label = "minLT_Plant", values = ~minLT_Plant),
      list(label = "minLeanBuffer", values = ~minLeanBuffer),
      list(label = "minWaitingParts", values = ~minWaitingParts)
    ),
    #text=~Cluster,
    color = ~Cluster,
    marker = list(
      #color = as.integer(.data$Cluster),
      #colorscale = ,
      size = 7
    ),
    showupperhalf = FALSE,
    diagonal = list(visible = FALSE))
}

plot2d <- function(.data, x, y, color, unselected_data=NULL, ...) {
  p <- plotly::plot_ly(type = "scattergl",
                  x = stats::as.formula(paste0("~", x)),
                  y = stats::as.formula(paste0("~", y)),
                  color = stats::formula(paste0("~", color)),
                  customdata = ~Iteration,
                  data = .data,
                  mode = "markers",
                  size = I(30),
                  opacity = 1,
                  hovertemplate = paste(paste0("<b>", x, "</b>: %{x}"),
                                        paste0("<br><b>", y, "</b>: %{y}"),
                                        paste0("<br><b>Iteration</b>: %{customdata}<extra></extra>")),
                  ...)
  if (is.null(unselected_data)) {
    p
  } else {
    p %>% addunselected(unselected_data)
  }
}

plot3d <- function(.data, x, y, z, color, unselected_data = NULL, ...) {
  suppressWarnings(
    p <- plotly::plot_ly(type = "scatter3d",
                    x = stats::as.formula(paste0("~", x)),
                    y = stats::as.formula(paste0("~", y)),
                    z = stats::as.formula(paste0("~", z)),
                    color = stats::formula(paste0("~", color)),
                    customdata = ~Iteration,
                    hovertemplate = paste(paste0("<b>", x, "</b>: %{x}"),
                                          paste0("<br><b>", y, "</b>: %{y}"),
                                          paste0("<br><b>", z, "</b>: %{z}"),
                                          paste0("<br><b>Iteration</b>: %{customdata}<extra></extra>")),
                    mode = "markers",
                    size = I(30),
                    opacity = 1,
                    data = .data,
                    ...))

  if (is.null(unselected_data)) {
      p
  } else {
    p %>% addunselected(unselected_data)
  }
}

addunselected <- function(.plot, unselected_data) {
  .plot %>% plotly::add_trace(
    color = I("gray"),
    size = I(10),
    opacity = 0.2,
    data = unselected_data,
    showlegend = F) %>%
    plotly::layout(updatemenus = list(
      list(
        type = "buttons",
        x = 1,
        buttons = list(
          list(method = "restyle",
               args = list("visible", c(TRUE, TRUE)),
               args2 = list("visible", c(TRUE, FALSE)),
               label = "Toggle filtered"))))
    )
}
