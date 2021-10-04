# Plotting functions for shiny and regular visualizations.

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
  pca  <- prcomp(FSMJ_dist %>% dplyr::select("Distance", .$inputs))
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

plot2d <- function(.data, x, y, color = "Rank", unselected_data = NULL, ...) {
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

plot3d <- function(.data, x, y, z, color = "Rank", unselected_data = NULL, ...) {
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

#' Plot multi-objective optimization data
#'
#' Plot data in either two or three dimensions using plotly.
#'
#' @param .data The dataset generated by [load_dataset()].
#'
#' @param ... Additional parameters to the function. Possible to define x, y, z, and color parameters.
#' If the dimensions are not set, they are read from .data.
#' Non-matched arguments will be further piped to [plotly::plot_ly()].
#'
#' @author Simon Lidberg
#'
#' @examples
#' # Plot in three dimensions with "Rank" as color parameter and
#' # default axis read from .data$objective_names.
#' plotnd(SCORER::FMC)
#'
#' # All objectives defined, along with color parameter.
#' plotnd(SCORER::FMC, x = "TP", y = "Investment", z = "LeanBuffer", color = "Iteration")
#'
#' @export
plotnd <- function(.data, ...) {
  stopifnot("You need more than one objective to plot" = length(.data$objective_names) > 1) # Requires two objectives as minimum.
  #arguments <- match.call(expand.dots = TRUE)
  arguments <- list(...)

  objectives <- .data$objective_names
  plotting_objectives <- list(x = NULL, y = NULL, z = NULL, color = "Rank")

  # Check for supplied arguments
  for (argname in c("x", "y", "z", "color")) {
    if (!is.null(arguments[[argname]])) {
      plotting_objectives[argname] <- arguments[[argname]]
      objectives <- objectives[!(objectives %in% arguments[[argname]])]
      arguments[argname] <- NULL
    }
  }

  # Set correct defaults.
  for (argname in c("x", "y", "z", "color")) {
    if (is.null(plotting_objectives[[argname]])) {
      plotting_objectives[[argname]] <- objectives[1]
      objectives <- objectives[!(objectives %in% plotting_objectives[[argname]])]
    }
  }

  if (length(.data$objective_names) == 2) {
    do.call(SCORER::plot2d, append(arguments, list(.data = .data,
                   x = plotting_objectives[["x"]],
                   y = plotting_objectives[["y"]],
                   color = plotting_objectives[["color"]])))
  } else {
    do.call(SCORER::plot3d, append(arguments, list(.data = .data,
                   x = plotting_objectives[["x"]],
                   y = plotting_objectives[["y"]],
                   z = plotting_objectives[["z"]],
                   color = plotting_objectives[["color"]])))
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
