# plotting

plot_parcoords <- function(.data, selection = c(Distance,attr(.data, "inputs"),names(attr(.data,"objectives")))) {
  .data %>%
    dplyr::select(selection) %>%
    plotly::plot_ly(type = 'parcoords',
                    line = list(color = ~Distance),
                    #colorscale = list(c(0,'red'),c(0.5,'green'),c(1,'blue'))),
                    dimensions = create_dimensions_list(.)
                    # dimensions = Map(function(x, y) {
                    #   list(values = x, range = range(x), label=y)
                    # }, ., names(.), USE.NAMES = FALSE)
    ) %>%
    plotly::toWebGL()
}

# Creates a list to be used as input to the parallel coordinates chart
create_dimensions_list <- function(.data) {
  obj <- c(attr(.data, "inputs"),names(attr(.data,"objectives")))
  params <- colnames(.data)
  dim_list <- vector("list", length(params))
  for (i in 1:length(params)) {
    if (params[i] %in% obj){
      dim_list[[i]] <- list(range = range(.data[[params[i]]]),
                            label = params[i], values = stats::formula(paste("~",params[i])))
    }
  }
  return(dim_list[lengths(dim_list) != 0])
}

# plot tree
plot_tree <- function(tree, save) {
  if (save) {
    setEPS()
    postscript(file=paste0(deparse(substitute(tree)),".eps"))
    rattle::fancyRpartPlot(tree, type = 5, sub = "")
    dev.off()
  }
  else {
    rattle::fancyRpartPlot(tree, type = 5, sub = deparse(substitute(tree)))
  }
}

plot_splom <- function(.data) {
  .data %>%
    plotly::plot_ly(type="splom", dimensions=list(
      list(label="maxOut", values=~maxOut),
      list(label="minLT_Plant", values=~minLT_Plant),
      list(label="minLeanBuffer", values=~minLeanBuffer),
      list(label="minWaitingParts", values=~minWaitingParts)
    ),
    #text=~Cluster,
    color=~Cluster,
    marker = list(
      #color = as.integer(.data$Cluster),
      #colorscale = ,
      size = 7
    ),
    showupperhalf = FALSE,
    diagonal= list(visible=FALSE))
}
