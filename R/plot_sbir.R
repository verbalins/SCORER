# plotting

plot_parcoords <- function(.data){
  .data %>% plotly::plot_ly(type = 'parcoords',
                            line = list(color = ~Distance,
                                        colorscale = list(c(0,'red'),c(0.5,'green'),c(1,'blue'))),
                            dimensions = create_dimensions_list(.data)
                            )
}

plot_parcoords_fixed <- function(.data){
  .data %>% plotly::plot_ly(type = 'parcoords',
                            line = list(color = ~Distance,
                                        colorscale = list(c(0,'red'),c(0.5,'green'),c(1,'blue'))),
                            dimensions = list(
                              list(range = c(min(.$"maxOut"),max(.$"maxOut")),
                                   label = 'maxOut', values = ~maxOut),
                              list(range = c(min(.$"minLeanBuffer"),max(.$"minLeanBuffer")),
                                   label = 'minLeanBuffer', values = ~minLeanBuffer),
                              list(range = c(min(.$"minWaitingParts"),max(.$"minWaitingParts")),
                                   label = 'minWaitingParts', values = ~minWaitingParts),
                              list(range = c(min(.$"minLT_Plant"),max(.$"minLT_Plant")),
                                   label = 'minLT_Plant', values = ~minLT_Plant),
                              list(range = c(min(.$"Shift"),max(.$"Shift")),
                                   label = 'Shift', values = ~Shift)
                            )
  )
}

# Creates a list to be used as input to the parallel coordinates chart
create_dimensions_list <- function(.data) {
  params <- c(attr(.data, "inputs"),names(attr(.data,"objectives")))
  dim_list <- vector("list", length(params))
  for (i in 1:length(params)) {
    dim_list[[i]] <- create_parcoords_param(.data, params[i])
  }
  return(dim_list)
}

create_parcoords_param <- function(.data, param) {
  res <- list(range = c(min(.data[[param]]),max(.data[[param]])),
       label = param, values = stats::formula(paste("~",param)))

  return(res)
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
