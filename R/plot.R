#' Creates a frequency chart from the supplied data set
#'
#' @param .data The data, preferably loaded from [loaddataset()]
#' @param objectives Which objectives to use, defaults to all
#' @param rank The rank of the included solutions, inclusive
#' @param n The number of bottlenecks to include
#'
#' @export
#' @return A ggplot2 object showing the frequencies of parameters
freqchart <-
  function(.data,
           objectives = names(attr(.data, "objectives")),
           rank = 1,
           n = 10) {
    filtered <- .data %>%
      dplyr::filter(Rank <= rank) %>%
      dplyr::distinct_at(dplyr::vars(objectives,
                                     attr(., "inputs"),
                                     Rank),
                         .keep_all = T) %>% # Always unique solutions to a SCORE problem.
      dplyr::select_at(dplyr::vars(dplyr::starts_with("imp")), function(x)
        stringr::str_replace(x, "^imp_", ""))

    frequencies <- filtered %>%
      tidyr::gather() %>%
      dplyr::group_by(key) %>%
      dplyr::summarize(freq = sum(value) / nrow(filtered)) %>%
      dplyr::arrange(desc(freq))

    frequencies %>%
      utils::head(n) %>%
      ggplot2::ggplot(ggplot2::aes(
        x = stats::reorder(key,-freq),
        y = freq,
        #label = signif(freq*100, 4))) +
        label = scales::percent(freq, accuracy = 0.1, suffix = "%")
      )) +
      ggplot2::geom_col() +
      ggtext::geom_richtext(
        mapping = ggplot2::aes(angle = 90),
        show.legend = FALSE,
        position = ggplot2::position_stack(vjust = 0.5),
        label.padding = ggplot2::unit(c(0.25, 0.25, 0.25, 0.25), "lines")
      ) +
      ggplot2::labs(y = "Frequency",
                    x = "Parameter") +
      ggplot2::scale_y_continuous(
        limits = c(0, 1),
        labels = scales::percent,
        expand = ggplot2::expansion(mult = c(0, 0))
      ) +
      ggplot2::theme_classic(base_size = 13, base_family = "sans") +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 0),
        axis.title.x = ggplot2::element_text(vjust = -0.5),
        axis.title.y = ggplot2::element_text(vjust = 2)
      )
  }

#' Creates a 2D plot to visualize the optimization results
#'
#' @export
#' @param data A dataset loaded with [loaddataset()], and has ranks applied through [ndsecr()]
#' @param objectives A character vector indicating the objectives to show, defaults to first two objectives
#' @return A ggplot2 object showing the fronts of the optimization
plotPareto <- function(.data, objectives = names(attr(.data, "objectives")), interactive = FALSE) {
    labelnames <- c("Improvements", "Output", "Lead Time")
    names(labelnames) <- c("minImp", "maxOut", "minLT")

    if (interactive) {
      if (length(objectives) < 3) {
        p <- .data %>%
          plotly::plot_ly(
            x = stats::as.formula(paste0('~', objectives[1])),
            y = stats::as.formula(paste0('~', objectives[2])),
            color = ~ Rank,
            type = "scattergl",
            mode = "markers",
            hoverinfo = "text",
            text = ~ paste(
              '</br> Improvements: ', minImp,
              '</br> Out: ', round(maxOut),
              '</br> Rank: ', Rank
            ),
            name = "Rank 1-5"
          )
        if (length(attr(.data, "objectives")) == 2) {
          p <- p %>% plotly::add_trace(
            data = .data %>%
              dplyr::filter(Rank == 1) %>%
              dplyr::select(-Iteration, objectives[1], objectives[2]) %>%
              dplyr::arrange(objectives[1]),
            type = "scattergl",
            mode = "lines",
            name = "Pareto Front",
            color = NULL
          )
        }
      } else if (length(objectives) == 3) {
      p <- .data %>% plotly::plot_ly(
        x = stats::as.formula(paste0('~', objectives[1])),
        y = stats::as.formula(paste0('~', objectives[2])),
        z = stats::as.formula(paste0('~', objectives[3])),
        color = ~ Rank,
        mode = "markers",
        type = "scatter3d",
        size = 2,
        text = ~ paste(
          '</br> Improvements: ', minImp,
          '</br> Out: ', round(maxOut),
          '</br> LT: ', round(minLT / 3600, 2), "h",
          '</br> Rank: ', Rank
        )
      )
    }
    p
  } else {
    chart <- .data %>%
      dplyr::distinct_at(dplyr::vars(all_of(objectives),
                                     attr(., "inputs"),
                                     Rank),
                         .keep_all = T) %>% # Always unique solutions to a SCORE problem.
      ggplot2::ggplot(ggplot2::aes_string(
        x = dplyr::if_else("minImp" %in% objectives, "minImp", objectives[1]),
        y = dplyr::if_else("maxOut" %in% objectives, "maxOut", objectives[2]),
        color = "Rank"
      )) +
      ggplot2::geom_point() +
      ggplot2::geom_line(data = .data %>% dplyr::filter(Rank == 1),
                         color = "red") +
      ggplot2::scale_y_continuous(n.breaks = 6) +
      ggplot2::theme_classic(base_size = 13, base_family = "sans") +
      ggplot2::labs(x = labelnames[objectives[1]],
                    y = labelnames[objectives[2]]) +
      ggplot2::theme(
        axis.title.x = ggplot2::element_text(vjust = -0.5),
        axis.title.y = ggplot2::element_text(vjust = 2)
      )

    if ("minLT" %in% objectives) {
      chart <- chart + ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "h"))
    }
    chart
  }
}

#' Exports a plot created by [freqchart()] or [plotPareto()]
#'
#' @export
#' @param filename The name of the resulting file
#' @param device pdf for pdf files or tex for tex file
exportPlot <-
  function(.plot = ggplot2::last_plot(),
           filename,
           path = getwd(),
           device = "pdf",
           width = 15,
           height = 7,
           ...) {
    if (device == "pdf" || device == "png") {
      ggplot2::ggsave(
        paste0(filename, ".", device),
        path = path,
        plot = .plot,
        device = device,
        family = "sans",
        width = width,
        height = height,
        dpi = "print",
        units = "cm",
        ...
      )
    }
    else if (device == "tex") {
      tikzDevice::tikz(
        file = paste0(path, filename, ".tex"),
        width = width,
        height = height,
        ...
      )
      print(.plot)
      grDevices::dev.off()
    }
  }
