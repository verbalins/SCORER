# Plotting functions for SCORE analysis.

#' Creates a frequency chart from the supplied data set
#'
#' @param .data The data, preferably loaded from [load_dataset()]
#' @param objectives Which objectives to use, defaults to all
#' @param inputs The inputs to use, defaults to all optimization inputs
#' @param rank The rank of the included solutions, inclusive
#' @param n The number of bottlenecks to include
#'
#' @export
#' @return A ggplot2 object showing the frequencies of parameters
#' @importFrom ggplot2 ggplot aes geom_col labs expansion element_text
#' @importFrom scales percent
#' @importFrom ggtext geom_richtext
freqchart <-
  function(.data,
           objectives = .data$objective_names,
           inputs = .data$inputs,
           rank = 1,
           n = 10) {

    filtered <- .data %>%
      dplyr::filter(Rank <= rank) %>%
      dplyr::distinct_at(dplyr::vars(objectives,
                                     inputs,
                                     Rank),
                         .keep_all = T) %>% # Always unique solutions to a SCORE problem.
      dplyr::select(inputs)

    frequencies <- filtered %>%
      tidyr::gather() %>%
      dplyr::group_by(key) %>%
      dplyr::summarize(freq = sum(value) / nrow(filtered)) %>%
      dplyr::arrange(dplyr::desc(freq))

    frequencies %>%
      utils::head(n) %>%
      ggplot2::ggplot(ggplot2::aes(
        x = stats::reorder(key, -freq),
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
#' @param data A dataset loaded with [load_dataset()], and has ranks applied through [ndsecr()]
#' @param objectives A character vector indicating the objectives to show, defaults to first two objectives
#' @param rank Set the ranking to include when creating the pareto optimal front. Default 1, NULL removes.
#' @param interactive Should an interactive plotly chart be created? Defaults to false.
#' @return A ggplot2 object showing the fronts of the optimization
#' @importFrom stats as.formula
#' @importFrom dplyr filter select arrange if_else vars distinct_at
#' @importFrom ggplot2 ggplot geom_point geom_text geom_line scale_y_continuous theme_classic element_text
plot_pareto <- function(.data, objectives = .data$objective_names, rank = 1, interactive = FALSE) {
    labelnames <- c("Improvements", "Output", "Lead Time")
    names(labelnames) <- c("minImp", "maxOut", "minLT")

    if (interactive) {
      if (length(objectives) < 3) {
        p <- .data %>%
          plotly::plot_ly(
            x = stats::as.formula(paste0("~", objectives[1])),
            y = stats::as.formula(paste0("~", objectives[2])),
            color = ~ Rank,
            type = "scattergl",
            mode = "markers",
            hoverinfo = "text",
            text = ~ paste(
              "</br> Improvements: ", minImp,
              "</br> Out: ", round(maxOut),
              "</br> Rank: ", Rank
            ),
            name = "Rank 1-5"
          )
        if (length(objectives) == 2 & !is.null(rank)) {
          p <- p %>% plotly::add_trace(
            data = .data %>%
              dplyr::filter(Rank == rank) %>%
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
          x = stats::as.formula(paste0("~", objectives[1])),
          y = stats::as.formula(paste0("~", objectives[2])),
          z = stats::as.formula(paste0("~", objectives[3])),
          color = ~ Rank,
          mode = "markers",
          type = "scatter3d",
          size = 2,
          text = ~ paste(
            "</br> Improvements: ", minImp,
            "</br> Out: ", round(maxOut),
            "</br> LT: ", round(minLT / 3600, 2), "h",
            "</br> Rank: ", Rank
          )
        )
      }
      p
    } else {
      chart <- .data %>%
        dplyr::distinct_at(dplyr::vars(all_of(objectives),
                                       .$inputs,
                                       Rank),
                           .keep_all = T) %>% # Always unique solutions to a SCORE problem.
        ggplot2::ggplot(ggplot2::aes_string(
          x = dplyr::if_else("minImp" %in% objectives, "minImp", objectives[1]),
          y = dplyr::if_else("maxOut" %in% objectives, "maxOut", objectives[2]),
          color = "Rank"
        )) +
        ggplot2::geom_point() +
        ggplot2::scale_y_continuous(n.breaks = 6) +
        ggplot2::theme_classic(base_size = 13, base_family = "sans") +
        ggplot2::labs(x = dplyr::if_else(objectives[1] %in% names(labelnames), labelnames[objectives[1]], objectives[1]),
                      y = dplyr::if_else(objectives[2] %in% names(labelnames), labelnames[objectives[2]], objectives[2])) +
        ggplot2::theme(
          axis.title.x = ggplot2::element_text(vjust = -0.5),
          axis.title.y = ggplot2::element_text(vjust = 2)
        )

      if (!is.null(rank)) {
        chart <- chart + ggplot2::geom_line(data = .data %>% dplyr::filter(Rank == rank),
                                          color = "red")
      }

      if ("minLT" %in% objectives) {
        chart <- chart + ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "h"))
      }
      chart
  }
}

#' Exports a plot created by [freqchart()] or [plot_pareto()]
#'
#' @export
#' @param filename The name of the resulting file
#' @param device png, pdf, or svg to ggsave or tex for tikz. Supply vector to save as multiple files.
#' @importFrom ggplot2 ggsave last_plot
#' @importFrom tikzDevice tikz
#' @importFrom grDevices dev.off
export_plot <-
  function(.plot = ggplot2::last_plot(),
           filename,
           path = getwd(),
           device = "pdf",
           width = 15,
           height = 7,
           ...) {

    if (length(device) > 1) {
      for (dev in device) {
        export_plot(.plot,
                    path = path,
                    filename = filename,
                    device = dev,
                    width = width,
                    height = height,
                    ...)
      }
    } else {
      if (device == "pdf" || device == "png" || device == "svg") {
        if (device == "svg") {
          require(svglite)
          ggplot2::ggsave(
            filename = paste0(filename, ".svg"),
            plot = .plot,
            path = path,
            device = device,
            width = width,
            height = height,
            units = "cm",
            ...
          )
        } else {
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
  }
