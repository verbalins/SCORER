# UI for the visualization tab
mod_visualization_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    shiny::fillPage(
      shiny::fillPage(
        shinycssloaders::withSpinner(plotly::plotlyOutput(ns("parcoords"))),
        shiny::tabsetPanel(id=ns("chartTab"), type = "tabs",
                           shiny::tabPanel("3D",
                                           shiny::fluidRow(
                                             shiny::column(10,
                                                           tags$style(type = "text/css", "#scatter3d {height: calc(100vh - 80px) !important;}"),
                                                           shinycssloaders::withSpinner(plotly::plotlyOutput(ns("scatter3d"), height = "100%", width = "100%"))),
                                             shiny::column(2,
                                                           shiny::inputPanel(
                                                             shiny::uiOutput(ns("x")),
                                                             shiny::uiOutput(ns("y")),
                                                             shiny::uiOutput(ns("z")),
                                                             shiny::uiOutput(ns("color")),
                                                             shiny::uiOutput(ns("colorslider"))))
                                           )),
                           tabPanel("2D",
                                    shinycssloaders::withSpinner(shiny::uiOutput(ns("plots2d")))
                           )
        )
      )
    )
  )
}

mod_visualization_server <- function(id, r) {
  shiny::moduleServer(id,
    function(input, output, session) {
      ns <- session$ns

      dim_list <- shiny::reactive({
        list(Objectives=unique(r$filtered_data$objectives),
             Inputs=unique(r$filtered_data$inputs),
             Outputs=unique(r$filtered_data$outputs),
             Filter=unique(grep("Rank|Distance|Cluster",colnames(r$filtered_data),value = TRUE)))
      })

      output$x <- shiny::renderUI({
        ns <- session$ns
        shiny::selectInput(ns("x"), "X:", dim_list(), selectize = FALSE, selected=dim_list()$Objectives[[1]])
      })

      output$y <- shiny::renderUI({
        ns <- session$ns
        shiny::selectInput(ns("y"), "Y:", dim_list(), selectize = FALSE, selected=dim_list()$Objectives[[2]])
      })

      output$z <- shiny::renderUI({
        ns <- session$ns
        shiny::selectInput(ns("z"), "Z:", dim_list(), selectize = FALSE, selected=dim_list()$Objectives[[3]])
      })

      output$color <- shiny::renderUI({
        ns <- session$ns
        choice <- grep("Rank|Distance|Cluster",colnames(r$filtered_data),value = TRUE)
        shiny::selectInput(ns("color"), "Color:", choice, selectize = FALSE)
      })

      r$plotdims <- shiny::reactive(list(x = input$x,
                                    y = input$y,
                                    z = input$z,
                                    color = input$color))

      output$colorslider <- shiny::renderUI({
        req(input$color)
        ns <- session$ns
        min_val <- 0
        max_val <- 0

        if(input$color %in% colnames(r$filtered_data)) {
          min_val <- floor(min(r$filtered_data[[input$color]]))
          max_val <- round(max(r$filtered_data[[input$color]]),2)
          stepsize <- switch(input$color, "Rank" = 1, "Distance" = NULL, "Cluster" = 1)
          shiny::sliderInput(ns("colorslider"), "Filter Color:", min_val, max_val, value=c(min_val, max_val), round=-2, step = stepsize)
        }
      })

      # Render the parallel coordinates plot
      output$parcoords <- plotly::renderPlotly({
        req(input$color)
        suppressWarnings(
          plotly::plot_ly(type = 'parcoords',
                          dimensions = create_dimensions_list(r$filtered_data),
                          line=list(color = stats::formula(paste0("~",shiny::isolate(input$color)))),
                          source = "pcoords",
                          data = r$filtered_data) %>%
            plotly::event_register(event="plotly_restyle") #%>%
            #plotly::toWebGL()
        )
      })

      #React to changes to the colouring variable
      # shiny::observeEvent(input$color, {
      #     if(input$color %in% colnames(r$filtered_data)) {
      #         isolate({
      #         form <- stats::formula(paste0("~",input$color))
      #         p <- plotly::plotlyProxy("scatter3d", session)
      #         test <- r$df_selected()$sel[,input$color]
      #         if (!is.null(cam_3d$scene)) {
      #             #p %>% plotly::plotlyProxyInvoke("relayout", list(scene = list(camera = cam_3d$scene)))
      #             p %>% plotly::plotlyProxyInvoke("relayout",list(scene.camera = cam_3d$scene)) %>%
      #                 plotly::plotlyProxyInvoke("relayout", list(color = test[[1]]))
      #         }
      #         #p %>% plotly::plotlyProxyInvoke("restyle", list(color = form))
      #
      #         })
      #     }
      # })

      # cam_3d <- shiny::reactiveValues(scene=NULL)

      # shiny::observeEvent(plotly::event_data("plotly_relayout", source = "s3d"), {
      #     scene <- plotly::event_data("plotly_relayout", source = "s3d")
      #     cam_3d$scene <- list(up=scene$scene.camera$up,
      #                               center=scene$scene.camera$center,
      #                               eye=scene$scene.camera$eye,
      #                               projection=scene$scene.camera$projection)
      # })

      # Render the scatter3d plot
      output$scatter3d <- plotly::renderPlotly({
        req(input$x, input$y, input$z)
        SCORER::plot3d(r$df_selected()$sel, input$x, input$y, input$z, input$color, r$df_selected()$unsel, height=600, source="s3d")

      })

      # maintain a collection of selection ranges
      # since each parcoord dimension is allowed to have multiple
      # selected ranges, this reactive values data structure is
      # allowed
      # list(
      #  var1 = list(c(min1, max1), c(min2, max2), ...),
      #  var2 = list(c(min1, max1)),
      #  ...
      # )
      ranges <- shiny::reactiveValues('data' = list())
      shiny::observeEvent(plotly::event_data("plotly_restyle", source = "pcoords"), {
        d <- plotly::event_data("plotly_restyle", source = "pcoords")
        if (is.null(d[[1]]) || names(d[[1]]) == "line" || names(d[[1]]) == "dimensions"){
          # Reordering dimensions or reordering
          return()
        }
        # what is the relevant dimension (i.e. variable)?
        dimension <- as.numeric(stringr::str_extract(names(d[[1]]), "[0-9]+"))
        # careful of the indexing in JS (0) versus R (1)!
        dimension_name <- unlist(c(dim_list()$Objectives,dim_list()$Inputs))[[dimension + 1]]
        # a given dimension can have multiple selected ranges
        # these will come in as 3D arrays, but a list of vectors
        # is nicer to work with
        info <- d[[1]][[1]]
        if (!is.null(info) && all(info < 0)) {
          info <- NULL
        }
        ranges$data[[dimension_name]] <- if (length(dim(info)) == 3) {
          lapply(seq_len(dim(info)[2]), function(i) info[,i,])
        } else {
          list(as.numeric(info))
        }
        selected_points$data <- NULL
      })

      # filter the dataset down to the rows that match the selection ranges
      df_filterdata <- shiny::reactiveValues()
      r$df_selected <- shiny::reactive({
        keep <- TRUE
        for (i in names(ranges$data)) {
          range_ <- ranges$data[[i]]
          keep_var <- FALSE
          for (j in seq_along(range_)) {
            rng <- range_[[j]]
            if (length(rng) == 0) # When deselecting
              keep_var <- TRUE
            else {
              keep_var <- keep_var | dplyr::between(r$filtered_data[[i]], min(rng), max(rng))
            }
          }
          keep <- keep & keep_var
        }
        parcoords_sel <- r$filtered_data[keep, ]

        for (i in names(selected_points$data)) {
          parcoords_sel <- parcoords_sel %>% dplyr::filter(Iteration %in% unlist(selected_points$data[i]))
        }
        isolate({
          if(!is.null(input$colorslider) && input$color %in% colnames(r$filtered_data)) {
            df_filterdata$sel <- parcoords_sel %>%
              dplyr::filter(dplyr::between(.[[input$color]], input$colorslider[1], input$colorslider[2]))
          } else {
            df_filterdata$sel <- parcoords_sel
          }
        })
        df_filterdata$unsel <- dplyr::anti_join(r$filtered_data, df_filterdata$sel, by="Iteration")
        df_filterdata
      })

      # These reactive values track the set of active brushes
      # Each reactive value corresponds to a different variable
      selected_points <- shiny::reactiveValues('data' = list())

      output$plots2d <- shiny::renderUI({
        ns <- session$ns
        plot_output_list <- lapply(seq(1:(length(isolate(r$filtered_data$objectives))-1)), function(i) {
          plotname <- paste0("plot", i)
          plotly::plotlyOutput(ns(plotname), # ns because it's a plotlyOutput
                               width = paste0(floor((1/(length(r$filtered_data$objectives)-1))*100)-1,"%"),
                               inline = TRUE, height = "100%")
        })

        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
      })

      # Call renderPlot for each one. Plots are only actually generated when they
      # are visible on the web page.
      shiny::observeEvent({r$filtered_data}, {
        ranges$data <- NULL
        selected_points$data <- NULL
        max_plots <- isolate(length(r$filtered_data$objectives))
        for (i in seq(1:(max_plots-1))) {
          # Need local so that each item gets its own number. Without it, the value
          # of i in the renderPlot() will be the same across all instances, because
          # of when the expression is evaluated.
          local({
            my_i <- i
            plotname <- paste0("plot", my_i) # Don't use ns here
            #x = stats::formula(paste0("~",dim_list()$Objectives[[1]])),
            #y = stats::formula(paste0("~",dim_list()$Objectives[[my_i+1]])),
            output[[plotname]] <- plotly::renderPlotly({
              SCORER::plot2d(r$df_selected()$sel,
                             r$df_selected()$sel$objectives[[1]],
                             r$df_selected()$sel$objectives[[my_i+1]],
                             input$color,
                             r$df_selected()$unsel,
                             source = plotname) %>%
                #plotly::toWebGL() %>%
                plotly::hide_colorbar() %>%
                plotly::event_register("plotly_doubleclick")
            })
          })
        }
      })

      lapply(paste0("plot", seq(1, isolate(length(r$filtered_data$objectives))-1)), function(nm) {
        #ns <- session$ns
        shiny::observeEvent(plotly::event_data("plotly_selected", source=nm), {
          # inform the module about the new brush range
          selected <- plotly::event_data("plotly_selected", source=nm)
          if (is.null(selected)) {
            selected_points$data <- NULL
          } else {
            selected_points$data[[nm]] <- selected$customdata
          }
        })

        shiny::observeEvent(plotly::event_data("plotly_doubleclick", source=nm), {
          # Reset the brushing
          selected_points$data <- NULL
        })
      })

      shiny::observeEvent(input$reset, {
        # Reset the brushing
        selected_points$data <- NULL
        # plotly::plotlyProxy(nm, session) %>%
        #     plotly::plotlyProxyInvoke("restyle", "data", r$df_selected())
      })

      return(shiny::reactive(shiny::reactiveValues(data = r$df_selected(),
                  sel = r$df_selected()$sel,
                  unsel = r$df_selected()$unsel,
                  dim_list = dim_list)))
    })
}
