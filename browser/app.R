#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
df <- FSMJ_clusters %>% dplyr::select(c(attr(., "inputs"),names(attr(.,"objectives")),Cluster,Distance))
d <- attr(df, "objectives")

# Define UI for application that draws a histogram
ui <- shiny::fluidPage(
    #plotly::plotlyOutput("parcoords"),
    lapply(names(d)[-1], function(nm) plotly::plotlyOutput(nm, width = "33%", inline = TRUE)),
    # fluidRow(
    #     column(12,
    #            plotly::plotlyOutput(""))
    # )
)

ui2 <- shiny::fluidPage(
    plotly::plotlyOutput("maxOut", width = "33%", inline = TRUE)
)

# Define server logic for parcoords and 3d plots
server <- function(input, output, session) {

    output$parcoords <- plotly::renderPlotly({
            plotly::plot_ly(type = 'parcoords',
                            dimensions = create_dimensions_list(.),
                            line=list(color = ~Cluster),
                            source = "pcoords",
                            data = df_selected()) %>%
            plotly::event_register(event="plotly_restyle") %>%
            plotly::toWebGL()
    })

    # output$scatter3d <- plotly::renderPlotly({
    #     df %>%
    #         plotly::plot_ly(type="scatter3d",
    #                         x = ~maxOut,
    #                         y = ~minLT_Plant,
    #                         z = ~minLeanBuffer,
    #                         #symbol = ~Cluster,
    #                         #color = ~minWaitingParts,
    #                         color = ~Cluster,
    #                         mode="markers",
    #                         size=4,
    #                         opacity=0.7,
    #                         source = "pcoords") %>%
    #         plotly::event_register(event="plotly_restyle")
    # })

    # maintain a collection of selection ranges
    # since each parcoord dimension is allowed to have multiple
    # selected ranges, this reactive values data structure is
    # allowed
    # list(
    #  var1 = list(c(min1, max1), c(min2, max2), ...),
    #  var2 = list(c(min1, max1)),
    #  ...
    # )
    ranges <- shiny::reactiveValues()
    shiny::observeEvent(plotly::event_data("plotly_restyle", source = "pcoords"), {
        d <- plotly::event_data("plotly_restyle", source = "pcoords")
        # what is the relevant dimension (i.e. variable)?
        dimension <- as.numeric(stringr::str_extract(names(d[[1]]), "[0-9]+"))
        # careful of the indexing in JS (0) versus R (1)!
        dimension_name <- names(df)[[dimension + 1]]
        # a given dimension can have multiple selected ranges
        # these will come in as 3D arrays, but a list of vectors
        # is nicer to work with
        info <- d[[1]][[1]]
        ranges[[dimension_name]] <- if (length(dim(info)) == 3) {
            lapply(seq_len(dim(info)[2]), function(i) info[,i,])
        } else {
            list(as.numeric(info))
        }

        # plotly::plotlyProxy("map", session) %>%
        #     plotly::plotlyProxyInvoke(
        #         "relayout",
        #         list(mapbox = list(style = input$style))
        #     )
    })

    # filter the dataset down to the rows that match the selection ranges
    df_selected <- shiny::reactive({
        keep <- TRUE
        for (i in names(ranges)) {
            range_ <- ranges[[i]]
            keep_var <- FALSE
            for (j in seq_along(range_)) {
                rng <- range_[[j]]
                if (length(rng) == 0) # When deselecting
                    keep_var <- TRUE
                else {
                    keep_var <- keep_var | dplyr::between(df[[i]], min(rng), max(rng))
                }
            }
            keep <- keep & keep_var
        }
        for (i in names(brush_ranges)) {

        }
        df[keep, ]
    })

    # These reactive values track the set of active brushes
    # Each reactive value corresponds to a different variable
    brush_ranges <- shiny::reactiveValues()

    lapply(names(d)[-1], function(nm) {
        output[[nm]] <- plotly::renderPlotly({
            plotly::plot_ly(type="scatter",
                            x = stats::formula(paste("~",names(d)[[1]])),
                            y = stats::formula(paste("~",nm)),
                            color = ~Cluster,
                            mode="markers",
                            size=4,
                            opacity=0.7,
                            data = df,
                            source = nm) %>%
                plotly::layout(
                    clickmode = "event+select",
                    dragmode = "select") %>%
                plotly::hide_colorbar() %>%
                plotly::event_register(event="plotly_brushed") %>%
                plotly::toWebGL()
        })

        shiny::observeEvent(plotly::event_data("plotly_brushed", source = nm), {
            # inform the world about the new brush range
            brush_ranges[[nm]] <- plotly::event_data("plotly_brushed", source = nm)
            in_bounds_x <- dplyr::between(df[[names(d)[[1]]]], min(brush_ranges[[nm]]$x), max(brush_ranges[[nm]]$x))
            in_bounds_y <- dplyr::between(df[[nm]], min(brush_ranges[[nm]]$y), max(brush_ranges[[nm]]$y))

            lapply(names(d)[-1], function(var) {
                p <- plotly::plotlyProxy(var, session)

                if (is.null(brush_ranges[[nm]])) {

                    # brush has been cleared, return the selection bars to a zero height
                    #plotly::plotlyProxyInvoke(p, "restyle", "y", list(counts$zeros))

                } else {

                    # if the brush originates from the proxy target
                    # just highlight the range of interest
                    props <- if (nm == var) {

                    } else {

                    }

                    plotly::plotlyProxyInvoke(p, "restyle", "y", list(props))
                }
            })

        })
    })

    # output$datatable <- shiny::renderTable({
    #     df_selected()
    # })

    #output$data <- shiny::renderTable(df %>% head(10))
}
server2 <- function(input, output, session) {
    output$maxOut <- plotly::renderPlotly({
        plotly::plot_ly(type="scatter",
                        x = ~maxOut,
                        y = ~minLT_Plant,
                        color = ~Cluster,
                        mode="markers",
                        size=4,
                        opacity=0.7,
                        data = df,
                        source = "maxOut") %>%
            plotly::layout(
                dragmode = "select") %>%
            #plotly::hide_colorbar() %>%
            plotly::event_register(event="plotly_brushing") %>%
            plotly::toWebGL()
    })

    shiny::observeEvent(plotly::event_data("plotly_brushing", source = "maxOut"), {
        b <- plotly::event_data("plotly_brushing", source = "maxOut")
        in_bounds <- between(d[["maxOut"]], min(b), max(b))

            p <- plotly::plotlyProxy("maxOut", session)

            if (is.null(b)) {

                # brush has been cleared, return the selection bars to a zero height
                #plotly::plotlyProxyInvoke(p, "restyle", "y", list(counts$zeros), trace_index())

            } else {

                # if the brush originates from the proxy target
                # then don't compute a new marginal distribution,
                # just highlight the range of interest
                props <- if ("maxOut" == var) {
                    if_else(
                        between(counts$xmin_, min(b), max(b)) &
                            between(counts$xmax_, min(b), max(b)),
                        counts$prop_,
                        0
                    )
                } else {
                    d[[var]] %>%
                        bin_fixed(bins = 150) %>%
                        compute_stat(d[[var]][in_bounds]) %>%
                        filter(!is.na(xmin_)) %>%
                        mutate(prop_ = count_ / sum(count_)) %>%
                        pull(prop_)
                }

                #plotly::plotlyProxyInvoke(p, "restyle", "y", list(props), trace_index())
            }
    })
}
# Run the application
shiny::shinyApp(ui = ui, server = server)
