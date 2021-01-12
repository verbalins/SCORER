#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Targets:
#   Identify interesting points
#   See correlations between parameters
# Prerequisites:
#   Parcoords for inputs and objectives
#       DONE
#   2D-plots for comparison of objectives
#       DONE
#   Filter on both parcoords and scatter plot
#       Parcoords should retain the filters
#       Scatter plots should just update
#

# Views:
#   Clustering
#       Prepare the data
#           Filter?
#       Specify the parameters to cluster by
#       Choose the number of clusters by evaluating a few metrics
#       Apply the clustering to the dataset
#   Visualization
#       Mostly as now, filtering will stick
#   Rule extraction
#       Extract rules from the selected data in the previous screens

# Input:
#   Dataset with clusters applied.
#       TODO: Set on this screen or another?
#

set.seed(42)
#df <- FSMJ_clusters %>% normalizeValues(attr(., "objectives")) %>% dplyr::select(c(attr(., "inputs"),names(attr(.,"objectives")),Cluster,Distance,Rank))
df <- FSMJ_clusters %>%
    dplyr::select(c(attr(., "inputs"),names(attr(.,"objectives")),Cluster,Distance,Rank))
df_original <- df
d <- attr(df, "objectives")

# UI for the import tab
# Handles:
# - Data import
# - Data filtering
loadfilterUI <- shiny::fillPage(
    shiny::fluidRow(
        shiny::column(width = 6,
            # Data selection and import
            shinydashboard::box(title="Loading data", width = NULL,
                h4("Current dataset:", shiny::textOutput("data_name")),
                shiny::fileInput("fileupload", "Choose CSV File",
                          multiple = FALSE,
                          accept = c("text/csv",
                                     "text/comma-separated-values,text/plain",
                                     ".csv"))
            ),
            # Data filtering, specific filters for certain values, initial screening
            shinydashboard::box(title="Data summary", width = NULL,
                                DT::dataTableOutput("datatable"),
                                shiny::actionButton("applyfilter","Apply Filter")
            )
        ),
        shiny::column(width=6,
            shinydashboard::box(title="Filters", width = NULL,
                                shiny::selectInput("filters", "Filter", names(attr(df, "objectives"))),
                                uiOutput("filterslider")
            )
        )
    )
)
# UI for the cluster tab
# Handles:
# - Cluster identification and application
clusterUI <- shiny::fillPage(
    # Choose the clustering dimensions

    # Start clustering, apply results to dataset

    # Cluster performance (Visualization is handled in next tab)

)

# UI for the visualization tab
visualizationUI <- shiny::fillPage(
    #tags$style(type = "text/css", "#parcoords {height: calc(100vh - 80px) !important;}"),
    plotly::plotlyOutput("parcoords"),
    shiny::tabsetPanel(type = "tabs",
                        tabPanel("3D",
                        shiny::fluidRow(
                            shiny::column(10,
                                         tags$style(type = "text/css", "#scatter3d {height: calc(100vh - 80px) !important;}"),
                                         plotly::plotlyOutput("scatter3d", height = "100%", width = "100%")),
                            shiny::column(2,
                                         shiny::inputPanel(shiny::selectInput("x", "X:", unique(names(d)), selectize = FALSE, selected=names(d)[[1]])),
                                         shiny::inputPanel(shiny::selectInput("y", "Y:", unique(names(d)), selectize = FALSE, selected=names(d)[[2]])),
                                         shiny::inputPanel(shiny::selectInput("z", "Z:", unique(names(d)), selectize = FALSE, selected=names(d)[[3]])),
                                         shiny::inputPanel(shiny::selectInput("color", "Color:", c("Cluster","Rank","Distance"), selectize = FALSE, selected="Cluster")))
                        )),
                       tabPanel("2D", lapply(names(d)[-1], function(nm) plotly::plotlyOutput(nm, width = "33%", inline = TRUE, height = "100%")))
                       )
    #shiny::actionButton("reset", "Reset")
)

ruleUI <- shiny::fillPage(

)

sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
        shinydashboard::menuItem("Import Data", tabName = "load", icon = shiny::icon("table", lib="font-awesome")),
        shinydashboard::menuItem("Clustering", tabName = "cluster", icon = shiny::icon("microscope", lib="font-awesome")),
        shinydashboard::menuItem("Visualization", tabName = "viz", icon = shiny::icon("chart-bar", lib="font-awesome")),
        shinydashboard::menuItem("Rule Extraction", tabName = "rule", icon = shiny::icon("sitemap", lib="font-awesome"))
    )
)

body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "load", loadfilterUI),
        shinydashboard::tabItem(tabName = "cluster", clusterUI),
        shinydashboard::tabItem(tabName = "viz", visualizationUI),
        shinydashboard::tabItem(tabName = "rule",ruleUI)
    )
)

dash <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "RulextR"),
    sidebar,
    body
)

# Define server logic
server <- function(input, output, session) {
    df_imported <- shiny::reactive({
        input$applyfilter
        df[input$datatable_rows_all,]
    })
    ### Data input logic
    output$data_name <- shiny::renderText({ attr(df, "opt_name") })
    output$datatable <- DT::renderDataTable({
        table <- df
        if (is.null(table))
            return(NULL)

        DT::datatable(table, filter="top", options = list(scrollX = TRUE))
    })
    output$filterslider <- renderUI({
        minVal <- min(df[,input$filters])
        maxVal <- max(df[,input$filters])
        shiny::sliderInput("filterslider", "", minVal, max=maxVal,value=c(minVal, maxVal))
    })

    shiny::observeEvent(input$applyfilter, {

    })

    ### Visualization logic
    # Render the parallel coordinates plot
    output$parcoords <- plotly::renderPlotly({
            plotly::plot_ly(type = 'parcoords',
                            dimensions = create_dimensions_list(df),
                            line=list(color = ~Cluster),
                            source = "pcoords",
                            data = df_imported()) %>%
            plotly::event_register(event="plotly_restyle") %>%
            plotly::toWebGL()
    })

    # React to changes to the colouring variable
    shiny::observeEvent(input$color, {
        plotly::plotlyProxy("scatter3d", session) %>%
        plotly::plotlyProxyInvoke("restyle", list(color = stats::formula(paste("~",input$color))))
    })

    # Render the scatter3d plot
    output$scatter3d <- plotly::renderPlotly({
            plotly::plot_ly(type="scatter3d",
                            x = stats::formula(paste("~",input$x)),
                            y = stats::formula(paste("~",input$y)),
                            z = stats::formula(paste("~",input$z)),
                            color = stats::formula(paste("~",input$color)),
                            #color = ~Cluster,
                            mode="markers",
                            size=4,
                            opacity=0.7,
                            height=650,
                            data = df_selected()) %>%
            #plotly::hide_colorbar() %>%
            plotly::toWebGL()
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
        if (names(d[[1]]) == "dimensions"){
            # Reordering dimensions
            return()
        }
        # what is the relevant dimension (i.e. variable)?
        dimension <- as.numeric(stringr::str_extract(names(d[[1]]), "[0-9]+"))
        # careful of the indexing in JS (0) versus R (1)!
        dimension_name <- names(df)[[dimension + 1]]
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
        brush_ranges$data <- NULL
    })

    # filter the dataset down to the rows that match the selection ranges
    df_selected <- shiny::reactive({
        keep <- TRUE
        for (i in names(ranges$data)) {
            range_ <- ranges$data[[i]]
            keep_var <- FALSE
            for (j in seq_along(range_)) {
                rng <- range_[[j]]
                if (length(rng) == 0) # When deselecting
                    keep_var <- TRUE
                else {
                    keep_var <- keep_var | dplyr::between(df_imported()[[i]], min(rng), max(rng))
                }
            }
            keep <- keep & keep_var
        }
        for (i in names(brush_ranges$data)) {
            range_ <- brush_ranges$data[[i]]
            keep_var <- FALSE
            if (is.null(range_)) # When deselecting
                keep_var <- TRUE
            else {
                keep_var <- keep_var | dplyr::between(df_imported()[[names(d)[[1]]]], min(range_$x), max(range_$x))
                keep_var <- keep_var & dplyr::between(df_imported()[[i]], min(range_$y), max(range_$y))
            }
            keep <- keep & keep_var
        }
        df_imported()[keep, ]
    })

    # These reactive values track the set of active brushes
    # Each reactive value corresponds to a different variable
    brush_ranges <- shiny::reactiveValues('data' = list())

    lapply(names(d)[-1], function(nm) {
        output[[nm]] <- plotly::renderPlotly({
            plotly::plot_ly(type="scatter",
                            x = stats::formula(paste("~",names(d)[[1]])),
                            y = stats::formula(paste("~",nm)),
                            #customdata = seq(1, nrow(df)),
                            color = stats::formula(paste("~",input$color)),
                            mode = "markers",
                            size = 4,
                            opacity = 0.7,
                            data = df_selected(),
                            source = nm) %>%
                plotly::layout(
                    clickmode = "event+select",
                    dragmode = "select") %>%
                plotly::hide_colorbar() %>%
                plotly::event_register(event="plotly_brushed") %>%
                plotly::event_register("plotly_doubleclick") %>%
                plotly::toWebGL()
        })

        # when the selection is cleared, return the selection layer bars to 0
        shiny::observeEvent(input$reset, {
            brush_ranges$data <- NULL
            # plotly::plotlyProxy(nm, session) %>%
            #     plotly::plotlyProxyInvoke("restyle", "data", df_selected())
        })

        shiny::observeEvent(plotly::event_data("plotly_doubleclick", source = nm), {
            brush_ranges$data <- NULL
        })

        shiny::observeEvent(plotly::event_data("plotly_brushed", source = nm), {
            # inform the world about the new brush range
            brushed <- plotly::event_data("plotly_brushed", source = nm)
            if (is.null(brushed)){
                brush_ranges$data <- NULL
            } else {
                brush_ranges$data[[nm]] <- brushed
            }
        })
    })
}

# Run the application
shiny::shinyApp(ui = dash, server = server)
