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
#           Filter
#       Specify the parameters to cluster by
#       Choose the clustering method
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
options(shiny.maxRequestSize = 30*1024^2) # 30MB

#df <- FSMJ_clusters %>% normalizeValues(attr(., "objectives")) %>% dplyr::select(c(attr(., "inputs"),names(attr(.,"objectives")),Cluster,Distance,Rank))
df <- FSMJ_clusters %>%
    dplyr::select(Iteration,c(attr(., "inputs"),names(attr(.,"objectives")),Cluster,Distance,Rank))
#dimension_names <- names(attr(df, "objectives"))

# UI for the import tab
# Handles:
# - Data import
# - Import handling of inputs, outputs, and objectives
importUI <- shiny::fluidPage(
    shiny::fluidRow(
        # Data selection and import
        shinydashboard::box(title="Upload data", width = NULL,
            shiny::fileInput("fileupload", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
        )
    ),
    shiny::fluidRow(
        shinydashboard::box(title="Select Parameters", width = NULL,
                            shiny::uiOutput("data_objectives"),
                            shiny::uiOutput("data_inputs"),
                            shiny::uiOutput("data_outputs"),
                            #shiny::textOutput("consoleoutput")
                            #shiny::selectInput("filters", "Filter", names(attr(df, "objectives"))),
                            #uiOutput("filterslider")
                            shiny::actionButton("importData", "Import")
        )
    )
)

# UI for the filter tab
# Handles:
# - Data filtering
filterUI <- shiny::fluidPage(
    shiny::fluidRow(
        # Data filtering, specific filters for certain values, initial screening
        shinydashboard::box(title="Data summary", width = NULL,
                            DT::dataTableOutput("datatable"),
                            shiny::actionButton("applyfilter","Apply Filter")
        )
    )
)

# UI for the cluster tab
# Handles:
# - Cluster identification and application
clusterUI <- shiny::fillPage(
    # Choose the clustering dimensions
    #   Selectinput
    # Choose the clustering algoritm
    #   kmeans
    #       Number of clusters
    #   clara
    #       Number of clusters
    #   hclust
    #       Number of clusters
    #   dbscan
    #
    # Start clustering, accept results to dataset
    #   Apply with new or same button as previous?
    # Cluster performance (Visualization is handled in next tab)
    #   Performance can be shown so the user can get some guidance on the number of clusters
    shiny::fluidRow(
        shinydashboard::box(title="Cluster Variables",
                            shiny::uiOutput("clusterVar")),
        shinydashboard::box(shiny::radioButtons("clustermethod", "Cluster method:",
                                                c("kmeans", "clara", "hclust", "dbscan", "rpart")),
                                # shiny::tabsetPanel(id="clustTab",type="tabs",
                                # shiny::tabPanel("kmeans",
                                #                 ),
                                # shiny::tabPanel("clara",
                                #                 ),
                                # shiny::tabPanel("hclust",
                                #                 ),
                                # shiny::tabPanel("dbscan",
                                #                 )),
                            shiny::actionButton("evalClusters", "Evaluate clusters"),
                            shiny::numericInput("numClust", "Clusters:", 5, min = 2, max=10),
                            shiny::actionButton("saveClusters", "Apply clusters"))
    ),
    shiny::fluidRow(
        shinydashboard::box(title="Visualization",
                            shiny::plotOutput("clusterViz", width = "100%")),
        shinydashboard::box(title="Cluster Performance",
                            shiny::textOutput("clusterText")
                            )

    )
)

# UI for the visualization tab
visualizationUI <- shiny::fillPage(
    #tags$style(type = "text/css", "#parcoords {height: calc(100vh - 80px) !important;}"),
    plotly::plotlyOutput("parcoords"),
    shiny::tabsetPanel(id="chartTab", type = "tabs",
                        shiny::tabPanel("3D",
                        shiny::fluidRow(
                            shiny::column(10,
                                         tags$style(type = "text/css", "#scatter3d {height: calc(100vh - 80px) !important;}"),
                                         plotly::plotlyOutput("scatter3d", height = "100%", width = "100%")),
                            shiny::column(2,
                                         shiny::inputPanel(
                                             shiny::uiOutput("x"),
                                             shiny::uiOutput("y"),
                                             shiny::uiOutput("z"),
                                             shiny::selectInput("color", "Color:", c("None", "Rank"), selectize = FALSE),
                                             shiny::uiOutput("colorslider")))
                        )),
                        tabPanel("2D",
                                 # lapply(dimension_names[-1], function(nm) plotly::plotlyOutput(nm, width = "33%", inline = TRUE, height = "100%"))
                                 uiOutput("plots2d")
                                )
                       )
    #shiny::actionButton("reset", "Reset")
)

# UI for the rule extraction tab
ruleUI <- shiny::fillPage(
    # Decision trees
    # FPM?
    shiny::tabsetPanel(id="ruleTab", type = "tabs",
                       tabPanel("Hierarchical"

                                ),
                       tabPanel("FPM"

                                )
    )
)

# FMCUI <- shiny::fillPage(
#     # Decision trees
#     # FPM?
#     shiny::fluidRow(
#         shiny::column(8,
#                       plotly::plotlyOutput("FMC", height = "100%", width = "100%")),
#         shiny::column(4,
#                       shiny::dataTableOutput("FMC_table"))
#     )
# )

sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id="tabs",
        shinydashboard::menuItem("Import Data", tabName = "import", icon = shiny::icon("table", lib="font-awesome")),
        shinydashboard::menuItem("Filter Data", tabName = "filter", icon = shiny::icon("filter", lib="font-awesome")),
        shinydashboard::menuItem("Clustering", tabName = "cluster", icon = shiny::icon("microscope", lib="font-awesome")),
        shinydashboard::menuItem("Visualization", tabName = "viz", icon = shiny::icon("chart-bar", lib="font-awesome")),
        shinydashboard::menuItem("Rule Extraction", tabName = "rule", icon = shiny::icon("sitemap", lib="font-awesome"))
    )
)

body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "import", importUI),
        shinydashboard::tabItem(tabName = "filter", filterUI),
        shinydashboard::tabItem(tabName = "cluster", clusterUI),
        shinydashboard::tabItem(tabName = "viz", visualizationUI),
        shinydashboard::tabItem(tabName = "rule", ruleUI)
    )
)

dash <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "RulextR"),
    sidebar,
    body
)

# Define server logic
server <- function(input, output, session) {
    current_data <- shiny::reactiveValues(data=df)

    df_filtered <- shiny::reactive({
        input$applyfilter
        current_data$data[input$datatable_rows_all,]
    })

    ### Data filter logic
    output$data_name <- shiny::renderText({ attr(current_data$data, "opt_name") })
    output$datatable <- DT::renderDataTable({
        table <- current_data$data
        if (is.null(table))
            return(NULL)

        DT::datatable(table, filter="top", options = list(scrollX = TRUE, scrollY = 600, pageLength = 50))
    })

    output$filterslider <- renderUI({
        minVal <- min(df_filtered()[,input$filters])
        maxVal <- max(df_filtered()[,input$filters])
        shiny::sliderInput("filterslider", "", minVal, max=maxVal,value=c(minVal, maxVal))
    })

    data_parameters <- shiny::reactiveValues(header = "",
                                      input = "",
                                      output = "",
                                      objectives = "")

    output$data_inputs <- shiny::renderUI({
        shiny::selectInput("data_inputs", "Inputs", data_parameters$header, multiple = TRUE)
    })
    output$data_outputs <- shiny::renderUI({
        shiny::selectInput("data_outputs", "Outputs", data_parameters$header, multiple = TRUE)
    })
    output$data_objectives <- shiny::renderUI({
        shiny::selectInput("data_objectives", "Objectives", data_parameters$header, multiple = TRUE)
    })

    shiny::observeEvent(input$fileupload, {
        new_data <- input$fileupload
        if (!is.null(new_data)) {
            # Start by getting the column names
            data_parameters$header <- unlist(strsplit(readLines(new_data$datapath, n=1),";"))
            current_data$data <- loaddataset(input$fileupload$datapath)
            updateSelectInput(session, "data_objectives", selected = names(attr(current_data$data,"objectives")))
        }
    })

    shiny::observeEvent(input$importData, {
        if (!is.null(input$fileupload)) {
            #dimension_names <- names(attr(current_data$data, "objectives"))
            if(!is.null(input$data_inputs)) attr(current_data$data, "inputs") <- input$data_inputs
            if(!is.null(input$data_objectives)) names(attr(current_data$data, "objectives")) <- input$data_objectives
            if(!is.null(input$data_outputs)) {
                attr(current_data$data, "outputs") <- input$data_outputs
            } else {
                attr(current_data$data, "outputs") <- attr(current_data$data,"parameters")[!(attr(current_data$data, "parameters") %in% c(attr(current_data$data, "inputs"),
                                                                                                                                          names(attr(current_data$data, "objectives"))))]
                shiny::updateSelectInput(session,"data_outputs",selected =attr(current_data$data, "outputs"))
            }
            # Reset filters on Visualization tab
            ranges$data <- NULL
            brush_ranges$data <- NULL
        }
    })

    shiny::observeEvent(input$applyfilter, {
        ranges$data <- NULL
        brush_ranges$data <- NULL
    })

    ### Cluster logic
    output$clusterVar <- shiny::renderUI({
        shiny::selectInput("clusterViz", "Cluster by:", dim_list(), selected=dim_list()$Objectives, multiple = T)
    })

    shiny::observeEvent(input$evalClusters, {
        # Evaluate the number of clusters to use.
        if(input$clustermethod=="kmeans"){

        } else if(input$clustermethod=="clara") {
            clara.res <- factoextra::fviz_nbclust(data_imported(), FUNcluster = cluster::clara)

        } else if (input$clustermethod=="hclust") {

        } else if (input$clustermethod=="rpart") {
            part.data <- current_data$data %>% normalizeValues()
            form <- as.formula(paste(paste(names(attr(df_filtered(),"objectives")),collapse="+"), " ~ ",paste(attr(df_filtered(),"inputs"),collapse = "+"),collapse=" "))
            rprt <- rpart::rpart(form, data=df_filtered())
            output$clusterViz <- shiny::renderPlot({
                rpart.plot::rpart.plot(rprt)
            })
        } else { # dbscan

        }
        # Send output to renderText("clusterText")
    })

    ### Visualization logic
    dim_list <- shiny::reactive({
        list(Objectives=unique(names(attr(current_data$data,"objectives"))),
             Inputs=unique(attr(current_data$data,"inputs")),
             Outputs=unique(attr(current_data$data,"outputs")))
    })

    output$x <- shiny::renderUI({
        shiny::selectInput("x", "X:", dim_list(), selectize = FALSE, selected=dim_list()$Objectives[[1]])
    })

    output$y <- shiny::renderUI({
        shiny::selectInput("y", "Y:", dim_list(), selectize = FALSE, selected=dim_list()$Objectives[[2]])
    })

    output$z <- shiny::renderUI({
        shiny::selectInput("z", "Z:", dim_list(), selectize = FALSE, selected=dim_list()$Objectives[[3]])
    })

    output$colorslider <- shiny::renderUI({
        min_val <- 0
        max_val <- 0

        if(input$color %in% colnames(df_filtered())) {
            min_val <- min(df_filtered()[[input$color]])
            max_val <- max(df_filtered()[[input$color]])
        }

        shiny::sliderInput("colorslider", "Filter Color:", min_val, max_val, value=c(min_val, max_val))
    })

    # output$color <- shiny::renderUI({
    #     shiny::selectInput("color", "Color:", c("Rank","Cluster","Distance"), selectize = FALSE)
    # })
    # Render the parallel coordinates plot
    output$parcoords <- plotly::renderPlotly({
        suppressWarnings(
            plotly::plot_ly(type = 'parcoords',
                            dimensions = create_dimensions_list(df_filtered()),
                            line=list(color = ifelse(input$color == "None", "I(\"blue\")", stats::formula(paste0("~",input$color)))),
                            source = "pcoords",
                            data = df_filtered()) %>%
            plotly::event_register(event="plotly_restyle") %>%
            plotly::toWebGL()
        )
    })

    # React to changes to the colouring variable
    shiny::observeEvent(input$color, {
        if(input$color %in% colnames(df_filtered()))
        plotly::plotlyProxy("scatter3d", session) %>%
        plotly::plotlyProxyInvoke("restyle", list(color = stats::formula(paste0("~",input$color))))
    })

    # Render the scatter3d plot
    output$scatter3d <- plotly::renderPlotly({
        suppressWarnings(
            plotly::plot_ly(type="scatter3d",
                            x = stats::formula(paste0("~",input$x)),
                            y = stats::formula(paste0("~",input$y)),
                            z = stats::formula(paste0("~",input$z)),
                            color = ifelse(input$color == "None", "I(\"blue\")", stats::formula(paste0("~",input$color))),
                            #color = ~Cluster,
                            mode="markers",
                            size=4,
                            opacity=1,
                            height=650,
                            data = df_selected()$sel) %>%
                plotly::add_trace(
                    color = I("gray"),
                    size = 0.5,
                    opacity = 0.7,
                    data = df_selected()$unsel) %>%
            plotly::layout(showlegend=F,
                           updatemenus = list(
                               list(
                                   type = "buttons",
                                   y = 0.8,
                                   buttons = list(
                                       list(method = "restyle",
                                            args = list("visible", c(T,T)),
                                            label = "All"),
                                       list(method = "restyle",
                                            args = list("visible", c(T,F)),
                                            label = "Only Filtered"))))
                          )
            #plotly::hide_colorbar() %>%
            #plotly::toWebGL()
        )
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
        if (is.null(d[[1]])){
            # Reordering dimensions
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
        brush_ranges$data <- NULL
    })

    # filter the dataset down to the rows that match the selection ranges
    df_filterdata <- shiny::reactiveValues()
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
                    keep_var <- keep_var | dplyr::between(df_filtered()[[i]], min(rng), max(rng))
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
                plotnumber <- as.numeric(substring(names(brush_ranges$data),5))
                keep_var <- keep_var | dplyr::between(df_filtered()[[dim_list()$Objectives[[1]]]], min(range_$x), max(range_$x))
                keep_var <- keep_var & dplyr::between(df_filtered()[[dim_list()$Objectives[[plotnumber+1]]]], min(range_$y), max(range_$y))
            }
            keep <- keep & keep_var
        }
        if(!is.null(input$colorslider) && input$color %in% colnames(df_filtered())) {
            df_filterdata$sel <- df_filtered()[keep, ] %>% dplyr::filter(dplyr::between(.[[input$color]], input$colorslider[1], input$colorslider[2]))
        } else {
            df_filterdata$sel <- df_filtered()[keep, ]
        }
        df_filterdata$unsel <- dplyr::anti_join(df_filtered(), df_filterdata$sel, by="Iteration")
        df_filterdata
    })

    # These reactive values track the set of active brushes
    # Each reactive value corresponds to a different variable
    brush_ranges <- shiny::reactiveValues('data' = list())

    output$plots2d <- renderUI({
        plot_output_list <- lapply(1:length(dim_list()$Objectives), function(i) {
            plotname <- paste0("plot", i)
            plotly::plotlyOutput(plotname, width = "33%", inline = TRUE, height = "100%")
        })

        # Convert the list to a tagList - this is necessary for the list of items
        # to display properly.
        do.call(tagList, plot_output_list)
    })

    # Call renderPlot for each one. Plots are only actually generated when they
    # are visible on the web page.
    observeEvent({{c(input$importData,input$applyfilter)}}, {
        max_plots <- length(dim_list()$Objectives)
        for (i in 1:max_plots-1) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
            my_i <- i
            plotname <- paste0("plot", my_i)

            output[[plotname]] <- plotly::renderPlotly({
                plotly::plot_ly(type="scattergl",
                                x = stats::formula(paste0("~",dim_list()$Objectives[[1]])),
                                y = stats::formula(paste0("~",dim_list()$Objectives[[my_i+1]])),
                                color = ifelse(input$color == "None", "I(\"blue\")", stats::formula(paste0("~",input$color))),
                                data = df_selected()$sel,
                                mode="markers",
                                source = plotname) %>%
                    plotly::hide_colorbar() %>%
                    plotly::event_register(event="plotly_brushed") %>%
                    plotly::event_register("plotly_doubleclick") %>%
                    # plotly::add_trace(
                    #     color = I("gray"),
                    #     size = 0.5,
                    #     opacity = 0.7,
                    #     data = df_selected()$unsel) %>%
                    # plotly::layout(showlegend=F) %>%
                    plotly::toWebGL()
            })
        })
        }
    })

    lapply(paste0("plot", seq(1,4)), function(nm) {
        shiny::observeEvent(plotly::event_data("plotly_brushed", source=nm), {
        # inform the world about the new brush range

            brushed <- plotly::event_data("plotly_brushed", source=nm)
            if (is.null(brushed)){
                brush_ranges$data <- NULL
            } else {
                brush_ranges$data[[nm]] <- brushed
            }
        })
    })

    shiny::observeEvent(input$reset, {
        brush_ranges$data <- NULL
        # plotly::plotlyProxy(nm, session) %>%
        #     plotly::plotlyProxyInvoke("restyle", "data", df_selected())
    })


    # lapply(dimension_names[-1], function(nm) {
    #     output[[nm]] <- plotly::renderPlotly({
    #         plotly::plot_ly(type="scatter",
    #                         x = stats::formula(paste0("~",dimension_names[[1]])),
    #                         y = stats::formula(paste0("~",nm)),
    #                         color = stats::formula(paste0("~",input$color)),
    #                         mode = "markers",
    #                         size = 4,
    #                         opacity = 0.7,
    #                         data = df_selected(),
    #                         source = nm) %>%
    #             plotly::layout(
    #                 clickmode = "event+select",
    #                 dragmode = "select") %>%
    #             plotly::hide_colorbar() %>%
    #             plotly::event_register(event="plotly_brushed") %>%
    #             plotly::event_register("plotly_doubleclick") %>%
    #             plotly::toWebGL()
    #     })
    #
    #     # when the selection is cleared, return the selection layer bars to 0
    #     shiny::observeEvent(plotly::event_data("plotly_doubleclick", source = nm), {
    #         brush_ranges$data <- NULL
    #     })
    #
    #     shiny::observeEvent(plotly::event_data("plotly_brushed", source = nm), {
    #         # inform the world about the new brush range
    #         brushed <- plotly::event_data("plotly_brushed", source = nm)
    #         if (is.null(brushed)){
    #             brush_ranges$data <- NULL
    #         } else {
    #             brush_ranges$data[[nm]] <- brushed
    #         }
    #     })
    # })
    #
    #
    # output$FMC <- plotly::renderPlotly({
    #     plotly::plot_ly(type="scatter",
    #                     x = ~Investment,
    #                     y = ~TP,
    #                     mode="markers",
    #                     size=4,
    #                     opacity=0.7,
    #                     customdata = seq(1, nrow(FMC)),
    #                     source = "FMC_source",
    #                     #height=650,
    #                     data = FMC) %>%
    #         plotly::layout(yaxis = list(range = c(0.75, 0.93))) %>%
    #         plotly::toWebGL()
    # })

    # shiny::observeEvent(plotly::event_data("plotly_selected", source="FMC_source"), {
    #     dt_data <- plotly::event_data("plotly_selected", source = "FMC_source")
    #     if (!is.null(dt_data)){
    #         dt_data$customdata
    #     }
    #
    #     output$FMC_table <- shiny::renderDataTable({
    #         dt_data
    #     })
    # })
}

# Run the application
shiny::shinyApp(ui = dash, server = server)
