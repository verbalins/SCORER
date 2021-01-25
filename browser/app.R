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
reticulate::source_python('../py/FPM.py')

#df <- FSMJ_clusters %>% normalizeValues(attr(., "objectives")) %>% dplyr::select(c(.$inputs,.$objectives,Cluster,Distance,Rank))
#df <- FSMJ_clusters %>% dplyr::select(Iteration,c(.$inputs,.$objectives,Cluster,Distance,Rank))
#dimension_names <- names(attr(df, "objectives"))
cluster_data <- shiny::reactiveValues("data" =list(), "rpart"=list(), "clara"=list(), "kmeans"=list(), "hclust"=list(), "dbscan"=list())
debug <- FALSE

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
    shiny::fluidRow(shinydashboard::box(title="Select Parameters", width = NULL,
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
                            shinycssloaders::withSpinner(DT::dataTableOutput("datatable")),
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
                            shiny::uiOutput("clusterDep"),
                            shiny::uiOutput("clusterInDep"),
                            shiny::checkboxInput("scaleDep","Scale Dependents",TRUE)),
        shinydashboard::box(#shiny::radioButtons("clustermethod", "Cluster method:",
                             #                   c("rpart", "kmeans", "clara", "hclust", "dbscan")),
                                shiny::tabsetPanel(id="clustTab",type="tabs",
                                shiny::tabPanel("rpart",
                                                ),
                                shiny::tabPanel("kmeans",
                                    shiny::numericInput("numClust", "Clusters:", 5, min = 2, max=10),
                                                ),
                                shiny::tabPanel("clara",
                                                ),
                                shiny::tabPanel("hclust",
                                                ),
                                shiny::tabPanel("dbscan",
                                    shiny::numericInput("minpts", "MinPts:", 5, min=1, step=1),
                                    shiny::numericInput("eps", "eps:", 0.1, min=0, step=0.01),
                                    shiny::actionButton("evalClusPerf", "Evaluate eps"))),
                            shiny::actionButton("evalClusters", "Evaluate clusters"),
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
    shinycssloaders::withSpinner(plotly::plotlyOutput("parcoords")),
    shiny::tabsetPanel(id="chartTab", type = "tabs",
                        shiny::tabPanel("3D",
                        shiny::fluidRow(
                            shiny::column(10,
                                         tags$style(type = "text/css", "#scatter3d {height: calc(100vh - 80px) !important;}"),
                                         shinycssloaders::withSpinner(plotly::plotlyOutput("scatter3d", height = "100%", width = "100%"))),
                            shiny::column(2,
                                         shiny::inputPanel(
                                             shiny::uiOutput("x"),
                                             shiny::uiOutput("y"),
                                             shiny::uiOutput("z"),
                                             shiny::selectInput("color", "Color:", c("Rank"), selectize = FALSE),
                                             shiny::uiOutput("colorslider")))
                        )),
                        tabPanel("2D",
                                 # lapply(dimension_names[-1], function(nm) plotly::plotlyOutput(nm, width = "33%", inline = TRUE, height = "100%"))
                                 shinycssloaders::withSpinner(uiOutput("plots2d"))
                                )
                       )
    #shiny::actionButton("reset", "Reset")
)

# UI for the rule extraction tab
ruleUI <- shiny::fillPage(
    # Decision trees
    # FPM, through python right now
    shiny::fluidRow(
        shinydashboard::box(
            shiny::checkboxInput("test", "test"),
            shiny::tabsetPanel(id="ruleTab", type = "tabs",
                           tabPanel("Hierarchical"

                                    ),
                           tabPanel("FPM",
                                    # Minimum significance
                                shiny::numericInput("minsig", "Minimum Significance", 0.5, 0.1, 1.0, 0.1),
                                shiny::numericInput("fpmlevel", "Rule levels", 1, 1, 4, 1)
                                    )
            ),
            shiny::actionButton("rulebutton", "Create Rules")
        ),
        shinydashboard::box(
            #shinycssloaders::withSpinner(shiny::tableOutput("ruletable"))
        )
    ),
    shiny::fluidRow(
        shinydashboard::box(title="PyFPM",
            shinycssloaders::withSpinner(shiny::tableOutput("pyruletable"))
        ),
        shinydashboard::box(title="RFPM",
            shinycssloaders::withSpinner(shiny::tableOutput("ruletable"))
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
    shinydashboard::dashboardHeader(title = "RulExtR"),
    sidebar,
    body
)

# Define server logic
server <- function(input, output, session) {
    ### Data upload logic ---------------------------------------------
    uploaded_data <- shiny::reactiveValues('data' = list())

    current_data <- reactive({
        # if (debug){
        #     uploaded_data$data <- FMC
        # }
        uploaded_data$data
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
            # Start by getting the column names if available
            data_parameters$header <- unlist(strsplit(readLines(new_data$datapath, n=1),";"))
            uploaded_data$data <- NULL
            uploaded_data$data  <- loaddataset(input$fileupload$datapath)
            updateSelectInput(session, "data_objectives", selected = unique(uploaded_data$data$objectives))
        }
    })

    shiny::observeEvent(input$importData, {
        if (!is.null(input$fileupload)) {
            #dimension_names <- current_data()$objectives
            if(!is.null(input$data_inputs)) {
                uploaded_data$data$inputs <- input$data_inputs }
            if(!is.null(input$data_objectives)) {
                uploaded_data$data$objectives <- input$data_objectives }
            if(!is.null(input$data_outputs)) {
                uploaded_data$data$outputs <- input$data_outputs
            } else {
                uploaded_data$data$outputs <- uploaded_data$data$parameters[!(uploaded_data$data$parameters %in% c(uploaded_data$data$inputs,
                                                                                                      uploaded_data$data$objectives))]
                shiny::updateSelectInput(session,"data_outputs",selected = uploaded_data$data$outputs)
            }
            #parcoords_headers$headers <- c(current_data()$objectives),attr(current_data(),"inputs"))
            # Reset filters on Visualization tab
            ranges$data <- NULL
            selected_points$data <- NULL
        }
    })

    ### Data filter logic ---------------------------------------------
    df_filtered <- shiny::reactive({
        if(is.null(nrow(current_data()))){
            return(NULL)
        }
        #input$applyfilter
        #input$importData
        current_data()[input$datatable_rows_all,]
    })

    output$data_name <- shiny::renderText({ current_data()$opt_name })
    output$datatable <- DT::renderDataTable({
        table <- current_data()
        if (is.null(table))
            return(NULL)

        DT::datatable(table, filter="top", options = list(scrollX = TRUE, scrollY = 600, pageLength = 50))
    })

    output$filterslider <- renderUI({
        minVal <- min(df_filtered()[,input$filters])
        maxVal <- max(df_filtered()[,input$filters])
        shiny::sliderInput("filterslider", "", minVal, max=maxVal,value=c(minVal, maxVal))
    })

    shiny::observeEvent(input$applyfilter, {
        ranges$data <- NULL
        selected_points$data <- NULL
    })

    ### Cluster logic ------------------------------------------
    output$clusterDep <- shiny::renderUI({
        shiny::selectInput("clusterDep", "Dependent variables:", dim_list(), selected=dim_list()$Objectives, multiple = T)
    })

    output$clusterInDep <- shiny::renderUI({
        shiny::selectInput("clusterInDep", "Independent variables:", dim_list(), selected=dim_list()$Inputs, multiple = T)
    })

    shiny::observeEvent(input$evalClusPerf, {
        # Ask to scale data
        cluster_data$data <- current_data() %>% dplyr::select(input$clusterDep, input$clusterInDep)
        if (input$scaleDep) {
            cluster_data$data <- cluster_data$data %>%
                dplyr::mutate(dplyr::across(input$clusterDep,collapse::fscale))
        }

        if (input$clustTab == "dbscan") {
            output$clusterViz <- shiny::renderPlot({
                dbscan::kNNdistplot(cluster_data$data, k=input$minpts)
            })
        }
    })

    shiny::observeEvent(input$evalClusters, {
        # Evaluate the number of clusters to use.
        # Ask to scale data
        cluster_data$data <- current_data() %>% dplyr::select(input$clusterDep, input$clusterInDep)
        if (input$scaleDep) {
            cluster_data$data <- cluster_data$data %>%
                dplyr::mutate(dplyr::across(input$clusterDep,collapse::fscale))
        }
        if(input$clustTab=="kmeans") {
            kclusts <- tibble::tibble(k = 1:10) %>%
                dplyr::mutate(
                    kclust = purrr::map(k, ~kmeans(., .x)),
                    tidied = purrr::map(kclust, broom::tidy),
                    glanced = purrr::map(kclust, broom::glance),
                    augmented = purrr::map(kclust, broom::augment, .)
                )

            clusters <- kclusts %>% tidyr::unnest(cols = c(tidied))
            assignments <- kclusts %>% tidyr::unnest(cols = c(augmented))
            clusterings <- kclusts %>% tidyr::unnest(cols = c(glanced))

            output$clusterViz <- shiny::renderPlot({
            ggplot2::ggplot(clusterings, ggplot2::aes(k, tot.withinss)) +
                ggplot2::geom_line() +
                ggplot2::geom_point()
            })
        } else if(input$clustTab=="clara") {
            cluster.suggestion <- factoextra::fviz_nbclust(cluster_data$data, FUNcluster = cluster::clara)
            output$clusterViz <- shiny::renderPlot({
                cluster.suggestion
            })
        } else if (input$clustTab=="hclust") {
            cluster_data$hclust <- fastcluster::hclust.vector(cluster_data$data)
        } else if (input$clustTab=="rpart") {
            part.data <- cluster_data$data
            form <- as.formula(paste(paste(input$clusterDep, collapse="+"), " ~ ",paste(input$clusterInDep,collapse = "+"),collapse=" "))
            rprt <- rpart::rpart(form, data=part.data, model=T)
            cluster_data$rpart <- rprt
            output$clusterViz <- shiny::renderPlot({
                rpart.plot::rpart.plot(rprt)
            })
        } else {
            res <- dbscan::dbscan(cluster_data$data, eps = input$eps, minPts = input$minpts)
            cluster_data$dbscan <- res$cluster
            res$cluster
            output$clusterViz <- shiny::renderPlot({
                ggplot2::ggplot(as.data.frame(cluster_data$data), ggplot2::aes_string(input$clusterDep[1], input$clusterDep[2], col=res$cluster)) +
                    ggplot2::geom_point()
            })
        }
        # Send output to renderText("clusterText")
    })

    shiny::observeEvent(input$saveClusters, {
        # Apply the clustering
        if (input$clustTab=="kmeans") {
            cluster_data$kmeans <- kmeans(current$data$Objectives, centers=input$numClust)
        } else if(input$clustTab=="clara") {
            cluster_data$clara <- cluster::clara(cluster_data$data, k=input$numClust, stand=T, samples=500, pamLike = T)
            clust <- cluster_data$clara$clustering
        } else if (input$clustTab=="hclust") {
        } else if (input$clustTab=="rpart") {
            # Use input$numClust to prune the tree approximately to the number of clusters

        } else { #dbscan
            res <- dbscan::dbscan(cluster_data$data, eps = input$eps, minPts = input$minpts)
            clust <- res$cluster
        }
        uploaded_data$data <- current_data() %>% dplyr::mutate(Cluster = clust)
        shiny::updateSelectInput(session, "color", choices = c())
    })

    ### Visualization logic
    dim_list <- shiny::reactive({
        list(Objectives=unique(current_data()$objectives),
             Inputs=unique(current_data()$inputs),
             Outputs=unique(current_data()$outputs))
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
            shiny::sliderInput("colorslider", "Filter Color:", min_val, max_val, value=c(min_val, max_val))
        }
    })

    # output$color <- shiny::renderUI({
    #     shiny::selectInput("color", "Color:", c("Rank","Cluster","Distance"), selectize = FALSE)
    # })
    # Render the parallel coordinates plot
    output$parcoords <- plotly::renderPlotly({
        # req(input$x)
        # req(input$y)
        # req(input$z)
        #req(input$color)
        suppressWarnings(
            plotly::plot_ly(type = 'parcoords',
                            dimensions = create_dimensions_list(df_filtered()),
                            line=list(color = stats::formula(paste0("~",input$color))),
                            source = "pcoords",
                            data = df_filtered()) %>%
            plotly::event_register(event="plotly_restyle") %>%
            plotly::toWebGL()
        )
    })

    # React to changes to the colouring variable
    shiny::observeEvent(input$color, {
        if(input$color %in% colnames(df_filtered())) {
            plotly::plotlyProxy("scatter3d", session, deferUntilFlush = F) %>%
                plotly::plotlyProxyInvoke("restyle", list(color = stats::formula(paste0("~",input$color))), 0)
        }
    })

    # Render the scatter3d plot
    output$scatter3d <- plotly::renderPlotly({
        suppressWarnings(
            plotly::plot_ly(type="scatter3d",
                            x = stats::formula(paste0("~",input$x)),
                            y = stats::formula(paste0("~",input$y)),
                            z = stats::formula(paste0("~",input$z)),
                            color = stats::formula(paste0("~",input$color)),
                            customdata = ~Iteration,
                            hovertemplate = paste(paste0('<b>',input$x,'</b>: %{x}'),
                                                  paste0('<br><b>',input$y,'</b>: %{y}'),
                                                  paste0('<br><b>',input$z,'</b>: %{z}'),
                                                  paste0('<br><b>Iteration</b>: %{customdata}<extra></extra>')),
                            mode="markers",
                            size = I(30),
                            opacity=1,
                            height=600,
                            data = df_selected()$sel) %>%
                plotly::add_trace(
                    color = I("gray"),
                    size = I(10),
                    opacity = 0.2,
                    data = df_selected()$unsel,
                    showlegend=F) %>%
            plotly::layout(updatemenus = list(
                               list(
                                   type = "buttons",
                                   x = 1,
                                   buttons = list(
                                       list(method = "restyle",
                                            args = list("visible", c(T,T)),
                                            args2 = list("visible", c(T,F)),
                                            label = "Toggle filtered"))))
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
    #parcoords_headers <- shiny::reactiveValues('data' = list())

    ranges <- shiny::reactiveValues('data' = list())
    shiny::observeEvent(plotly::event_data("plotly_restyle", source = "pcoords"), {
        d <- plotly::event_data("plotly_restyle", source = "pcoords")
        if (is.null(d[[1]]) || names(d[[1]]) == "line"){
            # Reordering dimensions
            return()
        } else if (names(d[[1]]) == "dimensions") {
            # changing the ordering of the parallel coordinates plot
            #parcoords_headers$headers <- d[[1]]$dimensions[[1]]$label # new ordering, save it to ensure proper results
            return()
        }
        # what is the relevant dimension (i.e. variable)?
        dimension <- as.numeric(stringr::str_extract(names(d[[1]]), "[0-9]+"))
        # careful of the indexing in JS (0) versus R (1)!
        #dimension_name <- parcoords_headers$headers[[dimension + 1]]
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
        parcoords_sel <- df_filtered()[keep, ]

        for (i in names(selected_points$data)) {
            parcoords_sel <- parcoords_sel %>% dplyr::filter(Iteration %in% unlist(selected_points$data[i]))
        }

        if(!is.null(input$colorslider) && input$color %in% colnames(df_filtered())) {
            df_filterdata$sel <- parcoords_sel %>% dplyr::filter(dplyr::between(.[[input$color]], input$colorslider[1], input$colorslider[2]))
        } else {
            df_filterdata$sel <- parcoords_sel
        }
        df_filterdata$unsel <- dplyr::anti_join(df_filtered(), df_filterdata$sel, by="Iteration")
        df_filterdata
    })

    # These reactive values track the set of active brushes
    # Each reactive value corresponds to a different variable
    selected_points <- shiny::reactiveValues('data' = list())

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
                                color = stats::formula(paste0("~",input$color)),
                                customdata = ~Iteration,
                                data = df_selected()$sel,
                                mode="markers",
                                size = I(30),
                                opacity = 1,
                                hovertemplate = paste(paste0('<b>',input$x,'</b>: %{x}'),
                                                      paste0('<br><b>',dim_list()$Objectives[[my_i+1]],'</b>: %{y}'),
                                                      paste0('<br><b>Iteration</b>: %{customdata}<extra></extra>')),
                                source = plotname) %>%
                    plotly::add_trace(
                        color = I("gray"),
                        size = I(10),
                        opacity = 0.2,
                        data = df_selected()$unsel,
                        showlegend=F) %>%
                    plotly::layout(updatemenus = list(
                        list(
                            type = "buttons",
                            x = 1,
                            buttons = list(
                                list(method = "restyle",
                                     args = list("visible", c(T,T)),
                                     args2 = list("visible", c(T,F)),
                                     label = "Toggle filtered"))))) %>%
                    plotly::toWebGL() %>%
                    plotly::hide_colorbar() %>%
                    plotly::event_register("plotly_doubleclick")
            })
        })
        }
    })

    lapply(paste0("plot", seq(1,4)), function(nm) {
        shiny::observeEvent(plotly::event_data("plotly_selected", source=nm), {
        # inform the world about the new brush range

            selected <- plotly::event_data("plotly_selected", source=nm)
            if (is.null(selected)){
                selected_points$data <- NULL
            } else {
                selected_points$data[[nm]] <- selected$customdata
            }
        })
        shiny::observeEvent(plotly::event_data("plotly_doubleclick", source=nm), {
            # inform the world about the new brush range
            selected_points$data <- NULL
        })
    })

    shiny::observeEvent(input$reset, {
        selected_points$data <- NULL
        # plotly::plotlyProxy(nm, session) %>%
        #     plotly::plotlyProxyInvoke("restyle", "data", df_selected())
    })

    ### Rule logic --------------------------
    shiny::observeEvent(input$rulebutton, {
        sel <- df_selected()$sel %>% dplyr::select(.$inputs) %>% as.data.frame()
        unsel <- df_selected()$unsel %>% dplyr::select(.$inputs) %>% as.data.frame()

        pyrules <- ExportRules(
            FPM(minimumSig=input$minsig,
                parameterNames=colnames(sel),
                selectedData=sel,
                unselectedData=unsel,
                useEquality=TRUE))

        rules <- fpm(df_filtered(), maxLevel = input$fpmlevel, minSig = input$minsig, selectedData = df_selected()$sel$Iteration)

        output$ruletable <- shiny::renderTable({
            rules %>% dplyr::mutate(Significance = Significance*100,
                                    Unsignificance = Unsignificance*100,
                                    Ratio = Ratio * 100) %>%
                dplyr::select(Rule, Significance, Unsignificance, Ratio) %>%
                head(10)
        })
        output$pyruletable <- shiny::renderTable({
            pyrules %>% dplyr::mutate(Rule = paste(Parameter, Sign, round(Value,0)),
                                      Significance = SEL*100,
                                      Unsignificance = UNSEL*100,
                                      Ratio = Ratio*100) %>%
                dplyr::select(Rule, Significance, Unsignificance, Ratio) %>%
                head(10)
        })
    })
}

# Run the application
shiny::shinyApp(ui = dash, server = server)
