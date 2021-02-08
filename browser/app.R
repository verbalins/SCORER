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
seed <- 42
set.seed(seed)
options(shiny.maxRequestSize = 30*1024^2) # 30MB
#options(shiny.reactlog = TRUE)
reticulate::source_python('../py/FPM.py')

cluster_data <- shiny::reactiveValues("data" =list(), "rpart"=list(), "clara"=list(), "kmeans"=list(), "hclust"=list(), "density"=list(), "saved"="")
debug <- FALSE

# UI for the import tab
# Handles:
# - Data import
# - Import handling of inputs, outputs, and objectives
importUI <- shiny::fluidPage(
    shiny::fluidRow(
        # Data selection and import
        shinydashboard::box(title="Upload data", #width = NULL,
                            shiny::helpText("Select a file exported from OptimizeBrowser, or which contains similar headers."),
            shiny::fileInput("fileupload", "Choose CSV File",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values",
                                 ".csv"))
        )
    ),
    shiny::fluidRow(shinydashboard::box(title="Select Parameters", #width = NULL,
                            shiny::helpText("Select the type of parameters for importing. Parameters not selected in either category will be excluded."),
                            shiny::uiOutput("data_objectives"),
                            shiny::uiOutput("data_inputs"),
                            shiny::uiOutput("data_outputs"),
                            shiny::checkboxInput("distancemetric", "Add distance metric"),
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
                            shiny::helpText("Use this table to filter the data you want to analyze. It will persist through the other tabs."),
                            shiny::helpText("Filter the table and then use the Apply Filter button at the bottom."),
                            shinycssloaders::withSpinner(DT::dataTableOutput("datatable")),
                            shiny::actionButton("applyfilter","Apply Filter",
                                                icon = shiny::icon("check-circle"),
                                                class = "btn-success"),
                            shiny::actionButton("resetfilter","Reset Filter",
                                                icon = shiny::icon("ban"),
                                                class = "btn-danger")
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
                                shiny::tabPanel("Hierarchical",
                                                shiny::radioButtons("hmethod", "Method:", choices=c("hclust","agnes","diana"), inline=TRUE),
                                                shiny::numericInput("numClustHC", "Number of clusters:", 5, min=1, step=1)
                                                ),
                                shiny::tabPanel("Partitioning",
                                                shiny::radioButtons("clustmethod", "Method:", choices=c("kmeans","pam","clara","fanny"), inline=TRUE),
                                                shiny::numericInput("numClust", "Number of clusters:", 5, min=1, step=1)
                                ),
                                shiny::tabPanel("Density",
                                    shiny::radioButtons("dbmethod", "Method:", choices=c("DBSCAN","OPTICS","HDBSCAN"), inline=TRUE),
                                    shiny::numericInput("minpts", "MinPts:", 5, min=1, step=1),
                                    shiny::numericInput("eps", "eps:", 0.1, min=0, step=0.01),
                                    shiny::actionButton("evalClusPerf", "Evaluate eps")),
                                shiny::tabPanel("Decision Trees",
                                                shiny::numericInput("cp", "cp:", 0.01, 0.01, 1, 0.01)
                                )),
                            shiny::actionButton("evalClusters", "Evaluate clusters"),
                            shiny::actionButton("saveClusters", "Apply clusters"))
    ),
    shiny::fluidRow(
        shinydashboard::box(
                            shinycssloaders::withSpinner(shiny::plotOutput("clusterViz", width = "100%"))),
        shinydashboard::box(
                            shinycssloaders::withSpinner(shiny::plotOutput("clusterViz2", width = "100%"))
                            )

    )
)

# UI for the visualization tab
visualizationUI <- shiny::fillPage(
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
                                             shiny::uiOutput("color"),
                                             shiny::uiOutput("colorslider")))
                        )),
                        tabPanel("2D",
                                 shinycssloaders::withSpinner(uiOutput("plots2d"))
                                )
                       )
)

# UI for the rule extraction tab
ruleUI <- shiny::fillPage(
    # Decision trees
    # FPM, through python and R
    shiny::fluidRow(
        shiny::column(3,
        shinydashboard::box(width = NULL,
            #shiny::checkboxInput("test", "test"),
            shiny::tabsetPanel(id="ruleTab", type = "tabs",
                               shiny::tabPanel("FPM",
                                               # Minimum significance
                                               shiny::numericInput("minsig", "Minimum Significance", 0.5, 0.1, 1.0, 0.1),
                                               shiny::numericInput("fpmlevel", "Rule levels", 1, 1, 4, 1)),
                               shiny::tabPanel("Hierarchical")
            ),
            shiny::actionButton("rulebutton", "Create Rules"),
            shiny::textOutput("ruleError", inline = TRUE)
        ),
        shinydashboard::box(width = NULL,
            shiny::tabsetPanel(type="tabs",
                               shiny::tabPanel("RFPM",
                                               shinycssloaders::withSpinner(DT::DTOutput("ruletable"))
                               ),
                               shiny::tabPanel("PyFPM",
                                               shinycssloaders::withSpinner(DT::DTOutput("pyruletable")))
            )
        )
        ),
        shiny::column(9,
                      shinydashboard::box(width = NULL,
                          tags$style(type = "text/css", "#scatter3d {height: calc(100vh - 80px) !important;}"),
                          shinycssloaders::withSpinner(plotly::plotlyOutput("ruleplot", height = "100%", width = "100%"))
                        )
                    )

    )
)

exportUI <- shiny::fillPage(
    shinydashboard::box(title="Download data",
        shiny::helpText("Specify the data to download."),
        shiny::radioButtons("downloadSelect", label="", choices = c("Imported", "Filtered", "Selected", "Rules", "Code")),
        shiny::downloadButton("exportData", "Download")
    ),
    shinydashboard::box(title="Generated Code",
                        shiny::htmlOutput("generateRcode"))
)

# Sidebar UI
sidebar <- shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(id="tabs",
        shinydashboard::menuItem("Import Data", tabName = "import", icon = shiny::icon("table")),
        shinydashboard::menuItem("Filter Data", tabName = "filter", icon = shiny::icon("filter")),
        shinydashboard::menuItem("Clustering", tabName = "cluster", icon = shiny::icon("microscope")),
        shinydashboard::menuItem("Visualization", tabName = "viz", icon = shiny::icon("chart-bar")),
        shinydashboard::menuItem("Rule Extraction", tabName = "rule", icon = shiny::icon("sitemap")),
        shinydashboard::menuItem("Export Data", tabName = "export", icon = shiny::icon("file-export"))
    )
)

# Body UI
body <- shinydashboard::dashboardBody(
    shinydashboard::tabItems(
        shinydashboard::tabItem(tabName = "import", importUI),
        shinydashboard::tabItem(tabName = "filter", filterUI),
        shinydashboard::tabItem(tabName = "cluster", clusterUI),
        shinydashboard::tabItem(tabName = "viz", visualizationUI),
        shinydashboard::tabItem(tabName = "rule", ruleUI),
        shinydashboard::tabItem(tabName = "export", exportUI)
    )
)

dash <- shinydashboard::dashboardPage(
    shinydashboard::dashboardHeader(title = "RulExtR"),
    sidebar,
    body
)

# Define server logic
server <- function(input, output, session) {
    ### Navigation logic ----------------------------------------------
    shiny::observeEvent(input$tabs, {
        if (input$tabs == "cluster") {
            if (is.null(input$clusterDep)) {
                shiny::updateSelectInput(session, "clusterDep", choices=dim_list(), selected=dim_list()$Objectives)
                shiny::updateSelectInput(session, "clusterInDep", choices=dim_list())#, selected=dim_list()$Inputs)
            }
        } else if (input$tabs =="rule") {
            output$ruleError <- shiny::renderText({
                shiny::validate(
                    shiny::need(nrow(df_selected()$unsel) != 0, "Please select data first")
                )
            })
        }
    })
    ### Data upload logic ---------------------------------------------
    uploaded_data <- shiny::reactiveValues(data=NULL)#('data' = FMC)

    current_data <- reactive({
        uploaded_data$data
    })

    data_parameters <- shiny::reactive({
        if (nrow(current_data())==0 || is.null(current_data())){
            return(list(header = "",
                 inputs = "",
                 outputs = "",
                 objectives = ""))
        } else {
            return(list(header = colnames(current_data()),
                 inputs = current_data()$inputs,
                 outputs = current_data()$outputs,
                 objectives = current_data()$objectives))
        }
    })

    output$data_inputs <- shiny::renderUI({
        shiny::selectInput("data_inputs", "Inputs", data_parameters()$header, data_parameters()$inputs, multiple = TRUE)
    })

    output$data_outputs <- shiny::renderUI({
        shiny::selectInput("data_outputs", "Outputs", data_parameters()$header, data_parameters()$outputs, multiple = TRUE)
    })

    output$data_objectives <- shiny::renderUI({
        shiny::selectInput("data_objectives", "Objectives", data_parameters()$header, data_parameters()$objectives, multiple = TRUE)
    })

    shiny::observeEvent(input$fileupload, {
        new_data <- input$fileupload
        if (!is.null(new_data)) {
            # Start by getting the column names if available
            uploaded_data$data <- NULL

            uploaded_data$data <- loaddataset(input$fileupload$datapath)
            uploaded_data$data$opt_name <- tools::file_path_sans_ext(input$fileupload$name)
            updateSelectInput(session, "data_objectives", selected = unique(uploaded_data$data$objectives))
        }
    })

    shiny::observeEvent(input$importData, {
        if (!is.null(input$fileupload)) {
            sel <- c(input$data_inputs, input$data_objectives, input$data_outputs)
            uploaded_data$data <- uploaded_data$data %>% dplyr::select("Iteration", dplyr::all_of(sel), "Rank")

            if (input$distancemetric) {
                uploaded_data$data <- uploaded_data$data %>% addDistances(parallelCores = 10)
            }

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
        current_data()[input$datatable_rows_all,]
    })

    output$data_name <- shiny::renderText({ current_data()$opt_name })
    output$datatable <- DT::renderDataTable({
        table <- current_data()
        if (is.null(table))
            return(NULL)

        DT::datatable(table, filter="top", options = list(scrollX = TRUE, scrollY = 600, pageLength = 50), rownames = FALSE)
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
        shiny::selectInput("clusterDep", "Dependent variables:", choices = c("",""), multiple = T)
    })

    output$clusterInDep <- shiny::renderUI({
        shiny::selectInput("clusterInDep", "Independent variables:", choices = c("",""), multiple = T)
    })

    output$clusterViz <- shiny::renderPlot({
        return()
    })

    output$clusterViz2 <- shiny::renderPlot({
        return()
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

    shiny::observeEvent(input$dbmethod, {
        shinyjs::toggleState("eps", condition = !(input$dbmethod == "HDBSCAN"))
        shinyjs::toggleState("evalClusPerf", condition = !(input$dbmethod == "HDBSCAN"))
    })

    shiny::observeEvent(input$evalClusters, {
        # Evaluate the number of clusters to use.
        # Ask to scale data
        cluster_data$data <- current_data() %>% dplyr::select(input$clusterDep, input$clusterInDep)
        if (input$scaleDep) {
            cluster_data$data <- cluster_data$data %>%
                dplyr::mutate(dplyr::across(input$clusterDep,collapse::fscale))
        }

        if (input$clustTab=="Partitioning" || input$clustTab == "Hierarchical") {
            if (input$clustTab == "Hierarchical") {
                clust_method <- switch (input$clustmethod,
                                        "kmeans" = kmeans,
                                        "clara" = cluster::clara,
                )
            } else {
                clust_method <- switch (input$hmethod,
                                        "hclust" = factoextra::hcut,
                                        "agnes" = cluster::agnes,
                                        "diana" = cluster::diana)
            }

            #dissim <- cluster::daisy(cluster_data$data, metric = "gower", stand = TRUE, warnType = FALSE)

            #cluster.suggestion <- factoextra::fviz_nbclust(cluster_data$data, diss=dissim, method="silhouette", FUNcluster = clust_method)
            cluster.suggestion <- factoextra::fviz_nbclust(as.data.frame(cluster_data$data), FUNcluster = clust_method, method="silhouette")
            output$clusterViz <- shiny::renderPlot({
                cluster.suggestion
            })
            k_val <- as.numeric(dplyr::arrange(cluster.suggestion$data, desc(y))[1,1])

            output$clusterViz2 <- shiny::renderPlot({
                factoextra::fviz_cluster(clust_method(as.data.frame(cluster_data$data), k_val), data=cluster_data$data)
            })

            shiny::updateNumericInput(session, "numClustHC", value = k_val)
            shiny::updateNumericInput(session, "numClust", value = k_val)
        } else if (input$clustTab=="Decision Trees") {
            part.data <- cluster_data$data
            dep <- dplyr::if_else(is.null(input$clusterInDep), cluster_data$data$inputs, input$clusterInDep)
            form <- as.formula(paste(paste(input$clusterDep, collapse="+"), " ~ ",paste(dep,collapse = "+"),collapse=" "))
            rprt <- rpart::rpart(form, data=part.data, model=TRUE, control=rpart::rpart.control(cp = input$cp))
            cluster_data$rpart <- rprt
            output$clusterViz <- shiny::renderPlot({
                rpart.plot::rpart.plot(rprt)
            })
        } else { # DBSCAN
            db_method <- input$dbmethod
            if (db_method == "DBSCAN") {
                res <- dbscan::dbscan(cluster_data$data, eps = input$eps, minPts = input$minpts)
            } else if (db_method == "OPTICS") {
                res <- dbscan::optics(cluster_data$data, minPts = input$minpts)
                res <- dbscan::extractXi(res, xi = input$eps)
            } else {
                diss <- cluster::daisy(cluster_data$data, metric = "gower")
                res <- dbscan::hdbscan(cluster_data$data, xdist=diss, minPts = input$minpts)
            }

            cluster_data$density <- res$cluster

            output$clusterViz2 <- shiny::renderPlot({
                ggplot2::ggplot(as.data.frame(cluster_data$data), ggplot2::aes_string(input$clusterDep[1], input$clusterDep[2])) +
                    ggplot2::geom_point(ggplot2::aes(color=res$cluster)) +
                    ggplot2::scale_color_viridis_c()
            })
        }
        # Send output to renderText("clusterText")
    })

    shiny::observeEvent(input$saveClusters, {
        # Apply the clustering
        #c("hclust","agnes","diana")
        #c("kmeans","pam","clara","fanny")
        if (input$clustTab=="Hierarchical") {
            if (input$hmethod=="hclust") {
                h.res <- fastcluster::hclust.vector(as.data.frame(cluster_data$data), method = "ward")
                res <- cutree(h.res, k=input$numClustHC)
                clust <- res
            } else if (input$hmethod=="agnes") {
                res  <- cluster::agnes(cluster::daisy(as.data.frame(cluster_data$data),metric = "gower"), stand=TRUE, method="ward", keep.diss=FALSE, keep.data=FALSE)
            } else if (input$hmethod=="diana") {

            }
        } else if (input$clustTab == "Partitioning") {
            if (input$clustmethod=="kmeans") {
                cluster_data$kmeans <- kmeans(cluster_data$data %>% dplyr::select(input$clusterDep), centers=input$numClust)
                clust <- cluster_data$kmeans$cluster
            } else if (input$clustmethod=="pam"){
                clust <- cluster::pam(cluster_data$data, k=input$numClust, pamonce=5, cluster.only = TRUE)
            } else if (input$clustmethod=="clara"){
                cluster_data$clara <- cluster::clara(cluster_data$data, k=input$numClust, samples=500, pamLike = TRUE, medoids.x = FALSE, keep.data = FALSE)
                clust <- cluster_data$clara$clustering
            } else if (input$clustmethod=="fanny"){
                clust <- cluster::fanny(cluster_data$data, k=input$numClust, cluster.only = TRUE, keep.diss = FALSE, keep.data = FALSE)
            }
        } else if (input$clustTab=="Decision Trees") {
            # Use input$numClust to prune the tree approximately to the number of clusters

        } else { #dbscan
            clust <- cluster_data$density
        }
        # Save selected values from clusterDep
        uploaded_data$data <- current_data() %>% dplyr::mutate(Cluster = clust)
        shiny::updateSelectInput(session, "color", choices = c(grep("Rank|Distance",colnames(current_data()),value = TRUE), "Cluster"))

        cluster_data$saved <- input$clustTab

        output$clusterViz2 <- shiny::renderPlot({
            ggplot2::ggplot(as.data.frame(cluster_data$data), ggplot2::aes_string(input$clusterDep[1], input$clusterDep[2])) +
                ggplot2::geom_point(ggplot2::aes(color=clust)) +
                ggplot2::scale_color_viridis_c()
        })
    })

    ### Visualization logic
    dim_list <- shiny::reactive({
        list(Objectives=unique(current_data()$objectives),
             Inputs=unique(current_data()$inputs),
             Outputs=unique(current_data()$outputs),
             Filter=unique(grep("Rank|Distance|Cluster",colnames(current_data()),value = TRUE)))
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

    output$color <- shiny::renderUI({
        choice <- grep("Rank|Distance|Cluster",colnames(current_data()),value = TRUE)
        shiny::selectInput("color", "Color:", choice, selectize = FALSE)
    })

    output$colorslider <- shiny::renderUI({
        req(input$color)
        min_val <- 0
        max_val <- 0

        if(input$color %in% colnames(df_filtered())) {
            min_val <- floor(min(df_filtered()[[input$color]]))
            max_val <- round(max(df_filtered()[[input$color]]),2)
            stepsize <- switch(input$color, "Rank" = 1, "Distance" = NULL, "Cluster" = 1)
            shiny::sliderInput("colorslider", "Filter Color:", min_val, max_val, value=c(min_val, max_val), round=-2, step = stepsize)
        }
    })

    # Render the parallel coordinates plot
    output$parcoords <- plotly::renderPlotly({
        req(input$color)
        suppressWarnings(
            plotly::plot_ly(type = 'parcoords',
                            dimensions = create_dimensions_list(df_filtered()),
                            line=list(color = stats::formula(paste0("~",shiny::isolate(input$color)))),
                            source = "pcoords",
                            data = df_filtered()) %>%
            plotly::event_register(event="plotly_restyle") %>%
            plotly::toWebGL()
        )
    })

    #React to changes to the colouring variable
    # shiny::observeEvent(input$color, {
    #     if(input$color %in% colnames(df_filtered())) {
    #         isolate({
    #         form <- stats::formula(paste0("~",input$color))
    #         p <- plotly::plotlyProxy("scatter3d", session)
    #         test <- df_selected()$sel[,input$color]
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
                            data = df_selected()$sel,
                            source ="s3d") %>%
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
        isolate({
            if(!is.null(input$colorslider) && input$color %in% colnames(df_filtered())) {
                df_filterdata$sel <- parcoords_sel %>% dplyr::filter(dplyr::between(.[[input$color]], input$colorslider[1], input$colorslider[2]))
            } else {
                df_filterdata$sel <- parcoords_sel
            }
        })
        df_filterdata$unsel <- dplyr::anti_join(df_filtered(), df_filterdata$sel, by="Iteration")
        df_filterdata
    })

    # These reactive values track the set of active brushes
    # Each reactive value corresponds to a different variable
    selected_points <- shiny::reactiveValues('data' = list())

    output$plots2d <- shiny::renderUI({
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
    shiny::observeEvent({{c(input$importData,input$applyfilter)}}, {
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
    output$ruletable <- DT::renderDT(data.frame())
    output$pyruletable <- DT::renderDT(data.frame())
    shiny::observeEvent(input$rulebutton, {
        if (nrow(df_selected()$unsel) == 0) {
           return() # Can't go forward without selecting from the Visualization tab
        }
        sel <- df_selected()$sel %>% dplyr::select(.$inputs) %>% as.data.frame()
        unsel <- df_selected()$unsel %>% dplyr::select(.$inputs) %>% as.data.frame()

        rules$Py <- ExportRules(
            FPM(minimumSig=input$minsig,
                parameterNames=colnames(sel),
                selectedData=sel,
                unselectedData=unsel,
                useEquality=TRUE)) %>%
            dplyr::mutate(Rule = paste(Parameter, Sign, round(Value,0)),
                          Significance = SEL,
                          Unsignificance = UNSEL) %>%
            dplyr::select(Rule, Significance, Unsignificance, Ratio)

        rules$R <- fpm(df_filtered(), maxLevel = input$fpmlevel, minSig = input$minsig, selectedData = df_selected()$sel$Iteration) %>%
            dplyr::select(Rule, Significance, Unsignificance, Ratio)

        output$ruletable <- DT::renderDT(DT_rules(rules$R))
        output$pyruletable <- DT::renderDT(DT_rules(rules$Py))
    })

    DT_rules <- function(data) {
        DT::datatable(data,
                      options = list(searching = FALSE), rownames = FALSE) %>%
            DT::formatPercentage(columns=c('Significance', 'Unsignificance', 'Ratio'), digits=2)
    }

    rules <- shiny::reactiveValues('R' = NULL, 'Py' = NULL)

    rule_sel <- shiny::reactive({
        req(input$ruletable_rows_selected)
        sel <- df_filtered()
        if (!is.null(input$ruletable_rows_selected)) {
            selected_rows <- input$ruletable_rows_selected
            rules_str <- paste(unlist(rules$R[selected_rows,"Rule"]), collapse = " & ")
            sel <- sel %>% dplyr::filter(eval(str2expression(rules_str)))
        }
        unsel <- df_filtered() %>% dplyr::filter(!(Iteration %in% sel$Iteration))
        return(list('sel' = sel, 'unsel' = unsel))
    })

    output$ruleplot <- plotly::renderPlotly({
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
                            data = rule_sel()$sel) %>%
                plotly::add_trace(
                    color = I("gray"),
                    size = I(10),
                    opacity = 0.2,
                    data = rule_sel()$unsel,
                    showlegend=F,
                    visible=T) %>%
                plotly::layout(updatemenus = list(
                    list(
                        type = "buttons",
                        x = 1,
                        buttons = list(
                            list(method = "restyle",
                                 args = list("visible", c(T,T)),
                                 args2 = list("visible", c(T,F)),
                                 label = "Toggle filtered")))
                    )
                )
        )
    })

    output$exportData <- shiny::downloadHandler(
        filename = function() {
            opt_name <- df_filtered()$opt_name
            if (input$downloadSelect =="Code") {
                paste0(opt_name,"-",input$downloadSelect,".R")
            } else {
                paste0(opt_name,"-",input$downloadSelect,".csv")
            }

        },
        content = function(file) {
            if (input$downloadSelect =="Code") {
                getCode() %>%
                    stringr::str_replace_all(pattern = "<br>", replacement = "\n") %>%
                    readr::write_lines(file = file)
            } else {
                data <- switch(input$downloadSelect,
                               "Imported" = uploaded_data$data,
                               "Filtered" = df_filtered(),
                               "Selected" = df_selected()$sel,
                               "Rules" = rules$R)
                write.csv2(as.data.frame(data), file, row.names = FALSE)
            }
        }
    )
    # Get code for the
    getCode <- shiny::reactive({
        pastevector  <- function(x, y) { paste0(x, "=c(", paste0("\"", y, "\"", collapse=", "),")") }
        createfilter <- function(x, y) {
            val <- unlist(stringr::str_split(x, " ... "))
            paste0("dplyr::between(", paste(y, val[1], val[2], sep=","), ")")
        }

        load_data <- paste0("df <- SCORER::loaddataset(file=\"", input$fileupload$name, "\",<br>",
                            pastevector("objectives",uploaded_data$data$objectives), ",<br>",
                            pastevector("inputs",uploaded_data$data$inputs), ",<br>",
                            pastevector("outputs",uploaded_data$data$outputs),")<br>")

        import_data <- paste0("df_imported <- df %>% <br>",
                              "dplyr::select(",paste("Iteration",
                                                     paste(uploaded_data$data$objectives, collapse=","),
                                                     paste(uploaded_data$data$inputs, collapse=","),
                                                     paste(uploaded_data$data$outputs, collapse=","),
                                                     "dplyr::starts_with(c('Rank','Distance'))", collapse=",", sep=","),")<br>")

        filters <- unlist(input$datatable_search_columns)
        names(filters) <- colnames(uploaded_data$data)
        filters <- filters[filters != ""]
        filter_col <- mapply(createfilter, filters, names(filters))
        if (length(filter_col)==0) {
            filter_data <- paste0("df_filtered <- df_imported #No filters applied <br>")
        } else {
            filter_data <- paste0("df_filtered <- df_imported %>% <br>",
                                  "dplyr::filter(", paste0(filter_col, collapse=","),")<br>")
        }

        clustered_data <- paste0("df_clustered <- df_filtered # Not implemented <br>")

        if (all(df_selected()$sel$Iteration %in% df_filtered()$Iteration)) {
            selected_data <- "df_selected <- df_clustered # No selected solutions <br> "
        } else {
            selected_data <- paste("df_selected <- df_clustered %>% <br>",
                                   "dplyr::filter(Iteration %in%",
                                   paste0("c(", paste0(df_selected()$sel$Iteration, collapse=", "),")"),
                                   ") <br>")
        }

        rules <- paste("rules <- SCORER::fpm(df_clustered",
                       paste0("maxLevel=", input$fpmlevel),
                       paste0("minSig=",input$minsig),
                       "selectedData=df_selected$Iteration)", sep=", <br>")

        preamble <- paste0("library(SCORER)<br>",
                           "set.seed(",seed,")<br>")
        paste(preamble,
              load_data,
              import_data,
              filter_data,
              clustered_data,
              selected_data,
              rules, sep = "<br>")
    })

    output$generateRcode <- shiny::renderText({
        getCode()
    })
}

# Run the application
shiny::shinyApp(ui = dash, server = server)
