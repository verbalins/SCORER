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
debug <- FALSE

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
        shinydashboard::tabItem(tabName = "import", mod_import_ui("import")),
        shinydashboard::tabItem(tabName = "filter", mod_filter_ui("filter")),
        shinydashboard::tabItem(tabName = "cluster", mod_cluster_ui("cluster")),
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

    ### Data import logic ---------------------------------------------
    current_data <- mod_import_server("import")
    # Reset filters on Visualization tab when current_data changes
    # shiny::observe({
    #     current_data
    #     ranges$data <- NULL
    #     selected_points$data <- NULL
    # })


    ### Data filter logic ---------------------------------------------
    df_filtered <- mod_filter_server("filter", current_data)

    shiny::observeEvent(input$applyfilter, {
        ranges$data <- NULL
        selected_points$data <- NULL
    })

    ### Cluster logic ------------------------------------------
    df_clustered <- mod_cluster_server("cluster", current_data)

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

        if(input$color %in% colnames(df_filtered()$data)) {
            min_val <- floor(min(df_filtered()$data[[input$color]]))
            max_val <- round(max(df_filtered()$data[[input$color]]),2)
            stepsize <- switch(input$color, "Rank" = 1, "Distance" = NULL, "Cluster" = 1)
            shiny::sliderInput("colorslider", "Filter Color:", min_val, max_val, value=c(min_val, max_val), round=-2, step = stepsize)
        }
    })

    # Render the parallel coordinates plot
    output$parcoords <- plotly::renderPlotly({
        req(input$color)
        suppressWarnings(
            plotly::plot_ly(type = 'parcoords',
                            dimensions = create_dimensions_list(df_filtered()$data),
                            line=list(color = stats::formula(paste0("~",shiny::isolate(input$color)))),
                            source = "pcoords",
                            data = df_filtered()$data) %>%
            plotly::event_register(event="plotly_restyle") %>%
            plotly::toWebGL()
        )
    })

    #React to changes to the colouring variable
    # shiny::observeEvent(input$color, {
    #     if(input$color %in% colnames(df_filtered()$data)) {
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
                    keep_var <- keep_var | dplyr::between(df_filtered()$data[[i]], min(rng), max(rng))
                }
            }
            keep <- keep & keep_var
        }
        parcoords_sel <- df_filtered()$data[keep, ]

        for (i in names(selected_points$data)) {
            parcoords_sel <- parcoords_sel %>% dplyr::filter(Iteration %in% unlist(selected_points$data[i]))
        }
        isolate({
            if(!is.null(input$colorslider) && input$color %in% colnames(df_filtered()$data)) {
                df_filterdata$sel <- parcoords_sel %>% dplyr::filter(dplyr::between(.[[input$color]], input$colorslider[1], input$colorslider[2]))
            } else {
                df_filterdata$sel <- parcoords_sel
            }
        })
        df_filterdata$unsel <- dplyr::anti_join(df_filtered()$data, df_filterdata$sel, by="Iteration")
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
    shiny::observeEvent({{df_filtered}}, {
        max_plots <- length(dim_list()$Objectives)
        for (i in 1:max_plots-1) {
        # Need local so that each item gets its own number. Without it, the value
        # of i in the renderPlot() will be the same across all instances, because
        # of when the expression is evaluated.
        local({
            my_i <- i
            plotname <- paste0("plot", my_i)

            output[[plotname]] <- plotly::renderPlotly({
                SCORER::plot2d(df_selected()$sel, input$x, input$y, input$color, df_selected()$unsel) %>%
                    #plotly::plot_ly(source = plotname) %>%
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

        rules$R <- fpm(df_filtered()$data, maxLevel = input$fpmlevel, minSig = input$minsig, selectedData = df_selected()$sel$Iteration) %>%
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
        sel <- df_filtered()$data
        if (!is.null(input$ruletable_rows_selected)) {
            selected_rows <- input$ruletable_rows_selected
            rules_str <- paste(unlist(rules$R[selected_rows,"Rule"]), collapse = " & ")
            sel <- sel %>% dplyr::filter(eval(str2expression(rules_str)))
        }
        unsel <- df_filtered()$data %>% dplyr::filter(!(Iteration %in% sel$Iteration))
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
            opt_name <- df_filtered()$data$opt_name
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
                               "Imported" = current_data(),
                               "Filtered" = df_filtered()$data,
                               "Selected" = df_selected()$sel,
                               "Rules" = rules$R)
                write.csv2(as.data.frame(data), file, row.names = FALSE)
            }
        }
    )
    # Get code for the analysis
    getCode <- shiny::reactive({
        pastevector  <- function(x, y) { paste0(x, "=c(", paste0("\"", y, "\"", collapse=", "),")") }
        createfilter <- function(x, y) {
            val <- unlist(stringr::str_split(x, " ... "))
            paste0("dplyr::between(", paste(y, val[1], val[2], sep=","), ")")
        }

        load_data <- paste0("df <- SCORER::loaddataset(file=\"", input$fileupload$name, "\",<br>",
                            pastevector("objectives",current_data()$objectives), ",<br>",
                            pastevector("inputs",current_data()$inputs), ",<br>",
                            pastevector("outputs",current_data()$outputs),")<br>")

        import_data <- paste0("df_imported <- df %>% <br>",
                              "dplyr::select(",paste("Iteration",
                                                     paste(current_data()$objectives, collapse=","),
                                                     paste(current_data()$inputs, collapse=","),
                                                     paste(current_data()$outputs, collapse=","),
                                                     "dplyr::starts_with(c('Rank','Distance'))", collapse=",", sep=","),")<br>")

        filters <- unlist(df_filtered()$filters)
        names(filters) <- colnames(current_data())
        filters <- filters[filters != ""]
        filter_col <- mapply(createfilter, filters, names(filters))
        if (length(filter_col)==0) {
            filter_data <- paste0("df_filtered <- df_imported #No filters applied <br>")
        } else {
            filter_data <- paste0("df_filtered <- df_imported %>% <br>",
                                  "dplyr::filter(", paste0(filter_col, collapse=","),")<br>")
        }

        clustered_data <- paste0("df_clustered <- df_filtered # Not implemented <br>")

        if (nrow(df_selected()$unsel) == 0) {
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
