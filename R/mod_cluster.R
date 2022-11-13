# UI for the cluster tab
# Handles:
# - Cluster identification and application
# Start clustering, accept results to dataset
#   Apply with new or same button as previous?
# Cluster performance (Visualization is handled in next tab)
#   Performance can be shown so the user can get some guidance on the
#   number of clusters
mod_cluster_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(shinyjs::useShinyjs(), shiny::fluidPage(
    # Choose the clustering dimensions
    #   Selectinput
    # Choose the clustering algorithm
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
    #   Performance can be shown so the user can get some
    #   guidance on the number of clusters
    shiny::fluidRow(
      shinydashboard::box(
        title = "Cluster Variables",
        shiny::uiOutput(ns("cluster_dep")),
        shiny::uiOutput(ns("cluster_in_dep")),
        shiny::actionButton(ns("add_inputs"), "Add all inputs"),
        shiny::actionButton(ns("reset_button"), "Reset values"),
        shiny::checkboxInput(ns("scale_dep"), "Scale Dependents", FALSE)
      ),
      shinydashboard::box(
        #shiny::radioButtons("clustermethod", "Cluster method:",
        #                   c("rpart", "kmeans", "clara", "hclust", "dbscan")),
        shiny::tabsetPanel(
          id = ns("clustTab"),
          type = "tabs",
          shiny::tabPanel("Hierarchical",
            shiny::radioButtons(ns("hmethod"),
                                "Method:",
                                #choices = c("hclust", "agnes", "diana"),
                                choices = c("hclust"),
                                inline = TRUE),
            shiny::numericInput(ns("numClustHC"),
                                "Number of clusters:", 5, min = 1, step = 1)
            ),
          shiny::tabPanel("Partitioning",
            shiny::radioButtons(ns("clustmethod"),
                                "Method:",
                                # Clara and fanny are difficult to shut down
                                #choices = c("kmeans", "pam", "clara", "fanny"),
                                choices = c("kmeans", "pam"),
                                inline = TRUE),
            shiny::numericInput(ns("numClust"),
                                "Number of clusters:", 5, min = 1, step = 1)
          ),
          shiny::tabPanel("Density",
            shiny::radioButtons(ns("dbmethod"),
                                "Method:",
                                choices = c("DBSCAN", "OPTICS", "HDBSCAN"),
                                inline = TRUE),
            shiny::numericInput(ns("minpts"),
                                "MinPts:",
                                5,
                                min = 1,
                                step = 1),
            shiny::numericInput(ns("eps"), "eps:", 0.1, min = 0, step = 0.01),
            shiny::actionButton(ns("eval_clus_perf"), "Evaluate eps")
          ),
          shiny::tabPanel("Decision Trees",
            shiny::helpText("Assign the pruning parameter and max depth of tree."),
            shiny::numericInput(ns("cp"), "Pruning parameter:", 0.05, 0.001, 1, 0.001),
            shiny::numericInput(ns("maxdepth"), "Maximum depth of tree:", 5, 1, 50, 1)
          )
        ),
        shiny::actionButton(ns("evalClusters"), "Evaluate clusters"),
        shiny::actionButton(ns("saveClusters"), "Apply clusters")
      )
    ),
    shiny::fluidRow(
      shinydashboard::box(shinycssloaders::withSpinner(shiny::plotOutput(
        ns("clusterVizLeft"), width = "100%"
      ))),
      shinydashboard::box(shinycssloaders::withSpinner(plotly::plotlyOutput(
        ns("clusterVizRight"), width = "100%"
      )))
    )
  ))
}

cluster_data <- shiny::reactiveValues("data" = list(),
                                      "rpart" = list(),
                                      "clara" = list(),
                                      "kmeans" = list(),
                                      "hclust" = list(),
                                      "density" = list(),
                                      "saved" = "",
                                      "cluster" = NULL)

mod_cluster_server <- function(id, r) {
  shiny::moduleServer(id,
  function(input, output, session) {
    ns <- session$ns
    ### Cluster logic ------------------------------------------
    output$cluster_dep <- shiny::renderUI({
      shiny::selectInput(ns("cluster_dep"),
                         "Dependent variables:",
                         choices = dim_list(),
                         selected = dim_list()$Objectives,
                         multiple = T)
    })

    output$cluster_in_dep <- shiny::renderUI({
      shiny::selectInput(ns("cluster_in_dep"),
                         "Independent variables:",
                         choices = dim_list(),
                         multiple = T)
    })

    dim_list <- shiny::reactive({
      list("Objectives" = as.list(unique(r$filtered_data$objective_names)),
           "Inputs" = as.list(unique(r$filtered_data$inputs)),
           "Outputs" = as.list(unique(r$filtered_data$outputs)),
           "Filter" = as.list(unique(grep("Rank|Distance|Cluster",
                                          colnames(r$filtered_data),
                                          value = TRUE))))
    })

    shiny::observeEvent(input$add_inputs, {
      shiny::updateSelectInput(session, "cluster_in_dep",
                               choices = dim_list(),
                               selected = dim_list()$Inputs)
    })

    shiny::observeEvent(input$reset_button, {
      shiny::updateSelectInput(session, "cluster_dep", choices = dim_list())
      shiny::updateSelectInput(session, "cluster_in_dep", choices = dim_list())
    })

    shiny::observeEvent(r$filepath, {
      output$clusterVizLeft <- shiny::renderPlot({
        return()
      })

      output$clusterVizRight <- shiny::renderPlot({
        return()
      })
    })


    output$clusterVizLeft <- shiny::renderPlot({
      return()
    })

    output$clusterVizRight <- shiny::renderPlot({
      return()
    })

    shiny::observeEvent(input$eval_clus_perf, {
      # Ask to scale data
      cluster_data$data <- r$filtered_data %>%
        dplyr::select(input$cluster_dep, input$cluster_in_dep)

      if (input$scale_dep) {
        cluster_data$data <- cluster_data$data %>%
          dplyr::mutate(dplyr::across(input$cluster_dep,
                                      scales::rescale,
                                      to = c(0, 1)))
      }

      if (input$dbmethod == "DBSCAN") {
        output$clusterVizLeft <- shiny::renderPlot({
          dbscan::kNNdistplot(shiny::isolate(cluster_data$data), k = input$minpts)
        })
      }
    })

    shiny::observeEvent(input$dbmethod, {
      if (input$dbmethod == "DBSCAN") {
        shiny::updateNumericInput(session, "eps",
                                  label = "eps:",
                                  max = NULL)
      } else {
        shiny::updateNumericInput(session, "eps",
                                  label = "Xi Threshold:",
                                  max = 1)
      }

      shinyjs::toggle("eps",
                           condition = !(input$dbmethod == "HDBSCAN"))
      shinyjs::toggle("eval_clus_perf",
                           condition = !(input$dbmethod == "HDBSCAN"))
    })

    shiny::observeEvent(input$evalClusters, {
      #validate(need(input$cluster_dep, label="objectives")) # Needs objectives
      # Evaluate the number of clusters to use.
      # Ask to scale data
      if (is.null(input$cluster_in_dep)) {
        cluster_data$data <- r$filtered_data %>%
          dplyr::select(input$cluster_dep, r$data$inputs)
        r$cluster$dep <- input$cluster_dep
      } else {
        cluster_data$data <- r$filtered_data %>%
          dplyr::select(input$cluster_dep, input$cluster_in_dep)
      }
      r$cluster$indep <- input$cluster_in_dep

      if (input$scale_dep) {
        cluster_data$data <- cluster_data$data %>%
          dplyr::mutate(dplyr::across(input$cluster_dep,
                                      scales::rescale,
                                      to = c(0, 1)))

        r$cluster$scale <- input$scale_dep
      }

      if (input$clustTab == "Partitioning" ||
          input$clustTab == "Hierarchical") {

        if (input$clustTab == "Partitioning") {
          clust_method <- switch(input$clustmethod,
                                 "kmeans" = kmeans,
                                 "pam" = cluster::pam,
                                 "clara" = cluster::clara,
                                 "fanny" = cluster::fanny
                                 )
        } else {
          clust_method <- switch(input$hmethod,
                                 "hclust" = factoextra::hcut,
                                 "agnes" = cluster::agnes,
                                 "diana" = cluster::diana
                                 )
        }

        output$clusterVizLeft <- shiny::renderPlot({
          cluster.suggestion <- factoextra::fviz_nbclust(
            as.data.frame(cluster_data$data),
            FUNcluster = clust_method,
            method = "silhouette"
          )

          k_val <- as.numeric(dplyr::arrange(cluster.suggestion$data,
                                             dplyr::desc(y))[1, 1])

          output$clusterVizRight <- shiny::renderPlot({
            factoextra::fviz_cluster(
              clust_method(as.data.frame(cluster_data$data), k_val),
              data = cluster_data$data)
          })

          shiny::updateNumericInput(session, "numClustHC", value = k_val)
          shiny::updateNumericInput(session, "numClust", value = k_val)
          cluster.suggestion # Plot it
        })


      } else if (input$clustTab == "Decision Trees") {
        part.data <- cluster_data$data

        if (is.null(input$cluster_in_dep)) {
          indep <- cluster_data$data$inputs
        } else {
          indep <- input$cluster_in_dep
        }

        r$cluster$method <- "rpart::rpart"
        r$cluster$form <- paste(
          paste(input$cluster_dep, collapse = " + "), " ~ ",
          paste(indep, collapse = " + "),
          collapse = " ")
        form <- as.formula(r$cluster$form)
        r$cluster$params <- paste0("model = TRUE, control = ", paste0("rpart::rpart.control(cp = ",
                                                    input$cp,
                                                    ", maxdepth = ",
                                                    input$maxdepth,
                                                    ")"))

        rprt <- rpart::rpart(
            form,
            data = part.data,
            model = TRUE,
            control = rpart::rpart.control(cp = input$cp,
                                           maxdepth = input$maxdepth)
          )

        cluster_data$rpart <- rprt

        output$clusterVizLeft <- shiny::renderPlot({
          rpart.plot::rpart.plot(rprt)
        })

      } else {
        # DBSCAN
        db_method <- input$dbmethod
        if (db_method == "DBSCAN") {
          res <- dbscan::dbscan(cluster_data$data,
                                eps = input$eps,
                                minPts = input$minpts)
          r$cluster$method <- "dbscan::dbscan"
          r$cluster$params <- paste0("eps = ", input$eps, ", minPts = ", input$minpts)
        }
        else if (db_method == "OPTICS") {
          res <- dbscan::optics(cluster_data$data, minPts = input$minpts)
          # TODO: Create validation for this condition
          if (dplyr::between(input$eps, 0, 1)) {
            res <- dbscan::extractXi(res, xi = input$eps)
          } else {
            res <- dbscan::extractXi(res, xi = 0.9)
          }
          r$cluster$method <- "dbscan::optics"
          r$cluster$params <- paste0("minPts = ", input$minpts)
          r$cluster$params2 <- paste0("xi = ", input$eps)
        } else {
          shiny::updateNumericInput(session, "minpts", min = 2)
          res <- dbscan::hdbscan(cluster_data$data,
                                 minPts = input$minpts)
          r$cluster$method <- "dbscan::hdbscan"
          r$cluster$params <- paste0("minPts = ", input$minpts)
        }
        cluster_data$density <- res$cluster

        output$clusterVizRight <- shiny::renderPlot({
          ggplot2::ggplot(
            as.data.frame(cluster_data$data),
            ggplot2::aes_string(input$cluster_dep[1], input$cluster_dep[2])) +
          ggplot2::geom_point(ggplot2::aes(color = res$cluster)) +
          ggplot2::scale_color_viridis_c()
        })
      }
      # Send output to renderText("clusterText")
    })

    shiny::observeEvent(input$saveClusters, {
      # Apply the clustering
      #c("hclust","agnes","diana")
      #c("kmeans","pam","clara","fanny")
      if (input$clustTab == "Hierarchical") {
        if (input$hmethod == "hclust") {
          h.res <- fastcluster::hclust.vector(as.data.frame(cluster_data$data),
                                              method = "ward")
          res <- cutree(h.res, k = input$numClustHC)
          clust <- res
          r$cluster$method <- "fastcluster::hclust.vector"
          r$cluster$params <- "method = 'ward'"
          r$cluster$params2 <- paste0("k = ", input$numClustHC)
        } else if (input$hmethod == "agnes") {
          res  <-
            cluster::agnes(
              cluster::daisy(as.data.frame(cluster_data$data),
                             metric = "gower",
                             stand = TRUE),
              stand = TRUE,
              method = "ward",
              keep.diss = FALSE,
              keep.data = FALSE
            )
          clust <- res
        } else if (input$hmethod == "diana") {
          res  <-
            cluster::diana(
              cluster::daisy(as.data.frame(cluster_data$data),
                             metric = "gower",
                             stand = TRUE),
              stand = TRUE,
              method = "ward",
              keep.diss = FALSE,
              keep.data = FALSE
            )
          clust <- res
        }
      } else if (input$clustTab == "Partitioning") {
        if (input$clustmethod == "kmeans") {
          cluster_data$kmeans <- stats::kmeans(cluster_data$data %>%
                                          dplyr::select(input$cluster_dep),
                                        centers = input$numClust)
          clust <- cluster_data$kmeans$cluster
          r$cluster$method <- "stats::kmeans"
          r$cluster$params <- paste0("centers = ", input$numClust)
        } else if (input$clustmethod == "pam") {
          clust <- cluster::pam(cluster_data$data,
                                k = input$numClust,
                                pamonce = 5,
                                cluster.only = TRUE)
          r$cluster$method <- "cluster::pam"
          r$cluster$params <- paste0("k = ", input$numClust,
                                   ", pamonce = 5, cluster.only = TRUE")
        } else if (input$clustmethod == "clara") {
          cluster_data$clara <-
            cluster::clara(cluster_data$data,
                           k = input$numClust,
                           samples = 500,
                           pamLike = TRUE,
                           medoids.x = FALSE,
                           keep.data = FALSE)
          clust <- cluster_data$clara$clustering
        } else if (input$clustmethod == "fanny") {
          clust <- cluster::fanny(cluster_data$data,
                                  k = input$numClust,
                                  cluster.only = TRUE,
                                  keep.diss = FALSE,
                                  keep.data = FALSE)
        }
      } else if (input$clustTab == "Decision Trees") {
        r$cluster$method = "rpart::rpart"
        # Use input$numClust to prune the tree approximately to the number of clusters
        tree_party <- partykit::as.party(cluster_data$rpart)
        tree_pred <- predict(tree_party, type = "node")
        clust <- as.numeric(
          forcats::fct_recode(factor(tree_pred),
                              !!!as.list(setNames(as.character(unique(tree_pred)),
                              seq_len(length(unique(tree_pred)))))))
      } else {
        #dbscan
        clust <- cluster_data$density
      }
      # Save selected values from cluster_dep
      cluster_data$cluster <- clust

      cluster_data$saved <- input$clustTab

      output$clusterVizRight <- plotly::renderPlotly({
        if (length(shiny::isolate(input$cluster_dep)) == 2) {
          x <- shiny::isolate(input$cluster_dep[1])
          y <- shiny::isolate(input$cluster_dep[2])
          plotly::plot_ly(data = shiny::isolate(cluster_data$data),
                          type = "scattergl",
                          name = "All Data",
                          x = stats::as.formula(paste0("~", x)),
                          y = stats::as.formula(paste0("~", y)),
                          color = stats::formula(paste0("~", "clust")),
                          #customdata = ~Iteration,
                          mode = "markers",
                          size = I(30),
                          opacity = 1,
                          hovertemplate = paste(paste0("<b>", x, "</b>: %{x}"),
                                                paste0("<br><b>", y, "</b>: %{y}")))
        } else if (length(shiny::isolate(input$cluster_dep)) == 3) {
          x <- shiny::isolate(input$cluster_dep[1])
          y <- shiny::isolate(input$cluster_dep[2])
          z <- shiny::isolate(input$cluster_dep[3])
          plotly::plot_ly(data = shiny::isolate(cluster_data$data),
                          type = "scatter3d",
                          name = "All Data",
                          x = stats::as.formula(paste0("~", x)),
                          y = stats::as.formula(paste0("~", y)),
                          z = stats::as.formula(paste0("~", z)),
                          color = stats::formula(paste0("~", "clust")),
                          #customdata = ~Iteration,
                          mode = "markers",
                          size = I(30),
                          opacity = 1,
                          hovertemplate = paste(paste0("<b>", x, "</b>: %{x}"),
                                                paste0("<br><b>", y, "</b>: %{y}"),
                                                paste0("<br><b>", z, "</b>: %{z}")))
        }

        #ggplot2::ggplot(
        #  as.data.frame(shiny::isolate(cluster_data$data)),
        #  ggplot2::aes_string(shiny::isolate(input$cluster_dep[1]), shiny::isolate(input$cluster_dep[2]))) +
        #  ggplot2::geom_point(ggplot2::aes(color = as.factor(clust))) +
        #  ggplot2::scale_color_viridis_d() +
        #  ggplot2::theme_minimal() +
        #  ggplot2::guides(color =
        #                    ggplot2::guide_legend(
        #                      title = "Cluster")
        #                  )
      })

      r$filtered_data <- r$filtered_data %>%
        dplyr::mutate(Cluster = cluster_data$cluster)
    })
  })
}
