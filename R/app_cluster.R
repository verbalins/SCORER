# UI for the cluster tab
# Handles:
# - Cluster identification and application
# Start clustering, accept results to dataset
#   Apply with new or same button as previous?
# Cluster performance (Visualization is handled in next tab)
#   Performance can be shown so the user can get some guidance on the number of clusters
mod_cluster_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(shiny::fillPage(
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
      shinydashboard::box(
        title = "Cluster Variables",
        shiny::uiOutput(ns("clusterDep")),
        shiny::uiOutput(ns("clusterInDep")),
        shiny::checkboxInput(ns("scaleDep"), "Scale Dependents", TRUE)
      ),
      shinydashboard::box(
        #shiny::radioButtons("clustermethod", "Cluster method:",
        #                   c("rpart", "kmeans", "clara", "hclust", "dbscan")),
        shiny::tabsetPanel(
          id = ns("clustTab"),
          type = "tabs",
          shiny::tabPanel("Hierarchical",
            shiny::radioButtons(ns("hmethod"), "Method:", choices = c("hclust", "agnes", "diana"), inline = TRUE),
            shiny::numericInput(ns("numClustHC"), "Number of clusters:", 5, min = 1, step = 1)
            ),
          shiny::tabPanel("Partitioning",
            shiny::radioButtons(ns("clustmethod"), "Method:", choices = c("kmeans", "pam", "clara", "fanny"), inline = TRUE),
            shiny::numericInput(ns("numClust"), "Number of clusters:", 5, min = 1, step = 1)
          ),
          shiny::tabPanel("Density",
            shiny::radioButtons(ns("dbmethod"), "Method:", choices = c("DBSCAN", "OPTICS", "HDBSCAN"), inline = TRUE),
            shiny::numericInput(ns("minpts"), "MinPts:", 5, min = 1, step = 1),
            shiny::numericInput(ns("eps"), "eps:", 0.1, min = 0, step = 0.01),
            shiny::actionButton(ns("evalClusPerf"), "Evaluate eps")
          ),
          shiny::tabPanel("Decision Trees",
            shiny::numericInput(ns("cp"), "cp:", 0.01, 0.01, 1, 0.01)
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
      shinydashboard::box(shinycssloaders::withSpinner(shiny::plotOutput(
        ns("clusterVizRight"), width = "100%"
      )))

    )
  ))
}

cluster_data <- shiny::reactiveValues("data" =list(), "rpart"=list(), "clara"=list(), "kmeans"=list(), "hclust"=list(), "density"=list(), "saved"="")

mod_cluster_server <- function(id, current_data) {
  shiny::moduleServer(id,
  function(input, output, session) {
    ### Cluster logic ------------------------------------------
    output$clusterDep <- shiny::renderUI({
      ns <- session$ns
      shiny::selectInput(ns("clusterDep"),"Dependent variables:",choices = c("", ""),multiple = T)
    })

    output$clusterInDep <- shiny::renderUI({
      ns <- session$ns
      shiny::selectInput(ns("clusterInDep"), "Independent variables:", choices = c("", ""), multiple = T)
    })

    output$clusterVizLeft <- shiny::renderPlot({
      return()
    })

    output$clusterVizRight <- shiny::renderPlot({
      return()
    })

    shiny::observeEvent(input$evalClusPerf, {
      # Ask to scale data
      cluster_data$data <- current_data() %>% dplyr::select(input$clusterDep, input$clusterInDep)
      if (input$scaleDep) {
        cluster_data$data <- cluster_data$data %>%
          dplyr::mutate(dplyr::across(input$clusterDep, collapse::fscale))
      }

      if (input$clustTab == "dbscan") {
        output$clusterVizLeft <- shiny::renderPlot({
          dbscan::kNNdistplot(cluster_data$data, k = input$minpts)
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
          dplyr::mutate(dplyr::across(input$clusterDep, collapse::fscale))
      }

      if (input$clustTab == "Partitioning" ||
          input$clustTab == "Hierarchical") {
        if (input$clustTab == "Hierarchical") {
          clust_method <- switch (input$clustmethod,
                                  "kmeans" = kmeans,
                                  "clara" = cluster::clara,)
        } else {
          clust_method <- switch (
            input$hmethod,
            "hclust" = factoextra::hcut,
            "agnes" = cluster::agnes,
            "diana" = cluster::diana
          )
        }

        #dissim <- cluster::daisy(cluster_data$data, metric = "gower", stand = TRUE, warnType = FALSE)

        #cluster.suggestion <- factoextra::fviz_nbclust(cluster_data$data, diss=dissim, method="silhouette", FUNcluster = clust_method)
        cluster.suggestion <-
          factoextra::fviz_nbclust(
            as.data.frame(cluster_data$data),
            FUNcluster = clust_method,
            method = "silhouette"
          )
        output$clusterVizLeft <- shiny::renderPlot({
          cluster.suggestion
        })
        k_val <-
          as.numeric(dplyr::arrange(cluster.suggestion$data, desc(y))[1, 1])

        output$clusterVizRight <- shiny::renderPlot({
          factoextra::fviz_cluster(clust_method(as.data.frame(cluster_data$data), k_val), data =
                                     cluster_data$data)
        })

        shiny::updateNumericInput(session, "numClustHC", value = k_val)
        shiny::updateNumericInput(session, "numClust", value = k_val)
      } else if (input$clustTab == "Decision Trees") {
        part.data <- cluster_data$data
        dep <-
          dplyr::if_else(
            is.null(input$clusterInDep),
            cluster_data$data$inputs,
            input$clusterInDep
          )
        form <- as.formula(paste(
            paste(input$clusterDep, collapse = "+"), " ~ ",
            paste(dep, collapse = "+"),
            collapse = " "))

        rprt <- rpart::rpart(
            form,
            data = part.data,
            model = TRUE,
            control = rpart::rpart.control(cp = input$cp)
          )
        cluster_data$rpart <- rprt
        output$clusterVizLeft <- shiny::renderPlot({
          rpart.plot::rpart.plot(rprt)
        })
      } else {
        # DBSCAN
        db_method <- input$dbmethod
        if (db_method == "DBSCAN") {
          res <- dbscan::dbscan(cluster_data$data, eps = input$eps, minPts = input$minpts)
        } else if (db_method == "OPTICS") {
          res <- dbscan::optics(cluster_data$data, minPts = input$minpts)
          res <- dbscan::extractXi(res, xi = input$eps)
        } else {
          diss <- cluster::daisy(cluster_data$data, metric = "gower")
          res <- dbscan::hdbscan(cluster_data$data, xdist = diss, minPts = input$minpts)
        }

        cluster_data$density <- res$cluster

        output$clusterVizRight <- shiny::renderPlot({
          ggplot2::ggplot(
            as.data.frame(cluster_data$data),
            ggplot2::aes_string(input$clusterDep[1], input$clusterDep[2])
          ) +
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
          h.res <-
            fastcluster::hclust.vector(as.data.frame(cluster_data$data), method = "ward")
          res <- cutree(h.res, k = input$numClustHC)
          clust <- res
        } else if (input$hmethod == "agnes") {
          res  <-
            cluster::agnes(
              cluster::daisy(as.data.frame(cluster_data$data), metric = "gower"),
              stand = TRUE,
              method = "ward",
              keep.diss = FALSE,
              keep.data = FALSE
            )
        } else if (input$hmethod == "diana") {

        }
      } else if (input$clustTab == "Partitioning") {
        if (input$clustmethod == "kmeans") {
          cluster_data$kmeans <-
            kmeans(
              cluster_data$data %>% dplyr::select(input$clusterDep),
              centers = input$numClust
            )
          clust <- cluster_data$kmeans$cluster
        } else if (input$clustmethod == "pam") {
          clust <-
            cluster::pam(
              cluster_data$data,
              k = input$numClust,
              pamonce = 5,
              cluster.only = TRUE
            )
        } else if (input$clustmethod == "clara") {
          cluster_data$clara <-
            cluster::clara(
              cluster_data$data,
              k = input$numClust,
              samples = 500,
              pamLike = TRUE,
              medoids.x = FALSE,
              keep.data = FALSE
            )
          clust <- cluster_data$clara$clustering
        } else if (input$clustmethod == "fanny") {
          clust <-
            cluster::fanny(
              cluster_data$data,
              k = input$numClust,
              cluster.only = TRUE,
              keep.diss = FALSE,
              keep.data = FALSE
            )
        }
      } else if (input$clustTab == "Decision Trees") {
        # Use input$numClust to prune the tree approximately to the number of clusters

      } else {
        #dbscan
        clust <- cluster_data$density
      }
      # Save selected values from clusterDep
      uploaded_data$data <-
        current_data() %>% dplyr::mutate(Cluster = clust)
      shiny::updateSelectInput(session, "color", choices = c(grep(
        "Rank|Distance", colnames(current_data()), value = TRUE
      ), "Cluster"))

      cluster_data$saved <- input$clustTab

      output$clusterVizRight <- shiny::renderPlot({
        ggplot2::ggplot(
          as.data.frame(cluster_data$data),
          ggplot2::aes_string(input$clusterDep[1], input$clusterDep[2])
        ) +
          ggplot2::geom_point(ggplot2::aes(color = clust)) +
          ggplot2::scale_color_viridis_c()
      })
    })
  })
}
