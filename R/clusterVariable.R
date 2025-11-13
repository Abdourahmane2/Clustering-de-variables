
#' The `clusterVariable` R6 class performs clustering on variables (columns) of a dataset
#' using a custom implementation of the K-means algorithm (`mon_kmeans`).
#' It supports automatic data cleaning, visualization, prediction for new data,
#' and quality evaluation via silhouette scores.
#'
#' @include mon_kmeans.R
#' @import R6 cluster factoextra klaR proxy ggplot2 pheatmap FactoMineR
#' @export
#'
#' @examples
#' data(iris)
#' cv <- clusterVariable$new(k = 3, data = iris[, 1:4], donnee_nettoyee = TRUE)
#' cv$fit()
#' cv$summary()
#' cv$tracer_coude(k_max = 6)
#' cv$indice_silhoute()
#'
clusterVariable <- R6::R6Class(
  "clusterVariable",

  public = list(

    #' @field k Number of clusters to create
    k = NULL,

    #' @field data The dataset used for clustering
    data = NULL,

    #' @field resultat_cluster Results of the clustering process
    resultat_cluster = NULL,

    #======================== Constructor ================================
    #' @description
    #' Create a new `clusterVariable` object.
    #'
    #' @param k Integer. Number of clusters to form (default = 3).
    #' @param data A numeric data frame containing variables to cluster.
    #' @param donnee_nettoyee Logical. If TRUE, applies automatic data cleaning.
    #'
    #' @return A new `clusterVariable` object.
    initialize = function(k = 3, data = NULL, donnee_nettoyee = FALSE) {
      self$k <- k
      self$resultat_cluster <- NULL

      if (!is.null(data)) {
        if (!is.data.frame(data)) stop("Error: data doit etre un data.frame.")
        self$data <- data

        if (donnee_nettoyee == TRUE) {
          self$data <- private$appliquer_nettoyage(self$data)
        }
      }
    },

    #======================== Fit ========================================
    #' @description
    #' Fit the K-means clustering model on the provided dataset.
    #'
    #' @details
    #' The method assumes all variables are numeric. It calls the internal
    #' `mon_kmeans()` function to compute clusters.
    #'
    #' @return Updates the `resultat_cluster` field with cluster assignments and centers.
    fit = function() {
      stopifnot(!is.null(self$data))

      if (is.numeric(as.matrix(self$data))) {
        set.seed(123)
        self$resultat_cluster <- mon_kmeans(self$data, k = self$k)
      } else {
        stop("Data  doit etre numeric.")
      }
    },

    #======================== Predict =====================================
    #' @description
    #' Predict cluster assignments for new data based on existing cluster centers.
    #'
    #' @param nouveau_data A data frame or matrix with the same columns as the training data.
    #' @return A new `clusterVariable` object containing the predicted clusters.
    predict = function(nouveau_data) {
      if (is.null(self$resultat_cluster)) stop("fait le fit avant de lancer la methode predict.")
      if (!is.data.frame(nouveau_data) && !is.matrix(nouveau_data)) {
        stop("Error: le nouveau dataframe fournit doit aussi etre un dataframe.")
      }
      if (!all(colnames(nouveau_data) %in% colnames(self$data))) {
        stop("les colonnes ne correspondent pas .")
      }

      data_t <- t(nouveau_data)
      centers <- self$resultat_cluster$centers

      distances <- matrix(NA, nrow = nrow(centers), ncol = nrow(data_t))
      for (i in 1:nrow(centers)) {
        distances[i, ] <- rowSums((data_t - matrix(centers[i, ],
                                                   nrow = nrow(data_t),
                                                   ncol = ncol(data_t),
                                                   byrow = TRUE))^2)
      }
      clusters_assigne <- apply(distances, 2, which.min)

      pred_obj <- clusterVariable$new(k = self$k, data = nouveau_data)
      pred_obj$resultat_cluster <- list(
        cluster = clusters_assigne,
        centers = self$resultat_cluster$centers
      )

      return(pred_obj)
    },

    #======================== Summary =====================================
    #' @description
    #' Display a summary of the clustering results.
    #'
    #' @return Prints information about clusters, number of variables per cluster,
    #' and the list of variables assigned to each.
    summary = function() {
      if (is.null(self$resultat_cluster)) stop("fait le fit avant de lancer la methode summary.")
      cat("Methode: K-means\n")
      cat("nombre de clusters:", self$k, "\n")
      cat("Variables par cluster:\n")

      for (i in 1:self$k) {
        cat("\nCluster", i, ":", sum(self$resultat_cluster$cluster == i))
      }

      cat("\nVariables par cluster:\n")
      for (i in 1:self$k) {
        cat("\nCluster", i, ": ")
        vars_in_cluster <- names(self$data)[which(self$resultat_cluster$cluster == i)]
        cat(vars_in_cluster, sep = ", ")
      }
      cat("\n")
    },


    #======================== Visualization ========================
    #' @description
    #' Visualize the clusters using `factoextra::fviz_cluster`.
    #'
    #' @return A ggplot object showing clustered variables.
    visualiser_clusters = function() {
      if (is.null(self$resultat_cluster)) stop("fait le fit avant de lancer la methode visualiser_clusters.")
      p <- factoextra::fviz_cluster(
        object = list(
          centers = self$resultat_cluster$centers,
          cluster = self$resultat_cluster$cluster,
          data = t(self$data)
        ),
        ellipse.type = "convex",
        palette = "jco",
        repel = TRUE,
        geom = "point",
        ggtheme = ggplot2::theme_minimal()
      )
      return(p)
    },

    #======================== tracer coude ========================
    #' @description
    #' Plot the elbow curve to determine the optimal number of clusters.
    #'
    #' @param k_max Maximum number of clusters to test (default = 5).
    #' @return Displays an elbow plot.
    tracer_coude = function(k_max = 5) {
      if (is.null(self$data)) stop("Data not set.")
      if (k_max > nrow(t(self$data))) stop("k_max doit etre inferieur au nombre de variables.")

      inertie <- numeric(k_max)
      for (k in 1:k_max) {
        set.seed(123)
        kmeans_result <- mon_kmeans(self$data, k = k)
        centres <- kmeans_result$centers
        clusters <- kmeans_result$cluster

        sum_sq <- 0
        data_t <- t(self$data)
        for (i in 1:k) {
          points_cluster <- data_t[clusters == i, , drop = FALSE]
          if (nrow(points_cluster) > 0) {
            sum_sq <- sum_sq + sum(rowSums((points_cluster -
                                              matrix(centres[i, ],
                                                     nrow = nrow(points_cluster),
                                                     ncol = ncol(points_cluster),
                                                     byrow = TRUE))^2))
          }
        }
        inertie[k] <- sum_sq
      }

      plot(1:k_max, inertie, type = "b",
           xlab = "valeur de k",
           ylab = "valeur de l'inertie intra-classe", ,
           main = "methode du coude")
    },

    #======================== Cluster Summary =================
    #' @description
    #' Return a detailed summary of the cluster partitions and statistics.
    #'
    #' @return A list containing cluster partitions, descriptive statistics,
    #' intra-cluster distances, and inter-cluster distances.
    resume_cluster = function() {
      if (is.null(self$resultat_cluster)) stop("Model not fitted yet. Run fit() first.")

      partitions <- data.frame(
        Cluster = 1:self$k,
        Num_variables = sapply(1:self$k, function(i) sum(self$resultat_cluster$cluster == i))
      )
      partitions$Percentage <- round(partitions$Num_variables /
                                       length(self$resultat_cluster$cluster) * 100, 2)

      groupes <- lapply(1:self$k, function(i) {
        vars_in_cluster <- names(self$data)[self$resultat_cluster$cluster == i]
        summary(self$data[, vars_in_cluster, drop = FALSE])
      })

      distances_intra <- sapply(1:self$k, function(i) {
        cluster_vars <- self$data[, self$resultat_cluster$cluster == i]
        dist_matrix <- dist(t(cluster_vars))
        mean(dist_matrix)
      })

      centers <- self$resultat_cluster$centers
      dist_inter_cluster <- dist(centers)

      return(list(
        partitions = partitions,
        groupes = groupes,
        distances_intra = distances_intra,
        dist_inter_cluster = dist_inter_cluster
      ))
    },

    #======================== Silhouette Index =====================
    #' @description
    #' Compute the average silhouette width for the clustering result.
    #'
    #' @return Numeric value: average silhouette width.
    indice_silhoute = function() {
      if (is.null(self$resultat_cluster)) stop("Model not fitted yet. Run fit() first.")
      data_t <- as.data.frame(t(self$data))
      dist_matrix <- dist(data_t)
      clusters <- self$resultat_cluster$cluster
      sil <- silhouette(clusters, dist_matrix)
      return(mean(sil[, "sil_width"]))
    },

    #======================== Print ===============================
    #' @description
    #' Print a short textual summary of the object.
    print = function() {
      cat("nombre de clusters:", self$k, "\n")
      cat("nombre de variables:", ncol(self$data), "\n")
    }
  ),

  private = list(
    #nettoyage des données
    appliquer_nettoyage = function(data) {
      data <- private$gestion_valeurs_manquantes(data)
      data <- private$gestion_valeurs_aberrantes(data)
      data <- private$normalisation_data(data)
      return(data)
    },

    #gerer les valeurs manquantes
    gestion_valeurs_manquantes = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]][is.na(data[[i]])] <- median(data[[i]], na.rm = TRUE)
        } else {
          data[[i]][is.na(data[[i]])] <- "missing"
        }
      }
      return(data)
    },

    #suprrimer les valeurs aberantes
    gestion_valeurs_aberrantes = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          Q1 <- quantile(data[[i]], 0.25, na.rm = TRUE)
          Q3 <- quantile(data[[i]], 0.75, na.rm = TRUE)
          iqr <- Q3 - Q1
          low <- Q1 - 1.5 * iqr
          high <- Q3 + 1.5 * iqr
          data[[i]][data[[i]] < low] <- low
          data[[i]][data[[i]] > high] <- high
        }
      }
      return(data)
    },

    #normaliser les données
    normalisation_data = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]] <- (data[[i]] - mean(data[[i]], na.rm = TRUE)) /
            sd(data[[i]], na.rm = TRUE)
        }
      }
      return(data)
    }
  )
)
