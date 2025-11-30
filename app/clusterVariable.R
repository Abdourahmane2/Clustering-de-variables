#' Variable Clustering using K-means Algorithm
#'
#' The `clusterVariable` R6 class performs clustering on variables (columns) of a dataset
#' using a custom implementation of the K-means algorithm (`mon_kmeans`).
#' It supports automatic data cleaning, visualization, prediction for new data,
#' and quality evaluation via silhouette scores.
#'
#' @include mon_kmeans.R
#' @import R6 cluster factoextra ggplot2 pheatmap FactoMineR
#' @export
#'
#' @examples
#' data(iris)
#' cv <- clusterVariable$new(k = 3)
#' cv$fit(iris[, 1:4])
#' cv$summary()
#' cv$plot_elbow(k_max = 6)
#' cv$plot_silhouette()
clusterVariable <- R6::R6Class(
  "clusterVariable",

  public = list(

    #' @field k Number of clusters to create
    k = NULL,

    #' @field data The dataset used for clustering (stored after fit)
    data = NULL,

    #' @field cluster_result Results of the clustering process
    cluster_result = NULL,

    #======================== Constructor ================================
    #' @description
    #' Create a new `clusterVariable` object.
    #'
    #' @param k Integer. Number of clusters to form (default = 3).
    #' @param max_iter Integer. Maximum number of iterations for k-means (default = 100).
    #' @param auto_clean Logical. If TRUE, applies automatic data cleaning during fit (default = TRUE).
    #'
    #' @return A new `clusterVariable` object.
    initialize = function(k = 3, max_iter = 100, auto_clean = TRUE) {
      if (!is.numeric(k) || k < 2) {
        stop("Error: k must be an integer >= 2")
      }
      self$k <- as.integer(k)
      private$max_iter <- max_iter
      private$auto_clean <- auto_clean
      self$cluster_result <- NULL
      self$data <- NULL
    },

    #======================== Fit ========================================
    #' @description
    #' Fit the K-means clustering model on the provided dataset.
    #' Data is passed here (not in constructor), following scikit-learn convention.
    #'
    #' @param X A numeric data frame or matrix containing variables to cluster.
    #' @param check_missing Logical. If TRUE, checks for missing values before fitting (default = TRUE).
    #'
    #' @return Self (invisible), to allow method chaining.

    fit = function(X, check_missing = TRUE) {
      # Input validation
      if (is.null(X)) stop("Error: X cannot be NULL")
      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("Error: X must be a data.frame or matrix")
      }

      # Convert to data frame if matrix
      if (is.matrix(X)) {
        X <- as.data.frame(X)
      }

      # Check for missing values
      if (check_missing && any(is.na(X))) {
        stop("Error: Missing values detected in X. Please handle them before fitting or set auto_clean=TRUE in constructor.")
      }

      # Store original data
      private$original_data <- X

      # Apply data cleaning if requested
      if (private$auto_clean) {
        X <- private$apply_cleaning(X)
      }

      # Check if data is numeric
      if (!all(sapply(X, is.numeric))) {
        stop("Error: All variables in X must be numeric for k-means clustering")
      }

      # Check if k is valid
      if (self$k > ncol(X)) {
        stop(paste("Error: k (", self$k, ") cannot be greater than number of variables (", ncol(X), ")", sep = ""))
      }

      # Store cleaned data
      self$data <- X

      # Perform k-means clustering
      set.seed(123)
      self$cluster_result <- mon_kmeans(self$data, k = self$k, max_iter = private$max_iter)

      # Compute silhouette for each variable
      private$compute_variable_silhouettes()

      cat("✓ Model fitted successfully with", self$k, "clusters on", ncol(X), "variables\n")

      invisible(self)
    },


    #============================End of fit ============================





    #======================== Start of Predict =====================================
    #' @description
    #' Predict cluster assignments for new variables based on existing cluster centers.
    #'
    #' @param X A data frame or matrix with the same number of observations as training data.
    #' @return A list containing cluster assignments and distances to centers.
    predict = function(X) {
      if (is.null(self$cluster_result)) {
        stop("Error: Model not fitted yet. Run fit() first.")
      }

      if (!is.data.frame(X) && !is.matrix(X)) {
        stop("Error: X must be a data.frame or matrix")
      }

      # Convert to data frame if needed
      if (is.matrix(X)) X <- as.data.frame(X)

      # Check number of observations
      if (nrow(X) != nrow(self$data)) {
        stop(paste("Error: X must have", nrow(self$data), "observations (same as training data)"))
      }

      # Transpose for variable clustering
      X_t <- t(X)
      centers <- self$cluster_result$centers

      # Compute distances to all cluster centers
      distances <- matrix(NA, nrow = nrow(centers), ncol = nrow(X_t))
      for (i in 1:nrow(centers)) {
        distances[i, ] <- rowSums((X_t - matrix(centers[i, ],
                                                nrow = nrow(X_t),
                                                ncol = ncol(X_t),
                                                byrow = TRUE))^2)
      }

      # Assign to nearest cluster
      cluster_assignments <- apply(distances, 2, which.min)
      min_distances <- apply(distances, 2, min)

      predictions_df <- data.frame(
        Variable = colnames(X),
        Cluster = cluster_assignments
      )


      return(predictions_df)

    },

    #======================== End of Predict =====================================

    #======================== Start of Summary =====================================

    #' @description
    #' Display a detailed summary of the clustering results.
    #'
    #' @return Prints comprehensive information about the clustering.
    summary = function() {
      if (is.null(self$cluster_result)) {
        stop("Error: Model not fitted yet. Run fit() first.")
      }

      cat("\n========================================\n")
      cat("  VARIABLE CLUSTERING SUMMARY\n")
      cat("========================================\n\n")

      cat("Method: K-means\n")
      cat("Number of clusters:", self$k, "\n")
      cat("Number of variables:", length(self$cluster_result$cluster), "\n")
      cat("Silhouette score:", round(private$overall_silhouette, 3), "\n\n")

      cat("Cluster distribution:\n")
      cat("--------------------\n")
      for (i in 1:self$k) {
        n_vars <- sum(self$cluster_result$cluster == i)
        pct <- round(100 * n_vars / length(self$cluster_result$cluster), 1)
        cat(sprintf("Cluster %d: %d variables (%.1f%%)\n", i, n_vars, pct))
      }

      cat("\nVariables by cluster:\n")
      cat("--------------------\n")
      for (i in 1:self$k) {
        vars_in_cluster <- names(self$data)[which(self$cluster_result$cluster == i)]
        cat("\nCluster", i, ":\n")
        if (length(vars_in_cluster) > 0) {
          cat("  ", paste(vars_in_cluster, collapse = ", "), "\n")

          # Show mean silhouette for this cluster
          sil_cluster <- private$variable_silhouettes[self$cluster_result$cluster == i]
          cat("  Mean silhouette:", round(mean(sil_cluster), 3), "\n")
        } else {
          cat("  (empty cluster)\n")
        }
      }

      cat("\n========================================\n\n")
    },

    #======================== End of Summary =====================================

    #======================== Start of Print ===============================

    #' @description
    #' Print a concise summary of the clusterVariable object.
    print = function() {
      cat("\n")
      cat("══════════════════════════════════════════\n")
      cat("   Clustering Results\n")
      cat("══════════════════════════════════════════\n\n")

      if (is.null(self$cluster_result)) {
        cat(" Status         : Not fitted\n")
        cat(" K (clusters)   : ", self$k, "\n", sep = "")
        cat(" Max iterations : ", private$max_iter, "\n", sep = "")
        cat(" Auto-clean     : ", ifelse(private$auto_clean, "✓ Enabled", "✗ Disabled"), "\n\n", sep = "")
        cat(" Use fit(X) to train the model\n")

      } else {
        cat(" Status         : ✓ Fitted\n")
        cat(" K (clusters)   : ", self$k, "\n", sep = "")
        cat(" Variables      : ", ncol(self$data), "\n", sep = "")
        cat(" Observations   : ", nrow(self$data), "\n", sep = "")

        # Silhouette with interpretation
        sil_score <- round(private$overall_silhouette, 3)
        sil_quality <- if (sil_score >= 0.7) {
          paste0(sil_score, " (Excellent)")
        } else if (sil_score >= 0.5) {
          paste0(sil_score, " (Good)")
        } else if (sil_score >= 0.25) {
          paste0(sil_score, " (Acceptable)")
        } else {
          paste0(sil_score, " (Poor)")
        }
        cat(" Silhouette     : ", sil_quality, "\n", sep = "")

        # Cluster distribution
        cat("\n Distribution:\n")
        cluster_counts <- table(self$cluster_result$cluster)
        for (i in 1:self$k) {
          count <- as.numeric(cluster_counts[i])
          pct <- round(100 * count / ncol(self$data), 1)
          bar_length <- round(pct / 5)  # Proportional bar
          bar <- paste(rep("█", bar_length), collapse = "")
          cat(sprintf("   Cluster %d: %2d vars (%5.1f%%) %s\n",
                      i, count, pct, bar))
        }


      }

      cat("\n══════════════════════════════════════════\n\n")
      invisible(self)
    } ,

    #======================== End of Print ===============================

    #======================== Visualization Methods ========================

    #' @description
    #' Visualize the clusters using PCA projection.
    #'
    #' @return A ggplot object showing clustered variables in 2D space.
    plot_clusters = function() {
      if (is.null(self$cluster_result)) {
        stop("Error: Model not fitted yet. Run fit() first.")
      }

      # Perform PCA on transposed data (variables as observations)
      data_t <- t(self$data)
      pca <- prcomp(data_t, scale. = TRUE)

      # Create dataframe for plotting
      plot_df <- data.frame(
        PC1 = pca$x[, 1],
        PC2 = pca$x[, 2],
        Cluster = factor(self$cluster_result$cluster),
        Variable = rownames(data_t),
        Silhouette = private$variable_silhouettes
      )

      # Create plot
      p <- ggplot(plot_df, aes(x = PC1, y = PC2, color = Cluster, size = Silhouette)) +
        geom_point(alpha = 0.7) +
        geom_text(aes(label = Variable), size = 3, vjust = -1, show.legend = FALSE) +
        scale_size_continuous(range = c(2, 6)) +
        labs(
          title = "Variable Cluster Visualization via PCA",
          subtitle = paste("K =", self$k, "| Overall Silhouette =", round(private$overall_silhouette, 3)),
          x = paste0("PC1 (", round(summary(pca)$importance[2, 1] * 100, 1), "%)"),
          y = paste0("PC2 (", round(summary(pca)$importance[2, 2] * 100, 1), "%)")
        ) +
        theme_minimal() +
        theme(
          plot.title = element_text(face = "bold", size = 14),
          legend.position = "right"
        )

      return(p)
    },
    #============================== Elbow method ==========================================
    #' @description
    #' Plot the elbow curve to determine optimal number of clusters.
    #'
    #' @param k_max Maximum number of clusters to test (default = 10).
    #' @return A ggplot object showing the elbow curve.
    #'
    plot_elbow = function(k_max = 10) {
      if (is.null(self$data)) {
        stop("Error: No data available. Run fit() first.")
      }

      n_max_possible <- nrow(t(self$data))
      if (k_max > n_max_possible) {
        warning(paste("k_max adjusted to", n_max_possible, "(maximum possible clusters)"))
        k_max <- n_max_possible
      }

      inertia <- numeric(k_max)
      for (k in 1:k_max) {
        set.seed(123)
        kmeans_result <- mon_kmeans(self$data, k = k)
        centers <- kmeans_result$centers
        clusters <- kmeans_result$cluster
        sum_sq <- 0
        data_t <- t(self$data)
        for (i in 1:k) {
          points_cluster <- data_t[clusters == i, , drop = FALSE]
          if (nrow(points_cluster) > 0) {
            sum_sq <- sum_sq + sum(rowSums((points_cluster -
                                              matrix(centers[i, ],
                                                     nrow = nrow(points_cluster),
                                                     ncol = ncol(points_cluster),
                                                     byrow = TRUE))^2))
          }
        }
        inertia[k] <- sum_sq
      }

      # Create plot
      plot_df <- data.frame(k = 1:k_max, inertia = inertia)
      p <- ggplot(plot_df, aes(x = k, y = inertia)) +
        geom_line(color = "steelblue", size = 1) +
        geom_point(color = "steelblue", size = 3) +
        labs(
          title = "Elbow Method to Choose Optimal Number of Clusters",
          x = "Number of clusters (k)",
          y = "Within-cluster inertia"
        ) +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 14)) +
        scale_x_continuous(breaks = 1:k_max)

      # Add vertical line and annotation only if k is in the displayed range
      if (!is.null(self$k) && self$k >= 1 && self$k <= k_max) {
        p <- p +
          geom_vline(xintercept = self$k, linetype = "dashed", color = "red", alpha = 0.7) +
          annotate("text", x = self$k, y = max(inertia) * 0.9,
                   label = paste("Selected k =", self$k), color = "red", hjust = -0.1)
      }

      return(p)
    } ,

    #============================= Heatmap =========================================

    #' @description
    #' Generate a correlation heatmap showing cluster structure.
    #'
    #' @return A pheatmap object.
    plot_heatmap = function() {
      if (is.null(self$cluster_result)) {
        stop("Error: Model not fitted yet. Run fit() first.")
      }

      cor_matrix <- cor(self$data)
      cluster_order <- order(self$cluster_result$cluster)
      cor_matrix_ordered <- cor_matrix[cluster_order, cluster_order]

      annotation_col <- data.frame(
        Cluster = factor(self$cluster_result$cluster[cluster_order])
      )
      rownames(annotation_col) <- colnames(cor_matrix_ordered)

      # Simplified version that always works in Shiny
      p <- pheatmap::pheatmap(
        cor_matrix_ordered,
        cluster_rows = FALSE,
        cluster_cols = FALSE,
        annotation_col = annotation_col,
        annotation_row = annotation_col,
        main = paste("Correlation Heatmap (k =", self$k, ")"),
        fontsize = 10,
        fontsize_row = 8,
        fontsize_col = 8,
        silent = TRUE  # DO NOT FORGET
      )

      # Explicit display
      grid::grid.newpage()
      grid::grid.draw(p$gtable)
    } ,


    #' @description
    #' Get silhouette score for each variable.
    #'
    #' @return A named vector of silhouette scores.
    get_variable_silhouettes = function() {
      if (is.null(private$variable_silhouettes)) {
        stop("Error: Model not fitted yet. Run fit() first.")
      }
      return(private$variable_silhouettes)
    },
    #======================== Quality Evaluation Methods ========================
    #' @description
    #' Get overall silhouette score.
    #'
    #' @return Numeric: average silhouette width.
    get_silhouette_score = function() {
      if (is.null(private$overall_silhouette)) {
        stop("Error: Model not fitted yet. Run fit() first.")
      }
      return(private$overall_silhouette)
    },



    #' @description
    #' Generate a comprehensive report of clustering quality.
    #'
    #' @return Prints detailed statistics about clustering quality.
    cluster_quality_report = function() {
      if (is.null(self$cluster_result)) {
        stop("Error: Model not fitted yet. Run fit() first.")
      }

      cat("\n========================================\n")
      cat("  VARIABLE CLUSTERING SUMMARY\n")
      cat("========================================\n\n")

      cat("Method: K-means\n")
      cat("Number of clusters:", self$k, "\n")
      cat("Number of variables:", length(self$cluster_result$cluster), "\n")

      # Global score interpretation
      cat("Silhouette score:", round(private$overall_silhouette, 3))
      if (private$overall_silhouette >= 0.7) {
        cat(" ✓✓✓ (Excellent structure)\n")
      } else if (private$overall_silhouette >= 0.5) {
        cat(" ✓✓  (Good structure)\n")
      } else if (private$overall_silhouette >= 0.25) {
        cat(" ✓   (Acceptable but weak structure)\n")
      } else {
        cat(" ✗   (Very weak structure)\n")
      }
      cat("\n")

      cat("Cluster distribution:\n")
      cat("--------------------\n")
      for (i in 1:self$k) {
        n_vars <- sum(self$cluster_result$cluster == i)
        pct <- round(100 * n_vars / length(self$cluster_result$cluster), 1)
        cat(sprintf("Cluster %d: %d variables (%.1f%%)\n", i, n_vars, pct))
      }

      cat("\nVariables by cluster:\n")
      cat("--------------------\n")

      for (i in 1:self$k) {
        vars_in_cluster <- names(self$data)[which(self$cluster_result$cluster == i)]
        sil_cluster <- private$variable_silhouettes[self$cluster_result$cluster == i]
        mean_sil <- mean(sil_cluster)

        cat("\nCluster", i, ":\n")

        if (length(vars_in_cluster) > 0) {
          # Display variables with their individual quality
          cat("  Variables: ")

          # Create annotated list
          annotated_vars <- character(length(vars_in_cluster))
          for (j in seq_along(vars_in_cluster)) {
            var_sil <- sil_cluster[j]
            # Add quality indicator
            if (var_sil >= 0.7) {
              annotated_vars[j] <- paste0(vars_in_cluster[j], " (✓✓✓ ", round(var_sil, 2), ")")
            } else if (var_sil >= 0.5) {
              annotated_vars[j] <- paste0(vars_in_cluster[j], " (✓✓ ", round(var_sil, 2), ")")
            } else if (var_sil >= 0.25) {
              annotated_vars[j] <- paste0(vars_in_cluster[j], " (✓ ", round(var_sil, 2), ")")
            } else if (var_sil >= 0) {
              annotated_vars[j] <- paste0(vars_in_cluster[j], " (~ ", round(var_sil, 2), ")")
            } else {
              annotated_vars[j] <- paste0(vars_in_cluster[j], " (✗ ", round(var_sil, 2), ")")
            }
          }

          cat(paste(annotated_vars, collapse = ", "), "\n")

          # Mean silhouette of cluster with interpretation
          cat("  Mean silhouette:", round(mean_sil, 3))
          if (mean_sil >= 0.5) {
            cat(" → Cohesive cluster ✓\n")
          } else if (mean_sil >= 0.25) {
            cat(" → Moderate cohesion\n")
          } else {
            cat(" → Weak cohesion ⚠\n")
          }

          # Detail of variable quality in this cluster
          n_excellent <- sum(sil_cluster >= 0.7)
          n_good <- sum(sil_cluster >= 0.5 & sil_cluster < 0.7)
          n_acceptable <- sum(sil_cluster >= 0.25 & sil_cluster < 0.5)
          n_poor <- sum(sil_cluster < 0.25)

          cat("  Quality: ")
          details <- c()
          if (n_excellent > 0) details <- c(details, paste(n_excellent, "excellent"))
          if (n_good > 0) details <- c(details, paste(n_good, "good"))
          if (n_acceptable > 0) details <- c(details, paste(n_acceptable, "acceptable"))
          if (n_poor > 0) details <- c(details, paste(n_poor, "poor ⚠"))

          cat(paste(details, collapse = ", "), "\n")

        } else {
          cat("  (empty cluster)\n")
        }
      }

      cat("\n========================================\n\n")

      cat("Legend of indicators:\n")
      cat("  ✓✓✓ = Excellent (≥ 0.7)\n")
      cat("  ✓✓  = Good (≥ 0.5)\n")
      cat("  ✓   = Acceptable (≥ 0.25)\n")
      cat("  ~   = Poor (< 0.25)\n")
      cat("  ✗   = Very poor (< 0)\n\n")


    }

  ),

  #======================== Private Methods ========================
  private = list(
    max_iter = NULL,
    auto_clean = NULL,
    original_data = NULL,
    variable_silhouettes = NULL,
    overall_silhouette = NULL,

    #' Apply data cleaning pipeline
    apply_cleaning = function(data) {
      data <- private$handle_missing_values(data)
      data <- private$handle_outliers(data)
      data <- private$normalize_data(data)
      return(data)
    },

    #' Handle missing values
    handle_missing_values = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]][is.na(data[[i]])] <- median(data[[i]], na.rm = TRUE)
        }
      }
      return(data)
    },

    #' Handle outliers using IQR method
    handle_outliers = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          Q1 <- quantile(data[[i]], 0.25, na.rm = TRUE)
          Q3 <- quantile(data[[i]], 0.75, na.rm = TRUE)
          iqr <- Q3 - Q1
          lower_bound <- Q1 - 1.5 * iqr
          upper_bound <- Q3 + 1.5 * iqr
          data[[i]][data[[i]] < lower_bound] <- lower_bound
          data[[i]][data[[i]] > upper_bound] <- upper_bound
        }
      }
      return(data)
    },

    #' Normalize data (standardization)
    normalize_data = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]] <- (data[[i]] - mean(data[[i]], na.rm = TRUE)) /
            sd(data[[i]], na.rm = TRUE)
        }
      }
      return(data)
    },

    #' Compute silhouette score for each variable
    compute_variable_silhouettes = function() {
      data_t <- t(self$data)
      dist_matrix <- dist(data_t)
      clusters <- self$cluster_result$cluster

      sil <- cluster::silhouette(clusters, dist_matrix)

      private$variable_silhouettes <- sil[, "sil_width"]  # Silhouette width for each variable
      names(private$variable_silhouettes) <- names(self$data)

      private$overall_silhouette <- mean(sil[, "sil_width"])  # Mean silhouette
    }
  )
)
