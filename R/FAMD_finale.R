# =============================================================================
# FAMD (Factor Analysis of Mixed Data) + VARIABLE CLUSTERING
# For MIXED data (quantitative + qualitative)
# =============================================================================

library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)
library(dplyr)
library(R6)
library(ggrepel)



#' CAH_mixtes: Hierarchical Clustering of Variables on Mixed Data
#'
#' @description
#' R6 class to perform factor analysis of mixed data (FAMD)
#' followed by hierarchical clustering of variables. This class allows
#' grouping quantitative and qualitative variables into homogeneous clusters
#' based on their coordinates in the factorial space.
#'
#' @details
#' The CAH_mixtes class combines two main analyses:
#' \itemize{
#'   \item \strong{FAMD (Factor Analysis of Mixed Data)}: Factor analysis
#'         adapted to data containing both quantitative and qualitative variables
#'   \item \strong{Hierarchical clustering}: Grouping of variables into
#'         clusters based on their factorial coordinates
#' }
#'
#' The complete process follows these steps:
#' \enumerate{
#'   \item Automatic detection of variable types (quantitative/qualitative)
#'   \item FAMD calculation to obtain factorial coordinates of variables
#'   \item Hierarchical clustering of variables in the factorial space
#'   \item Visualization and evaluation of clustering quality
#' }
#'
#' @field n_components Number of principal components to retain (integer)
#' @field famd_result Complete FAMD analysis result (FactoMineR object)
#' @field data Training data (data.frame)
#' @field data_type Detected data type: "mixte", "quali" or "quanti"
#' @field quanti_vars Vector of quantitative variable names
#' @field quali_vars Vector of qualitative variable names
#' @field coord_var Matrix of variable coordinates in factorial space
#' @field eigenvalues FAMD eigenvalues
#' @field inertia_explained Percentage of inertia explained by component
#' @field labels_var Vector of cluster labels for each variable
#' @field hclust_result Hierarchical clustering result (hclust object)
#' @field cluster_centers Matrix of cluster centers
#' @field clustering_method Clustering method used
#' @field n_clusters Number of clusters created
#'
#' @examples
#' \dontrun{
#' # Create a CAH_mixtes object with 5 components
#' model <- CAH_mixtes$new(n_components = 5)
#'
#' # Load mixed data
#' data <- data.frame(
#'   age = c(25, 30, 35, 40, 45),
#'   income = c(30000, 45000, 60000, 75000, 90000),
#'   category = factor(c("A", "B", "A", "C", "B")),
#'   level = factor(c("low", "medium", "high", "high", "medium"))
#' )
#'
#' # Perform FAMD analysis
#' model$fit(data)
#'
#' # Perform hierarchical clustering
#' model$clustering_hierarchical(n_clusters = 2, method = "ward")
#'
#' # Visualize results
#' model$plot_variables(axes = c(1, 2))
#' model$dendo()
#' model$qualite_clustering()
#'
#' # Display summary
#' model$summary()
#' }
#'
#' @export
CAH_mixtes <- R6Class("CAH_mixtes",
                      public = list(
                        # Attributes
                        n_components = NULL,
                        famd_result = NULL,
                        data = NULL,
                        data_type = NULL,
                        quanti_vars = NULL,
                        quali_vars = NULL,
                        coord_var = NULL,
                        eigenvalues = NULL,
                        inertia_explained = NULL,
                        labels_var = NULL,
                        hclust_result = NULL,
                        cluster_centers = NULL,
                        clustering_method = NULL,
                        n_clusters = NULL,

                        #' @description
                        #' Initialize a new CAH_mixtes object
                        #'
                        #' @param n_components Number of principal components to retain for FAMD.
                        #'   Default: 5. This parameter determines the dimensionality of the
                        #'   factorial space in which clustering will be performed.
                        #'
                        #' @return A new initialized CAH_mixtes object
                        #'
                        #' @examples
                        #' \dontrun{
                        #' # Create a model with 5 components (default)
                        #' model <- CAH_mixtes$new()
                        #'
                        #' # Create a model with 10 components
                        #' model <- CAH_mixtes$new(n_components = 10)
                        #' }
                        initialize = function(n_components = 5) {
                          self$n_components <- n_components
                          cat("═══════════════════════════════════════════════════\n")
                          cat("  FAMD VARIABLE CLUSTERING\n")
                          cat(" (Quantitative + Qualitative)\n")
                          cat("═══════════════════════════════════════════════════\n\n")
                          cat(sprintf("Number of components: %d\n", n_components))
                        },

                        #' @description
                        #' Perform FAMD analysis on provided data
                        #'
                        #' @param df Data.frame containing data to analyze. Can contain
                        #'   quantitative variables (numeric) and/or qualitative variables (factors
                        #'   or character strings). Variable type detection is automatic.
                        #'
                        #' @details
                        #' This method:
                        #' \itemize{
                        #'   \item Automatically detects variable types
                        #'   \item Converts qualitative variables to factors
                        #'   \item Calculates FAMD with specified number of components
                        #'   \item Extracts variable coordinates in factorial space
                        #'   \item For qualitative variables, calculates the barycenter of modalities
                        #' }
                        #'
                        #' @return Updated CAH_mixtes object (invisible), allowing chaining
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new(n_components = 5)
                        #' model$fit(my_data)
                        #' }
                        fit = function(df) {
                          cat("\n========================================\n")
                          cat("STEP 1: DATA ANALYSIS\n")
                          cat("========================================\n\n")

                          self$data <- as.data.frame(df)

                          # Detect variable types
                          self$quanti_vars <- names(self$data)[sapply(self$data, is.numeric)]
                          self$quali_vars <- names(self$data)[sapply(self$data, function(x) is.factor(x) || is.character(x))]

                          # Determine data type
                          has_quanti <- length(self$quanti_vars) > 0
                          has_quali <- length(self$quali_vars) > 0

                          if (has_quanti && has_quali) {
                            self$data_type <- "mixte"
                            cat("Detected data type: MIXED\n\n")
                          } else if (has_quali && !has_quanti) {
                            self$data_type <- "quali"
                            cat("Detected data type: QUALITATIVE only\n")
                            cat("  (FAMD will behave like MCA)\n\n")
                          } else {
                            stop("No variables detected!")
                          }

                          # Convert qualitative variables to factors
                          for (var in self$quali_vars) {
                            self$data[[var]] <- as.factor(self$data[[var]])
                          }

                          cat("========================================\n")
                          cat("STEP 2: FAMD (Factor Analysis of Mixed Data)\n")
                          cat("========================================\n\n")

                          # Perform FAMD
                          self$famd_result <- FAMD(
                            self$data,
                            ncp = self$n_components,
                            graph = FALSE
                          )

                          # Extract VARIABLE coordinates
                          if (self$data_type %in% c("mixte", "quali")) {
                            # Get coordinates of variables and modalities
                            coord_quali_raw <- self$famd_result$var$coord
                            coord_quali_list <- list()

                            for (var in self$quali_vars) {
                              # Identify rows corresponding to variable modalities
                              modalities <- rownames(coord_quali_raw)[startsWith(rownames(coord_quali_raw), paste0(var, "="))]

                              # Fallback if no result, search for "_" or "."
                              if (length(modalities) == 0) {
                                modalities <- rownames(coord_quali_raw)[grepl(var, rownames(coord_quali_raw))]
                              }

                              if (length(modalities) > 0) {
                                # Take barycenter of modalities to have 1 row per variable
                                coord_quali_list[[var]] <- colMeans(coord_quali_raw[modalities, , drop = FALSE])
                              } else {
                                cat(sprintf("⚠️ No modalities found for %s\n", var))
                              }
                            }

                            # Combine all coordinates
                            coord_quali <- do.call(rbind, coord_quali_list)

                            if (self$data_type == "mixte") {
                              # For quantitative variables
                              coord_quanti <- self$famd_result$quanti.var$coord
                              # Combine quanti + quali
                              self$coord_var <- rbind(coord_quanti, coord_quali)
                            } else {
                              # For quali only
                              self$coord_var <- coord_quali
                            }

                            # Display for verification
                            cat("✅ Variable coordinates prepared for clustering:\n")
                            print(rownames(self$coord_var))
                          } else if (self$data_type == "quanti") {
                            cat("We are only processing quantitative variables\n")
                          }

                          # Extract eigenvalues
                          self$eigenvalues <- self$famd_result$eig[, 1]
                          self$inertia_explained <- self$famd_result$eig[, 2]

                          cat(sprintf("✓ FAMD performed on %d individuals and %d variables\n",
                                      nrow(self$data), nrow(self$coord_var)))
                          cat(sprintf("✓ Inertia explained (cumulative) by %d components: %.2f%%\n\n",
                                      self$n_components,
                                      sum(self$inertia_explained[1:self$n_components])))

                          invisible(self)
                        },

                        #' @description
                        #' Perform hierarchical clustering of variables
                        #'
                        #' @param n_clusters Number of clusters to create (positive integer)
                        #' @param method Linkage method for HAC. Available options:
                        #'   \itemize{
                        #'     \item \code{"ward"} (default): Ward's method (ward.D2)
                        #'     \item \code{"complete"}: Complete linkage
                        #'     \item \code{"single"}: Single linkage
                        #'     \item \code{"average"}: Average linkage
                        #'   }
                        #'
                        #' @details
                        #' This method:
                        #' \itemize{
                        #'   \item Calculates Euclidean distance matrix between variables
                        #'     in factorial space
                        #'   \item Performs hierarchical clustering with specified method
                        #'   \item Cuts hierarchical tree to obtain desired number of clusters
                        #'   \item Calculates centers of each cluster
                        #'   \item Displays distribution of variables by cluster
                        #' }
                        #'
                        #' @return Named vector of cluster labels for each variable
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #'
                        #' # Clustering with Ward's method (default)
                        #' labels <- model$clustering_hierarchical(n_clusters = 3)
                        #'
                        #' # Clustering with complete linkage
                        #' labels <- model$clustering_hierarchical(n_clusters = 3, method = "complete")
                        #' }
                        clustering_hierarchical = function(n_clusters, method = "ward") {
                          if (is.null(self$coord_var)) {
                            stop("Error: You must first run fit()!")
                          }

                          cat("\n========================================\n")
                          cat("STEP 3: HIERARCHICAL CLUSTERING OF VARIABLES\n")
                          cat("========================================\n\n")

                          cat("Variables used for clustering:\n")
                          print(rownames(self$coord_var))

                          self$n_clusters <- n_clusters

                          # Calculate distance matrix between VARIABLES
                          dist_matrix <- dist(self$coord_var)

                          # HAC
                          if (method == "ward") {
                            self$hclust_result <- hclust(dist_matrix, method = "ward.D2")
                          } else {
                            self$hclust_result <- hclust(dist_matrix, method = method)
                          }

                          # Cut tree
                          self$labels_var <- cutree(self$hclust_result, k = n_clusters)
                          names(self$labels_var) <- rownames(self$coord_var)
                          self$clustering_method <- "hierarchical"

                          # Calculate cluster centers
                          self$cluster_centers <- matrix(0, nrow = n_clusters, ncol = ncol(self$coord_var))
                          for (i in 1:n_clusters) {
                            cluster_points <- self$coord_var[self$labels_var == i, , drop = FALSE]
                            self$cluster_centers[i, ] <- colMeans(cluster_points)
                          }
                          colnames(self$cluster_centers) <- colnames(self$coord_var)
                          rownames(self$cluster_centers) <- paste0("Cluster_", 1:n_clusters)

                          cat(sprintf("✓ HAC with %d clusters (method: %s)\n\n", n_clusters, method))
                          cat("Distribution of variables by cluster:\n")
                          print(table(self$labels_var))

                          cat("\nVARIABLES BY CLUSTER:\n")
                          cat("═══════════════════════════════════════\n")
                          for (i in 1:n_clusters) {
                            vars_in_cluster <- names(self$labels_var[self$labels_var == i])
                            cat(sprintf("\n📊 Cluster %d (%d variables):\n", i, length(vars_in_cluster)))
                            for (var in vars_in_cluster) {
                              # Indicate variable type
                              if (var %in% self$quanti_vars) {
                                cat(sprintf("   • %s (quantitative)\n", var))
                              } else {
                                cat(sprintf("   • %s (qualitative)\n", var))
                              }
                            }
                          }
                          cat("\n")

                          return(self$labels_var)
                        },

                        #' @description
                        #' Predict cluster membership for new variables
                        #'
                        #' @param new_vars Data.frame containing new variables to classify.
                        #'   Must have the same number of rows (individuals) as training data.
                        #'
                        #' @details
                        #' This method:
                        #' \itemize{
                        #'   \item Combines training data with new variables
                        #'   \item Performs new FAMD on combined set
                        #'   \item Extracts coordinates of new variables
                        #'   \item Calculates distance to existing cluster centers
                        #'   \item Assigns each new variable to nearest cluster
                        #' }
                        #'
                        #' @return Data.frame with three columns:
                        #'   \itemize{
                        #'     \item \code{Variable}: Variable name
                        #'     \item \code{Type}: Variable type ("Quantitative" or "Qualitative")
                        #'     \item \code{Cluster_Predit}: Predicted cluster number
                        #'   }
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(train_data)
                        #' model$clustering_hierarchical(n_clusters = 3)
                        #'
                        #' # Predict for new variables
                        #' new_vars <- data.frame(
                        #'   new_var1 = c(1, 2, 3, 4, 5),
                        #'   new_var2 = factor(c("A", "B", "A", "C", "B"))
                        #' )
                        #' predictions <- model$predict(new_vars)
                        #' print(predictions)
                        #' }
                        predict = function(new_vars) {
                          if (is.null(self$cluster_centers)) {
                            stop("Error: You must first run clustering_hierarchical()!")
                          }

                          cat("\n========================================\n")
                          cat("CLUSTER PREDICTION FOR NEW VARIABLES\n")
                          cat("========================================\n\n")

                          # new_vars must be a data.frame with NEW VARIABLES as columns
                          # and same individuals as self$data in rows
                          new_vars <- as.data.frame(new_vars)

                          if (nrow(new_vars) != nrow(self$data)) {
                            stop(sprintf("❌ Error: new_vars must have %d rows (same number of individuals)",
                                         nrow(self$data)))
                          }

                          n_new_vars <- ncol(new_vars)
                          cat(sprintf("📊 Number of new variables to predict: %d\n", n_new_vars))
                          cat(sprintf("   Variables: %s\n\n", paste(names(new_vars), collapse = ", ")))

                          # --- 1. Combine old + new variables ---
                          data_combined <- cbind(self$data, new_vars)

                          # --- 2. Detect type of new variables ---
                          new_quanti <- names(new_vars)[sapply(new_vars, is.numeric)]
                          new_quali <- names(new_vars)[sapply(new_vars, function(x) is.factor(x) || is.character(x))]

                          cat("Type of new variables:\n")
                          if (length(new_quanti) > 0) {
                            cat(sprintf("  • Quantitative: %s\n", paste(new_quanti, collapse = ", ")))
                          }
                          if (length(new_quali) > 0) {
                            cat(sprintf("  • Qualitative: %s\n", paste(new_quali, collapse = ", ")))
                          }
                          cat("\n")

                          # Convert qualitative to factors
                          for (var in new_quali) {
                            data_combined[[var]] <- as.factor(data_combined[[var]])
                          }

                          # --- 3. FAMD on combined set (old + new variables) ---
                          cat("⏳ Calculating FAMD including new variables...\n")
                          famd_new <- FAMD(data_combined, ncp = self$n_components, graph = FALSE)

                          # --- 4. Extract coordinates of NEW variables only ---
                          coord_new_vars <- matrix(0, nrow = n_new_vars, ncol = self$n_components)
                          rownames(coord_new_vars) <- names(new_vars)
                          colnames(coord_new_vars) <- colnames(self$coord_var)

                          for (i in 1:n_new_vars) {
                            var_name <- names(new_vars)[i]

                            if (var_name %in% new_quanti) {
                              # Quantitative variable
                              coord_new_vars[i, ] <- famd_new$quanti.var$coord[var_name, ]

                            } else if (var_name %in% new_quali) {
                              # Qualitative variable: barycenter of modalities
                              coord_quali_raw <- famd_new$var$coord
                              modalities <- rownames(coord_quali_raw)[startsWith(rownames(coord_quali_raw),
                                                                                 paste0(var_name, "="))]

                              # Fallback
                              if (length(modalities) == 0) {
                                modalities <- rownames(coord_quali_raw)[grepl(var_name, rownames(coord_quali_raw))]
                              }

                              if (length(modalities) > 0) {
                                coord_new_vars[i, ] <- colMeans(coord_quali_raw[modalities, , drop = FALSE])
                              } else {
                                warning(sprintf("⚠️ Unable to find modalities for %s", var_name))
                              }
                            }
                          }

                          cat("✓ Coordinates extracted for new variables\n\n")

                          # --- 5. Calculate distance to original cluster centers ---
                          predicted_clusters <- apply(coord_new_vars, 1, function(var_coord) {
                            distances <- apply(self$cluster_centers, 1, function(center) {
                              sum((var_coord - center)^2)
                            })
                            which.min(distances)
                          })

                          # --- 6. Return results ---
                          results <- data.frame(
                            Variable = names(new_vars),
                            Type = ifelse(names(new_vars) %in% new_quanti, "Quantitative", "Qualitative"),
                            Cluster_Predit = predicted_clusters,
                            stringsAsFactors = FALSE
                          )

                          cat("═══════════════════════════════════════\n")
                          cat("PREDICTION RESULTS\n")
                          cat("═══════════════════════════════════════\n\n")

                          return(results)
                        },

                        #' @description
                        #' Display dendrogram of hierarchical clustering
                        #'
                        #' @details
                        #' Generates a dendrogram plot (hierarchical tree) showing
                        #' the clustering structure of variables. The y-axis represents
                        #' the distance at which clusters are merged.
                        #'
                        #' @return CAH_mixtes object (invisible), allowing chaining
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #' model$clustering_hierarchical(n_clusters = 3)
                        #' model$dendo()
                        #' }
                        dendo = function() {
                          if (is.null(self$hclust_result)) {
                            stop("Error: You must first run clustering_hierarchical()!")
                          }

                          cat("\n========================================\n")
                          cat("HIERARCHICAL CLUSTERING DENDROGRAM\n")
                          cat("========================================\n\n")

                          plot(self$hclust_result,
                               main = "Dendrogram of hierarchical clustering of variables",
                               xlab = "Variables",
                               ylab = "Distance",
                               sub = "",
                               cex = 0.8)

                          invisible(self)
                        },

                        #' @description
                        #' Visualize variables in factorial plane
                        #'
                        #' @param axes Vector of two integers specifying axes to display.
                        #'   Default: c(1, 2) for first two dimensions.
                        #'
                        #' @details
                        #' Generates a ggplot2 graph showing:
                        #' \itemize{
                        #'   \item Position of variables in factorial plane
                        #'   \item Type of each variable (point shape)
                        #'   \item Cluster membership (point color) if clustering
                        #'         has been performed
                        #'   \item Name of each variable (with overlap avoidance)
                        #' }
                        #'
                        #' @return CAH_mixtes object (invisible), allowing chaining
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #' model$clustering_hierarchical(n_clusters = 3)
                        #'
                        #' # Visualize in plane (Dim1, Dim2)
                        #' model$plot_variables(axes = c(1, 2))
                        #'
                        #' # Visualize in plane (Dim2, Dim3)
                        #' model$plot_variables(axes = c(2, 3))
                        #' }
                        plot_variables = function(axes = c(1, 2)) {
                          if (is.null(self$coord_var)) {
                            stop("Error: You must first run fit()!")
                          }

                          cat("\n========================================\n")
                          cat("VARIABLE PROJECTION\n")
                          cat("========================================\n\n")

                          plot_df <- data.frame(
                            Dim1 = self$coord_var[, axes[1]],
                            Dim2 = self$coord_var[, axes[2]],
                            Variable = rownames(self$coord_var)
                          )

                          # Add variable type
                          plot_df$Type <- ifelse(plot_df$Variable %in% self$quanti_vars,
                                                 "Quantitative", "Qualitative")

                          # Add cluster if available
                          if (!is.null(self$labels_var)) {
                            plot_df$Cluster <- as.factor(self$labels_var[plot_df$Variable])
                          }

                          p <- ggplot(plot_df, aes(x = Dim1, y = Dim2))

                          if (!is.null(self$labels_var)) {
                            p <- p +
                              geom_point(aes(color = Cluster, shape = Type), alpha = 0.7, size = 4) +
                              scale_color_brewer(palette = "Set1") +
                              labs(color = "Cluster", shape = "Type")
                          } else {
                            p <- p +
                              geom_point(aes(color = Type), alpha = 0.7, size = 4) +
                              scale_color_manual(values = c("Quantitative" = "steelblue",
                                                            "Qualitative" = "coral")) +
                              labs(color = "Variable type")
                          }

                          p <- p +
                            ggrepel::geom_text_repel(aes(label = Variable), size = 3.5,
                                                     max.overlaps = 20, fontface = "bold") +
                            geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
                            geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
                            labs(
                              title = "Projection of variables in factorial plane (FAMD)",
                              x = sprintf("Dim %d (%.2f%%)", axes[1], self$inertia_explained[axes[1]]),
                              y = sprintf("Dim %d (%.2f%%)", axes[2], self$inertia_explained[axes[2]])
                            ) +
                            theme_minimal() +
                            theme(panel.grid.minor = element_blank(),
                                  legend.position = "right")

                          print(p)
                          invisible(self)
                        },

                        #' @description
                        #' Apply elbow method to determine optimal number of clusters
                        #'
                        #' @param max_clusters Maximum number of clusters to test. Default: 10
                        #' @param method Linkage method for HAC. Default: "ward"
                        #'
                        #' @details
                        #' This method:
                        #' \itemize{
                        #'   \item Calculates within-cluster inertia for each number of clusters
                        #'     from 2 to max_clusters
                        #'   \item Generates a plot of inertia evolution
                        #'   \item Allows visual identification of optimal "elbow"
                        #' }
                        #'
                        #' The optimal number of clusters generally corresponds to the point where
                        #' inertia decreases less rapidly (the "elbow" of the curve).
                        #'
                        #' @return CAH_mixtes object (invisible), allowing chaining
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #'
                        #' # Test up to 10 clusters
                        #' model$elbow_method(max_clusters = 10)
                        #'
                        #' # Test up to 15 clusters with complete linkage
                        #' model$elbow_method(max_clusters = 15, method = "complete")
                        #' }
                        elbow_method = function(max_clusters = 10, method = "ward") {
                          if (is.null(self$coord_var)) {
                            stop("Error: You must first run fit()!")
                          }

                          cat("\n========================================\n")
                          cat("ELBOW METHOD\n")
                          cat("========================================\n\n")

                          n_vars <- nrow(self$coord_var)
                          max_clusters <- min(max_clusters, n_vars - 1)

                          dist_matrix <- dist(self$coord_var)

                          if (method == "ward") {
                            hc <- hclust(dist_matrix, method = "ward.D2")
                          } else {
                            hc <- hclust(dist_matrix, method = method)
                          }

                          inertias <- numeric(max_clusters - 1)

                          for (k in 2:max_clusters) {
                            clusters <- cutree(hc, k = k)
                            total_inertia <- 0
                            for (i in 1:k) {
                              cluster_points <- self$coord_var[clusters == i, , drop = FALSE]
                              if (nrow(cluster_points) > 0) {
                                center <- colMeans(cluster_points)
                                total_inertia <- total_inertia +
                                  sum(apply(cluster_points, 1, function(x) sum((x - center)^2)))
                              }
                            }
                            inertias[k - 1] <- total_inertia
                          }

                          elbow_df <- data.frame(
                            K = 2:max_clusters,
                            Inertia = inertias
                          )

                          p <- ggplot(elbow_df, aes(x = K, y = Inertia)) +
                            geom_line(color = "steelblue", linewidth = 1.2) +
                            geom_point(color = "steelblue", size = 3.5) +
                            geom_text(aes(label = sprintf("%.1f", Inertia)),
                                      vjust = -1, size = 3) +
                            labs(
                              title = "Elbow method - Variable clustering (FAMD)",
                              x = "Number of clusters",
                              y = "Within-cluster inertia"
                            ) +
                            scale_x_continuous(breaks = 2:max_clusters) +
                            theme_minimal() +
                            theme(panel.grid.minor = element_blank())

                          print(p)
                          cat("💡 Suggestion: Look for the 'elbow' in the curve\n")
                          cat("   where inertia decreases less rapidly.\n\n")

                          invisible(self)
                        },

                        #' @description
                        #' Evaluate clustering quality using silhouette
                        #'
                        #' @details
                        #' This method:
                        #' \itemize{
                        #'   \item Calculates silhouette coefficient for each variable
                        #'   \item Calculates average silhouette width (overall quality)
                        #'   \item Generates silhouette plot colored by cluster
                        #' }
                        #'
                        #' Silhouette interpretation:
                        #' \itemize{
                        #'   \item 0.71 - 1.0: Strong structure
                        #'   \item 0.51 - 0.70: Reasonable structure
                        #'   \item 0.26 - 0.50: Weak structure
                        #'   \item < 0.25: No substantial structure
                        #' }
                        #'
                        #' @return CAH_mixtes object (invisible), allowing chaining
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #' model$clustering_hierarchical(n_clusters = 3)
                        #' model$qualite_clustering()
                        #' }
                        qualite_clustering = function() {
                          if (is.null(self$labels_var)) {
                            stop("Error: You must first run clustering_hierarchical()!")
                          }

                          cat("\n========================================\n")
                          cat("VARIABLE CLUSTERING QUALITY\n")
                          cat("========================================\n\n")

                          dist_matrix <- dist(self$coord_var)
                          sil <- silhouette(self$labels_var, dist_matrix)
                          avg_sil_width <- mean(sil[, 3])

                          cat(sprintf("✓ Average silhouette width: %.3f\n\n", avg_sil_width))

                          fviz_silhouette(sil) +
                            ggtitle("Silhouette of variable clustering (FAMD)") +
                            theme_minimal()

                          invisible(self)
                        },

                        #' @description
                        #' Display summary of analysis results
                        #'
                        #' @details
                        #' Displays information about:
                        #' \itemize{
                        #'   \item Number of individuals and variables
                        #'   \item Type of data (mixed, qualitative, quantitative)
                        #'   \item Number of components and explained inertia
                        #'   \item Number of clusters created
                        #'   \item Distribution of variables by cluster
                        #'   \item Accessible R objects
                        #' }
                        #'
                        #' @return CAH_mixtes object (invisible), allowing chaining
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #' model$clustering_hierarchical(n_clusters = 3)
                        #' model$print()
                        #' }
                        print = function() {
                          cat("\n═══════════════════════════════════════════════════\n")
                          cat("  FAMD + VARIABLE CLUSTERING RESULTS\n")
                          cat("═══════════════════════════════════════════════════\n\n")

                          if (!is.null(self$data)) {
                            cat(sprintf("📊 Analysis performed on %d individuals and %d variables\n\n",
                                        nrow(self$data), ncol(self$data)))

                            cat(sprintf("Data type: %s\n", toupper(self$data_type)))
                            cat(sprintf("  • Quantitative variables: %d\n", length(self$quanti_vars)))
                            cat(sprintf("  • Qualitative variables: %d\n\n", length(self$quali_vars)))
                          }

                          if (!is.null(self$famd_result)) {
                            cat(sprintf("📈 Principal components: %d\n", self$n_components))
                            cat(sprintf("📈 Total explained inertia: %.2f%%\n\n",
                                        sum(self$inertia_explained[1:min(self$n_components,
                                                                         length(self$inertia_explained))])))
                          }

                          if (!is.null(self$labels_var)) {
                            cat(sprintf("🎯 Clustering performed: %d clusters (method: %s)\n",
                                        self$n_clusters, self$clustering_method))
                            cat(sprintf("🎯 Total number of variables: %d\n\n", length(self$labels_var)))

                            cat("📋 Variable distribution:\n")
                            for (i in 1:self$n_clusters) {
                              vars <- names(self$labels_var[self$labels_var == i])
                              cat(sprintf("   Cluster %d: %d variables\n", i, length(vars)))
                            }
                          }

                          cat("\n🔍 Available objects:\n")
                          cat("   $famd_result        : Complete FAMD result\n")
                          cat("   $coord_var          : Variable coordinates\n")
                          cat("   $labels_var         : Cluster labels for variables\n")
                          cat("   $eigenvalues        : Eigenvalues\n")
                          cat("   $inertia_explained  : % explained inertia\n")
                          cat("   $data               : Training data\n")
                          cat("\n")

                          invisible(self)
                        },

                        #' @description
                        #' Display detailed summary of variable clusters
                        #'
                        #' @details
                        #' For each cluster, displays:
                        #' \itemize{
                        #'   \item Cluster number and number of variables
                        #'   \item List of quantitative variables
                        #'   \item List of qualitative variables
                        #' }
                        #'
                        #' This method helps interpret the meaning of each cluster by
                        #' identifying variables grouped together.
                        #'
                        #' @return CAH_mixtes object (invisible), allowing chaining
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #' model$clustering_hierarchical(n_clusters = 3)
                        #' model$summary()
                        #' }
                        summary = function() {
                          if (is.null(self$labels_var)) {
                            cat("❌ No clustering has been performed.\n")
                            cat("   Use clustering_hierarchical() first.\n")
                            return(invisible(self))
                          }

                          cat("\n═══════════════════════════════════════════════════\n")
                          cat("  INTERPRETATION OF VARIABLE CLUSTERS\n")
                          cat("═══════════════════════════════════════════════════\n\n")

                          n_clusters <- max(self$labels_var)

                          for (i in 1:n_clusters) {
                            vars_in_cluster <- names(self$labels_var[self$labels_var == i])

                            cat(sprintf("━━━ CLUSTER %d (%d variables) ━━━\n", i, length(vars_in_cluster)))

                            # Separate quanti and quali
                            vars_quanti <- vars_in_cluster[vars_in_cluster %in% self$quanti_vars]
                            vars_quali <- vars_in_cluster[vars_in_cluster %in% self$quali_vars]

                            if (length(vars_quanti) > 0) {
                              cat("\n  📊 Quantitative variables:\n")
                              for (var in vars_quanti) {
                                cat(sprintf("     • %s\n", var))
                              }
                            }

                            if (length(vars_quali) > 0) {
                              cat("\n  📝 Qualitative variables:\n")
                              for (var in vars_quali) {
                                cat(sprintf("     • %s\n", var))
                              }
                            }
                          }

                          cat("═══════════════════════════════════════════════════\n\n")

                          invisible(self)
                        }
                      )
)
