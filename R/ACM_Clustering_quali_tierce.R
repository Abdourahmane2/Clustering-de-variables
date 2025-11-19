# =============================================================================
# MULTIPLE CORRESPONDENCE ANALYSIS (MCA) AND HIERARCHICAL CLUSTERING
# =============================================================================

library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)
library(dplyr)
library(R6)

ACMClustering <- R6Class("ACMClustering",
                         public = list(
                           # Attributes
                           n_components = NULL,
                           mca_result = NULL,
                           coord_ind = NULL,
                           coord_mod = NULL,
                           eigenvalues = NULL,
                           inertia_explained = NULL,
                           labels_mod = NULL,
                           labels_var = NULL,
                           data = NULL,
                           modality_names = NULL,
                           variable_names = NULL,
                           cluster_centers = NULL,
                           clustering_method = NULL,
                           
                           # Constructor
                           initialize = function(n_components = 5) {
                             self$n_components <- n_components
                             cat("ACMClustering initialized with", n_components, "components\n")
                             cat("MODE: HIERARCHICAL clustering on MODALITIES (variables)\n")
                           },
                           
                           # Method to perform MCA
                           fit = function(df) {
                             cat("\n========================================\n")
                             cat("EXECUTING MCA\n")
                             cat("========================================\n")
                             
                             df <- as.data.frame(lapply(df, as.factor))
                             self$data <- df
                             
                             self$mca_result <- MCA(df, 
                                                    ncp = self$n_components, 
                                                    graph = FALSE)
                             
                             self$coord_ind <- self$mca_result$ind$coord
                             self$coord_mod <- self$mca_result$var$coord
                             
                             self$modality_names <- rownames(self$coord_mod)
                             self$variable_names <- names(df)
                             
                             self$eigenvalues <- self$mca_result$eig[, 1]
                             self$inertia_explained <- self$mca_result$eig[, 2]
                             
                             n_vars <- ncol(df)
                             n_modalities <- nrow(self$coord_mod)
                             cat(sprintf("\nMCA performed on %d individuals and %d variables\n", 
                                         nrow(df), n_vars))
                             cat(sprintf("Total number of modalities: %d\n", n_modalities))
                             cat(sprintf("\nInertia explained (cumulative) by %d components: %.2f%%\n",
                                         self$n_components,
                                         sum(self$inertia_explained[1:self$n_components])))
                             
                             cat("\nModalities by variable:\n")
                             for (var in names(df)) {
                               cat(sprintf("  - %s: %s\n", var, paste(levels(df[[var]]), collapse = ", ")))
                             }
                             
                             invisible(self)
                           },
                           
                           # Hierarchical Agglomerative Clustering (HAC) on MODALITIES
                           clustering_hierarchical = function(n_clusters, method = "ward") {
                             cat("\n========================================\n")
                             cat("HIERARCHICAL CLUSTERING ON MODALITIES\n")
                             cat("========================================\n")
                             
                             dist_matrix <- dist(self$coord_mod)
                             
                             if (method == "ward") {
                               hc <- hclust(dist_matrix, method = "ward.D2")
                             } else {
                               hc <- hclust(dist_matrix, method = method)
                             }
                             
                             self$labels_mod <- cutree(hc, k = n_clusters)
                             names(self$labels_mod) <- self$modality_names
                             self$clustering_method <- "hierarchical"
                             
                             # Calculate cluster centers
                             self$cluster_centers <- matrix(0, nrow = n_clusters, ncol = ncol(self$coord_mod))
                             for (i in 1:n_clusters) {
                               cluster_points <- self$coord_mod[self$labels_mod == i, , drop = FALSE]
                               self$cluster_centers[i, ] <- colMeans(cluster_points)
                             }
                             colnames(self$cluster_centers) <- colnames(self$coord_mod)
                             
                             #sil <- silhouette(self$labels_mod, dist_matrix)
                             #sil_score <- mean(sil[, 3])
                             
                             cat(sprintf("\nHAC with %d clusters (method: %s)\n", n_clusters, method))
                             #cat(sprintf("Silhouette score: %.3f\n", sil_score))
                             cat("\nDistribution of modalities by cluster:\n")
                             print(table(self$labels_mod))
                             
                             cat("\nModalities by cluster:\n")
                             for (i in 1:n_clusters) {
                               cat(sprintf("\nCluster %d:\n", i))
                               modalities_in_cluster <- names(self$labels_mod[self$labels_mod == i])
                               for (mod in modalities_in_cluster) {
                                 cat(sprintf("  - %s\n", mod))
                               }
                             }
                             
                             plot(hc, main = "Dendrogram - Modality Classification",
                                  xlab = "Modalities", ylab = "Distance", 
                                  labels = self$modality_names,
                                  sub = "", cex = 0.7)
                             rect.hclust(hc, k = n_clusters, border = 2:5)
                             
                             return(self$labels_mod)
                           },
                           
                           # PREDICT FUNCTION
                           predict = function(new_data) {
                             cat("\n========================================\n")
                             cat("PREDICTING CLUSTERS FOR NEW DATA\n")
                             cat("========================================\n")
                             
                             # Checks
                             if (is.null(self$mca_result)) {
                               stop("Error: You must first run fit() before predicting.")
                             }
                             
                             if (is.null(self$labels_mod)) {
                               stop("Error: You must first run clustering_hierarchical() before predicting.")
                             }
                             
                             # Convert to data frame and factors
                             new_data <- as.data.frame(new_data)
                             
                             # Check that columns match
                             if (!all(names(new_data) %in% self$variable_names)) {
                               stop("Error: Variables in new_data must match the variables of the trained model.")
                             }
                             
                             # Reorder columns
                             new_data <- new_data[, self$variable_names, drop = FALSE]
                             
                             # Convert to factors with the same levels as training data
                             for (var in self$variable_names) {
                               original_levels <- levels(self$data[[var]])
                               new_data[[var]] <- factor(new_data[[var]], levels = original_levels)
                               
                               # Check for unknown modalities
                               unknown_levels <- setdiff(unique(as.character(new_data[[var]])), original_levels)
                               if (length(unknown_levels) > 0) {
                                 warning(sprintf("Variable '%s' contains unknown modalities: %s. They will be treated as NA.",
                                                 var, paste(unknown_levels, collapse = ", ")))
                               }
                             }
                             
                             cat(sprintf("\nPredicting for %d new individuals\n", nrow(new_data)))
                             
                             # Project new individuals into factorial space
                             new_coords <- predict(self$mca_result, newdata = new_data)$coord
                             
                             cat(sprintf("Projection performed on %d dimensions\n", ncol(new_coords)))
                             
                             # For each individual, find activated modalities and their cluster
                             predictions <- data.frame(
                               individual = 1:nrow(new_data),
                               predicted_cluster = NA,
                               distance_to_center = NA
                             )
                             
                             # Create a dictionary of clusters by modality
                             for (i in 1:nrow(new_data)) {
                               # Identify active modalities for this individual
                               active_modalities <- c()
                               for (var in self$variable_names) {
                                 value <- as.character(new_data[i, var])
                                 if (!is.na(value)) {
                                   modality_name <- paste0(var, "_", value)
                                   active_modalities <- c(active_modalities, modality_name)
                                 }
                               }
                               
                               # Retrieve clusters of active modalities
                               clusters_of_modalities <- self$labels_mod[active_modalities]
                               clusters_of_modalities <- clusters_of_modalities[!is.na(clusters_of_modalities)]
                               
                               if (length(clusters_of_modalities) > 0) {
                                 # Majority vote: most frequent cluster
                                 predicted_cluster <- as.numeric(names(sort(table(clusters_of_modalities), decreasing = TRUE)[1]))
                               } else {
                                 # If no known modality, use distance to centers
                                 distances <- apply(self$cluster_centers, 1, function(center) {
                                   sqrt(sum((new_coords[i, 1:ncol(self$cluster_centers)] - center)^2))
                                 })
                                 predicted_cluster <- which.min(distances)
                               }
                               
                               # Calculate distance to predicted cluster center
                               center <- self$cluster_centers[predicted_cluster, ]
                               distance <- sqrt(sum((new_coords[i, 1:length(center)] - center)^2))
                               
                               predictions$predicted_cluster[i] <- predicted_cluster
                               predictions$distance_to_center[i] <- distance
                             }
                             
                             # Display results
                             cat("\nPrediction summary:\n")
                             print(table(predictions$predicted_cluster))
                             
                             cat("\nAverage distance to cluster center by predicted cluster:\n")
                             avg_distances <- aggregate(distance_to_center ~ predicted_cluster, 
                                                        data = predictions, 
                                                        FUN = mean)
                             print(avg_distances)
                             
                             # Display first predictions
                             cat("\nFirst predictions:\n")
                             result_display <- cbind(new_data[1:min(10, nrow(new_data)), ], 
                                                     predictions[1:min(10, nrow(new_data)), ])
                             print(result_display)
                             
                             # Return results
                             return(list(
                               predictions = predictions,
                               coordinates = new_coords
                             ))
                           },
                           
                           # Eigenvalues plot (scree plot)
                           plot_scree = function() {
                             cat("\n========================================\n")
                             cat("EIGENVALUES PLOT\n")
                             cat("========================================\n")
                             
                             n_eig <- min(10, length(self$eigenvalues))
                             eig_df <- data.frame(
                               Dimension = 1:n_eig,
                               Eigenvalue = self$eigenvalues[1:n_eig],
                               Variance = self$inertia_explained[1:n_eig],
                               Cumulative = cumsum(self$inertia_explained[1:n_eig])
                             )
                             
                             p1 <- ggplot(eig_df, aes(x = Dimension, y = Eigenvalue)) +
                               geom_bar(stat = "identity", fill = "steelblue") +
                               labs(title = "Eigenvalues",
                                    x = "Dimension",
                                    y = "Eigenvalue") +
                               theme_minimal() +
                               theme(panel.grid.minor = element_blank())
                             
                             p2 <- ggplot(eig_df, aes(x = Dimension, y = Cumulative)) +
                               geom_line(color = "darkred", linewidth = 1) +
                               geom_point(color = "darkred", size = 3) +
                               labs(title = "Cumulative explained inertia",
                                    x = "Dimension",
                                    y = "% Cumulative inertia") +
                               theme_minimal() +
                               theme(panel.grid.minor = element_blank())
                             
                             # Display plots
                             print(p1)
                             print(p2)
                             
                             invisible(self)
                           },
                           
                           # Visualize MODALITIES in factorial plane
                           plot_modalities = function(axes = c(1, 2), labels = NULL, show_variables = TRUE) {
                             cat("\n========================================\n")
                             cat("MODALITY PROJECTION\n")
                             cat("========================================\n")
                             
                             plot_df <- data.frame(
                               Dim1 = self$coord_mod[, axes[1]],
                               Dim2 = self$coord_mod[, axes[2]],
                               Modality = self$modality_names
                             )
                             
                             if (show_variables) {
                               plot_df$Variable <- sapply(strsplit(self$modality_names, "_"), `[`, 1)
                             }
                             
                             if (!is.null(labels)) {
                               plot_df$Cluster <- as.factor(labels)
                             }
                             
                             p <- ggplot(plot_df, aes(x = Dim1, y = Dim2))
                             
                             if (!is.null(labels)) {
                               p <- p + 
                                 geom_point(aes(color = Cluster), alpha = 0.7, size = 3) +
                                 scale_color_viridis_d() +
                                 labs(color = "Cluster")
                             } else if (show_variables) {
                               p <- p + 
                                 geom_point(aes(color = Variable), alpha = 0.7, size = 3) +
                                 scale_color_brewer(palette = "Set2") +
                                 labs(color = "Variable")
                             } else {
                               p <- p + geom_point(alpha = 0.7, size = 3, color = "steelblue")
                             }
                             
                             p <- p +
                               ggrepel::geom_text_repel(aes(label = Modality), size = 3, max.overlaps = 15) +
                               geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
                               geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
                               labs(
                                 title = "Projection of modalities in factorial plane",
                                 x = sprintf("Dim %d (%.2f%%)", axes[1], self$inertia_explained[axes[1]]),
                                 y = sprintf("Dim %d (%.2f%%)", axes[2], self$inertia_explained[axes[2]])
                               ) +
                               theme_minimal() +
                               theme(panel.grid.minor = element_blank())
                             
                             print(p)
                             invisible(self)
                           },
                           
                           # Evaluation method with hierarchical clustering
                           elbow_method = function(max_clusters = 10, method = "ward") {
                             cat("\n========================================\n")
                             cat("EVALUATING OPTIMAL NUMBER OF CLUSTERS\n")
                             cat("========================================\n")
                             
                             n_modalities <- nrow(self$coord_mod)
                             max_clusters <- min(max_clusters, n_modalities - 1)
                             
                             dist_matrix <- dist(self$coord_mod)
                             
                             if (method == "ward") {
                               hc <- hclust(dist_matrix, method = "ward.D2")
                             } else {
                               hc <- hclust(dist_matrix, method = method)
                             }
                             
                             inertias <- numeric(max_clusters - 1)
                             
                             
                             for (k in 2:max_clusters) {
                               clusters <- cutree(hc, k = k)
                               
                               # Calculate within-cluster inertia
                               total_inertia <- 0
                               for (i in 1:k) {
                                 cluster_points <- self$coord_mod[clusters == i, , drop = FALSE]
                                 if (nrow(cluster_points) > 0) {
                                   center <- colMeans(cluster_points)
                                   total_inertia <- total_inertia + sum(apply(cluster_points, 1, function(x) sum((x - center)^2)))
                                 }
                               }
                               inertias[k - 1] <- total_inertia
                               
                               
                             }
                             
                             elbow_df <- data.frame(
                               K = 2:max_clusters,
                               Inertia = inertias
                               
                             )
                             
                             p1 <- ggplot(elbow_df, aes(x = K, y = Inertia)) +
                               geom_line(color = "steelblue", linewidth = 1) +
                               geom_point(color = "steelblue", size = 3) +
                               labs(title = "Elbow method (modalities - HAC)",
                                    x = "Number of clusters",
                                    y = "Within-cluster inertia") +
                               scale_x_continuous(breaks = 2:max_clusters) +
                               theme_minimal()
                             
                             
                             
                             # Display plots
                             print(p1)
                             #print(p2)
                             
                             invisible(self)
                           },
                           
                           # PRINT method inspired by FactoMineR
                           print = function() {
                             cat("\n")
                             cat("**Results of Multiple Correspondence Analysis (MCA) with Clustering**\n")
                             
                             if (!is.null(self$data)) {
                               cat(sprintf("The analysis was performed on %d individuals, described by %d variables\n", 
                                           nrow(self$data), ncol(self$data)))
                             }
                             
                             cat("*The results are available in the following objects:\n\n")
                             
                             # Create results table
                             results_table <- data.frame(
                               nom = character(),
                               description = character(),
                               stringsAsFactors = FALSE
                             )
                             
                             idx <- 1
                             
                             # MCA results
                             if (!is.null(self$mca_result)) {
                               results_table[idx, ] <- c("$mca_result", "complete MCA results (FactoMineR)")
                               idx <- idx + 1
                               results_table[idx, ] <- c("$eigenvalues", "eigenvalues")
                               idx <- idx + 1
                               results_table[idx, ] <- c("$inertia_explained", "percentage of explained inertia")
                               idx <- idx + 1
                             }
                             
                             # Coordinates
                             if (!is.null(self$coord_ind)) {
                               results_table[idx, ] <- c("$coord_ind", "coordinates of individuals")
                               idx <- idx + 1
                             }
                             
                             if (!is.null(self$coord_mod)) {
                               results_table[idx, ] <- c("$coord_mod", "coordinates of modalities")
                               idx <- idx + 1
                             }
                             
                             # Information about modalities and variables
                             if (!is.null(self$modality_names)) {
                               results_table[idx, ] <- c("$modality_names", "names of modalities")
                               idx <- idx + 1
                             }
                             
                             if (!is.null(self$variable_names)) {
                               results_table[idx, ] <- c("$variable_names", "names of variables")
                               idx <- idx + 1
                             }
                             
                             # Clustering results
                             if (!is.null(self$labels_mod)) {
                               results_table[idx, ] <- c("$labels_mod", "cluster labels for modalities")
                               idx <- idx + 1
                               results_table[idx, ] <- c("$cluster_centers", "cluster centers")
                               idx <- idx + 1
                               results_table[idx, ] <- c("$clustering_method", sprintf("clustering method (%s)", self$clustering_method))
                               idx <- idx + 1
                             }
                             
                             # Data
                             if (!is.null(self$data)) {
                               results_table[idx, ] <- c("$data", "training data")
                               idx <- idx + 1
                             }
                             
                             # Display table with alignment
                             max_name_length <- max(nchar(results_table$nom))
                             
                             for (i in 1:nrow(results_table)) {
                               cat(sprintf("  %-*s  %s\n", 
                                           max_name_length, 
                                           results_table$nom[i], 
                                           results_table$description[i]))
                             }
                             
                             # Additional information
                             if (!is.null(self$mca_result)) {
                               cat(sprintf("\n*Principal components: %d\n", self$n_components))
                               cat(sprintf("*Total explained inertia: %.2f%%\n", 
                                           sum(self$inertia_explained[1:min(self$n_components, length(self$inertia_explained))])))
                             }
                             
                             if (!is.null(self$labels_mod)) {
                               n_clusters <- max(self$labels_mod)
                               cat(sprintf("\n*Clustering performed: %d clusters (method: %s)\n", 
                                           n_clusters, self$clustering_method))
                               cat(sprintf("*Total number of modalities: %d\n", length(self$labels_mod)))
                             }
                             
                             cat("\n")
                             invisible(self)
                           },
                           
                           # Summary of modality clusters
                           summary = function() {
                             if (is.null(self$labels_mod)) {
                               cat("No clustering has been performed. Use clustering_hierarchical().\n")
                               return(invisible(self))
                             }
                             
                             cat("\n========================================\n")
                             cat("SUMMARY OF MODALITY CLUSTERS\n")
                             cat("========================================\n")
                             
                             n_clusters <- max(self$labels_mod)
                             
                             for (i in 1:n_clusters) {
                               modalities <- names(self$labels_mod[self$labels_mod == i])
                               cat(sprintf("\n--- CLUSTER %d (%d modalities) ---\n", i, length(modalities)))
                               
                               mod_by_var <- list()
                               for (mod in modalities) {
                                 var_name <- strsplit(mod, "_")[[1]][1]
                                 if (is.null(mod_by_var[[var_name]])) {
                                   mod_by_var[[var_name]] <- c()
                                 }
                                 mod_by_var[[var_name]] <- c(mod_by_var[[var_name]], mod)
                               }
                               
                               for (var in names(mod_by_var)) {
                                 cat(sprintf("  %s: %s\n", var, paste(mod_by_var[[var]], collapse = ", ")))
                               }
                             }
                             
                             invisible(self)
                           }
                         )
)


# =============================================================================
# USAGE EXAMPLE
# =============================================================================

cat("\n")
cat("==================================================\n")
cat("MCA + HIERARCHICAL CLUSTERING DEMONSTRATION\n")
cat("==================================================\n")

# Create training dataset
set.seed(42)
n_train <- 300

data_train <- data.frame(
  education = sample(c("Bac", "Licence", "Master", "Doctorat"), n_train, replace = TRUE),
  sector = sample(c("Tech", "Finance", "Santé", "Education"), n_train, replace = TRUE),
  experience = sample(c("Junior", "Confirmé", "Senior"), n_train, replace = TRUE),
  city = sample(c("Paris", "Lyon", "Marseille", "Toulouse"), n_train, replace = TRUE)
)

cat(sprintf("\nTraining data: %d individuals\n", nrow(data_train)))

# Initialize and train the model
acm <- ACMClustering$new(n_components = 5)
acm$fit(data_train)

# Display results with print()
print(acm)

# Perform hierarchical clustering
acm$elbow_method(max_clusters = 8, method = "ward")
labels_hierarchical <- acm$clustering_hierarchical(n_clusters = 4, method = "ward")

# Display results again after clustering
cat("\n\n=== AFTER CLUSTERING ===\n")
print(acm)

acm$plot_modalities(axes = c(1, 2), labels = labels_hierarchical, show_variables = FALSE)

# Create new data for prediction
set.seed(123)
n_test <- 50

data_test <- data.frame(
  education = sample(c("Bac", "Licence", "Master", "Doctorat"), n_test, replace = TRUE),
  sector = sample(c("Tech", "Finance", "Santé", "Education"), n_test, replace = TRUE),
  experience = sample(c("Junior", "Confirmé", "Senior"), n_test, replace = TRUE),
  city = sample(c("Paris", "Lyon", "Marseille", "Toulouse"), n_test, replace = TRUE)
)

cat(sprintf("\nTest data: %d individuals\n", nrow(data_test)))
cat("\nFirst rows of test data:\n")
print(head(data_test))

# PREDICT clusters for new data
predictions <- acm$predict(data_test)

# Display detailed results
cat("\n\nDetailed prediction results:\n")
print(head(predictions$predictions, 15))

cat("\n")
cat("==================================================\n")
cat("ANALYSIS COMPLETED\n")
cat("==================================================\n")
