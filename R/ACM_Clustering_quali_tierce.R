
#' @title ClusteringACM: Multiple Correspondence Analysis and Variable Clustering
#'
#' @description
#' An R6 class that combines Multiple Correspondence Analysis (MCA) with 
#' hierarchical clustering of categorical variables. This unified approach 
#' allows for comprehensive analysis of categorical data, including 
#' dimensionality reduction and variable grouping based on Cramér's V 
#' similarity measure.
#'
#' @details
#' The `ClusteringACM` class provides a complete workflow for:
#' * Multiple Correspondence Analysis using FactoMineR
#' * Hierarchical clustering of categorical variables
#' * Visualization of results
#' * Prediction on new data
#'
#' The clustering uses Cramér's V coefficient as a similarity measure between
#' categorical variables, then applies hierarchical clustering with various
#' linkage methods (Ward, complete, average, etc.).
#'
#' @import R6
#' @import FactoMineR
#' @import factoextra
#' @importFrom stats as.dist hclust cutree chisq.test
#' @importFrom graphics barplot lines plot rect.hclust
#' @importFrom utils head tail
#'
#' @export
#' @examples
#' \dontrun{
#' # Load data
#' data(iris)
#' iris_cat <- data.frame(
#'   Sepal.Length = cut(iris$Sepal.Length, 3),
#'   Sepal.Width = cut(iris$Sepal.Width, 3),
#'   Petal.Length = cut(iris$Petal.Length, 3),
#'   Petal.Width = cut(iris$Petal.Width, 3),
#'   Species = iris$Species
#' )
#'
#' # Initialize model with parameters
#' model <- ClusteringACM$new(
#'   do_acm = TRUE,
#'   do_clustering = TRUE,
#'   ncp = 5,
#'   graph = FALSE,
#'   clustering_method = "ward.D2",
#'   k = 3
#' )
#'
#' # Fit on data
#' model$fit(iris_cat)
#'
#' # Print results
#' model$print()
#' model$summary()
#'
#' # Visualizations
#' model$plot_acm(choix = "var")
#' model$plot_dendrogram()
#' model$plot_scree()
#'
#' # Access results
#' eigenvalues <- model$get_eigenvalues()
#' clusters <- model$get_clusters()
#' }
ClusteringACM <- R6::R6Class(
  "ClusteringACM",
  
  # ==========================================================================
  # PUBLIC ATTRIBUTES
  # ==========================================================================
  public = list(
    #' @field data Original data frame
    data = NULL,
    
    #' @field acm_result MCA result object from FactoMineR
    acm_result = NULL,
    
    #' @field acm_params Parameters used for MCA
    acm_params = NULL,
    
    #' @field similarity_matrix Cramér's V similarity matrix between variables
    similarity_matrix = NULL,
    
    #' @field dissim_matrix Dissimilarity matrix (1 - similarity)
    dissim_matrix = NULL,
    
    #' @field hclust_result Hierarchical clustering result
    hclust_result = NULL,
    
    #' @field clusters Cluster assignments for variables
    clusters = NULL,
    
    #' @field variables Variables used for clustering
    variables = NULL,
    
    #' @field method Clustering method (ward.D2, complete, etc.)
    method = NULL,
    
    #' @field n_clusters Number of clusters
    n_clusters = NULL,
    
    #' @field acm_fitted Boolean indicating if MCA has been fitted
    acm_fitted = FALSE,
    
    #' @field clustering_fitted Boolean indicating if clustering has been fitted
    clustering_fitted = FALSE,
    
    
    # ========================================================================
    # CONSTRUCTOR
    # ========================================================================
    
    #' @description
    #' Initialize a new ClusteringACM object
    #'
    #' @param do_acm Logical, perform MCA (default: TRUE)
    #' @param do_clustering Logical, perform clustering (default: TRUE)
    #' @param ncp Number of dimensions to keep in MCA (default: 5)
    #' @param ind.sup Vector of supplementary individuals
    #' @param quanti.sup Vector of supplementary quantitative variables
    #' @param quali.sup Vector of supplementary qualitative variables
    #' @param excl Vector indicating categories to exclude
    #' @param graph Logical, whether to display graphs (default: FALSE)
    #' @param level.ventil Threshold for grouping rare categories
    #' @param axes Axes to plot (default: c(1, 2))
    #' @param row.w Row weights for MCA
    #' @param method MCA method: "Indicator" or "Burt" (default: "Indicator")
    #' @param na.method How to handle missing values (default: "NA")
    #' @param tab.disj Pre-computed indicator matrix
    #' @param variables Variables to use for clustering (default: all)
    #' @param clustering_method Hierarchical clustering method 
    #'   (default: "ward.D2"). Options: "ward.D2", "ward.D", "single", 
    #'   "complete", "average", "mcquitty", "median", "centroid"
    #' @param k Number of clusters to create (optional)
    #'
    #' @return A new ClusteringACM object
    initialize = function(do_acm = TRUE,
                          do_clustering = TRUE,
                          ncp = 5,
                          ind.sup = NULL,
                          quanti.sup = NULL,
                          quali.sup = NULL,
                          excl = NULL,
                          graph = FALSE,
                          level.ventil = 0,
                          axes = c(1, 2),
                          row.w = NULL,
                          method = "Indicator",
                          na.method = "NA",
                          tab.disj = NULL,
                          variables = NULL,
                          clustering_method = "ward.D2",
                          k = NULL) {
      
      # Stockage des options de fit
      private$do_acm <- do_acm
      private$do_clustering <- do_clustering
      private$clustering_vars <- variables
      private$clustering_k <- k
      
      # Stockage des paramètres ACM
      self$acm_params <- list(
        ncp = ncp,
        ind.sup = ind.sup,
        quanti.sup = quanti.sup,
        quali.sup = quali.sup,
        excl = excl,
        graph = graph,
        level.ventil = level.ventil,
        axes = axes,
        row.w = row.w,
        method = method,
        na.method = na.method,
        tab.disj = tab.disj
      )
      
      # Stockage de la méthode de clustering
      self$method <- clustering_method
      self$n_clusters <- k
      
      message("ClusteringACM initialized")
      message("  • MCA: ", ifelse(do_acm, "enabled", "disabled"))
      message("  • Clustering: ", ifelse(do_clustering, "enabled", "disabled"))
      if (do_clustering && !is.null(k)) {
        message("  • Number of clusters: ", k)
      }
      
      invisible(self)
    },
    
    
    # ========================================================================
    # FIT METHOD
    # ========================================================================
    
    #' @description
    #' Fit the model on data
    #'
    #' @param X A data frame with categorical variables to analyze
    #'
    #' @return Self (invisibly) for method chaining
    #' 
    #' @examples
    #' \dontrun{
    #' # Initialize with parameters
    #' model <- ClusteringACM$new(
    #'   do_acm = TRUE,
    #'   do_clustering = TRUE,
    #'   ncp = 5,
    #'   clustering_method = "ward.D2",
    #'   k = 3
    #' )
    #' 
    #' # Fit on data
    #' model$fit(my_data)
    #' }
    fit = function(X) {
      
      # Validation des données
      if (!is.data.frame(X)) {
        stop("X must be a data frame")
      }
      
      # Stockage des données
      self$data <- X
      
      message("Fitting model on data with ", 
              nrow(X), " individuals and ", 
              ncol(X), " variables")
      
      # Récupération des paramètres d'initialisation
      do_acm <- private$do_acm
      do_clustering <- private$do_clustering
      variables <- private$clustering_vars
      k <- private$clustering_k
      
      # --- STEP 1: MCA ---
      if (do_acm) {
        message(">>> Fitting MCA...")
        self$acm_result <- do.call(FactoMineR::MCA, 
                                   c(list(X = self$data), self$acm_params))
        self$acm_fitted <- TRUE
        message("    MCA fitted successfully")
      }
      
      # --- STEP 2: Variable clustering ---
      if (do_clustering) {
        message(">>> Computing variable clustering...")
        
        if (is.null(variables)) {
          self$variables <- names(self$data)
        } else {
          self$variables <- variables
        }
        
        # Convert to factors if needed
        for (var in self$variables) {
          if (!is.factor(self$data[[var]])) {
            self$data[[var]] <- as.factor(self$data[[var]])
          }
        }
        
        # Compute similarity matrix (Cramér's V)
        n_vars <- length(self$variables)
        sim_mat <- matrix(1, nrow = n_vars, ncol = n_vars)
        rownames(sim_mat) <- self$variables
        colnames(sim_mat) <- self$variables
        
        message("    Computing similarity matrix...")
        for (i in 1:(n_vars - 1)) {
          for (j in (i + 1):n_vars) {
            sim_mat[i, j] <- private$cramer(
              self$data[[self$variables[i]]], 
              self$data[[self$variables[j]]]
            )
            sim_mat[j, i] <- sim_mat[i, j]
          }
        }
        
        self$similarity_matrix <- sim_mat
        self$dissim_matrix <- as.dist(1 - sim_mat)
        
        # Hierarchical clustering
        message("    Hierarchical clustering (method: ", clustering_method, ")...")
        self$method <- clustering_method
        self$hclust_result <- hclust(self$dissim_matrix, method = clustering_method)
        
        # Cut tree if k specified
        if (!is.null(k)) {
          self$n_clusters <- k
          self$clusters <- cutree(self$hclust_result, k = k)
          message("    Clusters created: ", k, " groups")
        }
        
        self$clustering_fitted <- TRUE
        message("    Clustering fitted successfully")
      }
      
      message("\n=== Model fitted successfully ===")
      invisible(self)
    },
    
    
    # ========================================================================
    # PREDICT METHOD
    # ========================================================================
    
    #' @description
    #' Predict cluster membership for new individuals or variables
    #'
    #' @param new_data Data frame with new observations
    #' @param new_variables Variables to predict (for type = "variables")
    #' @param type Type of prediction: "individuals" or "variables"
    #'
    #' @return Predictions (format depends on type)
    predict = function(new_data = NULL, 
                       new_variables = NULL,
                       type = c("individuals", "variables")) {
      
      type <- match.arg(type)
      
      if (type == "individuals") {
        return(private$predict_individuals(new_data))
      } else {
        return(private$predict_variables(new_data, new_variables))
      }
    },
    
    
    # ========================================================================
    # PRINT METHOD
    # ========================================================================
    
    #' @description
    #' Print model summary
    print = function() {
      cat("╔════════════════════════════════════════════════════════════╗\n")
      cat("║          ClusteringACM - Factorial Analysis Model         ║\n")
      cat("╚════════════════════════════════════════════════════════════╝\n\n")
      
      cat("📊 DATA\n")
      cat("   • Number of individuals:", nrow(self$data), "\n")
      cat("   • Number of variables:", ncol(self$data), "\n\n")
      
      cat("🔍 MULTIPLE CORRESPONDENCE ANALYSIS (MCA)\n")
      if (self$acm_fitted) {
        cat("   • Status: ✓ Fitted\n")
        cat("   • Dimensions:", self$acm_params$ncp, "\n")
        cat("   • Method:", self$acm_params$method, "\n")
        
        eig <- self$acm_result$eig
        cat("   • Explained variance (first 3 dimensions):\n")
        for (i in 1:min(3, nrow(eig))) {
          cat(sprintf("     - Dim %d: %.2f%%\n", i, eig[i, 2]))
        }
      } else {
        cat("   • Status: ✗ Not fitted\n")
      }
      cat("\n")
      
      cat("🌳 HIERARCHICAL VARIABLE CLUSTERING\n")
      if (self$clustering_fitted) {
        cat("   • Status: ✓ Fitted\n")
        cat("   • Method:", self$method, "\n")
        cat("   • Variables analyzed:", length(self$variables), "\n")
        
        if (!is.null(self$n_clusters)) {
          cat("   • Number of clusters:", self$n_clusters, "\n")
          cat("   • Cluster composition:\n")
          
          for (i in 1:self$n_clusters) {
            vars_in_cluster <- names(self$clusters)[self$clusters == i]
            cat(sprintf("     - Cluster %d (%d var): %s\n", 
                        i, 
                        length(vars_in_cluster),
                        paste(vars_in_cluster, collapse = ", ")))
          }
          
          cat(sprintf("   • Last fusion height: %.4f\n", 
                      tail(self$hclust_result$height, 1)))
        } else {
          cat("   • Number of clusters: not specified\n")
        }
      } else {
        cat("   • Status: ✗ Not fitted\n")
      }
      
      cat("\n")
      cat("💡 Use fit() to fit the model\n")
      cat("💡 Use predict() for predictions\n")
      
      invisible(self)
    },
    
    
    # ========================================================================
    # MCA METHODS
    # ========================================================================
    
    #' @description Get eigenvalues from MCA
    #' @return Matrix of eigenvalues with variance percentages
    get_eigenvalues = function() {
      if (!self$acm_fitted) stop("MCA not fitted. Use fit()")
      return(self$acm_result$eig)
    },
    
    #' @description Get variable coordinates from MCA
    #' @param axes Axes to extract (default: c(1, 2))
    #' @return Matrix of variable coordinates
    get_var_coords = function(axes = c(1, 2)) {
      if (!self$acm_fitted) stop("MCA not fitted. Use fit()")
      return(self$acm_result$var$coord[, axes, drop = FALSE])
    },
    
    #' @description Get variable contributions from MCA
    #' @param axes Axes to extract (default: c(1, 2))
    #' @return Matrix of variable contributions
    get_var_contrib = function(axes = c(1, 2)) {
      if (!self$acm_fitted) stop("MCA not fitted. Use fit()")
      return(self$acm_result$var$contrib[, axes, drop = FALSE])
    },
    
    #' @description Get variable cos2 (quality of representation) from MCA
    #' @param axes Axes to extract (default: c(1, 2))
    #' @return Matrix of cos2 values
    get_var_cos2 = function(axes = c(1, 2)) {
      if (!self$acm_fitted) stop("MCA not fitted. Use fit()")
      return(self$acm_result$var$cos2[, axes, drop = FALSE])
    },
    
    #' @description Get individual coordinates from MCA
    #' @param axes Axes to extract (default: c(1, 2))
    #' @return Matrix of individual coordinates
    get_ind_coords = function(axes = c(1, 2)) {
      if (!self$acm_fitted) stop("MCA not fitted. Use fit()")
      return(self$acm_result$ind$coord[, axes, drop = FALSE])
    },
    
    
    # ========================================================================
    # CLUSTERING METHODS
    # ========================================================================
    
    #' @description Get similarity matrix (Cramér's V)
    #' @return Similarity matrix between variables
    get_similarity_matrix = function() {
      if (!self$clustering_fitted) {
        stop("Clustering not fitted. Use fit()")
      }
      return(self$similarity_matrix)
    },
    
    #' @description Get cluster assignments
    #' @return Named vector of cluster assignments
    get_clusters = function() {
      if (!self$clustering_fitted || is.null(self$clusters)) {
        stop("Clustering not fitted or k not specified. Use fit(k = ...)")
      }
      return(self$clusters)
    },
    
    
    # ========================================================================
    # VISUALIZATION METHODS
    # ========================================================================
    
    #' @description Plot MCA results
    #' @param choix Type of plot: "ind" or "var"
    #' @param axes Axes to plot (default: c(1, 2))
    #' @param ... Additional arguments passed to plot.MCA
    plot_acm = function(choix = "ind", axes = c(1, 2), ...) {
      if (!self$acm_fitted) stop("MCA not fitted. Use fit()")
      plot.MCA(self$acm_result, choix = choix, axes = axes, ...)
    },
    
    #' @description Plot dendrogram
    #' @param k Number of clusters to highlight (optional)
    plot_dendrogram = function(k = NULL) {
      if (!self$clustering_fitted) {
        stop("Clustering not fitted. Use fit()")
      }
      
      k_to_use <- if (is.null(k)) self$n_clusters else k
      
      plot(self$hclust_result, 
           main = "Dendrogram - Variable Clustering",
           xlab = "Variables", 
           ylab = "Distance (1 - Cramér's V)",
           sub = paste("Method:", self$method))
      
      if (!is.null(k_to_use)) {
        rect.hclust(self$hclust_result, k = k_to_use, border = 2:6)
      }
    },
    
    #' @description Plot scree plot (variance explained)
    plot_scree = function() {
      if (!self$acm_fitted) stop("MCA not fitted. Use fit()")
      
      eig <- self$acm_result$eig
      barplot(eig[, 2], 
              names.arg = 1:nrow(eig),
              main = "Scree plot - Explained variance",
              xlab = "Dimensions",
              ylab = "Percentage of variance (%)",
              col = "steelblue")
      lines(x = 1:nrow(eig), y = eig[, 2], type = "b", col = "red", pch = 19)
    },
    
    
    # ========================================================================
    # SUMMARY METHOD
    # ========================================================================
    
    #' @description Print detailed summary
    summary = function() {
      cat("\n")
      cat("═══════════════════════════════════════════════════════════════\n")
      cat("                ClusteringACM MODEL SUMMARY                    \n")
      cat("═══════════════════════════════════════════════════════════════\n\n")
      
      if (self$acm_fitted) {
        cat("--- MULTIPLE CORRESPONDENCE ANALYSIS ---\n\n")
        cat("Dimensions:", self$acm_params$ncp, "\n")
        cat("Method:", self$acm_params$method, "\n\n")
        
        cat("Eigenvalues (explained variance):\n")
        eig <- self$acm_result$eig
        print(head(eig, 10))
        cat("\n")
      }
      
      if (self$clustering_fitted) {
        cat("--- HIERARCHICAL CLUSTERING ---\n\n")
        cat("Method:", self$method, "\n")
        cat("Variables:", length(self$variables), "\n")
        
        if (!is.null(self$n_clusters)) {
          cat("Clusters:", self$n_clusters, "\n\n")
          
          cat("Detailed composition:\n")
          for (i in 1:self$n_clusters) {
            vars <- names(self$clusters)[self$clusters == i]
            cat("Cluster", i, ":\n")
            cat("  ", paste(vars, collapse = ", "), "\n\n")
          }
        }
      }
      
      invisible(self)
    }
  ),
  
  
  # ==========================================================================
  # PRIVATE METHODS
  # ==========================================================================
  private = list(
    
    # Options de fit
    do_acm = TRUE,
    do_clustering = TRUE,
    clustering_vars = NULL,
    clustering_k = NULL,
    
    # Cramér's V coefficient
    cramer = function(y, x, print_chi2 = FALSE) {
      if (!is.factor(y)) y <- as.factor(y)
      if (!is.factor(x)) x <- as.factor(x)
      
      K <- nlevels(y)
      L <- nlevels(x)
      n <- length(y)
      
      chi2 <- chisq.test(y, x, correct = FALSE)
      
      if (print_chi2) print(chi2$statistic)
      
      v <- sqrt(chi2$statistic / (n * min(K - 1, L - 1)))
      return(as.numeric(v))
    },
    
    
    # Prediction for new individuals (MCA projection)
    predict_individuals = function(new_data) {
      if (!self$acm_fitted) {
        stop("MCA not fitted. Use fit() first")
      }
      
      if (is.null(new_data)) {
        stop("new_data cannot be NULL for predicting individuals")
      }
      
      if (!all(names(new_data) %in% names(self$data))) {
        stop("new_data must contain the same variables as training data")
      }
      
      for (var in names(new_data)) {
        if (!is.factor(new_data[[var]])) {
          new_data[[var]] <- as.factor(new_data[[var]])
        }
      }
      
      coords <- self$acm_result$var$coord
      
      message("Projecting ", nrow(new_data), " new individuals")
      
      return(list(
        message = "Full MCA projection not implemented",
        method = "Use predict.MCA from FactoMineR for complete projection"
      ))
    },
    
    
    # Prediction for new variables (clustering)
    predict_variables = function(new_data, new_variables = NULL) {
      if (!self$clustering_fitted) {
        stop("Clustering not fitted. Use fit() first")
      }
      
      if (is.null(self$clusters)) {
        stop("Number of clusters must be specified in fit() to use predict()")
      }
      
      if (is.null(new_data)) {
        stop("new_data cannot be NULL for predicting variables")
      }
      
      if (is.null(new_variables)) {
        new_variables <- names(new_data)
      }
      
      for (var in new_variables) {
        if (!is.factor(new_data[[var]])) {
          new_data[[var]] <- as.factor(new_data[[var]])
        }
      }
      
      cluster_centers <- private$compute_cluster_centers()
      
      predictions <- sapply(new_variables, function(new_var) {
        similarities <- sapply(names(cluster_centers), function(center_var) {
          private$cramer(new_data[[new_var]], self$data[[center_var]])
        })
        cluster_centers[which.max(similarities)]
      })
      
      message("Prediction done for ", length(new_variables), " variable(s)")
      return(predictions)
    },
    
    
    # Compute cluster centers
    compute_cluster_centers = function() {
      centers <- sapply(1:self$n_clusters, function(cluster_id) {
        vars_in_cluster <- names(self$clusters)[self$clusters == cluster_id]
        
        if (length(vars_in_cluster) == 1) {
          return(vars_in_cluster)
        }
        
        avg_similarities <- sapply(vars_in_cluster, function(var1) {
          mean(sapply(vars_in_cluster[vars_in_cluster != var1], function(var2) {
            self$similarity_matrix[var1, var2]
          }))
        })
        
        return(vars_in_cluster[which.max(avg_similarities)])
      })
      
      names(centers) <- 1:self$n_clusters
      return(centers)
    }
  )
)


