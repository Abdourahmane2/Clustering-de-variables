library (R6)

CAH <- R6Class(
  "CAH",

  public = list (
    data = NULL, # Raw data
    X_last = NULL, # Last X used in predict
    method = NULL, # Aggregation method (default: ward.D2)
    dist_method = "correlation", # Distance method for variable clustering
    corr_moy = NULL, # Average correlation between variables
    dist_matrix = NULL, # Distance matrix(dissimularity)
    hc = NULL, # hclust object
    best_k = NULL, # Optimal number of clusters
    clusters = NULL, # Final partition
    predict_result = NULL, # Prediction results
    compo_latent = NULL, # Latent components per cluster


    # ---- Constructor ----
initialize = function(method = "ward.D2") {
  self$method <- method
  message ("[CAH] CAH initialized. Please call $fit(data) to fit the model.")
},

# ---- Fit method ----
fit = function(data) {
  # Data validation
  if (!is.data.frame(data) && !is.matrix(data)){
    stop ("[CAH] It is not possible to perform CAH; the data must be in the form of a dataframe or a matrix.")
  }

  df <- as.data.frame(data)

  # Minimum size check
  if (nrow(df) < 2 || ncol(df) < 2) {
    stop("[CAH] Not enough data: To perform a CAH, you need at least 2 individuals and 2 variables.")
  }
  # Variable type verification
  type <- sapply (df, class)

  # - Quantivative variables (numeric)
  quanti_var <- names(type[type %in% c("numeric", "integer")])
  # - Qualitative variables (strings...)
  quali_var <- names(type[type %in% c("factor","character", "logical")])

  if (length(quali_var) > 0) {
    warning(" [CAH] There are qualitative variables in the dataframe:", paste(quali_var, collapse =", "), ". They will not be used for CAH.")
  }
  #We only keep quantitative variables
  df <- df[, quanti_var, drop = FALSE]

  # Remove constant variables (variance = 0 or NA)
  if (ncol(df) > 0) {
    variance <- sapply(df, function(x) var(x, na.rm = TRUE))
    var_zero <- names(variance[variance == 0])
    if (length(var_zero) > 0) {
      warning("[CAH] Constant variables are removed: ", paste(var_zero, collapse = ", "))
      df <- df[, !(names(df) %in% var_zero), drop = FALSE]
    }
  }

  # Check remaining variables
  if (ncol(df) == 0){
    stop("[CAH] There are no valid quantitative variables available for analysis.")
  }
  # Remove missing values
  if (anyNA(df)) {
    warning("[CAH] Removal of lines containing missing data")
    df <- na.omit(df)
  }
  # Check remaining individuals
  if (nrow(df) == 0){
    stop("[CAH] There are not enough individuals available for analysis.")
  }
  self$data <- df
  message("[CAH] The data is loaded ! There are :", nrow(self$data)," individuals and ", ncol(self$data), " variables.")


  # Correlation matrix (on raw data - correlation is scale-invariant)
  corr_matrix <- cor(self$data)
  self$corr_moy <- mean(abs(corr_matrix[upper.tri(corr_matrix)])) # Voir si je peux le placer dans le summary ou print car inutile dans calcule.
  #message("[CAH] Calculated correlation matrix (average correlation = ", round(self$corr_moy, 3), ").")

  # Distance matrix based on correlation
  self$dist_matrix <- as.dist(sqrt(2 * (1 - abs(corr_matrix))))
  #message("[CAH] Distance matrix created between variables (correlational).")

  # Hierarchical clustering
  self$hc <- hclust(self$dist_matrix, method = self$method)
  #message("[CAH] is performed using the method = ", method, ".")

  # Detect optimal k using elbow method
  h <- self$hc$height
  d1 <- diff(h)
  d2 <- diff(d1)
  idx <- which.min(d2) + 1
  self$best_k <- idx
  #message("[CAH] The ideal number of classes detected is: k =", self$best_k)

  invisible(self)

},

# ---- Cut tree method ----
cutree = function(k=NULL) {
  if (is.null(self$hc)){
    stop("[CAH] : Call $fit() first.")
  }
  if (is.null(k)){
    k<- self$best_k
  }
  cl <- cutree(self$hc, k=k)
  names(cl) <- colnames(self$data)

  self$clusters <- cl

  # Compute latent components for each cluster (PCA)
  self$compo_latent <- list()

  for (groupe in unique(cl)){
    var_groupe <- names(cl[cl == groupe])

    if (length(var_groupe) > 1) {
      #LOCAL STANDARDIZATION for PCA
      data_groupe_scaled <- scale(self$data[, var_groupe])

      acp <- prcomp(data_groupe_scaled, center = FALSE, scale. = FALSE)
      Zk <- acp$x[, 1]

      # Correlations with standardized data
      cor_vals <- cor(data_groupe_scaled, Zk)

      self$compo_latent[[as.character(groupe)]] <- list(
        Zk = Zk,
        vars = var_groupe,
        cor_vals = cor_vals,
        scaled_data = data_groupe_scaled # Save for the predict()
        #message("\nCluster, "group": latent component (1st PCA axis)")
        #print(round(cor_vals^2, 3))
      )
    } else {
      # Single variable: standardize it too
      data_scaled <- scale(self$data[, var_groupe])

      self$compo_latent[[as.character(groupe)]] <- list(
        Zk = as.vector(data_scaled),
        vars = var_groupe,
        cor_vals = 1,
        scaled_data = data_scaled
      )
    }
  }

  message("Partitioning completed: ", k, " clusters and calculated latent components. ")
  return(cl)
},

#---- Predict method ----
predict = function(X_new) {
  if (is.null(self$clusters)){
    stop("[CAH] first call $cutree().")
  }
  if (is.null(self$compo_latent)){
    stop("[CAH] Missing latent components (internal error).")
  }
  X <- as.data.frame(X_new)

  # Step 1 : Validation
  if (nrow(X) != nrow(self$data)) {
    stop("The number of individuals must be identical to that in our base dataframe.")
  }
  if (anyNA(X)) {
    stop ("The game contains missing values (NA).")
  }

  self$X_last <- X

  # Assign new variables to cluster
  nouv_clusters <- rep(NA, ncol(X))
  names(nouv_clusters) <- colnames(X)


  for (j in seq_len(ncol(X))) {
    # STANDARDIZE the new variable
    var_data_scaled <- scale(X[, j])
    cor_latent <- c()

    # Compute correlation with each cluster's latent component
    for (groupe in unique(self$clusters)) {
      Zk <- self$compo_latent[[as.character(groupe)]]$Zk
      cor_latent[as.character(groupe)] <- abs(cor(var_data_scaled, Zk))

    }

    # Assign to cluster with highest correlation
    best_cluster <- as.numeric(names(which.max(cor_latent)))
    nouv_clusters[j] <- best_cluster
  }

  # Save the result
  self$predict_result <- nouv_clusters

  message("[CAH] Assignment of new variables to existing clusters completed.")
  print(nouv_clusters)

  invisible(self)

},

print = function(...) {
  cat("\n──────────────────────────────────────────────\n")
  cat("    Hierarchical Clustering on Variables (HCA)\n")
  cat("──────────────────────────────────────────────\n")

  # Checking the data
  if (is.null(self$data)) {
    cat(" No data loaded.\n")
    cat("Call $fit(data) to load and fit the model.\n")
    return(invisible(self))
  }

  # General informations
  cat(" Data: ", nrow(self$data), " individuals × ", ncol(self$data), " variables\n", sep = "")
  cat(" Aggregation method: ", ifelse(is.null(self$method), "non spécifiée", self$method), "\n")
  cat(" Distance method:", ifelse(is.null(self$dist_method), "corrélation", self$dist_method), "\n")

  # Correlation
  if (!is.null(self$corr_moy)) {
    cat(" Average correlation:", round(self$corr_moy, 3), "\n")
  }

  # Nombre optimal de clusters
  if (!is.null(self$best_k)) {
    cat(" Optimal k detected :", self$best_k, "\n")
  } else {
    cat(" Optimal k: not determined (call $fit())\n")
  }

  # Preparation of the clusters
  if (!is.null(self$clusters)) {
    tb <- table(self$clusters)
    cat(" Variables per cluster:\n")
    print(tb)
  } else {
    cat(" Partitioning: not done (call $cutree())\n")
  }

  # Illustrative variables added (via predict)
  if (!is.null(self$predict_result)) {
    cat("Supplementary variables added: ", length(self$predict_result), "\n")
  }

  cat("──────────────────────────────────────────────\n\n")
  invisible(self)
},


summary = function(...) {
  cat("\n──────────────────────────────────────────────\n")
  cat("   HCA Model - Detailed Summary\n")
  cat("──────────────────────────────────────────────\n")

  # First Check
  if (is.null(self$hc)) {
    cat("No model fitted. Call $fit(data) first.")
    return(invisible(NULL))
  }

  # General information
  cat("• Aggregation method: ", self$method, "\n")
  cat("• Average correlation: ", round(self$corr_moy, 3), "\n")
  cat("• Optimal k: ", self$best_k, "\n\n")

  if (is.null(self$clusters)) {
    cat(" No partitioning done. Call $cutree() first.\n")
    return(invisible(NULL))
  }

  # Distribution of clusters
  cat("Distribution of active variables:\n")
  print(table(self$clusters))

  # List of variables per cluster
  cat("\n Variables per cluster:\n")
  for (grp in unique(self$clusters)) {
    vars_grp <- names(self$clusters[self$clusters == grp])
    cat("  -> Cluster", grp, "(", length(vars_grp), "variables) :", paste(vars_grp, collapse = ", "), "\n")

  }

  # Local PCA on each cluster (interpretation analysis)
  cat("\nLocal PCA (standardized within each cluster):\n")
    if (!is.null(self$compo_latent)) {
      for (group in names(self$compo_latent)) {
        compo <- self$compo_latent[[group]]

      cat("\n Cluster", group, ":\n")

      if (length(compo$vars) > 1 && !is.null(compo$cor_vals)) {
        cor_vec <- as.numeric(compo$cor_vals)
        names(cor_vec) <- compo$vars
        cat("  Squared correlations with latent component:\n")
        print(round(cor_vec^2, 3))
        best_var <- names(which.max(cor_vec^2))
        cat("-> Representative variable (parangon) :", best_var, "\n")
      } else {
        cat(" Single variable: ", compo$vars, "\n", sep = "")
      }
    }
  }

  # Information on illustrative variables (from predict)
  if (!is.null(self$predict_result)) {
    cat("\n──────────────────────────────────────────────\n")
    cat("Supplementary variables:\n")
    print(self$predict_result)

    # Correlations between new variables and latent components
    if (is.null(self$X_last)) {
      cat("\n Unable to display correlations for illustrative variables (X not retained in the object).\n")
    } else {
      cat("\n Correlations |r(X_j, Z_k)| of the new variables with each cluster:\n")

      nouv_vars <- names(self$predict_result)
      for (v in nouv_vars) {
        var_data <- self$X_last[, v]
        cor_latent <- c()

        for (group in unique(self$clusters)) {
          vars_group <- names(self$clusters[self$clusters == grp])

          if (length(vars_grp) > 1) {
            acp <- prcomp(self$data[, vars_grp], center = FALSE, scale. = FALSE)
            Zk <- acp$x[, 1]
            cor_latent[as.character(grp)] <- abs(cor(var_data, Zk))
          } else {
            cor_latent[as.character(grp)] <- abs(cor(var_data, self$data[, vars_grp]))
          }
        }

        cat("\n -> Variable", v, ":\n")
        print(round(cor_latent, 3))
        best_cl <- as.numeric(names(which.max(cor_latent)))
        cat("   Meilleure corrélation avec le cluster", best_cl, "\n")
      }
    }

    # New complete distribution (with illustrative variables)
    cat("\nComplete partition (active + supplementary):\n")
    all_clusters <- c(self$clusters, self$predict_result)
    for (grp in sort(unique(all_clusters))) {
      vars <- names(all_clusters[all_clusters == grp])
      cat("  -> Cluster", grp, "(", length(vars), "variables) :", paste(vars, collapse = ", "), "\n")
    }
  }

  cat("\n──────────────────────────────────────────────\n")
  invisible(self)
}
  )
)































