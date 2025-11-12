library(R6)
library(readxl)

# Vérification et chargement packages
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(FactoMineR)
library(factoextra)




ClusteringACM <- R6Class(
  "ClusteringACM",
  
  # ==========================================================================
  # ATTRIBUTS PUBLICS
  # ==========================================================================
  public = list(
    # Données
    data = NULL,
    
    # ACM
    acm_result = NULL,
    acm_params = NULL,
    
    # Clustering
    similarity_matrix = NULL,
    dissim_matrix = NULL,
    hclust_result = NULL,
    clusters = NULL,
    
    # Paramètres du modèle
    variables = NULL,
    method = NULL,
    n_clusters = NULL,
    
    # État du modèle
    acm_fitted = FALSE,
    clustering_fitted = FALSE,
    
    
    # ========================================================================
    # CONSTRUCTEUR
    # ========================================================================
    initialize = function(X, 
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
                          tab.disj = NULL) {
      
      # Validation des données
      if (!is.data.frame(X)) {
        stop("X doit être un dataframe")
      }
      
      # Stockage des données
      self$data <- X
      
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
      
      message("ClusteringACM initialisé avec ", 
              nrow(X), " individus et ", 
              ncol(X), " variables")
      
      invisible(self)
    },
    
    
    # ========================================================================
    # MÉTHODE FIT - Entraînement du modèle complet
    # ========================================================================
    fit = function(do_acm = TRUE, 
                   do_clustering = TRUE,
                   variables = NULL, 
                   clustering_method = "ward.D2", 
                   k = NULL) {
      
      # --- ÉTAPE 1 : ACM ---
      if (do_acm) {
        message(">>> Ajustement de l'ACM...")
        self$acm_result <- do.call(MCA, c(list(X = self$data), self$acm_params))
        self$acm_fitted <- TRUE
        message("    ACM ajustée avec succès")
      }
      
      # --- ÉTAPE 2 : Clustering des variables ---
      if (do_clustering) {
        message(">>> Calcul du clustering de variables...")
        
        # Variables à utiliser
        if (is.null(variables)) {
          self$variables <- names(self$data)
        } else {
          self$variables <- variables
        }
        
        # Conversion en facteurs si nécessaire
        for (var in self$variables) {
          if (!is.factor(self$data[[var]])) {
            self$data[[var]] <- as.factor(self$data[[var]])
          }
        }
        
        # Calcul de la matrice de similarité (V de Cramer)
        n_vars <- length(self$variables)
        sim_mat <- matrix(1, nrow = n_vars, ncol = n_vars)
        rownames(sim_mat) <- self$variables
        colnames(sim_mat) <- self$variables
        
        message("    Calcul de la matrice de similarité...")
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
        
        # Clustering hiérarchique
        message("    Clustering hiérarchique (méthode: ", clustering_method, ")...")
        self$method <- clustering_method
        self$hclust_result <- hclust(self$dissim_matrix, method = clustering_method)
        
        # Découpage en k clusters si spécifié
        if (!is.null(k)) {
          self$n_clusters <- k
          self$clusters <- cutree(self$hclust_result, k = k)
          message("    Clusters créés: ", k, " groupes")
        }
        
        self$clustering_fitted <- TRUE
        message("    Clustering ajusté avec succès")
      }
      
      message("\n=== Modèle ajusté avec succès ===")
      invisible(self)
    },
    
    
    # ========================================================================
    # MÉTHODE PREDICT - Classification de nouveaux individus et variables
    # ========================================================================
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
    # MÉTHODE PRINT - Affichage du modèle
    # ========================================================================
    print = function() {
      cat("╔════════════════════════════════════════════════════════════╗\n")
      cat("║          ClusteringACM - Modèle d'Analyse Factorielle     ║\n")
      cat("╚════════════════════════════════════════════════════════════╝\n\n")
      
      # --- Informations générales ---
      cat("📊 DONNÉES\n")
      cat("   • Nombre d'individus:", nrow(self$data), "\n")
      cat("   • Nombre de variables:", ncol(self$data), "\n\n")
      
      # --- ACM ---
      cat("🔍 ANALYSE DES CORRESPONDANCES MULTIPLES (ACM)\n")
      if (self$acm_fitted) {
        cat("   • Statut: ✓ Ajustée\n")
        cat("   • Dimensions:", self$acm_params$ncp, "\n")
        cat("   • Méthode:", self$acm_params$method, "\n")
        
        # Variance expliquée
        eig <- self$acm_result$eig
        cat("   • Variance expliquée (3 premières dimensions):\n")
        for (i in 1:min(3, nrow(eig))) {
          cat(sprintf("     - Dim %d: %.2f%%\n", i, eig[i, 2]))
        }
      } else {
        cat("   • Statut: ✗ Non ajustée\n")
      }
      cat("\n")
      
      # --- Clustering ---
      cat("🌳 CLUSTERING HIÉRARCHIQUE DE VARIABLES\n")
      if (self$clustering_fitted) {
        cat("   • Statut: ✓ Ajusté\n")
        cat("   • Méthode:", self$method, "\n")
        cat("   • Variables analysées:", length(self$variables), "\n")
        
        if (!is.null(self$n_clusters)) {
          cat("   • Nombre de clusters:", self$n_clusters, "\n")
          cat("   • Composition des clusters:\n")
          
          for (i in 1:self$n_clusters) {
            vars_in_cluster <- names(self$clusters)[self$clusters == i]
            cat(sprintf("     - Cluster %d (%d var): %s\n", 
                        i, 
                        length(vars_in_cluster),
                        paste(vars_in_cluster, collapse = ", ")))
          }
          
          # Hauteur de fusion
          cat(sprintf("   • Hauteur dernière fusion: %.4f\n", 
                      tail(self$hclust_result$height, 1)))
        } else {
          cat("   • Nombre de clusters: non spécifié\n")
        }
      } else {
        cat("   • Statut: ✗ Non ajusté\n")
      }
      
      cat("\n")
      cat("💡 Utilisez fit() pour ajuster le modèle\n")
      cat("💡 Utilisez predict() pour faire des prédictions\n")
      
      invisible(self)
    },
    
    
    # ========================================================================
    # MÉTHODES ACM - Accesseurs
    # ========================================================================
    
    get_eigenvalues = function() {
      if (!self$acm_fitted) stop("ACM non ajustée. Utilisez fit()")
      return(self$acm_result$eig)
    },
    
    get_var_coords = function(axes = c(1, 2)) {
      if (!self$acm_fitted) stop("ACM non ajustée. Utilisez fit()")
      return(self$acm_result$var$coord[, axes, drop = FALSE])
    },
    
    get_var_contrib = function(axes = c(1, 2)) {
      if (!self$acm_fitted) stop("ACM non ajustée. Utilisez fit()")
      return(self$acm_result$var$contrib[, axes, drop = FALSE])
    },
    
    get_var_cos2 = function(axes = c(1, 2)) {
      if (!self$acm_fitted) stop("ACM non ajustée. Utilisez fit()")
      return(self$acm_result$var$cos2[, axes, drop = FALSE])
    },
    
    get_ind_coords = function(axes = c(1, 2)) {
      if (!self$acm_fitted) stop("ACM non ajustée. Utilisez fit()")
      return(self$acm_result$ind$coord[, axes, drop = FALSE])
    },
    
    
    # ========================================================================
    # MÉTHODES CLUSTERING - Accesseurs
    # ========================================================================
    
    get_similarity_matrix = function() {
      if (!self$clustering_fitted) {
        stop("Clustering non ajusté. Utilisez fit()")
      }
      return(self$similarity_matrix)
    },
    
    get_clusters = function() {
      if (!self$clustering_fitted || is.null(self$clusters)) {
        stop("Clustering non ajusté ou k non spécifié. Utilisez fit(k = ...)")
      }
      return(self$clusters)
    },
    
    
    # ========================================================================
    # MÉTHODES DE VISUALISATION
    # ========================================================================
    
    plot_acm = function(choix = "ind", axes = c(1, 2), ...) {
      if (!self$acm_fitted) stop("ACM non ajustée. Utilisez fit()")
      plot.MCA(self$acm_result, choix = choix, axes = axes, ...)
    },
    
    plot_dendrogram = function(k = NULL) {
      if (!self$clustering_fitted) {
        stop("Clustering non ajusté. Utilisez fit()")
      }
      
      k_to_use <- if (is.null(k)) self$n_clusters else k
      
      plot(self$hclust_result, 
           main = "Dendrogramme - Clustering de variables",
           xlab = "Variables", 
           ylab = "Distance (1 - V de Cramer)",
           sub = paste("Méthode:", self$method))
      
      if (!is.null(k_to_use)) {
        rect.hclust(self$hclust_result, k = k_to_use, border = 2:6)
      }
    },
    
    plot_scree = function() {
      if (!self$acm_fitted) stop("ACM non ajustée. Utilisez fit()")
      
      eig <- self$acm_result$eig
      barplot(eig[, 2], 
              names.arg = 1:nrow(eig),
              main = "Scree plot - Variance expliquée",
              xlab = "Dimensions",
              ylab = "Pourcentage de variance (%)",
              col = "steelblue")
      lines(x = 1:nrow(eig), y = eig[, 2], type = "b", col = "red", pch = 19)
    },
    
    
    # ========================================================================
    # MÉTHODE SUMMARY - Résumé détaillé
    # ========================================================================
    
    summary = function() {
      cat("\n")
      cat("═══════════════════════════════════════════════════════════════\n")
      cat("                RÉSUMÉ DU MODÈLE ClusteringACM                 \n")
      cat("═══════════════════════════════════════════════════════════════\n\n")
      
      # ACM
      if (self$acm_fitted) {
        cat("--- ANALYSE DES CORRESPONDANCES MULTIPLES ---\n\n")
        cat("Dimensions:", self$acm_params$ncp, "\n")
        cat("Méthode:", self$acm_params$method, "\n\n")
        
        cat("Valeurs propres (variance expliquée):\n")
        eig <- self$acm_result$eig
        print(head(eig, 10))
        cat("\n")
      }
      
      # Clustering
      if (self$clustering_fitted) {
        cat("--- CLUSTERING HIÉRARCHIQUE ---\n\n")
        cat("Méthode:", self$method, "\n")
        cat("Variables:", length(self$variables), "\n")
        
        if (!is.null(self$n_clusters)) {
          cat("Clusters:", self$n_clusters, "\n\n")
          
          cat("Composition détaillée:\n")
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
  # MÉTHODES PRIVÉES
  # ==========================================================================
  private = list(
    
    # V de Cramer
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
    
    
    # Prédiction pour nouveaux individus (projection ACM)
    predict_individuals = function(new_data) {
      if (!self$acm_fitted) {
        stop("ACM non ajustée. Utilisez fit() d'abord")
      }
      
      if (is.null(new_data)) {
        stop("new_data ne peut pas être NULL pour prédire des individus")
      }
      
      # Vérifier que les variables sont les mêmes
      if (!all(names(new_data) %in% names(self$data))) {
        stop("new_data doit contenir les mêmes variables que les données d'entraînement")
      }
      
      # Conversion en facteurs
      for (var in names(new_data)) {
        if (!is.factor(new_data[[var]])) {
          new_data[[var]] <- as.factor(new_data[[var]])
        }
      }
      
      # Projection des nouveaux individus sur l'ACM
      # (Utilisation des coordonnées des modalités)
      coords <- self$acm_result$var$coord
      
      message("Projection de ", nrow(new_data), " nouveaux individus")
      
      # Retourner les coordonnées (simplifié - une vraie projection serait plus complexe)
      return(list(
        message = "Projection ACM non implémentée complètement",
        method = "Utilisez la fonction predict.MCA de FactoMineR pour une vraie projection"
      ))
    },
    
    
    # Prédiction pour nouvelles variables (clustering)
    predict_variables = function(new_data, new_variables = NULL) {
      if (!self$clustering_fitted) {
        stop("Clustering non ajusté. Utilisez fit() d'abord")
      }
      
      if (is.null(self$clusters)) {
        stop("Le nombre de clusters doit être spécifié dans fit() pour utiliser predict()")
      }
      
      if (is.null(new_data)) {
        stop("new_data ne peut pas être NULL pour prédire des variables")
      }
      
      # Variables à prédire
      if (is.null(new_variables)) {
        new_variables <- names(new_data)
      }
      
      # Conversion en facteurs
      for (var in new_variables) {
        if (!is.factor(new_data[[var]])) {
          new_data[[var]] <- as.factor(new_data[[var]])
        }
      }
      
      # Calcul des centres de clusters
      cluster_centers <- private$compute_cluster_centers()
      
      # Pour chaque nouvelle variable, trouver le cluster le plus proche
      predictions <- sapply(new_variables, function(new_var) {
        similarities <- sapply(names(cluster_centers), function(center_var) {
          private$cramer(new_data[[new_var]], self$data[[center_var]])
        })
        cluster_centers[which.max(similarities)]
      })
      
      message("Prédiction effectuée pour ", length(new_variables), " variable(s)")
      return(predictions)
    },
    
    
    # Calcul des centres de clusters
    compute_cluster_centers = function() {
      centers <- sapply(1:self$n_clusters, function(cluster_id) {
        vars_in_cluster <- names(self$clusters)[self$clusters == cluster_id]
        
        if (length(vars_in_cluster) == 1) {
          return(vars_in_cluster)
        }
        
        # Variable la plus centrale (plus similaire aux autres)
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


# ============================================================================
# EXEMPLE D'UTILISATION
# ============================================================================

# # 1. Chargement des données
canines <- read_xls("D:/1_Prog_R/PROJET_CLUSTERING/races_canines_acm.xls",col_names=TRUE, col_types="guess", sheet=1)
# 
# # 2. Initialisation du modèle
model <- ClusteringACM$new(canines, ncp = 5, graph = FALSE)
# 
# # 3. Affichage avant fit
model$print()
# 
# # 4. Ajustement du modèle complet (ACM + Clustering)
model$fit(do_acm = TRUE, 
           do_clustering = TRUE, 
           clustering_method = "ward.D2", 
           k = 4)
# 
# # 5. Affichage après fit
model$print()
# 
# # 6. Résumé détaillé
model$summary()
# 
# # 7. Visualisations
model$plot_acm(choix = "var", axes = c(1, 2))
model$plot_dendrogram()
model$plot_scree()
# 
# # 8. Accès aux résultats
# eigenvalues <- model$get_eigenvalues()
# clusters <- model$get_clusters()
# var_coords <- model$get_var_coords()
# 
# # 9. Prédiction pour nouvelles variables
# # new_data <- data.frame(...)
# # predictions <- model$predict(new_data, type = "variables")

