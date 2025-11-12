#' @include mon_kmeans.R
NULL

library(R6)
library(cluster)
library(factoextra)
library(klaR)
library(proxy)
library(ggplot2)
library(pheatmap)
library(FactoMineR)

clusterVariable <- R6::R6Class(
  "clusterVariable",
  public = list(
    k = NULL,
    data = NULL,
    resultat_cluster = NULL,

    #======================== Constructeur ================================
    initialize = function(k = 3, data = NULL, donnee_nettoyee = FALSE ) {
      self$k <- k
      self$resultat_cluster <- NULL

      if (!is.null(data)) {
        if (!is.data.frame(data)) stop("Erreur : 'data' doit être un data.frame")
        self$data <- data

        if (donnee_nettoyee == TRUE) {
          self$data <- private$appliquer_nettoyage(self$data)
        }
      }
    },

    #======================== Fit ========================================
    fit = function() {
      stopifnot(!is.null(self$data))

      # Clustering sur des variables quanti (kmeans)
      if((is.numeric(as.matrix(self$data)))) {
        set.seed(123)
        self$resultat_cluster <- mon_kmeans((self$data), k = self$k)
      } else {
        stop("données non numériques ")
      }
    },

    #======================== Predict =====================================
    predict = function(nouveau_data) {
      if (is.null(self$resultat_cluster)) stop("Clustering non réalisé. Faites fit() d'abord.")

      # Vérification du type
      if (!is.data.frame(nouveau_data) && !is.matrix(nouveau_data)) {
        stop("Erreur : 'nouveau_data' doit être un data.frame ou une matrice")
      }

      # Vérifier que les colonnes correspondent
      if (!all(colnames(nouveau_data) %in% colnames(self$data))) {
        stop("Les colonnes ne sont pas les mêmes que les données d'entraînement")
      }
      data_t <- t(nouveau_data)
      centres <- self$resultat_cluster$centers

      # Calcul des distances
      distances <- matrix(NA, nrow = nrow(centres), ncol = nrow(data_t))
      for (i in 1:nrow(centres)) {
        distances[i, ] <- rowSums((data_t - matrix(centres[i, ], nrow = nrow(data_t), ncol = ncol(data_t), byrow = TRUE))^2)
      }
      clusters_assigne <- apply(distances, 2, which.min)

      # Création de l'objet R6 pour le résultat
      pred_obj <- clusterVariable$new(
        k = self$k,
        data = nouveau_data
      )
      pred_obj$resultat_cluster <- list(
        cluster = clusters_assigne,
        centers = self$resultat_cluster$centers
      )

      return(pred_obj)
    },

    #======================== Summary =====================================
    summary = function() {
      if (is.null(self$resultat_cluster)) stop("Clustering non réalisé. Faites fit() d'abord.")
      cat("Méthode :", "kmeans", "\n")
      cat("Nombre de clusters :", self$k, "\n")
      cat("Nombre de variables par cluster :\n")

      for (i in 1:self$k) {
        cat("\nCluster", i, ":", sum(self$resultat_cluster$cluster == i))
      }

      cat("\nVariables par cluster :\n")
      for (i in 1:self$k) {
        cat("\nCluster", i, ": ")
        vars_in_cluster <- names(self$data)[which(self$resultat_cluster$cluster == i)]
        cat(vars_in_cluster, sep = ",")
      }
      cat("\n")
    },

    ##===================== heatmap des clusters =========================
    heatmap_clusters = function() {
      if (is.null(self$resultat_cluster)) {
        stop("Clustering non réalisé. Faites fit() d'abord.")
      }

      centers <- self$resultat_cluster$centers
      rownames(centers) <- paste0("Cluster_", 1:nrow(centers))

      # Affichage de la heatmap
      pheatmap::pheatmap(
        centers,
        main = "Heatmap des centres des clusters",
        cluster_rows = TRUE,
        cluster_cols = TRUE,
        fontsize_row = 10,
        fontsize_col = 10,
        angle_col = 45
      )
    },

    #======================== Visualiser clusters ========================
    visualiser_clusters = function() {
      if (is.null(self$resultat_cluster)) stop("Clustering non réalisé. Faites fit() d'abord.")
      p <- factoextra::fviz_cluster(
        object = list(
          centers = self$resultat_cluster$centers,
          cluster = self$resultat_cluster$cluster,
          data = t(self$data)
        ),
        data = as.data.frame(self$data_scaled),
        ellipse.type = "convex",
        palette = "jco",
        repel = TRUE,
        geom = "point",
        ggtheme = ggplot2::theme_minimal()
      )
      return(p)
    },

    #======================== tracer la coude ========================
    tracer_coude = function(k_max = 5) {
      if (is.null(self$data)) stop("Les données n'ont pas été définies")
      if (k_max > nrow(t(self$data))) {
        stop("Le nombre de clusters 'k' ne peut pas être supérieur au nombre de variables.")
      }
      inertie <- numeric(k_max)  # vecteur pour stocker l’inertie

      for (k in 1:k_max) {
        set.seed(123)
        # utiliser la version pour variables
        kmeans_result <- mon_kmeans(self$data, k = k)

        centres <- kmeans_result$centers
        clusters <- kmeans_result$cluster

        # Calcul de l'inertie intra-cluster
        sum_sq <- 0
        data_t <- t(self$data)

        for (i in 1:k) {
          points_cluster <- data_t[clusters == i, , drop = FALSE]
          if (nrow(points_cluster) > 0) {
            sum_sq <- sum_sq + sum(rowSums((points_cluster - matrix(centres[i, ], nrow = nrow(points_cluster), ncol = ncol(points_cluster), byrow = TRUE))^2))
          }
        }
        inertie[k] <- sum_sq
      }

      # Tracer la courbe du coude
      plot(1:k_max, inertie, type = "b",
           xlab = "Nombre de clusters (k)",
           ylab = "Inertie intra-cluster",
           main = "Méthode du coude"
      )
    },

    #======================== resume des clusters  =================
    resume_cluster = function() {
      if (is.null(self$resultat_cluster)) stop("Clustering non réalisé. Faites fit() d'abord.")

      # --- Nature des partitions (Cluster de variables) ---
      partitions <- data.frame(
        Cluster = integer(self$k),
        Nombre_de_variables = integer(self$k),
        Pourcentage = numeric(self$k)
      )

      for (i in 1:self$k) {
        partitions$Cluster[i] <- i
        partitions$Nombre_de_variables[i] <- sum(self$resultat_cluster$cluster == i)
        partitions$Pourcentage[i] <- round(partitions$Nombre_de_variables[i] / length(self$resultat_cluster$cluster) * 100, 2)
      }

      # --- Nature des groupes (Statistiques descriptives pour les variables par cluster) ---
      groupes <- lapply(1:self$k, function(i) {
        # Sélectionner les variables appartenant à chaque cluster
        vars_in_cluster <- names(self$data)[self$resultat_cluster$cluster == i]
        summary(self$data[, vars_in_cluster, drop = FALSE])
      })

      # --- Distances intra-cluster ---
      distances_intra <- sapply(1:self$k, function(i) {
        # Variables dans chaque cluster
        cluster_vars <- self$data[, self$resultat_cluster$cluster == i]
        dist_matrix <- dist(t(cluster_vars))
        mean(dist_matrix)
      })

      # --- Distances inter-cluster entre les centres des clusters ---
      centers <- self$resultat_cluster$centers
      dist_inter_cluster <- dist(centers)

      # --- Création des résultats pour l'affichage ---
      return(list(
        partitions = partitions,
        groupes = groupes,
        dist_to_centers = dist_to_centers,
        distances_intra = distances_intra,
        dist_inter_cluster = dist_inter_cluster
      ))
    },


    # ========== indice pour la qualité du clustering =====================
    indice_silhoute = function() {
      if (is.null(self$resultat_cluster)) stop("Clustering non réalisé. Faites fit() d'abord.")

      data_t <- as.data.frame(t(self$data))
      dist_matrix <- dist(data_t)

      # Vérification et extraction du vecteur de clusters
      clusters <- self$resultat_cluster$cluster

      # Calcul du silhouette
      sil <- silhouette(clusters, dist_matrix)

      # Retourner la moyenne de la largeur des silhouettes
      return(mean(sil[, "sil_width"]))
    },

    #======================== Affichage ===============================
    print = function() {
      cat("Clustering de variables avec la méthode :", self$method_algo, "\n")
      cat("Nombre de clusters :", self$k, "\n")
      cat("Nombre de variables :", ncol(self$data), "\n")
    }
  ),

  #======================== Méthodes privées de nettoyage  ============================
  private = list(
    appliquer_nettoyage = function(data) {
      data <- private$gestion_valeurs_manquantes(data)
      data <- private$gestion_valeurs_aberrantes(data)
      data <- private$normalisation_data(data)
      return(data)
    },

    gestion_valeurs_manquantes = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]][is.na(data[[i]])] <- median(data[[i]], na.rm = TRUE)
        } else {
          data[[i]][is.na(data[[i]])] <- "manquant"
        }
      }
      return(data)
    },

    gestion_valeurs_aberrantes = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          Q1 <- quantile(data[[i]], 0.25, na.rm = TRUE)
          Q3 <- quantile(data[[i]], 0.75, na.rm = TRUE)
          iqr <- Q3 - Q1
          borne_inf <- Q1 - 1.5 * iqr
          borne_sup <- Q3 + 1.5 * iqr
          data[[i]][data[[i]] < borne_inf] <- borne_inf
          data[[i]][data[[i]] > borne_sup] <- borne_sup
        }
      }
      return(data)
    },

    normalisation_data = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]] <- (data[[i]] - mean(data[[i]], na.rm = TRUE)) / sd(data[[i]], na.rm = TRUE)
        }
      }
      return(data)
    }
  )
)
