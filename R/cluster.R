library(R6)
library(cluster)
library(factoextra)
library(klaR)
library(proxy)


clusterVariable <- R6::R6Class(
  "clusterVariable",
  public = list(
    k = NULL,
    data = NULL,
    method_algo = NULL,
    resultat_cluster = NULL,



    #======================== Constructeur ================================
    initialize = function(k = 3, data = NULL, method_algo = "Kmeans", donnee_nettoyee = FALSE ) {
      self$k <- k
      self$method_algo <- method_algo
      self$resultat_cluster <- NULL

      if (!is.null(data)) {
        if (!is.data.frame(data)) stop("Erreur : 'data' doit être un data.frame")
        self$data <- data

        if (donnee_nettoyee == TRUE) {
          self$data <- private$appliquer_nettoyage(self$data)
        }
      }
      if(missing(method_algo)) {
        stop("La methode de clustering doit être  renseigne")
      }
    },

    #======================== Fit ========================================
    fit = function() {
      stopifnot(!is.null(self$data))

      # Clustering sur des variables quanti (kmeans)
      if((self$method_algo == "kmeans") & (is.numeric(as.matrix(self$data)))) {
        set.seed(123)
        self$resultat_cluster <- mon_kmeans((self$data), k = self$k)
      }

      # Clustering des variables quali (kmodes)
      else if ((self$method_algo == "kmodes") & all(sapply(self$data, is.factor))) {
        set.seed(123)
        self$resultat_cluster <- kmodes(t(self$data), modes = self$k)

        # Calcul du silhouette score pour kmodes
        data_t <- as.data.frame(t(self$data), stringsAsFactors = TRUE)
        d <- daisy(data_t, metric = "gower")
        sil <- silhouette(self$resultat_cluster$cluster, d)
        cat("Silhouette score moyen :", mean(sil[, 3]), "\n")
      }

      # Clustering des variables mixtes (hiérarchique)
      else if (self$method_algo == "hierarchical") {
        set.seed(123)

        # Calcul de la matrice de distance (utilisation de Gower pour les données mixtes)
        data_t =  t(self$data)
        data_t_df <- as.data.frame(data_t, stringsAsFactors = TRUE)
        dist_matrix <- daisy(data_t_df, metric = "gower")
        hc <- hclust(dist_matrix, method = "ward.D2")
        self$resultat_cluster <- cutree(hc, k = self$k)
        plot(hc, main = "Dendrogramme des variables")


      }

      else {
        stop("Méthode non reconnue")
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
      if (self$method_algo == "kmeans") {
       centres <- self$resultat_cluster$centers

        # Calcul des distances
        distances <- matrix(NA, nrow = nrow(centres), ncol = nrow(data_t))
        for (i in 1:nrow(centres)) {
          distances[i, ] <- rowSums((data_t - matrix(centres[i, ], nrow = nrow(data_t), ncol = ncol(data_t), byrow = TRUE))^2)
        }
        clusters_assigne <- apply(distances, 2, which.min)

      } else if (self$method_algo == "kmodes") {
        cluster_mode <- self$resultat_cluster$modes
        distances <- as.matrix(proxy::dist(rbind(cluster_mode, nouveau_data), method = "Hamming"))[-(1:nrow(cluster_mode)), 1:nrow(cluster_mode)]
        clusters_assigne <- apply(distances, 1, which.min)

      } else if (self$method_algo == "hierarchical") {
        dist_mat <- daisy(rbind(t(self$data), t(nouveau_data)), metric = "gower")
        hc <- hclust(dist_mat, method = "ward.D2")
        clusters_assigne <- cutree(hc, k = self$k)[(ncol(self$data)+1):(ncol(self$data)+ncol(nouveau_data))]

      } else {
        stop("Méthode non reconnue")
      }

      # Création de l'objet R6 pour le résultat
      pred_obj <- clusterVariable$new(
        data = nouveau_data,
        k = self$k,
        method_algo = self$method_algo
      )
      pred_obj$resultat_cluster <- list(
        cluster = clusters_assigne,
        centers = if (self$method_algo == "kmeans") self$resultat_cluster$centers else NULL,
        modes = if (self$method_algo == "kmodes") self$resultat_cluster$modes else NULL
      )

      return(pred_obj)
    } ,





    #======================== Summary =====================================
    summary = function() {
      if (is.null(self$resultat_cluster)) stop("Clustering non réalisé. Faites fit() d'abord.")

      if (self$method_algo == "kmeans") {
        cat("Méthode :", self$method_algo, "\n")
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
      } else if (self$method_algo == "kmodes") {
        cat("Méthode :", self$method_algo, "\n")
        cat("Nombre de clusters :", self$k, "\n")
        cat("Nombre de variables par cluster :\n")

        for (i in 1:self$k) {
          cat("\nCluster", i, ":", sum(self$resultat_cluster$cluster == i))
        }

        cat("\nVariables par cluster :\n")
        for (i in 1:self$k) {
          cat("\nCluster", i, ": ")
          vars_in_cluster <- names(self$data)[which(self$resultat_cluster$cluster == i)]
          cat(vars_in_cluster, sep = ", ")
        }

        cat("\n: profil type par cluster : \n")
        print("joli affichage des profils types")
        print(self$resultat_cluster$modes)

      } else if (self$method_algo == "hierarchical") {
        cat('Méthode : ', self$method_algo, '\n')
        cat('Nombre de clusters : ', self$k, '\n')
        cat("nombre de variables par cluster :\n")
        for (i in 1:self$k) {
          cat('\nCluster', i, ':', sum(self$resultat_cluster == i))
        }
        cat('\nVariables par cluster :\n')
        for (i in 1:self$k) {
          cat('\nCluster', i, ': ')
          vars_in_cluster <- names(self$data)[which(self$resultat_cluster == i)]
          cat(vars_in_cluster, sep = ', ')
        }

      } else {
        stop("Méthode non reconnue") }

    },





    #======================== Visualiser clusters ========================
    visualiser_clusters = function() {
      if (is.null(self$resultat_cluster)) stop("Clustering non réalisé. Faites fit() d'abord.")

      if (self$method_algo == "kmeans") {
        library(factoextra)
        library(ggplot2)
        pseudo_kmeans <- list(
          cluster = self$resultat_cluster$cluster,
          centers = self$resultat_cluster$centers
        )
        D <- as.dist(1 - cor((self$data), use = "pairwise.complete.obs"))
        hc <- hclust(D, method = "ward.D2")
        K <- self$k
        factoextra::fviz_dend(hc, k = K, cex = 0.9,
                              main = "Dendrogramme des variables (distance corrélation)")
      }
      else if (self$method_algo == "kmodes") {
        library(cluster)
        library(ggplot2)
        data_t <- as.data.frame(t(self$data), stringsAsFactors = TRUE)
        d <- daisy(data_t, metric = "gower")
        sil <- silhouette(self$resultat_cluster$cluster, d)
        fviz_silhouette(sil) + ggtitle("Silhouette plot des clusters de variables")
      }

      else {
        cat("Méthode de clustering non supportée pour la visualisation.\n")
      }
    },






    #========================tracer la coude ========================
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
            sum_sq <- sum_sq + sum(rowSums((points_cluster - matrix(centres[i, ], nrow = nrow(points_cluster), ncol =             ncol(points_cluster), byrow = TRUE))^2))
          }
        }
        inertie[k] <- sum_sq
      }

      # Tracer la courbe du coude
      plot(1:k_max, inertie, type = "b",
           xlab = "Nombre de clusters (k)",
           ylab = "Inertie intra-cluster",
           main = "Méthode du coude")
    } ,







    #======================== Interprétation des clusters =================
    interprete_clusters = function() {
      if (is.null(self$resultat_cluster)) stop("Clustering non réalisé. Faites fit() d'abord.")

      if (self$method_algo == "kmeans") {
        library(cluster)
        library(ggplot2)
        library(pheatmap)
        library(FactoMineR)

        cat("\n=== Statistiques descriptives par cluster ===\n")

        # Affichage des statistiques descriptives pour chaque cluster
        for (i in 1:self$k) {
          cat("\nCluster", i, ":\n")
          vars_in_cluster <- names(self$data)[self$resultat_cluster$cluster == i]
          # Statistiques descriptives (moyennes, écarts-types, etc.)
          print(summary(self$data[, vars_in_cluster, drop = FALSE]))

          # Moyennes et écarts-types par cluster
          cat("\nMoyennes et écarts-types :\n")
          print(apply(self$data[self$resultat_cluster$cluster == i, ], 2, function(x) c(mean = mean(x), sd = sd(x))))
        }


        # Affichage des distances intra-cluster
        cat("\n=== Analyse des distances intra-cluster ===\n")
        for (i in 1:self$k) {
          # Calcul des distances intra-cluster pour chaque cluster
          cluster_data <- self$data[self$resultat_cluster$cluster == i, ]
          dist_matrix <- dist(cluster_data)
          mean_distance <- mean(dist_matrix)
          cat("\nCluster", i, " - Distance moyenne intra-cluster : ", mean_distance, "\n")
        }

        # Affichage des distances inter-cluster entre les centres des clusters
        cat("\n=== Analyse des distances inter-cluster ===\n")
        centers <- self$resultat_cluster$centers
        dist_inter_cluster <- dist(centers)
        cat("\nDistances inter-cluster entre les centres :\n")
        print(dist_inter_cluster)

      } else {
        cat("Méthode de clustering non supportée pour la visualisation.\n")
      }
    } ,







    #======================== Affichage ===============================
    print = function() {
      if(self$method_algo == "kmeans" | self$method_algo == "kmodes") {
        cat("Clustering de variables avec la méthode :", self$method_algo, "\n")
        cat("Nombre de clusters :", self$k, "\n")
        cat("Nombre de variables :", ncol(self$data), "\n")
      } else if (self$method_algo == "hierarchical") {
        cat("Clustering hiérarchique de variables\n")
        cat("Nombre de clusters :", self$k, "\n")
        cat("Nombre de variables :", ncol(self$data), "\n")
      } else {
        cat("Méthode non reconnue\n")
      }}
  ),



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

#==================== Exemple d'utilisation ==============================
set.seed(123)
data_numeric <- data.frame(
  var1 = rnorm(10, mean = 5, sd = 2),
  var2 = rnorm(10, mean = 10, sd = 3),
  var3 = rnorm(10, mean = 5, sd = 1),
  var4 = rnorm(10, mean = 7, sd = 2),
  var5 = rnorm(10, mean = 10, sd = 3),
  var6 = rnorm(10, mean = 6, sd = 1)
)
data_predict <- data.frame(
  var1 = rnorm(10, mean = 5, sd = 2),
  var2 = rnorm(10, mean = 10, sd = 3),
  var3 = rnorm(10, mean = 5, sd = 1),
  var4 = rnorm(10, mean = 7, sd = 2),
  var5 = rnorm(10, mean = 10, sd = 3),
  var6 = rnorm(10, mean = 6, sd = 1)   )


data_quali =  data.frame(
  var1 = as.factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
  var2 = as.factor(sample(c("X", "Y"), 10, replace = TRUE)),
  var3 = as.factor(sample(c("Red", "Blue", "Green"), 10, replace = TRUE)),
  var4 = as.factor(sample(c("Low", "Medium", "High"), 10, replace = TRUE))
)

donne_mixte = data.frame(
  var1 = rnorm(10, mean = 5, sd = 2),
  var2 = as.factor(sample(c("A", "B", "C"), 10, replace = TRUE)),
  var3 = rnorm(10, mean = 10, sd = 3),
  var4 = as.factor(sample(c("X", "Y"), 10, replace = TRUE))
)



# cluster_mixte <- clusterVariable$new(
#   k = 2,
#   data = donne_mixte,
#   method_algo = "hierarchical"
# )
# cluster_mixte$fit()
# cluster_mixte$summary()
# cluster_mixte$visualiser_clusters()

#=============================


# cluster_quali <- clusterVariable$new(
#   k = 2,
#   data = data_quali,
#   method_algo = "kmodes",
#   donnee_nettoyee = FALSE
# )
# cluster_quali$fit()
# cluster_quali$summary()
# cluster_quali$visualiser_clusters()

#=========================================

cluster_quanti <- clusterVariable$new(
  k = 3,
  data = data_numeric,
  method_algo = "kmeans",
  donnee_nettoyee = TRUE
)

cluster_quanti$fit()
test <- cluster_quanti$predict(data_predict)
test$summary()
test$tracer_coude()
test$visualiser_clusters()
