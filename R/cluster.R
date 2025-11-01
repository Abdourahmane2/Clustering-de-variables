# Objectif : Classe R6 pour algorithme de clustering de variables
library(R6)

clusterVariable <- R6::R6Class(
  "clusterVariable",
  public = list(
    k = NULL,
    data = NULL,
    method_algo = NULL,
    resultat_cluster = NULL,

    # Constructeur
    initialize = function(k = 3, data = NULL, method_algo = "correlation", donnee_nettoyee = TRUE ) {
      self$k <- k
      self$method_algo <- method_algo
      self$resultat_cluster <- NULL

      # Vérification
      if (!is.null(data)) {
        if (!is.data.frame(data)) {
          stop("Erreur : 'data' doit être un data.frame")
        }
        self$data <- data

        # Nettoyage si demandé
        if (donnee_nettoyee) {
          self$data <- self$appliquer_nettoyage(self$data)
        }
      }
    }
    ,

    # Appliquer le nettoyage sur les données
    appliquer_nettoyage = function(data) {
      data <- self$gestion_valeurs_manquantes(data)
      data <- self$gestion_valeurs_aberrantes(data)
      data <- self$normalisation_data(data)
      self$data <- data
      return(self$data)
    },

    # Gestion des valeurs manquantes
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

    # Gestion des valeurs aberrantes
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

    # Normalisation des données
    normalisation_data = function(data) {
      for (i in 1:ncol(data)) {
        if (is.numeric(data[[i]])) {
          data[[i]] <- (data[[i]] - mean(data[[i]], na.rm = TRUE)) / sd(data[[i]], na.rm = TRUE)
        }
      }
      return(data)
    },


    #========================fonction de fit================================
    fit = function() {
      stopifnot(!is.null(self$data) )
      #Méthode de réallocation
      if((self$method_algo == "kmeans") & (is.numeric(as.matrix(self$data))) ){
        set.seed(123)  # Pour la reproductibilité
        self$resultat_cluster <- kmeans(t(self$data), centers = self$k)

      } else {
        stop("Methode  non reconnue")
      }
    } ,

    #========================fonction de predict================================
    predict =  function(new_data) {
      #stoper si il y'a pas de cluster
      if (is.null(self$resultat_cluster)) {
        stop("Clustering non realisé. Faites fit() d'abord.")
      }
      if(self$method_algo == "kmeans"){
        centre <- self$resultat_cluster$centers    #moyenne de chaue cluster
        distances <- as.matrix(dist(rbind(centers, new_data)))[-(1:nrow(centers)), 1:nrow(centers)]  #distance entre         les nouvelles données et les centres
        cluster_assigne <- apply(distances, 1, which.min) #assigner chaque nouvelle donnée au cluster le plus proche
        return(cluster_assigne)
      } else {
        stop("Methode  non reconnue")
      }

    } ,



    #======================== fonction de summary================================
    summary = function() {
      if (is.null(self$resultat_cluster)) {
        stop("Clustering non realisé. Faites fit() d'abord.")
      }
      if(self$method_algo == "kmeans"){
        cat("methode" , self$method_algo , "\n")
        cat("nombre de clusters :" , self$k , "\n")
        cat("nombre de variable par cluster :")
        for(i in 1:self$k){
          cat("\n Cluster" , i , ":" , sum(self$resultat_cluster$cluster == i))
        }
        cat("\n les variables par cluster :\n")
        for(i in 1:self$k){
          cat("\n Cluster" , i , ":")
          vars_in_cluster <- names(self$data)[which(self$resultat_cluster$cluster == i)]
          cat(vars_in_cluster, sep = ", ")
        }
      } else {
        stop("Methode  non reconnue")
      }

    } ,

    #======================== visualiser des cluster ================================
    visualiser_clusters = function() {
      if (is.null(self$resultat_cluster)) {
        stop("Clustering non réalisé. Faites fit() d'abord.")}
      if (self$method_algo == "kmeans") {
        library(factoextra)
        library(ggplot2)
        #plot des clusters
        fviz_cluster(self$resultat_cluster, data = t(self$data),

                     main = "Visualisation des clusters de variables")
       } else {
          cat("Méthode de clustering non supportée pour la visualisation.")
        }
      }   ,




      #======================== Affichage de l'objet================================
      print = function() {
        cat("Clustering de variables avec k =", self$k, "et la méthode =", self$method_algo, "\n")
        if (!is.null(self$data)) {

        }
      }

  )
)






#==================== exemple d'utilisation ==============================
# Exemple de dataframe avec 6 variables numériques
set.seed(123)
data_numeric <- data.frame(
  var1 = rnorm(10, mean = 5, sd = 2),
  var2 = rnorm(10, mean = 10, sd = 3),
  var3 = rnorm(10, mean = 5, sd = 1),
  var4 = rnorm(10, mean = 7, sd = 2),
  var5 = rnorm(10, mean = 10, sd = 3),
  var6 = rnorm(10, mean = 6, sd = 1)
)

cluster_model <- clusterVariable$new(k = 3, data = data_numeric, method_algo = "kmeans", donnee_nettoyee = FALSE)

cluster_model$fit()

cluster_model$summary()

cluster_model$visualiser_clusters()

