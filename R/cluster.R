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
      self$resultat_cluster <- resultat_cluster

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
       stopifnot(!is.null(self$data))
       if(self$method_algo == "correlation"){
         self
       } else {
         stop("Methode  non reconnue")
       }
    } ,

#========================fonction de predict================================
    predict =  function(new_data) {
      #stoper si il y'a pas de cluster
      stopifnot(!is.null(self$resultat_cluster) )
      if(self$method_algo == "correlation"){
        predictions = data.frame(c(1 , 2,3))
      } else {
        stop("Methode  non reconnue")
      }

    } ,



#======================== fonction de summary================================
summary = fonction() {
  cat("Résumé du clustering de variables :\n")
}


    # Affichage de l'objet
    print = function() {
      cat("Clustering de variables avec k =", self$k, "et la méthode =", self$method_algo, "\n")
      if (!is.null(self$data)) {
        cat("Nombre de variables :", ncol(self$data), "\n")
      }
    }

  )
)






# #Exemple d’appel de la classe
# data1 = c(1 ,2,3,4,5,NA,7,8,9,10)
# data1 = as.data.frame(data1)
#
#  cluster_var <- clusterVariable$new(k = 4, data = data1, method_algo = "correlation", donnee_nettoyee = TRUE)
# cluster_var$print()
