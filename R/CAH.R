library (R6)

CAH <- R6Class(
  "CAH",
  # membres publics
  public = list (
    data = NULL, # Donnée brute
    X_last = NULL, # Sauvegarde du dernier X
    method = NULL, # Plus tard methode de ward.D2
    dist_method = NULL, #methode euclidean
    acp_true = NULL, # Booléen : ACP utilisé
    acp = NULL, #Acp utilisé
    q = NULL, # Nombre axes retenu
    corr_moy = NULL, #Correlation moyenne entre les variables
    dist_matrix = NULL, # Matrice de distance
    hc = NULL, # Object hclust
    R2_tableau = NULL, #Tbleau de R2
    best_k = NULL, # k optimal
    clusters = NULL, # Partition finale
    predict_result = NULL,
    nouv_cor = NULL,
    compo_latent = NULL,

    #Champs

# ---- Constructeur (je dois vérifier si : 1. Le type des données, 2. type des variable données, 3. Constantes, 4. Donnée manquante )-----

initialize = function(data) {
    if (!is.data.frame(data) && !is.matrix(data)){
      stop ("CAH : Il n'est pas possible de faire la CAH, les données doivent être sous forme d une dataframe ou une matrice.")
    }
  df <- as.data.frame(data)
# "identifier des groupes d'observation ayant des caractéristiques similaires" (donc au moins 2 individues et 2 variables)
    if (nrow(df) < 2 || ncol(df) < 2) {
      stop("CAH: Pas assez de données : Pour faire une CAH il est nécessaire d'avoir au moins 2 individues et 2 variables.")
    }

# === CAH = 1. mesure de distance. Donc vérification du type de variables (variable numérique)
  type <- sapply (df, class)
  #Variable quantivative (numérique)
  quanti_var <- names(type[type %in% c("numeric", "integer")])
  #Variable qualitative (strings...)
  quali_var <- names(type[type %in% c("factor","character", "logical")])

    if (length(quali_var) > 0) {
      warning(" CAH : Il y a des variables qualitatives dans la dataframe :", paste(quali_var, collapse =", "), ". Elle ne seront pas utilisées pour la CAH.")
    }
  #On garde que les variables quantitatives
  df <- df[, quanti_var, drop = FALSE]


# === Supression des constantes (exemple si sexe(0,1) si que des 1 alors inutile dans la Df)
    if (ncol(df) > 0) {
        variance <- sapply(df, function(x) var(x, na.rm = TRUE))
        var_zero <- names(variance[variance == 0])
      if (length(var_zero) > 0) {
        warning("CAH : On supprime des variables constantes : ", paste(var_zero, collapse = ", "))
  df <- df[, !(names(df) %in% var_zero), drop = FALSE]
      }
    }
# === Vérification si il y a assez de variables disponible pour faire la CAH
  if (ncol(df) == 0){
    stop("CAH : Il n'y a pas de variable quantitative valide disponible pour l'analyse.")
  }

# === Suppréssion des NA
  if (anyNA(df)) {
    warning("CAH : Suppression des lignes comportant des données manquantes")
    df <- na.omit(df)
  }

# === Vérification si il y a assez d'individues disponible
  if (nrow(df) == 0){
    stop("CAH : Il n'y a pas assez d'individue disponible pour l'analyse.")
  }

  self$data <- df
  message("Les données sont chargées ! il y a :", nrow(self$data)," individus et ", ncol(self$data), " variables.")
},

#Calcul du tableau des distances entre individus
# Chaque individu constitue un groupe (classe)
#(Définir une mesure de distance entre individus)
#L’ACP nettoie les données et rend la distance euclidienne plus pertinente.

# ---------- FIT -----------
# 1. Transformer les variables (centrée, réduites), 3. Corrélation et ACP si besion, 4. Matrice des distances, 5. Détection du coude pour trouver k (utilisateur)

fit = function(method = "ward.D2"){

  self$method <- method

  corr_matrix <- cor(self$data)
  self$corr_moy <- mean(abs(corr_matrix[upper.tri(corr_matrix)])) # Voir si je peux le placer dans le summary ou print car inutile dans calcule.
  #message(" 2. Matrice de corrélation calculée ( corrélation moyenne = ", round(self$corr_moy, 3), ").")

#Etape 3 - Distance basée sur la corrélation (entre chaque variable)
  self$dist_matrix <- as.dist(sqrt(2 * (1 - abs(corr_matrix))))
  #message("Matrice de distance créée entre variables (corrélationnelle).")


# Etape 4 - Construction de la CAH
  self$hc <- hclust(self$dist_matrix, method = method)
  #message("CAH est effectuée avec la méthode = ", method, ".")

  # Etape 5 - Detection du coude pour trouver k (nombre optimal de classe avec la méthode du coude)

  h <- self$hc$height
  d1 <- diff(h)
  d2 <- diff(d1)
  idx <- which.min(d2) + 1
  self$best_k <- idx
  #message("Le nombre idéal de classes détécté est : k = ", self$best_k)

  invisible(self)
},

#Predict CAH classique ( Voir Cours)
predict = function(k= NULL, X= NULL) {
  if (is.null(self$hc))
    stop("Merci de bien vouloir passer par le $fit() avant.")

  #Si on a pas une nouvelle variable X.
  if (is.null(X)) {
    if (is.null(k)){
      k <- self$best_k
    }


  cl <- cutree(self$hc, k = k)
  names(cl) <- colnames(self$data)
  self$clusters <- cl
  tableau <- table(cl)

  self$compo_latent <- list()

  #message("CAH : Répartition des variables par groupe :")
  #print(tableau)

  #ACP pour calculer la composante latente (interprétation)
  for (groupe in unique(cl)){
    var_groupe <- names(cl[cl == groupe])
    if (length(var_groupe) > 1) {
      acp <- prcomp(self$data[, var_groupe], center = FALSE, scale. = FALSE)
      Zk <- acp$x[, 1]
      self$compo_latent[[as.character(groupe)]] <- list(
        Zk = Zk,
        vars = var_groupe,
        cor_vals = cor(self$data[, var_groupe, drop = FALSE], Zk)
      #message("\nCluster ", groupe, ": composante latente (1er axe ACP)")
      #print(round(cor_vals^2, 3))
      )
    } else {
      self$compo_latent[[as.character(groupe)]] <- list(
        Zk = self$data[, var_groupe],
        vars = var_groupe,
        cor_vals = 1
      )
    }
  }
  }
  if (length(unique(self$clusters)) != length(self$compo_latent)) {
    stop("Incohérence entre les clusters et les composantes latentes. Exécutez $predict() sans X avant.")
  }

  X <- as.data.frame(X)

  # Etape 1 : Vérification de X
  if (nrow(X) != nrow(self$data)) {
    stop("Le nombre d'individus doit être identique à celui de notre dataframe de base.")
  }
  if (anyNA(X)) {
    stop ("Le jeu contient des valeurs manquantes (NA).")
  }
  #Correlation entre nouvelles et anciennes variable
  self$X_last <- X

  #Attribution des nouvelles variables
  nouv_clusters <- rep(NA, ncol(X))
  names(nouv_clusters) <- colnames(X)

  # Boucle sur les nouvelles variables
  for (j in seq_len(ncol(X))) {
    var_data <- X[, j]
    cor_latent <- c()

    # Boucle sur les clusters existants
    for (groupe in unique(self$clusters)) {
      Zk <- self$compo_latent[[as.character(groupe)]]$Zk
      cor_latent[as.character(groupe)] <- abs(cor(var_data, Zk))
    }

    # Rattachement : cluster avec la plus forte corrélation absolue
    best_cluster <- as.numeric(names(which.max(cor_latent)))
    nouv_clusters[j] <- best_cluster
  }

  # Enregistrement des résultats
  self$predict_result <- nouv_clusters
  self$nouv_cor <- NULL  # plus utilisée dans cette version

  message("Attribution des nouvelles variables aux clusters existants (méthode Ricco) terminée.")
  print(nouv_clusters)
},

print = function(...) {
  cat("\n──────────────────────────────────────────────\n")
  cat("   Modèle de Classification Hiérarchique (CAH)\n")
  cat("──────────────────────────────────────────────\n")

  # Vérification des données
  if (is.null(self$data)) {
    cat("⚠️  Aucun jeu de données chargé.\n")
    cat("Veuillez exécuter :  $initialize(data)\n")
    return(invisible(self))
  }

  # Informations générales sur les données
  cat(" Données : ", nrow(self$data), " individus × ", ncol(self$data), " variables\n", sep = "")
  cat(" Méthode d’agrégation :", ifelse(is.null(self$method), "non spécifiée", self$method), "\n")
  cat(" Méthode de distance :", ifelse(is.null(self$dist_method), "corrélation", self$dist_method), "\n")

  # Corrélation moyenne
  if (!is.null(self$corr_moy)) {
    cat(" Corrélation moyenne entre variables :", round(self$corr_moy, 3), "\n")
  }

  # Nombre optimal de clusters
  if (!is.null(self$best_k)) {
    cat(" Nombre optimal de clusters détecté :", self$best_k, "\n")
  } else {
    cat(" Nombre optimal de clusters : non déterminé (exécutez $fit())\n")
  }

  # Répartition des variables par cluster
  if (!is.null(self$clusters)) {
    tb <- table(self$clusters)
    cat(" Répartition des variables par cluster :\n")
    print(tb)
  } else {
    cat(" Répartition des variables : non encore effectuée (exécutez $predict())\n")
  }

  # Variables illustratives ajoutées (via predict)
  if (!is.null(self$predict_result)) {
    cat(" Variables illustratives ajoutées :", length(self$predict_result), "\n")
  }

  cat("──────────────────────────────────────────────\n\n")
  invisible(self)
},


summary = function(...) {
  cat("\n──────────────────────────────────────────────\n")
  cat("   Résumé détaillé du modèle CAH : \n")
  cat("──────────────────────────────────────────────\n")
  # Vérification que la CAH a été effectuée
  if (is.null(self$hc)) {
    cat("Aucun modèle ajusté. Veuillez exécuter $fit() d’abord.")
    return(invisible(NULL))
  }

  # Informations générales
  cat("- Méthode d’agrégation :", self$method, "\n")
  cat("- Corrélation moyenne :", round(self$corr_moy, 3), "\n")
  cat("- Nombre optimal de clusters :", self$best_k, "\n\n")

  # Répartition des clusters de base
  cat("Répartition des variables actives par groupe :\n")
  print(table(self$clusters))

  # Liste des variables par cluster
  cat("\n Variables par cluster :\n")
  for (grp in unique(self$clusters)) {
    vars <- names(self$clusters[self$clusters == grp])
    cat("  -> Cluster", grp, "(", length(vars), "variables) :", paste(vars, collapse = ", "), "\n")
  }

  # ACP locale sur chaque cluster (analyse d'interprétation)
  cat("\n Analyse factorielle (ACP interne à chaque cluster) :\n")
    if (!is.null(self$compo_latent)) {
      for (grp in names(self$compo_latent)) {
        compo <- self$compo_latent[[grp]]

      cat("\nCluster", grp, ":\n")
      if (length(compo$vars) > 1 && !is.null(compo$cor_vals)) {
        cor_vec <- as.numeric(compo$cor_vals)
        names(cor_vec) <- compo$vars
        print(round(cor_vec^2, 3))
        best_var <- names(which.max(cor_vec^2))
        cat("-> Variable parangon :", best_var, "\n")
      } else {
        cat("Une seule variable :", compo$vars, "(pas d’ACP nécessaire)\n")
      }
    }
  } else {
    cat("Aucune composante latente enregistrée (exécutez $predict() sans X avant).\n")
  }

  # Informations sur les variables illustratives (issues de predict)
  if (!is.null(self$predict_result)) {
    cat("\n Variables illustratives ajoutées (résultat du predict) :\n")
    print(self$predict_result)

    # Corrélations des nouvelles variables avec les composantes latentes
    if (is.null(self$X_last)) {
      cat("\n Impossible d'afficher les corrélations des variables illustratives (X non conservé dans l'objet).\n")
    } else {
      cat("\n Corrélations |r(X_j, Z_k)| des nouvelles variables avec chaque cluster :\n")

      nouv_vars <- names(self$predict_result)
      for (v in nouv_vars) {
        var_data <- self$X_last[, v]
        cor_latent <- c()

        for (grp in unique(self$clusters)) {
          vars_grp <- names(self$clusters[self$clusters == grp])

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

    # Nouvelle répartition complète (avec variables illustratives)
    cat("\n Variables par cluster (avec les variables illustratives ajoutées) :\n")
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































