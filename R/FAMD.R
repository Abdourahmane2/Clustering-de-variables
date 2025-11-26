# =============================================================================
# FAMD (Factor Analysis of Mixed Data) + CLUSTERING DE VARIABLES
# Pour données MIXTES (quantitatives + qualitatives)
# =============================================================================

library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)
library(dplyr)
library(R6)
library(ggrepel)

#' CAH_mixtes: Clustering Hiérarchique de Variables sur Données Mixtes
#'
#' @description
#' Classe R6 pour effectuer une analyse factorielle de données mixtes (FAMD)
#' suivie d'un clustering hiérarchique des variables. Cette classe permet de
#' regrouper des variables quantitatives et qualitatives en clusters homogènes
#' en se basant sur leurs coordonnées dans l'espace factoriel.
#'
#' @details
#' La classe CAH_mixtes combine deux analyses principales :
#' \itemize{
#'   \item \strong{FAMD (Factor Analysis of Mixed Data)} : Analyse factorielle
#'         adaptée aux données contenant à la fois des variables quantitatives
#'         et qualitatives
#'   \item \strong{Clustering hiérarchique} : Regroupement des variables en
#'         clusters basé sur leurs coordonnées factorielles
#' }
#'
#' Le processus complet suit ces étapes :
#' \enumerate{
#'   \item Détection automatique des types de variables (quantitatives/qualitatives)
#'   \item Calcul de la FAMD pour obtenir les coordonnées factorielles des variables
#'   \item Clustering hiérarchique des variables dans l'espace factoriel
#'   \item Visualisation et évaluation de la qualité du clustering
#' }
#'
#' @field n_components Nombre de composantes principales à conserver (entier)
#' @field famd_result Résultat complet de l'analyse FAMD (objet FactoMineR)
#' @field data Données d'entraînement (data.frame)
#' @field data_type Type de données détecté : "mixte", "quali" ou "quanti"
#' @field quanti_vars Vecteur des noms des variables quantitatives
#' @field quali_vars Vecteur des noms des variables qualitatives
#' @field coord_var Matrice des coordonnées des variables dans l'espace factoriel
#' @field eigenvalues Valeurs propres de la FAMD
#' @field inertia_explained Pourcentage d'inertie expliquée par composante
#' @field labels_var Vecteur des labels de clusters pour chaque variable
#' @field hclust_result Résultat du clustering hiérarchique (objet hclust)
#' @field cluster_centers Matrice des centres de clusters
#' @field clustering_method Méthode de clustering utilisée
#' @field n_clusters Nombre de clusters créés
#'
#' @examples
#' \dontrun{
#' # Créer un objet CAH_mixtes avec 5 composantes
#' model <- CAH_mixtes$new(n_components = 5)
#'
#' # Charger des données mixtes
#' data <- data.frame(
#'   age = c(25, 30, 35, 40, 45),
#'   revenu = c(30000, 45000, 60000, 75000, 90000),
#'   categorie = factor(c("A", "B", "A", "C", "B")),
#'   niveau = factor(c("bas", "moyen", "haut", "haut", "moyen"))
#' )
#'
#' # Effectuer l'analyse FAMD
#' model$fit(data)
#'
#' # Effectuer le clustering hiérarchique
#' model$clustering_hierarchical(n_clusters = 2, method = "ward")
#'
#' # Visualiser les résultats
#' model$plot_variables(axes = c(1, 2))
#' model$dendo()
#' model$qualite_clustering()
#'
#' # Afficher le résumé
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
                        #' Initialise un nouvel objet CAH_mixtes
                        #'
                        #' @param n_components Nombre de composantes principales à conserver pour la FAMD.
                        #'   Par défaut : 5. Ce paramètre détermine la dimensionnalité de l'espace
                        #'   factoriel dans lequel le clustering sera effectué.
                        #'
                        #' @return Un nouvel objet CAH_mixtes initialisé
                        #'
                        #' @examples
                        #' \dontrun{
                        #' # Créer un modèle avec 5 composantes (par défaut)
                        #' model <- CAH_mixtes$new()
                        #'
                        #' # Créer un modèle avec 10 composantes
                        #' model <- CAH_mixtes$new(n_components = 10)
                        #' }
                        initialize = function(n_components = 5) {
                          self$n_components <- n_components
                          cat("═══════════════════════════════════════════════════\n")
                          cat("  FAMD CLUSTERING DE VARIABLES\n")
                          cat("  Pour données MIXTES (Quantitatives + Qualitatives)\n")
                          cat("═══════════════════════════════════════════════════\n\n")
                          cat(sprintf("Nombre de composantes : %d\n", n_components))
                        },

                        #' @description
                        #' Effectue l'analyse FAMD sur les données fournies
                        #'
                        #' @param df Data.frame contenant les données à analyser. Peut contenir
                        #'   des variables quantitatives (numériques) et/ou qualitatives (facteurs
                        #'   ou chaînes de caractères). La détection du type de variables est
                        #'   automatique.
                        #'
                        #' @details
                        #' Cette méthode :
                        #' \itemize{
                        #'   \item Détecte automatiquement les types de variables
                        #'   \item Convertit les variables qualitatives en facteurs
                        #'   \item Calcule la FAMD avec le nombre de composantes spécifié
                        #'   \item Extrait les coordonnées des variables dans l'espace factoriel
                        #'   \item Pour les variables qualitatives, calcule le barycentre des modalités
                        #' }
                        #'
                        #' @return L'objet CAH_mixtes mis à jour (invisible), permettant le chaînage
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new(n_components = 5)
                        #' model$fit(my_data)
                        #' }
                        fit = function(df) {
                          cat("\n========================================\n")
                          cat("ÉTAPE 1 : ANALYSE DES DONNÉES\n")
                          cat("========================================\n\n")

                          self$data <- as.data.frame(df)

                          # Détecter les types de variables
                          self$quanti_vars <- names(self$data)[sapply(self$data, is.numeric)]
                          self$quali_vars <- names(self$data)[sapply(self$data, function(x) is.factor(x) || is.character(x))]

                          # Déterminer le type de données
                          has_quanti <- length(self$quanti_vars) > 0
                          has_quali <- length(self$quali_vars) > 0

                          if (has_quanti && has_quali) {
                            self$data_type <- "mixte"
                            cat("Type de données détecté : MIXTE\n\n")
                          } else if (has_quali && !has_quanti) {
                            self$data_type <- "quali"
                            cat("Type de données détecté : QUALITATIVES uniquement\n")
                            cat("  (FAMD se comportera comme une ACM)\n\n")
                          } else {
                            stop("Aucune variable détectée !")
                          }

                          # Convertir les variables qualitatives en facteurs
                          for (var in self$quali_vars) {
                            self$data[[var]] <- as.factor(self$data[[var]])
                          }

                          cat("========================================\n")
                          cat("ÉTAPE 2 : FAMD (Factor Analysis of Mixed Data)\n")
                          cat("========================================\n\n")

                          # Effectuer FAMD
                          self$famd_result <- FAMD(
                            self$data,
                            ncp = self$n_components,
                            graph = FALSE
                          )

                          # Extraire les coordonnées des VARIABLES
                          if (self$data_type %in% c("mixte", "quali")) {
                            # Récupérer les coordonnées des variables et modalités
                            coord_quali_raw <- self$famd_result$var$coord
                            coord_quali_list <- list()

                            for (var in self$quali_vars) {
                              # Identifier les lignes correspondant aux modalités de la variable
                              # FactoMineR nomme souvent les modalités comme "variable=modalité"
                              modalities <- rownames(coord_quali_raw)[startsWith(rownames(coord_quali_raw), paste0(var, "="))]

                              # Fallback si aucun résultat, chercher "_" ou "."
                              if (length(modalities) == 0) {
                                modalities <- rownames(coord_quali_raw)[grepl(var, rownames(coord_quali_raw))]
                              }

                              if (length(modalities) > 0) {
                                # Prendre le barycentre des modalités pour avoir 1 ligne par variable
                                coord_quali_list[[var]] <- colMeans(coord_quali_raw[modalities, , drop = FALSE])
                              } else {
                                cat(sprintf("⚠️ Aucune modalité trouvée pour %s\n", var))
                              }
                            }

                            # Combiner toutes les coordonnées
                            coord_quali <- do.call(rbind, coord_quali_list)

                            if (self$data_type == "mixte") {
                              # Pour les variables quantitatives
                              coord_quanti <- self$famd_result$quanti.var$coord
                              # Combiner quanti + quali
                              self$coord_var <- rbind(coord_quanti, coord_quali)
                            } else {
                              # Pour uniquement quali
                              self$coord_var <- coord_quali
                            }

                            # Affichage pour contrôle
                            cat("✅ Coordonnées des variables préparées pour le clustering :\n")
                            print(rownames(self$coord_var))
                          } else if (self$data_type == "quanti") {
                            cat("Nous traitons uniquement des variables quantitatives\n")
                          }

                          # Extraire valeurs propres
                          self$eigenvalues <- self$famd_result$eig[, 1]
                          self$inertia_explained <- self$famd_result$eig[, 2]

                          cat(sprintf("✓ FAMD effectuée sur %d individus et %d variables\n",
                                      nrow(self$data), nrow(self$coord_var)))
                          cat(sprintf("✓ Inertie expliquée (cumulée) par %d composantes : %.2f%%\n\n",
                                      self$n_components,
                                      sum(self$inertia_explained[1:self$n_components])))

                          invisible(self)
                        },

                        #' @description
                        #' Effectue un clustering hiérarchique des variables
                        #'
                        #' @param n_clusters Nombre de clusters à créer (entier positif)
                        #' @param method Méthode de liaison pour la CAH. Options disponibles :
                        #'   \itemize{
                        #'     \item \code{"ward"} (par défaut) : Méthode de Ward (ward.D2)
                        #'     \item \code{"complete"} : Liaison complète
                        #'     \item \code{"single"} : Liaison simple
                        #'     \item \code{"average"} : Liaison moyenne
                        #'   }
                        #'
                        #' @details
                        #' Cette méthode :
                        #' \itemize{
                        #'   \item Calcule la matrice de distances euclidiennes entre les variables
                        #'     dans l'espace factoriel
                        #'   \item Effectue un clustering hiérarchique avec la méthode spécifiée
                        #'   \item Coupe l'arbre hiérarchique pour obtenir le nombre de clusters souhaité
                        #'   \item Calcule les centres de chaque cluster
                        #'   \item Affiche la distribution des variables par cluster
                        #' }
                        #'
                        #' @return Vecteur nommé des labels de clusters pour chaque variable
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #'
                        #' # Clustering avec méthode de Ward (par défaut)
                        #' labels <- model$clustering_hierarchical(n_clusters = 3)
                        #'
                        #' # Clustering avec liaison complète
                        #' labels <- model$clustering_hierarchical(n_clusters = 3, method = "complete")
                        #' }
                        clustering_hierarchical = function(n_clusters, method = "ward") {
                          if (is.null(self$coord_var)) {
                            stop("Erreur : Vous devez d'abord exécuter fit() !")
                          }

                          cat("\n========================================\n")
                          cat("ÉTAPE 3 : CLUSTERING HIÉRARCHIQUE DES VARIABLES\n")
                          cat("========================================\n\n")

                          cat("Variables utilisées pour le clustering :\n")
                          print(rownames(self$coord_var))

                          self$n_clusters <- n_clusters

                          # Calculer la matrice de distances entre VARIABLES
                          dist_matrix <- dist(self$coord_var)

                          # CAH
                          if (method == "ward") {
                            self$hclust_result <- hclust(dist_matrix, method = "ward.D2")
                          } else {
                            self$hclust_result <- hclust(dist_matrix, method = method)
                          }

                          # Couper l'arbre
                          self$labels_var <- cutree(self$hclust_result, k = n_clusters)
                          names(self$labels_var) <- rownames(self$coord_var)
                          self$clustering_method <- "hierarchical"

                          # Calculer les centres de clusters
                          self$cluster_centers <- matrix(0, nrow = n_clusters, ncol = ncol(self$coord_var))
                          for (i in 1:n_clusters) {
                            cluster_points <- self$coord_var[self$labels_var == i, , drop = FALSE]
                            self$cluster_centers[i, ] <- colMeans(cluster_points)
                          }
                          colnames(self$cluster_centers) <- colnames(self$coord_var)
                          rownames(self$cluster_centers) <- paste0("Cluster_", 1:n_clusters)

                          cat(sprintf("✓ CAH avec %d clusters (méthode : %s)\n\n", n_clusters, method))
                          cat("Distribution des variables par cluster :\n")
                          print(table(self$labels_var))

                          cat("\nVARIABLES PAR CLUSTER :\n")
                          cat("═══════════════════════════════════════\n")
                          for (i in 1:n_clusters) {
                            vars_in_cluster <- names(self$labels_var[self$labels_var == i])
                            cat(sprintf("\n📊 Cluster %d (%d variables) :\n", i, length(vars_in_cluster)))
                            for (var in vars_in_cluster) {
                              # Indiquer le type de variable
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
                        #' Prédit le cluster d'appartenance pour de nouvelles variables
                        #'
                        #' @param new_vars Data.frame contenant les nouvelles variables à classer.
                        #'   Doit avoir le même nombre de lignes (individus) que les données
                        #'   d'entraînement.
                        #'
                        #' @details
                        #' Cette méthode :
                        #' \itemize{
                        #'   \item Combine les données d'entraînement avec les nouvelles variables
                        #'   \item Effectue une nouvelle FAMD sur l'ensemble combiné
                        #'   \item Extrait les coordonnées des nouvelles variables
                        #'   \item Calcule la distance aux centres de clusters existants
                        #'   \item Assigne chaque nouvelle variable au cluster le plus proche
                        #' }
                        #'
                        #' @return Data.frame avec trois colonnes :
                        #'   \itemize{
                        #'     \item \code{Variable} : Nom de la variable
                        #'     \item \code{Type} : Type de la variable ("Quantitative" ou "Qualitative")
                        #'     \item \code{Cluster_Predit} : Numéro du cluster prédit
                        #'   }
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(train_data)
                        #' model$clustering_hierarchical(n_clusters = 3)
                        #'
                        #' # Prédire pour de nouvelles variables
                        #' new_vars <- data.frame(
                        #'   nouvelle_var1 = c(1, 2, 3, 4, 5),
                        #'   nouvelle_var2 = factor(c("A", "B", "A", "C", "B"))
                        #' )
                        #' predictions <- model$predict(new_vars)
                        #' print(predictions)
                        #' }
                        predict = function(new_vars) {
                          if (is.null(self$cluster_centers)) {
                            stop("Erreur : Vous devez d'abord exécuter clustering_hierarchical() !")
                          }

                          cat("\n========================================\n")
                          cat("PRÉDICTION DU CLUSTER POUR NOUVELLES VARIABLES\n")
                          cat("========================================\n\n")

                          # new_vars doit être un data.frame avec les NOUVELLES VARIABLES en colonnes
                          # et les mêmes individus que self$data en lignes
                          new_vars <- as.data.frame(new_vars)

                          if (nrow(new_vars) != nrow(self$data)) {
                            stop(sprintf("❌ Erreur : new_vars doit avoir %d lignes (même nombre d'individus)",
                                         nrow(self$data)))
                          }

                          n_new_vars <- ncol(new_vars)
                          cat(sprintf("📊 Nombre de nouvelles variables à prédire : %d\n", n_new_vars))
                          cat(sprintf("   Variables : %s\n\n", paste(names(new_vars), collapse = ", ")))

                          # --- 1. Combiner anciennes + nouvelles variables ---
                          data_combined <- cbind(self$data, new_vars)

                          # --- 2. Détecter le type des nouvelles variables ---
                          new_quanti <- names(new_vars)[sapply(new_vars, is.numeric)]
                          new_quali <- names(new_vars)[sapply(new_vars, function(x) is.factor(x) || is.character(x))]

                          cat("Type des nouvelles variables :\n")
                          if (length(new_quanti) > 0) {
                            cat(sprintf("  • Quantitatives : %s\n", paste(new_quanti, collapse = ", ")))
                          }
                          if (length(new_quali) > 0) {
                            cat(sprintf("  • Qualitatives : %s\n", paste(new_quali, collapse = ", ")))
                          }
                          cat("\n")

                          # Convertir les qualitatives en facteurs
                          for (var in new_quali) {
                            data_combined[[var]] <- as.factor(data_combined[[var]])
                          }

                          # --- 3. FAMD sur l'ensemble (anciennes + nouvelles variables) ---
                          cat("⏳ Calcul FAMD incluant les nouvelles variables...\n")
                          famd_new <- FAMD(data_combined, ncp = self$n_components, graph = FALSE)

                          # --- 4. Extraire les coordonnées des NOUVELLES variables uniquement ---
                          coord_new_vars <- matrix(0, nrow = n_new_vars, ncol = self$n_components)
                          rownames(coord_new_vars) <- names(new_vars)
                          colnames(coord_new_vars) <- colnames(self$coord_var)

                          for (i in 1:n_new_vars) {
                            var_name <- names(new_vars)[i]

                            if (var_name %in% new_quanti) {
                              # Variable quantitative
                              coord_new_vars[i, ] <- famd_new$quanti.var$coord[var_name, ]

                            } else if (var_name %in% new_quali) {
                              # Variable qualitative : barycentre des modalités
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
                                warning(sprintf("⚠️ Impossible de trouver les modalités pour %s", var_name))
                              }
                            }
                          }

                          cat("✓ Coordonnées extraites pour les nouvelles variables\n\n")

                          # --- 5. Calcul de la distance aux centres de clusters originaux ---
                          predicted_clusters <- apply(coord_new_vars, 1, function(var_coord) {
                            distances <- apply(self$cluster_centers, 1, function(center) {
                              sum((var_coord - center)^2)
                            })
                            which.min(distances)
                          })

                          # --- 6. Retourner les résultats ---
                          results <- data.frame(
                            Variable = names(new_vars),
                            Type = ifelse(names(new_vars) %in% new_quanti, "Quantitative", "Qualitative"),
                            Cluster_Predit = predicted_clusters,
                            stringsAsFactors = FALSE
                          )

                          cat("═══════════════════════════════════════\n")
                          cat("RÉSULTATS DE PRÉDICTION\n")
                          cat("═══════════════════════════════════════\n\n")

                          return(results)
                        },

                        #' @description
                        #' Affiche le dendrogramme du clustering hiérarchique
                        #'
                        #' @details
                        #' Génère un graphique du dendrogramme (arbre hiérarchique) montrant
                        #' la structure du clustering des variables. L'axe des ordonnées représente
                        #' la distance à laquelle les clusters sont fusionnés.
                        #'
                        #' @return L'objet CAH_mixtes (invisible), permettant le chaînage
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
                            stop("Erreur : Vous devez d'abord exécuter clustering_hierarchical() !")
                          }

                          cat("\n========================================\n")
                          cat("DENDROGRAMME DU CLUSTERING HIÉRARCHIQUE\n")
                          cat("========================================\n\n")

                          plot(self$hclust_result,
                               main = "Dendrogramme du clustering hiérarchique des variables",
                               xlab = "Variables",
                               ylab = "Distance",
                               sub = "",
                               cex = 0.8)

                          invisible(self)
                        },

                        #' @description
                        #' Visualise les variables dans le plan factoriel
                        #'
                        #' @param axes Vecteur de deux entiers spécifiant les axes à afficher.
                        #'   Par défaut : c(1, 2) pour les deux premières dimensions.
                        #'
                        #' @details
                        #' Génère un graphique ggplot2 montrant :
                        #' \itemize{
                        #'   \item La position des variables dans le plan factoriel
                        #'   \item Le type de chaque variable (forme du point)
                        #'   \item Le cluster d'appartenance (couleur du point) si le clustering
                        #'         a été effectué
                        #'   \item Le nom de chaque variable (avec évitement de chevauchement)
                        #' }
                        #'
                        #' @return L'objet CAH_mixtes (invisible), permettant le chaînage
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #' model$clustering_hierarchical(n_clusters = 3)
                        #'
                        #' # Visualiser dans le plan (Dim1, Dim2)
                        #' model$plot_variables(axes = c(1, 2))
                        #'
                        #' # Visualiser dans le plan (Dim2, Dim3)
                        #' model$plot_variables(axes = c(2, 3))
                        #' }
                        plot_variables = function(axes = c(1, 2)) {
                          if (is.null(self$coord_var)) {
                            stop("Erreur : Vous devez d'abord exécuter fit() !")
                          }

                          cat("\n========================================\n")
                          cat("PROJECTION DES VARIABLES\n")
                          cat("========================================\n\n")

                          plot_df <- data.frame(
                            Dim1 = self$coord_var[, axes[1]],
                            Dim2 = self$coord_var[, axes[2]],
                            Variable = rownames(self$coord_var)
                          )

                          # Ajouter le type de variable
                          plot_df$Type <- ifelse(plot_df$Variable %in% self$quanti_vars,
                                                 "Quantitative", "Qualitative")

                          # Ajouter le cluster si disponible
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
                              labs(color = "Type de variable")
                          }

                          p <- p +
                            ggrepel::geom_text_repel(aes(label = Variable), size = 3.5,
                                                     max.overlaps = 20, fontface = "bold") +
                            geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
                            geom_vline(xintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.5) +
                            labs(
                              title = "Projection des variables dans le plan factoriel (FAMD)",
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
                        #' Applique la méthode du coude pour déterminer le nombre optimal de clusters
                        #'
                        #' @param max_clusters Nombre maximum de clusters à tester. Par défaut : 10
                        #' @param method Méthode de liaison pour la CAH. Par défaut : "ward"
                        #'
                        #' @details
                        #' Cette méthode :
                        #' \itemize{
                        #'   \item Calcule l'inertie intra-cluster pour chaque nombre de clusters
                        #'     de 2 à max_clusters
                        #'   \item Génère un graphique de l'évolution de l'inertie
                        #'   \item Permet d'identifier visuellement le "coude" optimal
                        #' }
                        #'
                        #' Le nombre optimal de clusters correspond généralement au point où
                        #' l'inertie diminue moins rapidement (le "coude" de la courbe).
                        #'
                        #' @return L'objet CAH_mixtes (invisible), permettant le chaînage
                        #'
                        #' @examples
                        #' \dontrun{
                        #' model <- CAH_mixtes$new()
                        #' model$fit(my_data)
                        #'
                        #' # Tester jusqu'à 10 clusters
                        #' model$elbow_method(max_clusters = 10)
                        #'
                        #' # Tester jusqu'à 15 clusters avec liaison complète
                        #' model$elbow_method(max_clusters = 15, method = "complete")
                        #' }
                        elbow_method = function(max_clusters = 10, method = "ward") {
                          if (is.null(self$coord_var)) {
                            stop("Erreur : Vous devez d'abord exécuter fit() !")
                          }

                          cat("\n========================================\n")
                          cat("MÉTHODE DU COUDE\n")
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
                              title = "Méthode du coude - Clustering de variables (FAMD)",
                              x = "Nombre de clusters",
                              y = "Inertie intra-cluster"
                            ) +
                            scale_x_continuous(breaks = 2:max_clusters) +
                            theme_minimal() +
                            theme(panel.grid.minor = element_blank())

                          print(p)
                          cat("💡 Suggestion : Cherchez le 'coude' dans la courbe\n")
                          cat("   où l'inertie diminue moins rapidement.\n\n")

                          invisible(self)
                        },

                        #' @description
                        #' Évalue la qualité du clustering à l'aide de la silhouette
                        #'
                        #' @details
                        #' Cette méthode :
                        #' \itemize{
                        #'   \item Calcule le coefficient de silhouette pour chaque variable
                        #'   \item Calcule la largeur moyenne de silhouette (qualité globale)
                        #'   \item Génère un graphique de silhouette coloré par cluster
                        #' }
                        #'
                        #' Interprétation de la silhouette :
                        #' \itemize{
                        #'   \item 0.71 - 1.0 : Structure forte
                        #'   \item 0.51 - 0.70 : Structure raisonnable
                        #'   \item 0.26 - 0.50 : Structure faible
                        #'   \item < 0.25 : Pas de structure substantielle
                        #' }
                        #'
                        #' @return L'objet CAH_mixtes (invisible), permettant le chaînage
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
                            stop("Erreur : Vous devez d'abord exécuter clustering_hierarchical() !")
                          }

                          cat("\n========================================\n")
                          cat("QUALITÉ DU CLUSTERING DES VARIABLES\n")
                          cat("========================================\n\n")

                          dist_matrix <- dist(self$coord_var)
                          sil <- silhouette(self$labels_var, dist_matrix)
                          avg_sil_width <- mean(sil[, 3])

                          cat(sprintf("✓ Largeur moyenne de silhouette : %.3f\n\n", avg_sil_width))

                          fviz_silhouette(sil) +
                            ggtitle("Silhouette du clustering des variables (FAMD)") +
                            theme_minimal()

                          invisible(self)
                        },

                        #' @description
                        #' Affiche un résumé des résultats de l'analyse
                        #'
                        #' @details
                        #' Affiche des informations sur :
                        #' \itemize{
                        #'   \item Le nombre d'individus et de variables
                        #'   \item Le type de données (mixte, qualitatives, quantitatives)
                        #'   \item Le nombre de composantes et l'inertie expliquée
                        #'   \item Le nombre de clusters créés
                        #'   \item La distribution des variables par cluster
                        #'   \item Les objets R accessibles
                        #' }
                        #'
                        #' @return L'objet CAH_mixtes (invisible), permettant le chaînage
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
                          cat("  RÉSULTATS FAMD + CLUSTERING DE VARIABLES\n")
                          cat("═══════════════════════════════════════════════════\n\n")

                          if (!is.null(self$data)) {
                            cat(sprintf("📊 Analyse effectuée sur %d individus et %d variables\n\n",
                                        nrow(self$data), ncol(self$data)))

                            cat(sprintf("Type de données : %s\n", toupper(self$data_type)))
                            cat(sprintf("  • Variables quantitatives : %d\n", length(self$quanti_vars)))
                            cat(sprintf("  • Variables qualitatives : %d\n\n", length(self$quali_vars)))
                          }

                          if (!is.null(self$famd_result)) {
                            cat(sprintf("📈 Composantes principales : %d\n", self$n_components))
                            cat(sprintf("📈 Inertie totale expliquée : %.2f%%\n\n",
                                        sum(self$inertia_explained[1:min(self$n_components,
                                                                         length(self$inertia_explained))])))
                          }

                          if (!is.null(self$labels_var)) {
                            cat(sprintf("🎯 Clustering effectué : %d clusters (méthode : %s)\n",
                                        self$n_clusters, self$clustering_method))
                            cat(sprintf("🎯 Nombre total de variables : %d\n\n", length(self$labels_var)))

                            cat("📋 Distribution des variables :\n")
                            for (i in 1:self$n_clusters) {
                              vars <- names(self$labels_var[self$labels_var == i])
                              cat(sprintf("   Cluster %d : %d variables\n", i, length(vars)))
                            }
                          }

                          cat("\n🔍 Objets disponibles :\n")
                          cat("   $famd_result        : Résultat FAMD complet\n")
                          cat("   $coord_var          : Coordonnées des variables\n")
                          cat("   $labels_var         : Labels de clusters pour variables\n")
                          cat("   $eigenvalues        : Valeurs propres\n")
                          cat("   $inertia_explained  : % d'inertie expliquée\n")
                          cat("   $data               : Données d'entraînement\n")
                          cat("\n")

                          invisible(self)
                        },

                        #' @description
                        #' Affiche un résumé détaillé des clusters de variables
                        #'
                        #' @details
                        #' Pour chaque cluster, affiche :
                        #' \itemize{
                        #'   \item Le numéro du cluster et le nombre de variables
                        #'   \item La liste des variables quantitatives
                        #'   \item La liste des variables qualitatives
                        #' }
                        #'
                        #' Cette méthode aide à interpréter le sens de chaque cluster en
                        #' identifiant les variables regroupées ensemble.
                        #'
                        #' @return L'objet CAH_mixtes (invisible), permettant le chaînage
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
                            cat("❌ Aucun clustering n'a été effectué.\n")
                            cat("   Utilisez clustering_hierarchical() d'abord.\n")
                            return(invisible(self))
                          }

                          cat("\n═══════════════════════════════════════════════════\n")
                          cat("  INTERPRÉTATION DES CLUSTERS DE VARIABLES\n")
                          cat("═══════════════════════════════════════════════════\n\n")

                          n_clusters <- max(self$labels_var)

                          for (i in 1:n_clusters) {
                            vars_in_cluster <- names(self$labels_var[self$labels_var == i])

                            cat(sprintf("━━━ CLUSTER %d (%d variables) ━━━\n", i, length(vars_in_cluster)))

                            # Séparer quanti et quali
                            vars_quanti <- vars_in_cluster[vars_in_cluster %in% self$quanti_vars]
                            vars_quali <- vars_in_cluster[vars_in_cluster %in% self$quali_vars]

                            if (length(vars_quanti) > 0) {
                              cat("\n  📊 Variables quantitatives :\n")
                              for (var in vars_quanti) {
                                cat(sprintf("     • %s\n", var))
                              }
                            }

                            if (length(vars_quali) > 0) {
                              cat("\n  📝 Variables qualitatives :\n")
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
