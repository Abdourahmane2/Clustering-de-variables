mon_kmeans <- function(donnees, k, max_iter = 100) {

  if (missing(k)) stop("Erreur : k est obligatoire")
  if (!is.data.frame(donnees) && !is.matrix(donnees)) stop("Erreur : donnees doit être un data.frame ou une matrice")

  # Transposer : variables → lignes
  donnees_t <- t(as.matrix(donnees))
  nb_variables <- nrow(donnees_t)
  nb_observations <- ncol(donnees_t)

  if (k > nb_variables) stop(paste("k =", k, "est trop grand. Maximum :", nb_variables))

  set.seed(123)
  indices_initiaux <- sample(1:nb_variables, k)
  centres <- donnees_t[indices_initiaux, , drop = FALSE]

  for (iteration in 1:max_iter) {
    # --- Affectation ---
    distances <- matrix(0, nrow = k, ncol = nb_variables)
    for (i in 1:k) {
      for (j in 1:nb_variables) {
        distances[i, j] <- sum((donnees_t[j, ] - centres[i, ])^2)
      }
    }

    cluster <- apply(distances, 2, which.min)

    # --- Mise à jour des centres ---
    anciens_centres <- centres
    for (i in 1:k) {
      vars_cluster <- which(cluster == i)
      if (length(vars_cluster) > 0) {
        centres[i, ] <- colMeans(donnees_t[vars_cluster, , drop = FALSE])
      } else {
        # Cluster vide → garder ancien centre
        centres[i, ] <- anciens_centres[i, ]
      }
    }

    # Convergence
    if (all(centres == anciens_centres)) break
  }

  # Assurer une matrice même si k = 1
  if (k == 1) centres <- matrix(centres, nrow = 1)

  rownames(centres) <- paste0("Cluster", 1:k)
  names(cluster) <- colnames(donnees)

  return(list(
    centers = centres,
    cluster = cluster
  ))
}
