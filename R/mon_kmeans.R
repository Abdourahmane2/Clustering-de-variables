#' @keywords internal
mon_kmeans <- function(data, max_iter = 100, k = 3) {
  if(missing(k)) stop("k est obligatoire pour kmeans")
  if (!is.data.frame(data) && !is.matrix(data)) stop("Erreur : 'data' doit être un data.frame ou une matrice.")

  data_t <- t(data)
  n_vars <- nrow(data_t)
  n_features <- ncol(data_t)

  centers <- data_t[sample(1:n_vars, k), , drop = FALSE]

  for (iter in 1:max_iter) {
    distances <- matrix(NA, nrow = k, ncol = n_vars)
    for (i in 1:k) {
      distances[i, ] <- rowSums((data_t - matrix(centers[i, ], nrow = n_vars, ncol = n_features, byrow = TRUE))^2)
    }

    cluster <- apply(distances, 2, which.min)

    new_centers <- matrix(NA, nrow = k, ncol = n_features)
    for (i in 1:k) {
      if (sum(cluster == i) > 0) {
        new_centers[i, ] <- colMeans(data_t[cluster == i, , drop = FALSE])
      } else {
        new_centers[i, ] <- centers[i, ]
      }
    }

    if (all(centers == new_centers, na.rm = TRUE)) break
    centers <- new_centers
  }

  return(list(centers = centers, cluster = cluster))
}


