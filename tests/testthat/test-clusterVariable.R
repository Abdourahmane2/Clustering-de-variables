# Test 1: Vérifier l'initialisation correcte de la classe
test_that("clusterVariable$initialize stocke bien k et les paramètres", {
  cv <- clusterVariable$new(k = 3, max_iter = 100, auto_clean = TRUE)

  # Vérifier que c'est bien un objet R6 de classe clusterVariable
  expect_true("clusterVariable" %in% class(cv))
  expect_equal(cv$k, 3)
  expect_null(cv$data)  # data est NULL avant fit()
  expect_null(cv$cluster_result)  # cluster_result est NULL avant fit()
})

# Test 2: Vérifier que fit() échoue avec des données non numériques
test_that("fit() échoue si les données contiennent des variables non numériques", {
  df_bad <- data.frame(
    x = c("a", "b", "c"),
    y = 1:3
  )
  cv <- clusterVariable$new(k = 2)

  expect_error(
    cv$fit(df_bad, check_missing = FALSE),
    "All variables in X must be numeric"
  )
})
# Test 3: Vérifier que fit() fonctionne avec des données numériques valides
test_that("fit() s'exécute correctement avec des données numériques valides", {
  data(iris)
  cv <- clusterVariable$new(k = 3, auto_clean = TRUE)

  # Vérifier que fit() produit le message attendu
  expect_output(cv$fit(iris[, 1:4]), "Model fitted successfully")

  # Vérifier que les résultats sont stockés
  expect_false(is.null(cv$data))
  expect_false(is.null(cv$cluster_result))
  expect_equal(length(cv$cluster_result$cluster), 4)
  expect_equal(cv$k, 3)
})

# Test 4: Vérifier que predict() échoue si fit() n'a pas été appelé
test_that("predict() renvoie une erreur si fit() n'a pas été appelé", {
  data(iris)
  cv <- clusterVariable$new(k = 3)

  expect_error(
    cv$predict(iris[1:10, 1:4]),
    "Model not fitted yet"
  )
})

# Test 5: Vérifier que auto_clean gère les valeurs manquantes
test_that("auto_clean gère correctement les valeurs manquantes", {
  set.seed(123)
  df <- iris[, 1:4]
  df[1, 1] <- NA
  df[2, 2] <- NA
  df[5, 3] <- NA

  cv <- clusterVariable$new(k = 3, auto_clean = TRUE)

  # fit() devrait fonctionner et afficher un message
  expect_output(cv$fit(df, check_missing = FALSE), "Model fitted successfully")

  # Vérifier qu'il n'y a plus de NA
  expect_false(any(is.na(cv$data)))
})
