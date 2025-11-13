#====================faire des test pour voir si ma classe fonctionne correctement

#==================voir si l'initialisation fonctionne bien ==========================

test_that("clusterVariable$initialize stocke bien k et data sans nettoyage", {
  data(iris)

  cv <- clusterVariable$new(
    k = 3,
    data = iris[, 1:4],
    donnee_nettoyee = FALSE
  )

  # c'est un objet R6 de classe clusterVariable
  expect_true("clusterVariable" %in% class(cv))
  expect_equal(cv$k, 3)
  expect_equal(ncol(cv$data), 4)
  expect_equal(nrow(cv$data), nrow(iris))
})


#================ tester si clusterVariable applique le nettoyage quand donnee_nettoyee = TRUE =============

test_that("clusterVariable applique le nettoyage quand donnee_nettoyee = TRUE", {
  set.seed(123)
  df <- iris[, 1:4]

  # on ajoute des NA et une valeur aberrante
  df[1, 1] <- NA
  df[2, 2] <- NA
  df[3, 3] <- 1e9

  cv <- clusterVariable$new(
    k = 3,
    data = df,
    donnee_nettoyee = TRUE
  )

  # après nettoyage, plus de NA
  expect_false(any(is.na(cv$data)))

  # normalisation ~ moyenne ~ 0, sd ~ 1 (pas exact mais raisonnable)
  means <- vapply(cv$data, mean, numeric(1))
  sds   <- vapply(cv$data, sd,   numeric(1))

  expect_true(all(abs(means) < 1e-6))
  expect_true(all(abs(sds - 1) < 1e-6))
})


#==================  voir si fit echoue quand les donn"es ne sont pas numeriques ==========

test_that("fit() échoue si data non numérique", {
  df_bad <- data.frame(
    x = c("a", "b", "c"),
    y = c("d", "e", "f")
  )

  cv <- clusterVariable$new(k = 2, data = df_bad)

  expect_error(cv$fit(), "data doit etre numeric")
})


#======================= predict renvoir erreur si fit n'est pas fait====================
test_that("predict() renvoie une erreur si fit() n'a pas été appelé", {
  data(iris)
  cv <- clusterVariable$new(
    k = 3,
    data = iris[, 1:4],
    donnee_nettoyee = TRUE
  )

  expect_error(cv$predict(iris[1:5, 1:4]), "vous n'avez pas encore fait le fit")
})





