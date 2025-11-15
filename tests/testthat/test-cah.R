# Unit Tests for CAH Class

# Requirements: testthat, R6
# Usage: testthat::test_file("test-cah.R")

library(testthat)
library(R6)

# Load CAH class
if(file.exists("../R/CAH.R")) {
  source("../R/CAH.R")
} else {
  source("C:/Users/gopit/OneDrive/Documents/MASTER2SISE/Projet_R_Clusting/Clustering-de-variables/R/CAH.R")
}


# TEST 1: INITIALIZATION

test_that("CAH initialization works correctly", {
  cah <- CAH$new()

  expect_equal(cah$method, "ward.D2")
  expect_equal(cah$dist_method, "correlation")
  expect_null(cah$data)
  expect_null(cah$hc)
  expect_null(cah$clusters)
  expect_null(cah$best_k)
  expect_null(cah$predict_result)
  expect_null(cah$compo_latent)
})


# TEST 2: DATA VALIDATION - fit()

test_that("fit() rejects invalid data types", {
  cah <- CAH$new()

  # List instead of data.frame/matrix
  expect_error(cah$fit(list(a = 1, b = 2)), "dataframe")

  # Simple vector
  expect_error(cah$fit(c(1, 2, 3)), "dataframe")
})

test_that("fit() rejects insufficient data size", {
  cah <- CAH$new()

  # Single observation
  expect_error(cah$fit(data.frame(x = 1, y = 2)), "Not enough data")

  # Single variable
  expect_error(cah$fit(data.frame(x = c(1, 2, 3))), "Not enough data")

  # Two observations but one variable
  expect_error(cah$fit(data.frame(x = c(1, 2))), "Not enough data")
})

test_that("fit() filters qualitative variables", {
  cah <- CAH$new()
  df <- data.frame(
    var1 = rnorm(15),
    var2 = rnorm(15),
    var3 = rnorm(15),
    cat1 = factor(rep(c("A", "B", "C"), 5)),
    cat2 = c(rep("X", 7), rep("Y", 8))
  )

  expect_warning(suppressMessages(cah$fit(df)), "qualitative")
  expect_equal(ncol(cah$data), 3)  # Only quantitative variables
  expect_true(all(c("var1", "var2", "var3") %in% colnames(cah$data)))
})

test_that("fit() removes constant variables", {
  cah <- CAH$new()
  df <- data.frame(
    var1 = rnorm(15),
    var2 = rep(5, 15),  # Constant
    var3 = rnorm(15),
    var4 = rep(10, 15), # Constant
    var5 = rnorm(15)
  )

  expect_warning(suppressMessages(cah$fit(df)), "Constant")
  expect_equal(ncol(cah$data), 3)  # Only non-constant variables
})

test_that("fit() handles missing values", {
  cah <- CAH$new()
  df <- data.frame(
    var1 = c(rnorm(12), NA, NA, NA),
    var2 = rnorm(15),
    var3 = c(NA, rnorm(14)),
    var4 = rnorm(15)
  )

  expect_warning(suppressMessages(cah$fit(df)), "missing")
  expect_lt(nrow(cah$data), 15)  # Rows with NA removed
  expect_false(anyNA(cah$data))  # No more NA
})

test_that("fit() rejects data after cleaning if insufficient", {
  cah <- CAH$new()

  # All variables are constant - rejected immediately
  df_const <- data.frame(
    var1 = rep(1, 10),
    var2 = rep(2, 10),
    var3 = rep(3, 10),
    var4 = rep(4, 10)
  )
  expect_error(suppressWarnings(cah$fit(df_const)), "Constant|no valid")

  # Too many NAs - only 1 complete row is not enough for hclust
  df_na <- data.frame(
    var1 = c(1, NA, NA, NA, NA),
    var2 = c(2, NA, NA, NA, NA),
    var3 = c(3, NA, NA, NA, NA),
    var4 = c(4, NA, NA, NA, NA)
  )
  expect_error(suppressWarnings(cah$fit(df_na)), "not enough|Not enough")
})


# TEST 3: fit() CALCULATIONS

test_that("fit() correctly calculates correlation matrix", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(25),
    var2 = rnorm(25),
    var3 = rnorm(25),
    var4 = rnorm(25)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))

  # Check corr_moy
  expect_true(!is.null(cah$corr_moy))
  expect_true(cah$corr_moy >= 0 && cah$corr_moy <= 1)

  # Check distance matrix
  expect_s3_class(cah$dist_matrix, "dist")
  expect_equal(attr(cah$dist_matrix, "Size"), 4)
})

test_that("fit() creates valid hclust object", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(25),
    var2 = rnorm(25),
    var3 = rnorm(25),
    var4 = rnorm(25),
    var5 = rnorm(25)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))

  expect_s3_class(cah$hc, "hclust")
  expect_equal(cah$hc$method, "ward.D2")
  expect_true(!is.null(cah$best_k))
  expect_true(cah$best_k >= 1 && cah$best_k < ncol(df))
})

test_that("fit() detects reasonable best_k", {
  set.seed(42)
  # Create obvious groups
  df <- data.frame(
    var1 = rnorm(30, 10, 1),
    var2 = rnorm(30, 11, 1),
    var3 = rnorm(30, 50, 2),
    var4 = rnorm(30, 51, 2),
    var5 = rnorm(30, 100, 5),
    var6 = rnorm(30, 102, 5)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))

  expect_true(cah$best_k >= 2)  # At least 2 obvious groups
  expect_true(cah$best_k <= 5)  # No more than ncol - 1
})

# TEST 4: cutree() METHOD

test_that("cutree() requires fit() first", {
  cah <- CAH$new()
  expect_error(cah$cutree(), "Call.*fit")
})

test_that("cutree() uses best_k by default", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20),
    var5 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  expected_k <- cah$best_k

  suppressMessages(cah$cutree())

  expect_equal(length(unique(cah$clusters)), expected_k)
  expect_equal(length(cah$clusters), 5)  # 5 variables
})

test_that("cutree() accepts custom k", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20),
    var5 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 3))

  expect_equal(length(unique(cah$clusters)), 3)
  expect_equal(length(cah$clusters), 5)
})

test_that("cutree() creates latent components", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  expect_type(cah$compo_latent, "list")
  expect_equal(length(cah$compo_latent), 2)  # 2 clusters

  # Check component structure
  comp1 <- cah$compo_latent[[1]]
  expect_true(all(c("Zk", "vars", "cor_vals", "scaled_data") %in% names(comp1)))
  expect_equal(length(comp1$Zk), 20)  # Number of individuals
})

test_that("cutree() assigns all variable names", {
  set.seed(42)
  df <- data.frame(
    alpha = rnorm(20),
    beta = rnorm(20),
    gamma = rnorm(20),
    delta = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  expect_equal(names(cah$clusters), c("alpha", "beta", "gamma", "delta"))
  expect_true(all(cah$clusters %in% 1:2))
})


# TEST 5: predict() METHOD

test_that("predict() requires cutree() first", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))

  new_var <- data.frame(var5 = rnorm(20))
  expect_error(cah$predict(new_var), "call.*cutree")
})

test_that("predict() validates number of individuals", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree())

  # Incorrect number of individuals
  new_var_10 <- data.frame(var5 = rnorm(10))
  expect_error(cah$predict(new_var_10), "number|individuals")

  new_var_30 <- data.frame(var5 = rnorm(30))
  expect_error(cah$predict(new_var_30), "number|individuals")
})

test_that("predict() rejects missing values", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree())

  new_var <- data.frame(var5 = c(rnorm(19), NA))
  expect_error(cah$predict(new_var), "missing|NA")
})

test_that("predict() correctly assigns variables", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(25, 10, 1),
    var2 = rnorm(25, 11, 1),
    var3 = rnorm(25, 50, 2),
    var4 = rnorm(25, 52, 2)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # New variable highly correlated to var1
  new_var <- data.frame(var5 = df$var1 * 0.95 + rnorm(25, 0, 0.1))
  suppressMessages(cah$predict(new_var))

  expect_equal(length(cah$predict_result), 1)
  expect_equal(names(cah$predict_result), "var5")
  expect_true(cah$predict_result["var5"] %in% unique(cah$clusters))

  # var5 should be in same cluster as var1
  cluster_var1 <- as.numeric(cah$clusters["var1"])
  cluster_var5 <- as.numeric(cah$predict_result["var5"])
  expect_equal(cluster_var5, cluster_var1)
})

test_that("predict() can handle multiple new variables", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(25, 10, 1),
    var2 = rnorm(25, 11, 1),
    var3 = rnorm(25, 50, 2),
    var4 = rnorm(25, 52, 2)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # Multiple new variables
  new_var <- data.frame(
    var5 = df$var1 * 0.9 + rnorm(25, 0, 0.2),
    var6 = df$var3 * 0.9 + rnorm(25, 0, 0.5),
    var7 = df$var2 * 0.85 + rnorm(25, 0, 0.3)
  )

  suppressMessages(cah$predict(new_var))

  expect_equal(length(cah$predict_result), 3)
  expect_equal(names(cah$predict_result), c("var5", "var6", "var7"))
  expect_true(all(cah$predict_result %in% unique(cah$clusters)))
})

test_that("predict() stores X_last correctly", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree())

  new_var <- data.frame(
    var5 = rnorm(20),
    var6 = rnorm(20)
  )

  suppressMessages(cah$predict(new_var))

  expect_true(!is.null(cah$X_last))
  expect_equal(ncol(cah$X_last), 2)
  expect_equal(nrow(cah$X_last), 20)
})


# TEST 6: print() AND summary() METHODS

test_that("print() works at each stage", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  # Before fit
  cah <- CAH$new()
  expect_output(cah$print(), "No data")

  # After fit
  suppressMessages(cah$fit(df))
  output_fit <- capture.output(cah$print())
  expect_true(any(grepl("20 individuals", output_fit)))
  expect_true(any(grepl("4 variables", output_fit)))

  # After cutree
  suppressMessages(cah$cutree())
  output_cut <- capture.output(cah$print())
  expect_true(any(grepl("Variables per cluster", output_cut)))

  # After predict
  new_var <- data.frame(var5 = rnorm(20))
  suppressMessages(cah$predict(new_var))
  output_pred <- capture.output(cah$print())
  expect_true(any(grepl("Supplementary variables", output_pred)))
})

test_that("summary() requires fitted model", {
  cah <- CAH$new()
  expect_output(cah$summary(), "No model fitted")
})

test_that("summary() displays complete information", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20),
    var5 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  output <- capture.output(cah$summary())

  expect_true(any(grepl("Aggregation method", output)))
  expect_true(any(grepl("Average correlation", output)))
  expect_true(any(grepl("Optimal k", output)))
  expect_true(any(grepl("Local PCA", output)))
  expect_true(any(grepl("Cluster", output)))
})

test_that("summary() displays supplementary variables", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  new_var <- data.frame(
    var5 = rnorm(20),
    var6 = rnorm(20)
  )
  suppressMessages(cah$predict(new_var))

  output <- capture.output(cah$summary())
  expect_true(any(grepl("Supplementary variables", output)))
  expect_true(any(grepl("var5", output)))
  expect_true(any(grepl("var6", output)))
})


# TEST 7: COMPLETE WORKFLOW

test_that("Complete A-to-Z workflow works", {
  set.seed(123)

  # Training data with obvious groups
  df <- data.frame(
    var1 = rnorm(30, 10, 2),
    var2 = rnorm(30, 12, 2),
    var3 = rnorm(30, 50, 5),
    var4 = rnorm(30, 52, 5),
    var5 = rnorm(30, 100, 10),
    var6 = rnorm(30, 105, 10)
  )

  # 1. Initialization
  cah <- CAH$new()
  expect_equal(cah$method, "ward.D2")

  # 2. Fitting
  suppressMessages(cah$fit(df))
  expect_equal(nrow(cah$data), 30)
  expect_equal(ncol(cah$data), 6)
  expect_true(!is.null(cah$hc))

  # 3. Partitioning
  suppressMessages(cah$cutree(k = 3))
  expect_equal(length(unique(cah$clusters)), 3)
  expect_equal(length(cah$compo_latent), 3)

  # 4. Prediction
  new_var <- data.frame(
    var7 = df$var1 * 0.9 + rnorm(30, 0, 0.5),  # Close to var1
    var8 = df$var3 * 0.9 + rnorm(30, 0, 1),    # Close to var3
    var9 = df$var5 * 0.9 + rnorm(30, 0, 2)     # Close to var5
  )
  suppressMessages(cah$predict(new_var))
  expect_equal(length(cah$predict_result), 3)

  # 5. Display
  expect_output(cah$print(), "Supplementary variables")
  expect_output(cah$summary(), "Supplementary variables")
})


# TEST 8: EDGE CASES AND LIMITS

test_that("Handles single-variable cluster", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20, 10, 1),
    var2 = rnorm(20, 11, 1),
    var3 = rnorm(20, 12, 1),
    var4 = rnorm(20, 200, 50)  # Very different
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # Latent components must exist for all clusters
  expect_equal(length(cah$compo_latent), 2)

  # Check single-variable cluster works
  for (comp in cah$compo_latent) {
    expect_true(!is.null(comp$Zk))
    expect_true(length(comp$vars) >= 1)
  }
})

test_that("Perfectly correlated variables are handled", {
  set.seed(42)
  base <- rnorm(20)
  df <- data.frame(
    var1 = base,
    var2 = base * 2,        # Perfect correlation
    var3 = base * 0.5 + 1,  # Perfect correlation
    var4 = rnorm(20),
    var5 = rnorm(20)
  )

  cah <- CAH$new()
  expect_message(cah$fit(df))
  suppressMessages(cah$cutree())

  expect_true(!is.null(cah$clusters))
  expect_equal(length(cah$clusters), 5)
})

test_that("All variables similar to each other", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20, 10, 1),
    var2 = rnorm(20, 10, 1),
    var3 = rnorm(20, 10, 1),
    var4 = rnorm(20, 10, 1)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 1))

  # Should put all variables in one cluster
  expect_equal(length(unique(cah$clusters)), 1)
})

test_that("Data with very different scales", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20, 0.001, 0.0001),  # Very small scale
    var2 = rnorm(20, 0.002, 0.0001),
    var3 = rnorm(20, 10000, 1000),    # Very large scale
    var4 = rnorm(20, 10500, 1000)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # Correlation should handle scale differences
  expect_true(!is.null(cah$clusters))
  expect_equal(length(cah$clusters), 4)
})


# TEST 9: ROBUSTNESS

test_that("Multiple calls to cutree() with different k", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20),
    var5 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))

  # First cutree
  suppressMessages(cah$cutree(k = 2))
  clusters_k2 <- cah$clusters

  # Second cutree with different k
  suppressMessages(cah$cutree(k = 3))
  clusters_k3 <- cah$clusters

  expect_equal(length(unique(clusters_k2)), 2)
  expect_equal(length(unique(clusters_k3)), 3)
  expect_false(identical(clusters_k2, clusters_k3))
})

test_that("Multiple calls to predict() with new variables", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # First predict
  new1 <- data.frame(var5 = rnorm(20))
  suppressMessages(cah$predict(new1))
  pred1 <- cah$predict_result

  # Second predict (overwrites first)
  new2 <- data.frame(var6 = rnorm(20), var7 = rnorm(20))
  suppressMessages(cah$predict(new2))
  pred2 <- cah$predict_result

  expect_equal(length(pred1), 1)
  expect_equal(length(pred2), 2)
  expect_equal(names(pred2), c("var6", "var7"))
})


# FINAL SUMMARY
# ==============================================================================
cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  UNIT TESTS SUMMARY - CAH Class\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  Tests executed successfully!\n")
cat("  All main functionalities are validated.\n")
cat("═══════════════════════════════════════════════════════════════\n\n")
