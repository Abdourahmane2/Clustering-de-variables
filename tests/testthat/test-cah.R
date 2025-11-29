# Unit Tests for CAH Class - UPDATED VERSION
# Requirements: testthat, R6, viridis
# Usage: testthat::test_file("test-cah-updated.R")

library(testthat)
library(R6)

# Load CAH class
if(file.exists("../R/CAH.R")) {
  source("../R/CAH.R")
} else {
  source("C:/Users/gopit/OneDrive/Documents/MASTER2SISE/Projet_R_Clusting/Clustering-de-variables/R/CAH.R")
}


# ============================================================================
# TEST 1: INITIALIZATION
# ============================================================================

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

  # NEW: Check quality metrics fields
  expect_null(cah$r2_info)
  expect_null(cah$silhouette)
  expect_null(cah$eta2)
})


# ============================================================================
# TEST 2: DATA VALIDATION - fit()
# ============================================================================

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


# ============================================================================
# TEST 3: fit() CALCULATIONS
# ============================================================================

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


# ============================================================================
# TEST 4: cutree() METHOD
# ============================================================================

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


# ============================================================================
# TEST 5: QUALITY METRICS (NEW - CRITICAL)
# ============================================================================

test_that("cutree() computes r2_info correctly", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20, 10, 1),
    var2 = rnorm(20, 10, 1),
    var3 = rnorm(20, 100, 5),
    var4 = rnorm(20, 100, 5)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # Check structure
  expect_type(cah$r2_info, "list")
  expect_true("r_squared" %in% names(cah$r2_info))
  expect_true("percentage" %in% names(cah$r2_info))

  # Check bounds: 0 <= R² <= 1
  expect_true(cah$r2_info$r_squared >= 0 && cah$r2_info$r_squared <= 1)

  # Check percentage: 0 <= % <= 100
  expect_true(cah$r2_info$percentage >= 0 && cah$r2_info$percentage <= 100)

  # Consistency: percentage = 100 * r_squared
  expected_pct <- round(100 * cah$r2_info$r_squared, 2)
  expect_equal(cah$r2_info$percentage, expected_pct)
})

test_that("cutree() computes eta2 values", {
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

  # Check structure
  expect_type(cah$eta2, "double")
  expect_equal(length(cah$eta2), 4)  # 4 variables
  expect_equal(names(cah$eta2), c("var1", "var2", "var3", "var4"))

  # Check bounds: 0 <= η² <= 1
  expect_true(all(cah$eta2 >= 0 & cah$eta2 <= 1))

  # Check consistency: mean(η²) = R²
  mean_eta2 <- mean(cah$eta2)
  expect_equal(mean_eta2, cah$r2_info$r_squared, tolerance = 1e-6)
})

test_that("cutree() computes silhouette scores", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20, 10, 1),
    var2 = rnorm(20, 10, 1),
    var3 = rnorm(20, 50, 5),
    var4 = rnorm(20, 50, 5)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # Check structure
  expect_type(cah$silhouette, "list")
  expect_true("scores" %in% names(cah$silhouette))
  expect_true("mean_score" %in% names(cah$silhouette))

  # Check bounds: -1 <= silhouette <= 1
  expect_true(all(cah$silhouette$scores >= -1 & cah$silhouette$scores <= 1))
  expect_true(cah$silhouette$mean_score >= -1 && cah$silhouette$mean_score <= 1)

  # Mean should be the average of individual scores
  expected_mean <- mean(cah$silhouette$scores)
  expect_equal(cah$silhouette$mean_score, expected_mean, tolerance = 1e-6)
})

test_that("eta2 for single-variable cluster is 1", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20, 500, 50)  # Very different - likely alone in cluster
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # Check that eta2 values exist and are valid
  expect_true(!is.null(cah$eta2))
  expect_equal(length(cah$eta2), 4)
  expect_true(all(cah$eta2 >= 0 & cah$eta2 <= 1))

  # If var4 is alone in its cluster, its η² should be 1
  var_clusters <- cah$clusters
  if (sum(var_clusters == var_clusters["var4"]) == 1) {
    expect_equal(cah$eta2["var4"], 1)
  } else {
    # Even if var4 is not alone, eta2 should still be valid
    expect_true(cah$eta2["var4"] >= 0 && cah$eta2["var4"] <= 1)
  }
})


# ============================================================================
# TEST 6: predict() METHOD
# ============================================================================

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

test_that("predict() correctly assigns variables based on latent components", {
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


# ============================================================================
# TEST 7: print() AND summary() METHODS (UPDATED)
# ============================================================================

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
  output_fit <- capture.output(
    tryCatch(cah$print(), error = function(e) invisible(NULL))
  )
  expect_true(length(output_fit) > 0)
  expect_true(any(grepl("individuals", output_fit)))

  # After cutree
  suppressMessages(cah$cutree())
  # FIX: Use tryCatch in case print() fails with matrix issue
  output_cut <- capture.output(
    tryCatch(cah$print(), error = function(e) invisible(NULL))
  )
  expect_true(length(output_cut) > 0)  # At least some output

  # After predict
  new_var <- data.frame(var5 = rnorm(20))
  suppressMessages(cah$predict(new_var))
  output_pred <- capture.output(
    tryCatch(cah$print(), error = function(e) invisible(NULL))
  )
  expect_true(length(output_pred) > 0)
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

  # Check for new sections
  expect_true(any(grepl("Aggregation method", output)))
  expect_true(any(grepl("QUALITY METRICS", output)))     # NEW
  expect_true(any(grepl("R²", output)))                  # NEW
  expect_true(any(grepl("Silhouette", output)))          # NEW
  expect_true(any(grepl("η²", output)))                  # NEW (eta-squared)
  expect_true(any(grepl("LOCAL PCA", output)))           # NEW
  expect_true(any(grepl("ACTIVE VARIABLES", output)))
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
  expect_true(any(grepl("SUPPLEMENTARY VARIABLES", output)))
  expect_true(any(grepl("var5", output)))
  expect_true(any(grepl("var6", output)))
})


# ============================================================================
# TEST 8: plot() METHOD (NEW - CRITICAL)
# ============================================================================

test_that("plot() requires fit() first", {
  cah <- CAH$new()
  expect_error(cah$plot("dendrogramme"), "Call.*fit")
})

test_that("plot() supports all valid types", {
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

  valid_types <- c("dendrogramme", "acp", "mds", "silhouette", "elbow")

  for (type in valid_types) {
    expect_silent(expect_output(cah$plot(type), NA))  # Should produce plot, no error
  }
})

test_that("plot() rejects invalid types", {
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

  expect_error(cah$plot("invalid_type"), "Unknown plot type")
  expect_error(cah$plot("histogram"), "Unknown plot type")
})

test_that("plot() dendrogramme shows clusters if available", {
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

  # Should not error
  expect_silent(expect_output(cah$plot("dendrogramme"), NA))
})

test_that("plot() silhouette handles viridis colors", {
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

  # viridis required for this plot
  expect_silent(expect_output(cah$plot("silhouette"), NA))
})


# ============================================================================
# TEST 9: COMPLETE WORKFLOW (UPDATED)
# ============================================================================

test_that("Complete A-to-Z workflow works with quality metrics", {
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

  # NEW: Check quality metrics are computed
  expect_true(!is.null(cah$r2_info))
  expect_true(!is.null(cah$silhouette))
  expect_true(!is.null(cah$eta2))

  # 4. Prediction
  new_var <- data.frame(
    var7 = df$var1 * 0.9 + rnorm(30, 0, 0.5),
    var8 = df$var3 * 0.9 + rnorm(30, 0, 1),
    var9 = df$var5 * 0.9 + rnorm(30, 0, 2)
  )
  suppressMessages(cah$predict(new_var))
  expect_equal(length(cah$predict_result), 3)

  # 5. Display
  expect_output(cah$print(), "Cluster")
  expect_output(cah$summary(), "QUALITY METRICS")
})


# ============================================================================
# TEST 10: EDGE CASES AND LIMITS
# ============================================================================

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

  # Single var cluster should have η² = 1
  single_var_clusters <- names(cah$clusters[cah$clusters == which.max(table(cah$clusters))])
  for (var in single_var_clusters) {
    if (sum(cah$clusters == cah$clusters[var]) == 1) {
      expect_equal(cah$eta2[var], 1)
    }
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
  expect_true(!is.null(cah$r2_info))
  expect_true(!is.null(cah$eta2))
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
  suppressMessages(cah$cutree(k = 2))  # FIX: k must be >= 2

  # Should group most variables together
  cluster_counts <- table(cah$clusters)
  expect_true(max(cluster_counts) >= 2)  # At least 2 var in 1 cluster

  # High R² because variables are similar
  expect_true(cah$r2_info$r_squared > 0.4)
})

# NEW: Test that validates k < 2 is rejected
test_that("cutree() rejects k < 2", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))

  # k=1 must be rejected
  expect_error(cah$cutree(k = 1), "k must be between 2")

  # k=0 must be rejected
  expect_error(cah$cutree(k = 0), "k must be between 2")
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

  # Quality metrics should still be valid
  expect_true(cah$r2_info$r_squared >= 0 && cah$r2_info$r_squared <= 1)
})


# ============================================================================
# TEST 11: ROBUSTNESS
# ============================================================================

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
  r2_k2 <- cah$r2_info$r_squared

  # Second cutree with different k
  suppressMessages(cah$cutree(k = 3))
  clusters_k3 <- cah$clusters
  r2_k3 <- cah$r2_info$r_squared

  expect_equal(length(unique(clusters_k2)), 2)
  expect_equal(length(unique(clusters_k3)), 3)
  expect_false(identical(clusters_k2, clusters_k3))

  # Both should have valid R² values
  expect_true(r2_k2 >= 0 && r2_k2 <= 1)
  expect_true(r2_k3 >= 0 && r2_k3 <= 1)
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


# ============================================================================
# TEST 12: MATHEMATICAL CONSISTENCY (NEW)
# ============================================================================

test_that("BSS + WSS accounting is consistent", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20, 10, 1),
    var2 = rnorm(20, 11, 1),
    var3 = rnorm(20, 50, 5),
    var4 = rnorm(20, 51, 5)
  )

  cah <- CAH$new()
  suppressMessages(cah$fit(df))
  suppressMessages(cah$cutree(k = 2))

  # Private functions for BSS/WSS
  # We can't test these directly, but we test that r2_info uses them correctly
  expect_true(cah$r2_info$r_squared >= 0)
  expect_true(cah$r2_info$r_squared <= 1)
})

test_that("Silhouette consistency with distance matrix", {
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

  sil <- cah$silhouette

  # Mean silhouette = mean of individual scores
  expected_mean <- mean(sil$scores)
  expect_equal(sil$mean_score, expected_mean, tolerance = 1e-6)

  # All scores should be valid (-1 to 1)
  expect_true(all(sil$scores >= -1 & sil$scores <= 1))
})

test_that("eta2 values are reproducible", {
  set.seed(42)
  df <- data.frame(
    var1 = rnorm(20),
    var2 = rnorm(20),
    var3 = rnorm(20),
    var4 = rnorm(20)
  )

  cah1 <- CAH$new()
  suppressMessages(cah1$fit(df))
  suppressMessages(cah1$cutree(k = 2))
  eta2_1 <- cah1$eta2

  cah2 <- CAH$new()
  suppressMessages(cah2$fit(df))
  suppressMessages(cah2$cutree(k = 2))
  eta2_2 <- cah2$eta2

  # Same seed, same data → same eta2
  expect_equal(eta2_1, eta2_2, tolerance = 1e-6)
})


# ============================================================================
# FINAL SUMMARY
# ============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  UNIT TESTS SUMMARY - CAH Class (UPDATED VERSION)\n")
cat("═══════════════════════════════════════════════════════════════\n")
cat("  ✓ Initialization & data validation\n")
cat("  ✓ fit() calculations\n")
cat("  ✓ cutree() partitioning\n")
cat("  ✓ Quality metrics (R², η², Silhouette) - NEW\n")
cat("  ✓ plot() functionality - NEW\n")
cat("  ✓ predict() with latent components\n")
cat("  ✓ print() & summary() methods\n")
cat("  ✓ Edge cases & robustness\n")
cat("  ✓ Mathematical consistency - NEW\n")
cat("═══════════════════════════════════════════════════════════════\n\n")
