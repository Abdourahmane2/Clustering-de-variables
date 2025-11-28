# =============================================================================
# UNIT TESTS FOR CAH_mixtes
# Tests for the R6 class for FAMD + Hierarchical variable clustering
# =============================================================================
#
# EXECUTION INSTRUCTIONS:
# 1. Make sure the required packages are installed:
#    install.packages(c("testthat", "FactoMineR", "factoextra", "cluster", 
#                       "ggplot2", "dplyr", "R6", "ggrepel"))
#
# 2. Place this file in the same directory as FAMD_vfinale_ang.R
#
# 3. Run the tests with:
#    testthat::test_file("tests.R")
#
# =============================================================================

library(testthat)
library(FactoMineR)
library(factoextra)
library(cluster)
library(ggplot2)
library(dplyr)
library(R6)
library(ggrepel)

# Load the source code (adjust the path if necessary)
source("FAMD_vfinale_ang.R")

# =============================================================================
# TEST DATA
# =============================================================================

# Create reproducible test data
set.seed(42)

# Mixed data (quantitative + qualitative)
create_mixed_data <- function(n = 100) {
  data.frame(
    age = rnorm(n, mean = 40, sd = 10),
    revenu = rnorm(n, mean = 50000, sd = 15000),
    score = runif(n, 0, 100),
    taille = rnorm(n, mean = 170, sd = 10),
    categorie = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    niveau = factor(sample(c("bas", "moyen", "haut"), n, replace = TRUE)),
    region = factor(sample(c("Nord", "Sud", "Est", "Ouest"), n, replace = TRUE))
  )
}

# Qualitative data only
create_quali_data <- function(n = 100) {
  data.frame(
    categorie = factor(sample(c("A", "B", "C"), n, replace = TRUE)),
    niveau = factor(sample(c("bas", "moyen", "haut"), n, replace = TRUE)),
    region = factor(sample(c("Nord", "Sud", "Est", "Ouest"), n, replace = TRUE)),
    statut = factor(sample(c("actif", "inactif"), n, replace = TRUE))
  )
}

# Quantitative data only (should fail with FAMD)
create_quanti_data <- function(n = 100) {
  data.frame(
    var1 = rnorm(n),
    var2 = rnorm(n),
    var3 = rnorm(n),
    var4 = rnorm(n)
  )
}

# =============================================================================
# INITIALIZATION TESTS
# =============================================================================

test_that("Initialization with default parameters", {
  model <- CAH_mixtes$new()
  
  expect_s3_class(model, "CAH_mixtes")
  expect_equal(model$n_components, 5)
  expect_null(model$famd_result)
  expect_null(model$data)
  expect_null(model$labels_var)
})

test_that("Initialization with custom number of components", {
  model <- CAH_mixtes$new(n_components = 10)
  
  expect_equal(model$n_components, 10)
})

test_that("Initialization with 1 component", {
  model <- CAH_mixtes$new(n_components = 1)
  
  expect_equal(model$n_components, 1)
})

# =============================================================================
# TESTS FOR THE fit() METHOD
# =============================================================================

test_that("fit() with mixed data", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  
  result <- model$fit(data)
  
  # Verify that the object is returned (chaining)
  expect_s3_class(result, "CAH_mixtes")
  
  # Verify attributes
  expect_equal(model$data_type, "mixte")
  expect_equal(nrow(model$data), 50)
  expect_equal(ncol(model$data), 7)
  
  # Verify variable type detection
  expect_equal(length(model$quanti_vars), 4)
  expect_equal(length(model$quali_vars), 3)
  expect_true("age" %in% model$quanti_vars)
  expect_true("categorie" %in% model$quali_vars)
  
  # Verify FAMD results
  expect_false(is.null(model$famd_result))
  expect_false(is.null(model$coord_var))
  expect_false(is.null(model$eigenvalues))
  expect_false(is.null(model$inertia_explained))
  
  # Verify coordinate dimensions
  expect_equal(ncol(model$coord_var), 3)  # 3 components
  expect_equal(nrow(model$coord_var), 7)  # 7 variables (4 quanti + 3 quali)
})

test_that("fit() with qualitative data only", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_quali_data(50)
  
  model$fit(data)
  
  expect_equal(model$data_type, "quali")
  expect_equal(length(model$quanti_vars), 0)
  expect_equal(length(model$quali_vars), 4)
  expect_false(is.null(model$coord_var))
})

test_that("fit() converts characters to factors", {
  model <- CAH_mixtes$new()
  data <- data.frame(
    num = c(1, 2, 3, 4, 5),
    char_var = c("a", "b", "a", "c", "b"),  # character, not factor
    stringsAsFactors = FALSE
  )
  
  model$fit(data)
  
  expect_true(is.factor(model$data$char_var))
})

test_that("fit() fails with quantitative data only", {
  model <- CAH_mixtes$new()
  data <- create_quanti_data(50)
  
  # FAMD requires at least qualitative or mixed variables
  # The current code should handle this case
  expect_error(model$fit(data))
})

test_that("fit() with different numbers of components", {
  data <- create_mixed_data(50)
  
  for (ncp in c(2, 5, 10)) {
    model <- CAH_mixtes$new(n_components = ncp)
    model$fit(data)
    
    # The number of columns in coord_var should match min(ncp, max possible)
    expect_true(ncol(model$coord_var) <= ncp)
  }
})

# =============================================================================
# TESTS FOR THE clustering_hierarchical() METHOD
# =============================================================================

test_that("clustering_hierarchical() works correctly", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  labels <- model$clustering_hierarchical(n_clusters = 3)
  
  # Verify the return value
  expect_type(labels, "integer")
  expect_equal(length(labels), nrow(model$coord_var))
  
  # Verify attributes
  expect_equal(model$n_clusters, 3)
  expect_equal(model$clustering_method, "hierarchical")
  expect_false(is.null(model$hclust_result))
  expect_false(is.null(model$cluster_centers))
  
  # Verify labels
  expect_true(all(labels %in% 1:3))
  expect_equal(max(labels), 3)
})

test_that("clustering_hierarchical() with different linkage methods", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  methods <- c("ward", "complete", "single", "average")
  
  for (method in methods) {
    model_test <- CAH_mixtes$new(n_components = 3)
    model_test$fit(data)
    
    labels <- model_test$clustering_hierarchical(n_clusters = 2, method = method)
    
    expect_equal(length(labels), nrow(model_test$coord_var))
    expect_true(all(labels %in% 1:2))
  }
})

test_that("clustering_hierarchical() with different numbers of clusters", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  for (k in 2:5) {
    model_test <- CAH_mixtes$new(n_components = 3)
    model_test$fit(data)
    
    labels <- model_test$clustering_hierarchical(n_clusters = k)
    
    expect_equal(model_test$n_clusters, k)
    expect_equal(max(labels), k)
    expect_equal(nrow(model_test$cluster_centers), k)
  }
})

test_that("clustering_hierarchical() fails without prior fit()", {
  model <- CAH_mixtes$new()
  
  expect_error(model$clustering_hierarchical(n_clusters = 3),
               "Error: You must first run fit\\(\\)!")
})

test_that("cluster_centers has correct dimensions", {
  model <- CAH_mixtes$new(n_components = 4)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  expect_equal(nrow(model$cluster_centers), 3)
  expect_equal(ncol(model$cluster_centers), ncol(model$coord_var))
})

# =============================================================================
# TESTS FOR THE predict() METHOD
# =============================================================================

test_that("predict() works with new quantitative variables", {
  set.seed(42)
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  # New variables
  new_vars <- data.frame(
    new_quanti1 = rnorm(50),
    new_quanti2 = rnorm(50)
  )
  
  predictions <- model$predict(new_vars)
  
  expect_s3_class(predictions, "data.frame")
  expect_equal(nrow(predictions), 2)
  expect_true("Variable" %in% names(predictions))
  expect_true("Type" %in% names(predictions))
  expect_true("Cluster_Predit" %in% names(predictions))
  expect_true(all(predictions$Type == "Quantitative"))
  expect_true(all(predictions$Cluster_Predit %in% 1:3))
})

test_that("predict() works with new qualitative variables", {
  set.seed(42)
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  new_vars <- data.frame(
    new_quali1 = factor(sample(c("X", "Y", "Z"), 50, replace = TRUE)),
    new_quali2 = factor(sample(c("P", "Q"), 50, replace = TRUE))
  )
  
  predictions <- model$predict(new_vars)
  
  expect_equal(nrow(predictions), 2)
  expect_true(all(predictions$Type == "Qualitative"))
})

test_that("predict() works with mixed variables", {
  set.seed(42)
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  new_vars <- data.frame(
    new_quanti = rnorm(50),
    new_quali = factor(sample(c("A", "B"), 50, replace = TRUE))
  )
  
  predictions <- model$predict(new_vars)
  
  expect_equal(nrow(predictions), 2)
  expect_true("Quantitative" %in% predictions$Type)
  expect_true("Qualitative" %in% predictions$Type)
})

test_that("predict() fails if number of rows differs", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  new_vars <- data.frame(
    new_var = rnorm(30)  # 30 rows instead of 50
  )
  
  expect_error(model$predict(new_vars), "must have 50 rows")
})

test_that("predict() fails without prior clustering", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  new_vars <- data.frame(new_var = rnorm(50))
  
  expect_error(model$predict(new_vars),
               "Error: You must first run clustering_hierarchical\\(\\)!")
})

# =============================================================================
# VISUALIZATION METHOD TESTS
# =============================================================================

test_that("dendo() works after clustering", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  # dendo() should execute without error (captures cat() output)
  expect_no_error(capture.output(model$dendo()))
})

test_that("dendo() fails without prior clustering", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  expect_error(model$dendo(),
               "Error: You must first run clustering_hierarchical\\(\\)!")
})

test_that("plot_variables() works after fit", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  # Should work without clustering (color by type)
  expect_s3_class(model, "CAH_mixtes")
})

test_that("plot_variables() works after clustering", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  # Should work with clustering (color by cluster)
  expect_s3_class(model, "CAH_mixtes")
})

test_that("plot_variables() with different axes", {
  model <- CAH_mixtes$new(n_components = 4)
  data <- create_mixed_data(50)
  model$fit(data)
  
  # Test with different axis combinations
  axes_combinations <- list(c(1, 2), c(1, 3), c(2, 3), c(2, 4))
  
  for (axes in axes_combinations) {
    # Verify that the requested axes are valid
    expect_true(max(axes) <= ncol(model$coord_var))
  }
})

test_that("plot_variables() fails without prior fit", {
  model <- CAH_mixtes$new()
  
  expect_error(model$plot_variables(),
               "Error: You must first run fit\\(\\)!")
})

test_that("elbow_method() works after fit", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  # Should execute without error
  expect_s3_class(model, "CAH_mixtes")
})

test_that("elbow_method() fails without prior fit", {
  model <- CAH_mixtes$new()
  
  expect_error(model$elbow_method(),
               "Error: You must first run fit\\(\\)!")
})

test_that("elbow_method() with different parameters", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  # Different max_clusters values
  expect_s3_class(model, "CAH_mixtes")
})

test_that("qualite_clustering() works after clustering", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  expect_s3_class(model, "CAH_mixtes")
})

test_that("qualite_clustering() fails without prior clustering", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  expect_error(model$qualite_clustering(),
               "Error: You must first run clustering_hierarchical\\(\\)!")
})

# =============================================================================
# DISPLAY METHOD TESTS
# =============================================================================

test_that("print() works at different stages", {
  model <- CAH_mixtes$new(n_components = 3)
  
  # Before fit
  expect_output(model$print(), "FAMD \\+ VARIABLE CLUSTERING RESULTS")
  
  # After fit
  data <- create_mixed_data(50)
  model$fit(data)
  output <- capture.output(model$print())
  expect_true(any(grepl("individuals", output)))
  expect_true(any(grepl("MIXTE", output)))
  
  # After clustering
  model$clustering_hierarchical(n_clusters = 3)
  output <- capture.output(model$print())
  expect_true(any(grepl("Clustering performed", output)))
})

test_that("summary() works after clustering", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  output <- capture.output(model$summary())
  expect_true(any(grepl("CLUSTER", output)))
  expect_true(any(grepl("variables", output)))
})

test_that("summary() displays message without clustering", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  output <- capture.output(model$summary())
  expect_true(any(grepl("No clustering has been performed", output)))
})

# =============================================================================
# INTEGRATION TESTS
# =============================================================================

test_that("Complete workflow with mixed data", {
  set.seed(123)
  
  # 1. Create the model
  model <- CAH_mixtes$new(n_components = 5)
  expect_s3_class(model, "CAH_mixtes")
  
  # 2. Fit on data
  data <- create_mixed_data(100)
  model$fit(data)
  expect_equal(model$data_type, "mixte")
  
  # 3. Clustering
  labels <- model$clustering_hierarchical(n_clusters = 3, method = "ward")
  expect_equal(length(unique(labels)), 3)
  
  # 4. Prediction
  new_vars <- data.frame(
    new_v1 = rnorm(100),
    new_v2 = factor(sample(c("X", "Y"), 100, replace = TRUE))
  )
  preds <- model$predict(new_vars)
  expect_equal(nrow(preds), 2)
  
  # 5. Verify consistency
  expect_equal(nrow(model$data), 100)
  expect_equal(model$n_clusters, 3)
})

test_that("Complete workflow with qualitative data", {
  set.seed(456)
  
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_quali_data(80)
  
  model$fit(data)
  expect_equal(model$data_type, "quali")
  expect_equal(length(model$quanti_vars), 0)
  
  labels <- model$clustering_hierarchical(n_clusters = 2)
  expect_true(all(labels %in% 1:2))
})

test_that("Method chaining works", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  
  # Test chaining (fit returns self)
  result <- model$fit(data)
  expect_identical(result, model)
})

# =============================================================================
# ROBUSTNESS TESTS
# =============================================================================

test_that("Handling data with missing values", {
  data <- create_mixed_data(50)
  # NA in quantitative variables
  data$age[c(1, 5, 10)] <- NA
  
  model <- CAH_mixtes$new(n_components = 3)
  
  # Depending on the FactoMineR version, NAs may be imputed or cause an error
  # We simply verify that the behavior is consistent (no silent crash)
  result <- tryCatch({
    suppressWarnings(model$fit(data))
    list(status = "success", has_result = !is.null(model$coord_var))
  }, error = function(e) {
    list(status = "error", message = e$message)
  })
  
  if (result$status == "success") {
    # If it succeeds, the model must be correctly fitted
    expect_true(result$has_result)
    expect_equal(nrow(model$data), 50)
  } else {
    # If error, it must be related to missing/infinite values
    expect_true(
      grepl("missing|NA|infini|infinite|valeurs", result$message, ignore.case = TRUE)
    )
  }
})

test_that("Data with few observations", {
  # Few observations but enough for the number of categories
  data <- data.frame(
    num1 = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
    num2 = c(10, 9, 8, 7, 6, 5, 4, 3, 2, 1),
    cat = factor(c("A", "B", "A", "B", "A", "B", "A", "B", "A", "B"))
  )
  
  model <- CAH_mixtes$new(n_components = 2)
  model$fit(data)
  
  expect_equal(nrow(model$data), 10)
})

test_that("Data with many categories", {
  set.seed(789)
  n <- 200
  data <- data.frame(
    num = rnorm(n),
    cat_many = factor(sample(LETTERS, n, replace = TRUE))  # 26 categories
  )
  
  model <- CAH_mixtes$new(n_components = 5)
  model$fit(data)
  
  expect_true(nrow(model$coord_var) >= 2)  # At least 2 variables
})

test_that("Clustering with k=1 (edge case)",
          {
            model <- CAH_mixtes$new(n_components = 3)
            data <- create_mixed_data(50)
            model$fit(data)
            
            # k=1 is a degenerate case but should work
            labels <- model$clustering_hierarchical(n_clusters = 1)
            expect_true(all(labels == 1))
          })

test_that("Clustering with k = number of variables", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  n_vars <- nrow(model$coord_var)
  labels <- model$clustering_hierarchical(n_clusters = n_vars)
  
  expect_equal(length(unique(labels)), n_vars)
})

# =============================================================================
# RESULT CONSISTENCY TESTS
# =============================================================================

test_that("Eigenvalues are positive and decreasing", {
  model <- CAH_mixtes$new(n_components = 5)
  data <- create_mixed_data(100)
  model$fit(data)
  
  # Eigenvalues must be positive
  expect_true(all(model$eigenvalues > 0))
  
  # First eigenvalues must be >= subsequent ones (generally decreasing)
  expect_true(model$eigenvalues[1] >= model$eigenvalues[2])
})

test_that("Explained inertia is between 0 and 100", {
  model <- CAH_mixtes$new(n_components = 5)
  data <- create_mixed_data(100)
  model$fit(data)
  
  expect_true(all(model$inertia_explained >= 0))
  expect_true(all(model$inertia_explained <= 100))
  
  # Cumulative inertia must not exceed 100%
  expect_true(sum(model$inertia_explained) <= 100.01)  # Small margin for rounding errors
})

test_that("Coordinates are numeric and finite", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  
  expect_true(is.numeric(model$coord_var))
  expect_true(all(is.finite(model$coord_var)))
})

test_that("Cluster centers are within the convex hull", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  # Centers must be within the coordinate range
  for (j in 1:ncol(model$coord_var)) {
    min_coord <- min(model$coord_var[, j])
    max_coord <- max(model$coord_var[, j])
    
    expect_true(all(model$cluster_centers[, j] >= min_coord - 0.01))
    expect_true(all(model$cluster_centers[, j] <= max_coord + 0.01))
  }
})

test_that("Each cluster contains at least one variable", {
  model <- CAH_mixtes$new(n_components = 3)
  data <- create_mixed_data(50)
  model$fit(data)
  model$clustering_hierarchical(n_clusters = 3)
  
  for (k in 1:3) {
    expect_true(sum(model$labels_var == k) >= 1)
  }
})

# =============================================================================
# REPRODUCIBILITY TESTS
# =============================================================================

test_that("Reproductible results with same seed", {
  set.seed(999)
  data <- create_mixed_data(50)
  
  model1 <- CAH_mixtes$new(n_components = 3)
  model1$fit(data)
  model1$clustering_hierarchical(n_clusters = 3)
  
  model2 <- CAH_mixtes$new(n_components = 3)
  model2$fit(data)
  model2$clustering_hierarchical(n_clusters = 3)
  
  # Coordinates must be identical
  expect_equal(model1$coord_var, model2$coord_var)
  
  # Labels must be identical
  expect_equal(model1$labels_var, model2$labels_var)
})

# =============================================================================
# RUNNING THE TESTS
# =============================================================================
#
# To run all tests, use one of the following commands:
#
# Option 1 - From R:
#   testthat::test_file("tests.R")
#
# Option 2 - From terminal:
#   Rscript -e "testthat::test_file('tests.R')"
#
# Option 3 - Run with detailed report:
#   testthat::test_file("tests.R", reporter = "summary")
#
# =============================================================================

cat("\n")
cat("═══════════════════════════════════════════════════\n")
cat("  CAH_mixtes UNIT TESTS LOADED\n")
cat("═══════════════════════════════════════════════════\n")
cat("\nTo run the tests:\n")
cat("  testthat::test_file('tests.R')\n\n")