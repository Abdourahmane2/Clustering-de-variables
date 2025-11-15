#' Hierarchical Clustering on Variables (CAH)
#'
#' @name CAH
#' @title Hierarchical Clustering on Variables
#'
#' @description
#' An R6 class for performing hierarchical clustering on variables using
#' correlation-based distances. This implementation uses Ward's method and
#' provides tools for partitioning variables into clusters, creating latent
#' components via PCA, and assigning new variables to existing clusters.
#'
#' @details
#' The CAH class implements variable clustering with the following features:
#' \itemize{
#'   \item Automatic data validation and cleaning
#'   \item Correlation-based distance matrix
#'   \item Ward's hierarchical clustering method (ward.D2)
#'   \item Automatic optimal k detection using elbow method
#'   \item PCA-based latent components for each cluster
#'   \item Prediction of new variable assignments
#' }
#'
#' The algorithm groups variables that are highly correlated, allowing for
#' dimensionality reduction and better interpretation of complex datasets.
#' Each cluster is represented by a latent component (first principal component)
#' that captures the common information shared by variables in that cluster.
#'
#' @section Methods:
#' \describe{
#'   \item{\code{new(method = "ward.D2")}}{
#'     Initialize a new CAH object.
#'     \itemize{
#'       \item \code{method}: Aggregation method for hierarchical clustering
#'             (default: "ward.D2")
#'     }
#'   }
#'
#'   \item{\code{fit(data)}}{
#'     Fit the hierarchical clustering model on the provided data.
#'
#'  \strong{Parameters:}
#'   \itemize{
#'    \item \code{data}: A data.frame or matrix with at least 2 individuals
#'             (rows) and 2 variables (columns)
#'     }
#'
#'  \strong{The method performs:}
#'  \enumerate{
#'    \item Data validation and type checking
#'    \item Removal of qualitative variables (factors, characters)
#'    \item Removal of constant variables (zero variance)
#'    \item Handling of missing values (complete cases only)
#'    \item Computation of correlation matrix
#'    \item Creation of correlation-based distance matrix
#'    \item Hierarchical clustering using Ward's method
#'    \item Automatic detection of optimal k using elbow method
#'  }
#'
#'  \strong{Returns:} Self (invisibly) for method chaining
#'   }
#'
#'  \item{\code{cutree(k = NULL)}}{
#'  Cut the dendrogram to create k clusters and compute latent components.
#'
#'  \strong{Parameters:}
#'   \itemize{
#'    \item \code{k}: Number of clusters (if NULL, uses automatically
#'             detected best_k). Must be between 1 and (number of variables - 1).
#'     }
#'
#'  \strong{Returns:} Named integer vector of cluster assignments
#'     For each cluster, a latent component is created using PCA on the
#'     standardized variables within that cluster. The latent component
#'     represents the first principal axis and captures the maximum variance.
#'   }
#'
#'   \item{\code{predict(X_new)}}{
#'     Assign new variables to existing clusters based on correlation with
#'     latent components.
#'
#'  \strong{Parameters:}
#'    \itemize{
#'     \item \code{X_new}: A data.frame with the same number of rows as the
#'             *original data. Must not contain missing values.
#'       }
#'  \strong{Returns:} Self (invisibly)
#'     New variables are standardized and assigned to the cluster whose latent
#'     component has the highest absolute correlation. The assignment is stored
#'     in the \code{predict_result} field.
#'   }
#'
#'   \item{\code{print(...)}}{
#'     Print a concise summary of the CAH object showing:
#'     \itemize{
#'       \item Data dimensions
#'       \item Clustering method
#'       \item Average correlation
#'       \item Optimal k
#'       \item Cluster sizes
#'       \item Number of supplementary variables
#'     }
#'   }
#'
#'   \item{\code{summary(...)}}{
#'     Print a detailed summary including:
#'     \itemize{
#'       \item Model information (method, correlation, optimal k)
#'       \item Distribution of variables per cluster
#'       \item List of variables in each cluster
#'       \item Local PCA results (squared correlations with latent component)
#'       \item Representative variable (parangon) for each cluster
#'       \item Supplementary variables and their assignments
#'       \item Complete partition (active + supplementary variables)
#'     }
#'   }
#' }
#'
#' @section Mathematical Details:
#'
#' \strong{Distance calculation:}
#'
#' The distance between two variables \eqn{X_i} and \eqn{X_j} is computed as:
#'
#' \deqn{d(X_i, X_j) = \sqrt{2(1 - |r_{ij}|)}}
#'
#' where \eqn{r_{ij}} is the Pearson correlation coefficient between \eqn{X_i}
#' and \eqn{X_j}. This distance ranges from 0 (perfect correlation) to
#' \eqn{\sqrt{2}} (perfect anti-correlation or independence).
#'
#' \strong{Ward's criterion:}
#'
#' Ward's method minimizes the within-cluster variance. The distance between
#' two clusters \eqn{C_i} and \eqn{C_j} is:
#'
#' \deqn{\Delta(C_i, C_j) = \frac{n_i n_j}{n_i + n_j} ||m_i - m_j||^2}
#'
#' where \eqn{n_i} and \eqn{n_j} are cluster sizes, and \eqn{m_i} and \eqn{m_j}
#' are cluster centroids in the variable space.
#'
#' \strong{Optimal k detection:}
#'
#' The optimal number of clusters is detected using the elbow method on the
#' dendrogram heights. The algorithm identifies the point of maximum curvature
#' by computing the second derivative of the height sequence:
#'
#' \deqn{k_{opt} = \arg\min_{i} \Delta^2 h_i}
#'
#' where \eqn{\Delta^2 h_i = (h_{i+1} - h_i) - (h_i - h_{i-1})} is the
#' discrete second derivative.
#'
#' \strong{Latent component:}
#'
#' For each cluster k, the latent component \eqn{Z_k} is the first principal
#' component obtained by PCA on the standardized variables within the cluster:
#'
#' \deqn{Z_k = \sum_{j \in C_k} a_j X_j^*}
#'
#' where \eqn{X_j^*} are the standardized variables and \eqn{a_j} are the
#' loadings maximizing the explained variance. The standardization is performed
#' locally within each cluster to ensure comparability.
#'
#' \strong{Variable assignment (prediction):}
#'
#' A new variable \eqn{X_{new}} is assigned to the cluster k that maximizes:
#'
#' \deqn{k^* = \arg\max_k |r(X_{new}^*, Z_k)|}
#'
#' where \eqn{X_{new}^*} is the standardized new variable and \eqn{r} denotes
#' the Pearson correlation coefficient.
#'
#' @section Public Fields:
#' \describe{
#'   \item{\code{data}}{Data.frame containing the cleaned input data (quantitative
#'         variables only, no missing values)}
#'   \item{\code{X_last}}{Data.frame of the last variables used in predict().
#'         Used for displaying correlations in summary()}
#'   \item{\code{method}}{Character string of aggregation method (default: "ward.D2")}
#'   \item{\code{dist_method}}{Character string of distance method (always "correlation")}
#'   \item{\code{corr_moy}}{Numeric value of average absolute correlation between
#'         all pairs of variables. Ranges from 0 (no correlation) to 1 (perfect correlation)}
#'   \item{\code{dist_matrix}}{Distance matrix of class "dist" computed from correlations}
#'   \item{\code{hc}}{Hierarchical clustering object of class "hclust". Contains the
#'         dendrogram structure and can be plotted with plot()}
#'   \item{\code{best_k}}{Integer, optimal number of clusters detected automatically
#'         using the elbow method}
#'   \item{\code{clusters}}{Named integer vector of cluster assignments for each
#'         variable. Names are variable names, values are cluster numbers (1 to k)}
#'   \item{\code{predict_result}}{Named integer vector of cluster assignments for
#'         new variables added via predict()}
#'   \item{\code{compo_latent}}{List of latent components, one per cluster. Each
#'         element contains:
#'     \itemize{
#'       \item \code{Zk}: Numeric vector of principal component scores (length = n individuals)
#'       \item \code{vars}: Character vector of variable names in the cluster
#'       \item \code{cor_vals}: Numeric vector of correlations between variables and Zk
#'       \item \code{scaled_data}: Matrix of standardized data for variables in the cluster
#'     }
#'   }
#' }
#'
#' @note
#' \strong{Important considerations:}
#' \itemize{
#'   \item The method works best with at least 4 variables for stable results
#'   \item All variables are automatically standardized within each cluster before PCA
#'   \item Missing values are removed using complete cases (listwise deletion)
#'   \item Constant variables (zero variance) are automatically excluded
#'   \item Qualitative variables (factors, characters) are automatically excluded
#'   \item The correlation-based distance is scale-invariant
#'   \item For large datasets (>1000 variables), consider using a subset or
#'         pre-filtering correlated variables
#'   \item The number of clusters k must be between 1 and (number of variables - 1)
#' }
#'
#' \strong{Interpretation guidelines:}
#' \itemize{
#'   \item Variables in the same cluster are highly correlated and measure similar concepts
#'   \item The latent component (Zk) represents the common information shared by
#'         variables in a cluster
#'   \item Use \code{summary()} to identify the most representative variable (parangon)
#'         in each cluster - this is the variable with the highest squared correlation
#'         with the latent component
#'   \item The parangon can be used as a single representative variable for the entire cluster
#'   \item Squared correlations (R²) in the summary indicate how well each variable is
#'         represented by the latent component (values close to 1 are better)
#'   \item The average correlation (\code{corr_moy}) gives an overall measure of
#'         redundancy in the dataset
#' }
#'
#' \strong{Computational complexity:}
#' \itemize{
#'   \item Time: O(n²p + p²n) where n = individuals, p = variables
#'   \item Space: O(p²) for correlation and distance matrices
#'   \item The bottleneck is typically the correlation matrix computation for large n
#' }
#'
#' @return
#' An R6 object of class CAH. The object is mutable and methods modify it in-place.
#' Most methods return \code{self} invisibly to allow method chaining:
#'
#' \code{cah$fit(data)$cutree(k = 3)$print()}
#'
#' The \code{cutree()} method additionally returns the cluster assignment vector.
#'
#' @examples
#' # ═══════════════════════════════════════════════════════════
#' # Example 1: Basic usage with synthetic data
#' # ═══════════════════════════════════════════════════════════
#' set.seed(123)
#' df <- data.frame(
#'   var1 = rnorm(30, 10, 2),   # Group 1
#'   var2 = rnorm(30, 12, 2),   # Group 1 (correlated with var1)
#'   var3 = rnorm(30, 50, 5),   # Group 2
#'   var4 = rnorm(30, 52, 5)    # Group 2 (correlated with var3)
#' )
#'
#' # Initialize and fit
#' cah <- CAH$new()
#' cah$fit(df)
#'
#' # View optimal k
#' cat("Optimal number of clusters:", cah$best_k, "\n")
#'
#' # Partition into clusters
#' cah$cutree(k = 2)
#'
#' # View concise results
#' cah$print()
#'
#' # View detailed results
#' cah$summary()
#'
#' # ═══════════════════════════════════════════════════════════
#' # Example 2: Adding supplementary variables
#' # ═══════════════════════════════════════════════════════════
#'
#' # Create new variables similar to existing ones
#' new_vars <- data.frame(
#'   var5 = df$var1 * 0.9 + rnorm(30, 0, 0.5),  # Similar to var1
#'   var6 = df$var3 * 0.9 + rnorm(30, 0, 1)     # Similar to var3
#' )
#'
#' # Assign to existing clusters
#' cah$predict(new_vars)
#'
#' # View assignments
#' print(cah$predict_result)
#'
#' # ═══════════════════════════════════════════════════════════
#' # Example 3: Method chaining
#' # ═══════════════════════════════════════════════════════════
#' \dontrun{
#' CAH$new()$fit(df)$cutree()$print()
#' }
#'
#' # ═══════════════════════════════════════════════════════════
#' # Example 4: Working with real data (mtcars)
#' # ═══════════════════════════════════════════════════════════
#' \dontrun{
#' data(mtcars)
#'
#' # Select numeric variables
#' df_cars <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]
#'
#' # Fit and cluster
#' cah_cars <- CAH$new()
#' cah_cars$fit(df_cars)
#' cah_cars$cutree()  # Uses optimal k automatically
#'
#' # Visualize dendrogram
#' plot(cah_cars$hc, main = "Vehicle Variables Clustering",
#'      xlab = "Variables", ylab = "Distance")
#' rect.hclust(cah_cars$hc, k = cah_cars$best_k, border = "red")
#'
#' # Identify representative variables
#' cah_cars$summary()
#'
#' # Extract cluster memberships
#' clusters <- cah_cars$clusters
#' print(clusters)
#'
#' # Get latent components for further analysis
#' for (i in seq_along(cah_cars$compo_latent)) {
#'   comp <- cah_cars$compo_latent[[i]]
#'   cat("\nCluster", i, ":\n")
#'   cat("  Variables:", paste(comp$vars, collapse = ", "), "\n")
#'   cat("  Variance explained:", round(var(comp$Zk), 2), "\n")
#' }
#' }
#'
#' # ═══════════════════════════════════════════════════════════
#' # Example 5: Handling different data quality issues
#' # ═══════════════════════════════════════════════════════════
#' \dontrun{
#' # Data with missing values
#' df_na <- data.frame(
#'   var1 = c(rnorm(25), rep(NA, 5)),
#'   var2 = rnorm(30),
#'   var3 = rnorm(30)
#' )
#'
#' cah_na <- CAH$new()
#' cah_na$fit(df_na)  # Will remove rows with NA (with warning)
#'
#' # Data with constant variables
#' df_const <- data.frame(
#'   var1 = rnorm(30),
#'   var2 = rep(5, 30),  # Constant - will be removed
#'   var3 = rnorm(30)
#' )
#'
#' cah_const <- CAH$new()
#' cah_const$fit(df_const)  # Will remove constant variable (with warning)
#'
#' # Data with qualitative variables
#' df_mixed <- data.frame(
#'   var1 = rnorm(30),
#'   var2 = rnorm(30),
#'   category = factor(rep(c("A", "B", "C"), 10))  # Will be removed
#' )
#'
#' cah_mixed <- CAH$new()
#' cah_mixed$fit(df_mixed)  # Will remove qualitative variable (with warning)
#' }
#'
#' # ═══════════════════════════════════════════════════════════
#' # Example 6: Accessing internal components
#' # ═══════════════════════════════════════════════════════════
#' \dontrun{
#' cah <- CAH$new()
#' cah$fit(df)
#' cah$cutree(k = 2)
#'
#' # Access correlation matrix through distance matrix
#' dist_mat <- as.matrix(cah$dist_matrix)
#'
#' # Get correlations from distances: r = 1 - (d²/2)
#' cor_mat <- 1 - (dist_mat^2 / 2)
#' print(cor_mat)
#'
#' # Access dendrogram heights
#' heights <- cah$hc$height
#' plot(heights, type = "b", main = "Dendrogram Heights",
#'      xlab = "Merge step", ylab = "Height")
#'
#' # Second derivative for elbow detection
#' d1 <- diff(heights)
#' d2 <- diff(d1)
#' plot(d2, type = "b", main = "Second Derivative (Elbow Detection)",
#'      xlab = "Step", ylab = "Curvature")
#' abline(v = which.min(d2), col = "red", lty = 2)
#' }
#'
#' @seealso
#' \strong{Related R functions:}
#' \itemize{
#'   \item \code{\link[stats]{hclust}} - Hierarchical clustering
#'   \item \code{\link[stats]{cutree}} - Cut dendrogram into groups
#'   \item \code{\link[stats]{prcomp}} - Principal component analysis
#'   \item \code{\link[stats]{cor}} - Correlation matrix
#'   \item \code{\link[stats]{dist}} - Distance matrix computation
#' }
#'
#' @references
#'
#' Rakotomalala, R. (2020). \emph{Classification ascendante hiérarchique}.
#' Université Lyon 2.
#' \url{https://eric.univ-lyon2.fr/ricco/cours/cours_classification_hierarchique.html}
#'
#' Rakotomalala, R. (2020). \emph{Caractérisation des classes en classification automatique}.
#' Université Lyon 2.
#'
#' Rakotomalala, R. (2019). \emph{Les classes R6 sous R (Programmation orientée objet sous R)}.
#' Université Lyon 2.
#'
#' Rakotomalala, R. \emph{Tanagra - Hierarchical Agglomerative Clustering with PCA}.
#' \url{http://tutoriels-data-mining.blogspot.com/}
#'
#' Husson, F., Lê, S., & Pagès, J. (2017). \emph{Exploratory Multivariate Analysis by Example Using R}
#' (2nd ed.). Chapman and Hall/CRC.
#
#' @export
#' @import R6

library (R6)

CAH <- R6Class(
  "CAH",

  public = list (
    #' @field data Data.frame containing the cleaned input data
    data = NULL,

    #' @field X_last Data.frame of last variables used in predict()
    X_last = NULL,

    #' @field method Character string of aggregation method (ward.D2)
    method = NULL,

    #' @field dist_method Distance method for variable clustering
    dist_method = "correlation",

    #' @field corr_moy Numeric, average correlation between variables
    corr_moy = NULL,

    #' @field hc Hierarchical clustering object (class "hclust")
    hc = NULL,

    #' @field best_k Integer, optimal number of clusters
    best_k = NULL,

    #' @field clusters Named integer vector of cluster assignments
    clusters = NULL,

    #' @field predict_result Named integer vector of new variable assignments
    predict_result = NULL,

    #' @field dist_matrix Distance matrix (dissimularity)
    dist_matrix = NULL,

    #' @field compo_latent List of latent components per cluster
    compo_latent = NULL, # Latent components per cluster


#' @description
#' Create a new CAH object (Constructor)
#' @param method Character string specifying the agglomeration method for
#'   hierarchical clustering. Default is "ward.D2"
#' @return A new `CAH` object
initialize = function(method = "ward.D2") {
  self$method <- method
  message ("[CAH] CAH initialized. Please call $fit(data) to fit the model.")
},

#' @description
#' Fit the hierarchical clustering model
#' @param data A data.frame or matrix with at least 2 rows and 2 columns

fit = function(data) {
  # Data validation
  if (!is.data.frame(data) && !is.matrix(data)){
    stop ("[CAH] It is not possible to perform CAH; the data must be in the form of a dataframe or a matrix.")
  }

  df <- as.data.frame(data)

  # Minimum size check
  if (nrow(df) < 3 || ncol(df) < 3 ) {
    stop("[CAH] Not enough data: To perform a CAH, you need at least 2 individuals and 2 variables.")
  }
  # Variable type verification
  type <- sapply (df, class)

  # - Quantivative variables (numeric)
  quanti_var <- names(type[type %in% c("numeric", "integer")])
  # - Qualitative variables (strings...)
  quali_var <- names(type[type %in% c("factor","character", "logical")])

  if (length(quali_var) > 0) {
    warning(" [CAH] There are qualitative variables in the dataframe:", paste(quali_var, collapse =", "), ". They will not be used for CAH.")
  }
  #We only keep quantitative variables
  df <- df[, quanti_var, drop = FALSE]

  # Remove constant variables (variance = 0 or NA)
  if (ncol(df) > 0) {
    variance <- sapply(df, function(x) var(x, na.rm = TRUE))
    var_zero <- names(variance[variance == 0])
    if (length(var_zero) > 0) {
      warning("[CAH] Constant variables are removed: ", paste(var_zero, collapse = ", "))
      df <- df[, !(names(df) %in% var_zero), drop = FALSE]
    }
  }

  # Check remaining variables
  if (ncol(df) == 0){
    stop("[CAH] There are no valid quantitative variables available for analysis.")
  }
  # Remove missing values
  if (anyNA(df)) {
    warning("[CAH] Removal of lines containing missing data")
    df <- na.omit(df)
  }
  # Check remaining individuals
  if (nrow(df) < 2){
    stop("[CAH] There are not enough individuals available for analysis (need at least 2).")
  }
  self$data <- df
  message("[CAH] The data is loaded ! There are :", nrow(self$data)," individuals and ", ncol(self$data), " variables.")


  # Correlation matrix (on raw data - correlation is scale-invariant)
  corr_matrix <- cor(self$data)
  self$corr_moy <- mean(abs(corr_matrix[upper.tri(corr_matrix)])) # Voir si je peux le placer dans le summary ou print car inutile dans calcule.
  #message("[CAH] Calculated correlation matrix (average correlation = ", round(self$corr_moy, 3), ").")

  # Distance matrix based on correlation
  self$dist_matrix <- as.dist(sqrt(2 * (1 - abs(corr_matrix))))
  #message("[CAH] Distance matrix created between variables (correlational).")

  # Hierarchical clustering
  self$hc <- hclust(self$dist_matrix, method = self$method)
  #message("[CAH] is performed using the method = ", method, ".")

  # Detect optimal k using elbow method
  h <- self$hc$height
  d1 <- diff(h)
  d2 <- diff(d1)
  idx <- which.min(d2) + 1
  self$best_k <- idx
  #message("[CAH] The ideal number of classes detected is: k =", self$best_k)

  invisible(self)

},

# ---- Cut tree method ----

#' @description
#' Cut the dendrogram to create clusters
#' @param k Integer, number of clusters. If NULL, uses best_k
#' @return Named integer vector of cluster assignments

cutree = function(k=NULL) {
  if (is.null(self$hc)){
    stop("[CAH] : Call $fit() first.")
  }
  if (is.null(k)){
    k<- self$best_k
  }
  n_vars <- ncol(self$data)
  if (k < 1 || k > n_vars - 1) {
    stop("[CAH] k doit être entre 1 et ", n_vars - 1,
         " (nombre de variables - 1). Valeur fournie : k = ", k)
  }

  cl <- cutree(self$hc, k=k)
  names(cl) <- colnames(self$data)

  self$clusters <- cl

  # Compute latent components for each cluster (PCA)
  self$compo_latent <- list()

  for (groupe in unique(cl)){
    var_groupe <- names(cl[cl == groupe])

    if (length(var_groupe) > 1) {
      #LOCAL STANDARDIZATION for PCA
      data_groupe_scaled <- scale(self$data[, var_groupe])

      acp <- prcomp(data_groupe_scaled, center = FALSE, scale. = FALSE)
      Zk <- acp$x[, 1]

      # Correlations with standardized data
      cor_vals <- cor(data_groupe_scaled, Zk)

      self$compo_latent[[as.character(groupe)]] <- list(
        Zk = Zk,
        vars = var_groupe,
        cor_vals = cor_vals,
        scaled_data = data_groupe_scaled # Save for the predict()
        #message("\nCluster, "group": latent component (1st PCA axis)")
        #print(round(cor_vals^2, 3))
      )
    } else {
      # Single variable: standardize it too
      data_scaled <- scale(self$data[, var_groupe])

      self$compo_latent[[as.character(groupe)]] <- list(
        Zk = as.vector(data_scaled),
        vars = var_groupe,
        cor_vals = 1,
        scaled_data = data_scaled
      )
    }
  }

  message("Partitioning completed: ", k, " clusters and calculated latent components. ")
  return(cl)
},

#---- Predict method ----

#' @description
#' Predict cluster assignments for new variables
#' @param X_new A data.frame with the same number of rows as the original data.
predict = function(X_new) {
  if (is.null(self$clusters)){
    stop("[CAH] first call $cutree().")
  }
  if (is.null(self$compo_latent)){
    stop("[CAH] Missing latent components (internal error).")
  }
  X <- as.data.frame(X_new)

  # Step 1 : Validation
  if (nrow(X) != nrow(self$data)) {
    stop("[CAH] The number of individuals must be identical to that in our base dataframe.")
  }
  if (anyNA(X)) {
    stop("[CAH] The data contains missing values (NA).")
  }

  self$X_last <- X

  # Assign new variables to cluster
  nouv_clusters <- rep(NA, ncol(X))
  names(nouv_clusters) <- colnames(X)


  for (j in seq_len(ncol(X))) {
    # STANDARDIZE the new variable
    var_data_scaled <- scale(X[, j])
    cor_latent <- c()

    # Compute correlation with each cluster's latent component
    for (groupe in unique(self$clusters)) {
      Zk <- self$compo_latent[[as.character(groupe)]]$Zk
      cor_latent[as.character(groupe)] <- abs(cor(var_data_scaled, Zk))

    }

    # Assign to cluster with highest correlation
    best_cluster <- as.numeric(names(which.max(cor_latent)))
    nouv_clusters[j] <- best_cluster
  }

  # Save the result
  self$predict_result <- nouv_clusters

  message("[CAH] Assignment of new variables to existing clusters completed.")
  print(nouv_clusters)

  invisible(self)

},

#' @description
#' Print a summary of the CAH object
#' @param Additional arguments (ignored)
#'
print = function(...) {
  cat("\n──────────────────────────────────────────────\n")
  cat("    Hierarchical Clustering on Variables (HCA)\n")
  cat("──────────────────────────────────────────────\n")

  # Checking the data
  if (is.null(self$data)) {
    cat(" No data loaded.\n")
    cat("Call $fit(data) to load and fit the model.\n")
    return(invisible(self))
  }

  # General informations
  cat(" Data: ", nrow(self$data), " individuals × ", ncol(self$data), " variables\n", sep = "")
  cat(" Aggregation method: ", ifelse(is.null(self$method), "non spécifiée", self$method), "\n")
  cat(" Distance method:", ifelse(is.null(self$dist_method), "corrélation", self$dist_method), "\n")

  # Correlation
  if (!is.null(self$corr_moy)) {
    cat(" Average correlation:", round(self$corr_moy, 3), "\n")
  }

  # Nombre optimal de clusters
  if (!is.null(self$best_k)) {
    cat(" Optimal k detected :", self$best_k, "\n")
  } else {
    cat(" Optimal k: not determined (call $fit())\n")
  }

  # Preparation of the clusters
  if (!is.null(self$clusters)) {
    tb <- table(self$clusters)
    cat(" Variables per cluster:\n")
    print(tb)
  } else {
    cat(" Partitioning: not done (call $cutree())\n")
  }

  # Illustrative variables added (via predict)
  if (!is.null(self$predict_result)) {
    cat("Supplementary variables added: ", length(self$predict_result), "\n")
  }

  cat("──────────────────────────────────────────────\n\n")
  invisible(self)
},

#' @description
#' Print a detailed summary of the CAH model
#' @param ... Additional arguments (ignored)
#'
summary = function(...) {
  cat("\n──────────────────────────────────────────────\n")
  cat("   HCA Model - Detailed Summary\n")
  cat("──────────────────────────────────────────────\n")

  # First Check
  if (is.null(self$hc)) {
    cat("No model fitted. Call $fit(data) first.")
    return(invisible(NULL))
  }

  # General information
  cat("• Aggregation method: ", self$method, "\n")
  cat("• Average correlation: ", round(self$corr_moy, 3), "\n")
  cat("• Optimal k: ", self$best_k, "\n\n")

  if (is.null(self$clusters)) {
    cat(" No partitioning done. Call $cutree() first.\n")
    return(invisible(NULL))
  }

  # Distribution of clusters
  cat("Distribution of active variables:\n")
  print(table(self$clusters))

  # List of variables per cluster
  cat("\n Variables per cluster:\n")
  for (grp in unique(self$clusters)) {
    vars_grp <- names(self$clusters[self$clusters == grp])
    cat("  -> Cluster", grp, "(", length(vars_grp), "variables) :", paste(vars_grp, collapse = ", "), "\n")

  }

  # Local PCA on each cluster (interpretation analysis)
  cat("\nLocal PCA (standardized within each cluster):\n")
    if (!is.null(self$compo_latent)) {
      for (group in names(self$compo_latent)) {
        compo <- self$compo_latent[[group]]

      cat("\n Cluster", group, ":\n")

      if (length(compo$vars) > 1 && !is.null(compo$cor_vals)) {
        cor_vec <- as.numeric(compo$cor_vals)
        names(cor_vec) <- compo$vars
        cat("  Squared correlations with latent component:\n")
        print(round(cor_vec^2, 3))
        best_var <- names(which.max(cor_vec^2))
        cat("-> Representative variable (parangon) :", best_var, "\n")
      } else {
        cat(" Single variable: ", compo$vars, "\n", sep = "")
      }
    }
  }

  # Information on illustrative variables (from predict)
  if (!is.null(self$predict_result)) {
    cat("\n──────────────────────────────────────────────\n")
    cat("Supplementary variables:\n")
    print(self$predict_result)

    # Correlations between new variables and latent components
    if (is.null(self$X_last)) {
      cat("\n Unable to display correlations for illustrative variables (X not retained in the object).\n")
    } else {
      cat("\n Correlations |r(X_j, Z_k)| of the new variables with each cluster:\n")

      nouv_vars <- names(self$predict_result)
      for (v in nouv_vars) {
        var_data_scaled <- scale(self$X_last[, v])
        cor_latent <- c()

        for (group in unique(self$clusters)) {
          Zk <- self$compo_latent[[as.character(group)]]$Zk
          cor_latent[as.character(group)] <- abs(cor(var_data_scaled, Zk))
        }

        cat("\n -> Variable", v, ":\n")
        print(round(cor_latent, 3))
        best_cl <- as.numeric(names(which.max(cor_latent)))
        cat("   Meilleure corrélation avec le cluster", best_cl, "\n")
      }
    }

    # New complete distribution (with illustrative variables)
    cat("\nComplete partition (active + supplementary):\n")
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































