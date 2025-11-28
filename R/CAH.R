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
#'   \item Quality metrics (R², Silhouette, n²)
#'   \item Multiple visualization types
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
#'     Also computes quality metrics : R², Silhouette scores, and n² (eta-squared)
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
#'     in the \code{predict_result} field. These are supplementary variables,
#'     which means they did not participate in cluster formation.
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
#'       \item BSS ratio and GAP statistics for cluster quality
#'       \item Variable membership
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
#'   \item{\code{plot(type = "dendrogramme")}}{
#'     Create visualizations of the CAH results.
#'
#'     \strong{Parameters:}
#'    \itemize{
#'     \item \code{type}: Type of plot to generate. Options are:
#'       \itemize{
#'         \item \code{"dendrogramme"}: Hierarchical clustering dendrogram with
#'               cluster rectangles (default)
#'         \item \code{"acp"}: PCA biplot showing variable correlations
#'         \item \code{"mds"}: Multidimensional scaling projection
#'         \item \code{"silhouette"}: Silhouette plot for cluster quality
#'         \item \code{"elbow"}: Elbow method plot for optimal k detection
#'       }
#'     }
#'  }
#'}
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
#' The optimal number of clusters is detected using a variation of the elbow
#' method applied to the dendrogram heights. Instead of computing curvature,
#' the algorithm identifies the largest jump between consecutive merge heights.
#'
#' \deqn{k_{opt} = \arg\max_{i} \Delta h_i + 1}
#'
#' where \eqn{\Delta h_i = h_{i+1} - h_i} represents the discrete first-order
#' difference between successive heights. A large value of \eqn{\Delta h_i}
#' indicates a natural separation between clusters. The optimal number of
#' clusters is thus selected just before the largest increase in height.
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
#' \strong{Quality Metrics:}
#'
#' \enumerate{
#'   \item \strong{R² (Coefficient of Determination):} Measures the proportion
#'         of variance explained by the clustering. Computed as the mean of η²
#'         values across all variables. Ranges from 0 to 1; higher values indicate
#'         better cluster cohesion.
#'
#'   \item \strong{Silhouette Score:} Measures how similar a variable is to its own
#'         cluster compared to other clusters. For variable i in cluster C:
#'
#'         \deqn{s(i) = \frac{b(i) - a(i)}{\max(a(i), b(i))}}
#'
#'         where a(i) is mean distance to variables in same cluster and
#'         b(i) is minimum mean distance to variables in other clusters.
#'         Ranges from -1 to 1; values > 0.6 indicate excellent clustering.
#'
#'   \item \strong{η² (Eta-squared):} Represents the squared correlation between
#'         a variable and its cluster's latent component. Indicates how well
#'         a variable is represented by the cluster. Ranges from 0 to 1.
#' }
#'
#' \strong{Cluster Quality Indices:}
#'
#' \enumerate{
#'   \item \strong{BSS Ratio:} Between-cluster sum of squares divided by total
#'         sum of squares. Higher values indicate more distinct clusters.
#'
#'   \item \strong{GAP statistic:} Measures the jump in BSS ratio at each k.
#'         The optimal k typically corresponds to the largest GAP value.
#' }
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
#' \item{\code{r2_info}}{List containing R² information:
#'     \itemize{
#'       \item \code{r_squared}: Numeric, mean η² across all variables
#'       \item \code{percentage}: Numeric, R² expressed as percentage
#'     }
#'   }
#'   \item{\code{silhouette}}{List containing silhouette analysis results:
#'     \itemize{
#'       \item \code{scores}: Numeric vector of silhouette scores per variable
#'       \item \code{mean_score}: Numeric, mean silhouette score for all variables
#'     }
#'   }
#'   \item{\code{eta2}}{Named numeric vector of η² values for each variable,
#'         sorted in descending order. Represents squared correlation with latent
#'         component. Higher values indicate variables better represented by their cluster}
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
#'   \item Supplementary variables (from predict()) do not affect the clustering
#'         structure; they are only assigned to existing clusters for illustration
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
#'   \item R² > 0.70 indicates good clustering; R² > 0.50 is acceptable
#'   \item Mean Silhouette > 0.6 indicates excellent structure; > 0.4 is acceptable
#'   \item η² > 0.8 for a variable indicates excellent representation in its cluster
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
#' # Example 3: Quality metrics interpretation
#' # ═══════════════════════════════════════════════════════════
#'
#' # After calling cutree(), access quality metrics:
#' cat("R² =", round(cah$r2_info$r_squared, 4), "\n")
#' cat("Mean Silhouette =", round(cah$silhouette$mean_score, 4), "\n")
#'
#' # View η² for each variable (quality of representation)
#' print(cah$eta2)
#'
#'
#' # ═══════════════════════════════════════════════════════════
#' # Example 4: Method chaining
#' # ═══════════════════════════════════════════════════════════
#' \dontrun{
#' CAH$new()$fit(df)$cutree()$print()
#' }
#'
#' # ═══════════════════════════════════════════════════════════
#' # Example 5: Working with real data (mtcars)
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
#' # Example 6: Visualizing clustering results
#' # ═══════════════════════════════════════════════════════════
#' \dontrun{
#' cah <- CAH$new()
#' cah$fit(df)
#' cah$cutree(k = 2)
#'
#' # Create all visualization types
#' cah$plot("dendrogramme")  # Dendrogram with cluster rectangles
#' cah$plot("acp")           # PCA biplot
#' cah$plot("mds")           # MDS projection
#' cah$plot("silhouette")    # Silhouette plot
#' cah$plot("elbow")         # Elbow method
#' }
#'
#' # ═══════════════════════════════════════════════════════════
#' # Example 7: Interpretation workflow for Shiny integration
#' # ═══════════════════════════════════════════════════════════
#' \dontrun{
#' # This workflow is ideal for Shiny applications where users
#' # can select active vs supplementary variables
#'
#' # Step 1: Fit on active variables
#' cah <- CAH$new()
#' active_vars <- df[, c("var1", "var2", "var3")]
#' cah$fit(active_vars)
#'
#' # Step 2: Choose optimal k
#' cah$cutree(k = cah$best_k)
#'
#' # Step 3: Print summary
#' cah$summary()
#'
#' # Step 4: Add supplementary variables for illustration
#' supp_vars <- df[, c("var4", "var5")]
#' cah$predict(supp_vars)
#'
#' # Step 5: Final detailed summary with both
#' cah$summary()
#'
#' # Step 6: Visualize
#' cah$plot("dendrogramme")
#' cah$plot("silhouette")
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
#'   \item \code{\link[stats]{cmdscale}} - Multidimensional scaling
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
#'
#' #' Kaufman, L., & Rousseeuw, P. J. (2005). \emph{Finding Groups in Data: An Introduction to
#' Cluster Analysis}. John Wiley & Sons.
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

    #' @field k_current Integer, current k used for cutree
    k_current = NULL,

    #' @field r2_info List containing R² metrics for each cluster and global R².
    r2_info = NULL,

    #' @field silhouette Numeric vector or list containing silhouette values
    silhouette = NULL,

    #' @field eta2 Named numeric vector containing η² (eta-squared) values
    eta2 = NULL,


    # ===== CONSTRUCTOR =====
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
    stop ("[CAH] It is not possible to perform CAH; The datas must be in the form of a dataframe or a matrix.")
  }

  df <- as.data.frame(data)

  # Minimum size check
  if (nrow(df) < 3 || ncol(df) < 3 ) {
    stop("[CAH] Not enough data: To perform a CAH, you need at least 3 individuals and 3 variables.")
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
  self$corr_moy <- mean(abs(corr_matrix[upper.tri(corr_matrix)]))
  #message("[CAH] Calculated correlation matrix (average correlation = ", round(self$corr_moy, 3), ").")

  # Distance matrix based on correlation
  self$dist_matrix <- as.dist(1 - abs(corr_matrix))
  #message("[CAH] Distance matrix created between variables (correlational) -> pearson absolute.")

  # Hierarchical clustering
  self$hc <- hclust(self$dist_matrix, method = self$method)
  #message("[CAH] is performed using the method = ", method, ".")

  # Detect optimal k using elbow method
  h <- self$hc$height # Length = p - 1

  if (length(h) >= 2){
    diff_h <- diff(h)
    idx_jump <- which.max(diff_h) # height jumps between merges (length = p - 2)

    p <- ncol(self$data)

    # Number of cluster if we cut just before the biggest jump
    self$best_k <- p - idx_jump

  }else{
    self$best_k <- 2
  }
  # Start from k=3 (ignore 2-class division which is often an artifact)

  self$best_k <- max(2, min(self$best_k, ncol(self$data) - 1))
  #message("[CAH] The ideal number of classes detected is: k =", self$best_k)

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

  # Validate k
  if (!is.numeric(k) || length(k) != 1) {
    stop("[CAH] k must be a single numeric value.")
  }

  if (k < 2 || k > (n_vars - 1)) {
    stop("[CAH] k must be between 2 and ", n_vars - 1,
         " (number totale of variables - 1). Provided value : k = ", k)
  }

  cl <- cutree(self$hc, k=k)
  names(cl) <- colnames(self$data) # Labeling

  self$clusters <- cl
  self$k_current <- k

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
  self$r2_info <- private$compute_r_squared()
  self$silhouette <- private$compute_silhouette()
  self$eta2 <- private$compute_eta_squared()

  message("[CAH] Partitioning completed: ", k, " clusters and calculated latent components. ")
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
  nouv_clusters <- setNames(rep(NA, ncol(X)), colnames(X))

  for (var_name in colnames(X)) {

    var_data <- X[[var_name]]



    # Must be numeric
    if (!is.numeric(var_data)) {
      warning("[CAH] Variable '", var_name, "' is not numeric and will be skipped.")
      nouv_clusters[var_name] <- NA
      next
    }

  #Locale Standardization
  var_scaled <- as.vector(scale(var_data))

    # Calcul des corrélations avec chaque composante latente Z_k
    cor_latent <- c()
  for (groupe in names(self$compo_latent)) {
    Zk <- self$compo_latent[[groupe]]$Zk
    cor_latent[groupe] <- abs(cor(var_scaled, Zk))
  }

  # Choix du cluster = argmax_k |corr|
  best_cluster <- as.numeric(names(which.max(cor_latent)))
  nouv_clusters[var_name] <- best_cluster
  }

  # Save the result
  self$predict_result <- nouv_clusters

  message("[CAH] Assignment of new variables to existing clusters completed.")
  print(nouv_clusters)

  invisible(self)
},

#' @description
#' Plot various visualizations of the CAH model
#' @param type Character string specifying plot type
plot = function(type = "dendrogramme") {
  if (is.null(self$hc)){
    stop("[CAH] Call $fit() first.")
  }
  type <- tolower(type)

  # Validate plot type
  valid_types <- c("dendrogramme", "acp", "mds", "silhouette", "elbow")
  if (!type %in% valid_types) {
    stop("[CAH] Unknown plot type: '", type, "'. Use one of: ",
         paste(valid_types, collapse = ", "))
  }

  switch(type,
         "dendrogramme" = private$plot_dendrogramme(),
         "acp" = private$plot_acp(),
         "mds" = private$plot_mds(),
         "silhouette" = private$plot_silhouette(),
         "elbow" = private$plot_elbow()
         )
},

#' @description
#' Print a summary of the CAH object
#' @param ... Additional arguments (ignored)
#'
print = function(...) {
  cat("\n──────────────────────────────────────────────\n")
  cat("       Hierarchical Clustering on Variables\n")
  cat("──────────────────────────────────────────────\n")

  # --- CHECK DATA ---
  if (is.null(self$data)) {
    cat(" No data loaded.\nCall $fit(data) first.\n")
    return(invisible(self))
  }

  # --- GENERAL INFO ---
  cat(" Data: ", nrow(self$data), " individuals × ", ncol(self$data), " variables\n", sep="")
  cat(" Aggregation method: ", self$method, "\n")
  cat(" Distance method: ", self$dist_method, "\n")
  cat(" Average correlation: ", round(self$corr_moy, 3), "\n")

  if (!is.null(self$best_k))
    cat(" Optimal k detected: ", self$best_k, "\n")

  if (is.null(self$clusters)) {
    cat("\n Partitioning not performed yet. Call $cutree().\n")
  }

  # =====================================================
  #                1. CLUSTERS SUMMARY
  # =====================================================
  cat("\n1. CLUSTERS SUMMARY\n")
  cat("______________________________\n")

  tb <- table(self$clusters)
  for (g in sort(unique(self$clusters))) {
    cat(" Cluster ", g, " (", tb[g], " variables)\n", sep="")
  }

  # =====================================================
  #       2. BEST K — BSS Ratio + GAP (SAFE VERSION)
  # =====================================================
  cat("\n2. BEST CLUSTER SELECTION (Internal)\n")
  cat("____________________________________\n")

  p <- ncol(self$data)
  ks <- 2:(p-1)

  BSS_ratio_values <- numeric(length(ks))
  names(BSS_ratio_values) <- ks

  # Save original partition
  old_clusters <- self$clusters

  # Compute BSS ratio for each k
  for (i in seq_along(ks)) {
    k <- ks[i]

    # Temporary clustering
    cl_temp <- cutree(self$hc, k = k)
    self$clusters <- cl_temp

    BSS_ratio_values[i] <- private$compute_BSS_ratio()
  }

  # Restore the true partition!
  self$clusters <- old_clusters

  # GAP computation
  GAP_values <- private$compute_gap(BSS_ratio_values)

  cat("Clusters |  BSS Ratio  |   GAP\n")
  for (i in seq_along(ks)) {
    cat(sprintf("%7d  |   %8.4f  |  %6.3f\n",
                ks[i], BSS_ratio_values[i], GAP_values[i]))
  }
  cat(" → Best k = largest GAP jump.\n")

  # =====================================================
  #                3. CLUSTER CENTROIDS
  # =====================================================
  cat("\n3. CLUSTER CENTROIDS (Barycenters)\n")
  cat("____________________________________\n")

  centroids <- private$compute_centroids()
  vars <- colnames(self$data)

  mat <- sapply(centroids, function(c) c[vars])
  rownames(mat) <- vars

  print(round(mat, 3))

  # =====================================================
  #          4. VARIABLE → CLUSTER MEMBERSHIP
  # =====================================================
  cat("\n4. VARIABLES MEMBERSHIP\n")
  cat("____________________________________\n")

  for (v in names(old_clusters)) {
    cat("  ", v, " → Cluster ", old_clusters[v], "\n", sep="")
  }

  cat("\n──────────────────────────────────────────────\n")
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
  cat("\n1. DATA & MODEL \n")
  cat("_____________________________\n")
  cat("Individuals: ", nrow(self$data), "\n")
  cat("Active variables: ", ncol(self$data), "\n")
  cat("• Aggregation method: ", self$method, "\n")
  cat("• Average correlation: ", round(self$corr_moy, 3), "\n")
  cat("• Optimal k detected : ", self$best_k, "\n\n")

  if (is.null(self$clusters)) {
    cat(" No partitioning done. Call $cutree() first.\n")
    return(invisible(NULL))
  }

  cat("\n2. CLUSTERING RESULTS\n")
  cat("_____________________________\n")
  cat("Number of groups: ", length(unique(self$clusters)), "\n")

  cat("\nGroup sizes:\n")
  for (g in sort(unique(self$clusters))) {
    n_vars <- sum(self$clusters == g)
    cat("  Group ", g, ": ", n_vars, " variables\n", sep = "")
  }

  # ===== SECTION 3 : QUALITY METRICS =====
  cat("\n3. QUALITY METRICS\n")
  cat("_____________________________\n")

  r2_info <- self$r2_info
  sil <- self$silhouette
  eta2 <- self$eta2

  cat(sprintf("R² = %.4f (%.2f%%)\n", r2_info$r_squared, r2_info$percentage))
  cat(sprintf("  → Interpretation: Grouping explains %.2f%% of variance\n", r2_info$percentage))

  cat(sprintf("\nMean Silhouette = %.4f\n", sil$mean_score))
  cat(sprintf("  → %s\n",
              if (sil$mean_score > 0.6) "✓✓ Excellent (>0.6)"
              else if (sil$mean_score > 0.4) "✓ Good (0.4-0.6)"
              else "~ Acceptable (<0.4)"))

  # ===== SECTION 4 : VARIABLE QUALITY =====
  cat("\n4. VARIABLE QUALITY (η²)\n")
  cat("__________________________\n")

  if (length(eta2) > 0) {
    cat("Top variables by discriminant power:\n")
    for (i in 1:min(5, length(eta2))) {
      cat(sprintf("  %d. %s: %.4f %s\n", i, names(eta2)[i], eta2[i],
                  if (eta2[i] > 0.80) "✓✓" else if (eta2[i] > 0.60) "✓" else "~"))
    }
  } else {
    cat("No η² values computed.\n")
  }

  # ===== SECTION 5 : VARIABLES DISTRIBUTION =====
  cat("\n5. ACTIVE VARIABLES DISTRIBUTION\n")
  cat("__________________________\n")
  print(table(self$clusters))

  # ===== SECTION 6 : VARIABLES PER GROUP =====
  cat("\nVariables per group:\n")
  for (grp in sort(unique(self$clusters))) {
    vars_grp <- names(self$clusters[self$clusters == grp])
    cat("  Group ", grp, " (", length(vars_grp), "): ",
        paste(vars_grp, collapse = ", "), "\n", sep = "")
  }

  # ===== SECTION 7 : LOCAL PCA (LATENT COMPONENTS) =====
  cat("\n6. LOCAL PCA - LATENT COMPONENTS PER GROUP\n")
  cat("_______________________________________________\n")
  if (!is.null(self$compo_latent) && length(self$compo_latent) > 0) {
    for (group in sort(as.numeric(names(self$compo_latent)))) {
      compo <- self$compo_latent[[as.character(group)]]
      cat("\n  Group ", group, ":\n", sep = "")

      if (length(compo$vars) > 1 && !is.null(compo$cor_vals)) {
        cor_vec <- as.numeric(compo$cor_vals)
        names(cor_vec) <- compo$vars
        cat("    Correlations² with latent component:\n")
        print(round(cor_vec^2, 3))
        best_var <- names(which.max(cor_vec^2))
        cat("    → Representative variable (parangon): ", best_var, "\n", sep = "")
      } else {
        cat("    Single variable: ", compo$vars, "\n", sep = "")
      }
    }
  }

  # ===== SECTION 8 : SUPPLEMENTARY VARIABLES =====
  if (!is.null(self$predict_result)) {
    cat("\n7. SUPPLEMENTARY VARIABLES (from predict())\n")
    cat("__________________________________________\n")
    print(self$predict_result)

    if (!is.null(self$X_last)) {
      cat("\nCorrelations with group latent components:\n")

      nouv_vars <- names(self$predict_result)
      for (v in nouv_vars) {
        if (!is.na(self$predict_result[v])) {
          var_data <- self$X_last[, v]

          if (is.numeric(var_data)) {
            var_data_scaled <- scale(var_data)
            cor_latent <- c()

            for (group in unique(self$clusters)) {
              groupe_str <- as.character(group)
              if (!is.null(self$compo_latent[[groupe_str]])) {
                Zk <- self$compo_latent[[groupe_str]]$Zk
                cor_latent[groupe_str] <- abs(cor(var_data_scaled, Zk))
              }
            }

        cat("\n  Variable ", v, ":\n")
        print(round(cor_latent, 3))
        best_cl <- as.numeric(names(which.max(cor_latent)))
        cat("  → Assigned to Group ", best_cl, "\n")
          }
        }
      }
    }

    # Distribution complète
    cat("\n8. COMPLETE PARTITION (active + supplementary)\n")
    cat("___________________________________\n")
    all_clusters <- c(self$clusters, self$predict_result)
    for (grp in sort(unique(all_clusters))) {
      vars <- names(all_clusters[all_clusters == grp])
      cat("  Group ", grp, " (", length(vars), "): ",
          paste(vars, collapse = ", "), "\n", sep = "")
    }
  }

  cat("\n____________________________________\n\n")
  invisible(self)
    }
  ),

# ============== PRIVATE FUNCTIONS ====================

private = list(

  # Compute eta² (η²) values for each variable.
  # η² measures how well each variable is represented by its cluster's latent component.
  # Single-variable clusters get η² = 1; others use squared correlations.
  compute_eta_squared = function() {
    eta <- rep(0, length(self$clusters))
    names(eta) <- names(self$clusters)

    for (g in names(self$compo_latent)){
      comp <- self$compo_latent[[g]]

      if (length(comp$vars) == 1){
        eta[comp$vars] <- 1
      } else {
        eta[comp$vars] <- comp$cor_vals^2
      }
    }

    return(sort(eta, decreasing = TRUE))
  },

  # Compute global R² from η² values.
  # R² corresponds to the average η² across all variables.
  # Returns both raw R² and percentage explained variance.
  compute_r_squared = function() {
    eta2 <- private$compute_eta_squared()
    r2 <- mean(eta2)

    return(list(
      r_squared = r2,
      percentage = round(100 * r2, 2)
    ))
  },

  # Compute Between-Cluster Sum of Squares (BSS) for the current partition.
  # Each variable is treated as a point in ℝⁿ (n = individuals).
  # BSS measures separation between cluster barycenters.
  compute_BSS = function() {
    # Standardisation des variables (colonnes)
    X <- scale(self$data)              # n × p
    clusters <- self$clusters
    if (is.null(clusters)) stop("[CAH] No clusters available for BSS.")

    # Chaque variable est un point en dimension n
    global_center <- rowMeans(X)       # vecteur de taille n

    BSS <- 0
    for (g in unique(clusters)) {
      vars_g_idx <- which(clusters == g)
      Xg <- X[, vars_g_idx, drop = FALSE]       # n × |C_g|
      z_g <- rowMeans(Xg)                       # barycentre du groupe (longueur n)
      BSS <- BSS + length(vars_g_idx) * sum((z_g - global_center)^2)
    }

    return(BSS)
  },

  # Compute Within-Cluster Sum of Squares (WSS).
  # Measures compactness of clusters as distance of variables to their group barycenter.
  compute_WSS = function() {
    X <- scale(self$data)
    clusters <- self$clusters
    if (is.null(clusters)) stop("[CAH] No clusters available for WSS.")

    WSS <- 0
    for (g in unique(clusters)) {
      vars_g_idx <- which(clusters == g)
      Xg <- X[, vars_g_idx, drop = FALSE]
      z_g <- rowMeans(Xg)

      # Somme des distances au carré de chaque variable à son barycentre
      for (j in seq_along(vars_g_idx)) {
        x_j <- Xg[, j]
        WSS <- WSS + sum((x_j - z_g)^2)
      }
    }

    return(WSS)
  },

  # Compute the ratio BSS / (BSS + WSS).
  # Higher values indicate better clustering separation.
  compute_BSS_ratio = function() {
    B <- private$compute_BSS()
    W <- private$compute_WSS()
    if ((B + W) == 0) {
      return(0)
    }
    return(B / (B + W))
  },

  # Compute the GAP vector from BSS progression across tested k.
  # GAP[i] = BSS(k_i) - BSS(k_{i-1}), used to detect the optimal k.
  compute_gap = function(BSS_progression) {
    if (length(BSS_progression) < 2)
      return(rep(0, length(BSS_progression)))
    gap <- c(0, diff(BSS_progression))
    return(gap)
  },

  # Compute centroids (barycenters) of all clusters.
  # Returns a named list of mean vectors, one per cluster.
  compute_centroids = function() {

    clusters <- self$clusters
    data <- self$data
    groups <- sort(unique(clusters))

    centroids <- list()

    for (g in groups) {
      vars_g <- names(clusters[clusters == g])

      centroids[[as.character(g)]] <- colMeans(
        data[, vars_g, drop = FALSE]
      )
    }

    return(centroids)
  },

  # Compute silhouette scores for each variable in the current partition.
  # Uses the correlation-based distance matrix.
  # Returns individual scores and the mean silhouette coefficient.
  compute_silhouette = function() {

    dist_mat <- as.matrix(self$dist_matrix)
    clusters <- self$clusters
    n <- length(clusters)
    sil <- numeric(n)

    for (i in seq_len(n)) {

      g <- clusters[i]
      in_g <- which(clusters == g)

      # a(i) : average distance inside the same cluster
      if (length(in_g) > 1)
        a_i <- mean(dist_mat[i, in_g[in_g != i]])
      else
        a_i <- 0

      # b(i) : smallest average distance to another cluster
      b_i <- Inf
      for (g2 in unique(clusters)[unique(clusters) != g]) {
        out_g <- which(clusters == g2)
        b_i <- min(b_i, mean(dist_mat[i, out_g]))
      }

      sil[i] <- (b_i - a_i) / max(a_i, b_i)
    }

    names(sil) <- names(clusters)

    return(list(
      scores = sil,
      mean_score = mean(sil)
    ))
  },

  # Plot the hierarchical clustering dendrogram (private)
  # Adds cluster rectangles if a partition is available.
  plot_dendrogramme = function() {
    plot(self$hc,
         main = "Hierarchical Clustering Dendrogram",
         xlab = "Variables",
         ylab = "Distance",
         sub = "")

    if (!is.null(self$clusters)) {
      k <- length(unique(self$clusters))
      rect.hclust(self$hc, k = k, border = "red")
    }
  },
  # Plot PCA of variables (private)
  # Uses first two principal components and colors by current clusters.
  plot_acp = function() {
    acp <- prcomp(self$data, scale. = TRUE, center = TRUE)
    var_explained <- acp$sdev^2 / sum(acp$sdev^2)

    if (!is.null(self$clusters)) {
      colors <- rainbow(length(unique(self$clusters)))
      col_vector <- colors[self$clusters]
    } else {
      col_vector <- "darkblue"
    }

    plot(acp$rotation[, 1], acp$rotation[, 2],
         type = "n",
         xlab = paste("PC1 (", round(var_explained[1]*100, 1), "%)"),
         ylab = paste("PC2 (", round(var_explained[2]*100, 1), "%)"),
         main = "ACP - Variables Biplot",
         xlim = c(-1, 1),
         ylim = c(-1, 1))

    arrows(0, 0, acp$rotation[, 1]*0.9, acp$rotation[, 2]*0.9,
           col = "gray40", lwd = 1.5, length = 0.1)

    text(acp$rotation[, 1], acp$rotation[, 2],
         colnames(self$data),
         col = col_vector,
         font = 2,
         cex = 0.9)

        },

  # Plot MDS projection of variables using the correlation-based distance matrix (private).
  plot_mds = function() {
    mds_coords <- cmdscale(self$dist_matrix, k = 2)

    if (!is.null(self$clusters)) {
      colors <- rainbow(length(unique(self$clusters)))
      col_vector <- colors[self$clusters]
    } else {
      col_vector <- "darkblue"
    }

    plot(mds_coords[, 1], mds_coords[, 2],
         type = "n",
         xlab = "Dimension 1",
         ylab = "Dimension 2",
         main = "MDS - Variables Projection",
         xlim = range(mds_coords[, 1]) * 1.2,
         ylim = range(mds_coords[, 2]) * 1.2)

    points(mds_coords[, 1], mds_coords[, 2],
           col = col_vector,
           pch = 19,
           cex = 2)

    text(mds_coords[, 1], mds_coords[, 2],
         colnames(self$data),
         col = col_vector,
         pos = 1,
         cex = 0.8,
         font = 2)
    abline(h = 0, v = 0, col = "gray", lty = 3)
  },

  # Plot silhouette scores for the current partition (private)
  # Colors correspond to clusters and mean silhouette is indicated.
  plot_silhouette = function() {

    sil <- private$compute_silhouette()
    s <- sil$scores
    ord <- order(s)

    # Create one color per cluster
    clust <- self$clusters[ord]
    cols <- viridis::viridis(length(unique(self$clusters)))[clust]

    barplot(s[ord],
            horiz = TRUE,
            col = cols,
            border = NA,
            xlab = "Silhouette",
            main = "Silhouette plot (1 color per cluster)",
            las = 1,
            xlim = c(-1, 1))

    abline(v = sil$mean_score, col = "black", lwd = 2, lty = 2)
    abline(v = 0, col = "gray50")
  },

  # Plot elbow curve showing dendrogram heights vs number of clusters (private)
  # Vertical lines show best_k and current k (if available).
  plot_elbow = function() {
    h <- self$hc$height
    n_clust <- length(h):1
    plot(n_clust, h,
         type = "b",
         main = "Elbow Method - Height vs Number of Clusters",
         xlab = "Number of Clusters",
         ylab = "Height",
         pch = 19,
         col = "darkblue",
         lwd = 2)

    if (!is.null(self$best_k)) {
      abline(v = self$best_k, col = "red", lty = 2, lwd = 2)
      text(self$best_k, max(h), paste("k =", self$best_k),
           col = "red", pos = 4)
    }

    if (!is.null(self$k_current)){
      abline(v = self$k_current, col = "blue", lty = 2, lwd = 2)
    }

    grid(col = "gray", lty = 3)
  }

  )
)































