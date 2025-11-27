# ClusterVariable

> **R Package for Clustering Variables**  
> Algorithms and Tools for Interpreting Results


---

##  Table of Contents

- [Introduction](#introduction)
- [Features](#features)
- [Installation](#installation)
- [Usage](#usage)
- [Shiny Application](#shiny-application)
- [Documentation](#documentation)
- [Package Structure](#package-structure)
- [Authors](#authors)

---

##  Presentation

**ClusterVariable** is an R package developed as part of the Master 2 SISE 2025-2026 program that implements variable clustering algorithms with comprehensive interpretation tools. The package offers three main approaches for grouping correlated variables

  

##  Features

### Algorithms

1. **Hierarchical Ascending Classification (HAC)**
    - Custom correlation-based distance matrix for variable similarity
    - Automatic preprocessing (numeric selection, constant-variable removal, NA handling)
    - Optimal k detection using dendrogram jump
    - Elbow method for selecting optimal k
    - Silhouette evaluation for partition quality
    - Local PCA components to compute latent factors within each cluster
    - Prediction module assigning new variables to clusters via latent-component correlations
     
2. **K-means**
   - Custom implementation for variable clustering
   - Elbow method for selecting optimal k
   - Evaluation by silhouette coefficient
   - Automatic data preprocessing

3. **Mixed Data Clustering (AFDM)**
   - Processes quantitative and qualitative variables
   - Integration of Multiple Correspondence Analysis
   - Hierarchical clustering on factorial axes
   - Suitable for heterogeneous datasets

### Interpretation Tools

- **Quality metrics**: R², Silhouette, η², BSS/WSS ratio
- **Visualizations**: Dendrograms, PCA projections, MDS, heatmaps, elbow curves

---

##  Installation

### From GitHub

```r
# Install devtools if necessary
if (!requireNamespace(“devtools”, quietly = TRUE)) {
  install.packages(“devtools”)
}

# Install ClusterVariable from GitHub
devtools::install_github(“Abdourahmane2/ClusterVariable”)
```



### Dependencies

```r
install.packages(c(“R6”, “stats”, ‘FactoMineR’, “factoextra”, 
                   “cluster”, “pheatmap”, ‘ggplot2’, “shiny”))
```

---

## 🚀 Usage

### Example 1: CAH

```r
library(ClusterVariable)

# Load data
data(mtcars)
df <- mtcars[, c(“mpg”, “disp”, “hp”, “drat”, ‘wt’, “qsec”)]

# Initialize and fit the model
cah <- CAH$new()
cah$fit(df)

# Automatic partitioning
cah$cutree()

# Display results
cah$print()
cah$summary()

# Visualize
cah$plot(“dendrogram”)
```

Example 2: K-means

```r
library(ClusterVariable)

# Prepare the data
data(iris)
X <- iris[, 1:4]

# Create and adjust the model
model <- clusterVariable$new(k = 3)
model$fit(X)

# Results
model$print()
model$summary()

# Visualizations
model$plot_clusters()
model$plot_elbow(k_max = 10)
```
Example 3 : AFDM
Marvin's part
```r

```
---

## 🖥️ Shiny Application

The package includes an interactive Shiny application.

### Launching the application

```r
library(shiny)
shiny::runApp()
```

### Application features

1. **Data import**: CSV and Excel support, column selection
2. **Cleaning**: Imputation of missing values
3. **Clustering**: Choice of algorithm (K-means, CAH, FADM) and parameter configuration
4. **Visualization**: Interactive graphs and results tables
5. **Prediction**: Assignment of illustrative variables to clusters

---

## 📖 Documentation




### Main methods

#### CAH class

| Method                    | Description                                              |
| ------------------------- | -------------------------------------------------------- |
| `new(method = "ward.D2")` | CAH initialization                                       |
| `fit(data)`               | Preprocessing, distance matrix, hierarchical clustering  |
| `cutree(k)`               | Generates clusters and latent components                 |
| `predict(X_new)`          | Assigns new variables to clusters                        |
| `summary()`               | Detailed clustering summary                              |
| `print()`                 | Compact model overview                                   |
| `plot(type)`              | Visualizations (dendrogram, PCA, MDS, silhouette, elbow) |


#### clusterVariable(Kmeans) class

| Method | Description |
|---------|-------------|
| `new(k, max_iter, auto_clean)` | K-means initialization |
| `fit(X)` | Fitting and clustering |
| `predict(X_new)` | Assigning new variables |
| `summary()` | Clustering summary |
| `plot_clusters()` | PCA projection |
| `plot_elbow()` | Elbow method |
| `cluster_quality_report()` | Detailed quality report |

#### CAH_mixtes (AFDM) class
| Method | Description |
|---------|-------------|
| `new(n_components)` | AFDM initialization |
| `fit(df)` | Adjustment to mixed data |
| `clustering_hierarchical(n_clusters, method)` | Hierarchical clustering |
| `predict(new_vars)` | Prediction for new variables |
| `plot_variables()` | Variable projection |
| `dendo()` | Dendrogram |

### Public fields

Direct access to results via public fields:
```r
# CAH
cah$data              # Cleaned data
cah$hc                # hclust object
cah$clusters          # Cluster assignments
cah$best_k            # Optimal k
cah$compo_latent      # Latent components
cah$predict_result    # Prediction results
cah$r2_info           # R² statistics
cah$silhouette        # Silhouette scores
cah$eta2              # η² values

# clusterVariable
model$k               # Number of clusters
model$data            # Training data
model$cluster_result  # Clustering results
```

---

## Package structure

```
ClusterVariable/
├── R/
│   ├── CAH.R                    # CAH (R6 class)
│   ├── clusterVariable.R        # K-means (R6 class)
│   ├── mon_kmeans.R             # K-means implementation
│   ├── ACM.R                    # Wrapper ACM (R6 class)
│   └── ACM_Clustering_quali_tierce.R  # Mixed data (class R6)
├── man/                         # Documentation
│   ├── CAH.Rd
│   └── clusterVariable.Rd
├── app/                         # Shiny Application
│   ├── ui.R
│   └── server.R
├── tests/                       # Unit tests
│   └── testthat/
├── DESCRIPTION                 # Package metadata
├── NAMESPACE                   # Exported functions
└── README.md                    # This file
```

---

## 👥Authors

**M2 SISE Team 2025-2026**

- **Abdourahmane**
- **Milena**
- **Marvin** 










