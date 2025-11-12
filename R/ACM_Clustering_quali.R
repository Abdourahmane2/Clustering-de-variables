library(R6)
library(readxl)
#Vérification et chargement packages
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(FactoMineR)
library(factoextra)

#ACM est un objet S3 que l'on réencapsule en R6

ACM <- R6Class(
  "ACM",
  public = list(
    result = NULL,
    data = NULL,
    params = NULL,
    
  #constructeur reprenant tous les paramètres de FactoMiner
    initialize = function(X, 
                          ncp = 5,
                          ind.sup = NULL,
                          quanti.sup = NULL,
                          quali.sup = NULL,
                          excl = NULL,
                          graph = FALSE,
                          level.ventil = 0,
                          axes = c(1, 2),
                          row.w = NULL,
                          method = "Indicator",
                          na.method = "NA",
                          tab.disj = NULL){
   #on conserve les données et paramètres
      self$data = X
      self$params = list(
        ncp = ncp,
        ind.sup = ind.sup,
        quanti.sup = quanti.sup,
        quali.sup = quali.sup,
        excl = excl,
        graph = graph,
        level.ventil = level.ventil,
        axes = axes,
        row.w = row.w,
        method = method,
        na.method = na.method,
        tab.disj = tab.disj
      )
      
    #on instancie l'ACM 
      self$fit()
      invisible(self)
    } ,
    #méthode pour ajuster le modèle
      fit = function() {
      self$result <- do.call(MCA, c(list(X = self$data), self$params))
      invisible(self)
    },
  
    #méthode valeurs propres
      eigenvalues = function() {
        if (is.null(self$result)) stop("Modèle non ajusté")
        return(self$result$eig)
      },
    
   

    # Méthode pour obtenir les coordonnées des variables
    var_coords = function(axes = c(1, 2)) {
      if (is.null(self$result)) stop("Modèle non ajusté")
      return(self$result$var$coord[, axes, drop = FALSE])
      },
    
    # Méthode pour obtenir les contributions des variables
    var_contrib = function(axes = c(1, 2)) {
      if (is.null(self$result)) stop("Modèle non ajusté")
      return(self$result$var$contrib[, axes, drop = FALSE])
      },   
      
    # Méthode pour obtenir le cos2 des variables
    var_cos2 = function(axes = c(1, 2)) {
      if (is.null(self$result)) stop("Modèle non ajusté")
      return(self$result$var$cos2[, axes, drop = FALSE])
    },    
    
   
      #Méthode affichage
  plot = function(choix = "ind", axes = c(1, 2), ...) {
    if (is.null(self$result)) stop("Modèle non ajusté")
    plot.MCA(self$result, choix = choix, axes = axes, ...)
  },    
    
    #Méthode de résumé
    
    summary = function() {
      if (is.null(self$result)) stop("Modèle non ajusté")
      
      cat("=== ACM (Analyse des Correspondances Multiples) ===\n\n")
      cat("Dimensions:", self$params$ncp, "\n")
      cat("Méthode:", self$params$method, "\n")
      cat("Nombre d'individus:", nrow(self$data), "\n")
      cat("Nombre de variables:", ncol(self$data), "\n\n")
      
      cat("Valeurs propres (variance expliquée):\n")
      eig <- self$get_eigenvalues()
      print(head(eig, 10))
      
      invisible(self)
    },
    
    # Méthode pour créer depuis S3
    from_s3 = function(mca_obj) {
      if (!inherits(mca_obj, "MCA")) {
        stop("L'objet doit être de classe MCA")
      }
      self$result <- mca_obj
      self$data <- mca_obj$call$X
      invisible(self)
    }
  )
)
canines = read_excel("D:/1_Prog_R/PROJET_CLUSTERING/races_canines_acm.xls",col_names=TRUE, col_types="guess", sheet=1)
mca = ACM$new(canines)

print(mca$result$eig)
print(names(mca$data))
print(mca$params$ncp)

print(mca$plot(choix = "var", axes = c(1, 2)))






ClusteringVariables <- R6Class(
  "ClusteringVariables",
  public = list (
    data = NULL,
    dissim_matrix = NULL,
    clusters = NULL,
    
    
    #CONSTRUCTEUR
    initialize = function(data) {
      #on teste si data est bien un df
      if (is.data.frame(data) == FALSE) {
        stop("Vos données doivent être de type dataframe")
      }
      #on conserve les données
      self$data = data
      message("VariableClustering initialisé avec ", ncol(data), " variables")
    },
    
    #METHODE V DE CRAMER
    cramer = function(y, x, print_chi2 = FALSE) {
      # Vérification que les variables sont des facteurs
      if (is.factor(y)==FALSE) {y <- as.factor(y)}
      if (is.factor(x)== FALSE){x <- as.factor(x)}
      
      K <- nlevels(y)
      L <- nlevels(x)
      n <- length(y)
      
      # Test du chi2
      chi2 <- chisq.test(y, x, correct = FALSE)
      
      if (print_chi2) {
        print(chi2$statistic)
      }
      
      # Calcul du V de Cramer
      v <- sqrt(chi2$statistic / (n * min(K - 1, L - 1)))
      
      return(as.numeric(v))
    },
    
    
    #METHODE MATRICE SIMILARITES
    similarity_matrix = function(variables=NULL){
      if(is.null(variables)==TRUE) {variables = names(self$data)}
      
      
      #contrôle du caractère catégoriel des variables
      for (var in variables) {
        if (is.factor(self$data[[var]])==FALSE) {
          self$data[[var]] = as.factor(self$data[[var]])
        }
      } 
      #création d'une matrice indicatrice au bon format
      n_vars = length(variables)
      mat = matrix(1, nrow = n_vars, ncol = n_vars)
      rownames(mat) = variables
      colnames(mat) = variables
      
      # calcul de la matrice de similarité
      for (i in 1:(nrow(mat)-1)) {
        for (j in (i+1):ncol(mat)){
          mat[i,j] = self$cramer(self$data[[variables[i]]], 
                                 self$data[[variables[j]]])
          mat[j,i] = mat[i,j]
          
        }
      }
      
      #METHODE CLUSTERING hclust
      #matrice de distance
      self$dissim_matrix = as.dist(1-mat)
      message("Matrice de distances calculée")
      return(self$dissim_matrix)
    },
    
    #Clustering hiérarchique sur variables qualitatives  
    predict = function(method = "ward.D2", k = NULL) {
      if (is.null(self$dissim_matrix)==TRUE) {
        stop("Vous devez calculer la matrice de dissimilarité avant d'effectuer le clustering")
      }
      
      hc = hclust(self$dissim_matrix, method = method)
      #decoupage en k clusters
      if (is.null(k)==FALSE) {
        self$clusters <- cutree(hc, k = k)
        message("Clustering effectué avec ", k, " groupes")
      }
      
      return(hc)
    },
    
    
    #Dendrogramme
    dendrogram = function(hc = NULL, k = NULL) {
      if (is.null(hc)==TRUE) {
        hc <- self$predict()
      }
      
      plot(hc, main = "Dendrogramme - Clustering de variables",
           xlab = "Variables", ylab = "Distance",
           sub = paste("Méthode:", hc$method))
      
      if (is.null(k)==FALSE) {
        rect.hclust(hc, k = k, border = 2:6)
      }
    }
    
    
    
  )
)


#TEST
#1. import données
canines <- read_xls("D:/1_Prog_R/PROJET_CLUSTERING/races_canines_acm.xls",col_names=TRUE, col_types="guess", sheet=1)


# 2. Initialisation
cv <- ClusteringVariables$new(canines)

# 3. Matrice de similarité
sim <- cv$similarity_matrix()

# 4. Clustering
hc <- cv$cluster_variables(k = 4)

# 5. Visualisation
cv$dendrogram(hc, k = 4)



