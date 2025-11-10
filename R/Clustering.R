library(R6)

#Vérification et chargement packages
if (!requireNamespace("FactoMineR", quietly = TRUE)) install.packages("FactoMineR")
if (!requireNamespace("factoextra", quietly = TRUE)) install.packages("factoextra")
library(FactoMineR)
library(factoextra)




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
  cluster_variables = function(method = "ward.D2", k = NULL) {
    if (is.null(self$dissim_matrix)==TRUE) {
      stop("Vous devez calculer la matrice de dissimilarité avant d'effectuer le clustering")
    }
    
    hc = hclust(self$dissim_matrix, method = method)
    
    if (is.null(k)==FALSE) {
      self$clusters <- cutree(hc, k = k)
      message("Clustering effectué avec ", k, " groupes")
    }
    
    return(hc)
  },
  
  
  #Dendogramme
  dendrogram = function(hc = NULL, k = NULL) {
    if (is.null(hc)==TRUE) {
      hc <- self$cluster_variables()
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
hc <- cv$cluster_variables(k = 2)

# 5. Visualisation
cv$dendrogram(hc, k = 2)
