library(R6)

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
    
   
      #Méthode graphique
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
canines <- read_xls("C:/Users/marvi/Documents/MASTER_SISE_RAN/G_Analyses factorielles/races_canines_acm.xls",col_names=TRUE, col_types="guess", sheet=1)
mca = ACM$new(canines)

print(mca$result$eig)
print(names(mca$data))
print(mca$params$ncp)

mca$plot(choix = "var", axes = c(1, 2))