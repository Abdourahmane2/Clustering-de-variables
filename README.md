# ClusterVariable

> **Package R de Clustering de Variables**  
> Algorithmes et Outils d'Interprétation des Résultats




##  Table des matières

- [Présentation](#présentation)
- [Fonctionnalités](#fonctionnalités)
- [Installation](#installation)
- [Utilisation](#utilisation)
- [Application Shiny](#application-shiny)
- [Documentation](#documentation)
- [Structure du package](#structure-du-package)
- [Auteurs](#auteurs)

---

##  Présentation

**ClusterVariable** est un package R développé dans le cadre du Master 2 SISE 2025-2026 qui implémente des algorithmes de clustering de variables avec des outils d'interprétation complets. Le package propose trois approches principales pour regrouper des variables corrélées

  

##  Fonctionnalités

### Algorithmes

1. **Classification Ascendante Hiérarchique (CAH)**
    -  ici les methodes de milna 
     
2. **K-means**
   - Implémentation personnalisée pour le clustering de variables
   - Méthode du coude pour sélectionner k optimal
   - Évaluation par coefficient de silhouette
   - Prétraitement automatique des données

3. **Clustering de Données Mixtes (AFDM)**
   - Traite les variables quantitatives et qualitatives
   - Intégration de l'Analyse des Correspondances Multiples
   - Clustering hiérarchique sur les axes factoriels
   - Adapté aux jeux de données hétérogènes

### Outils d'Interprétation

- **Métriques de qualité** : R², Silhouette, η², ratio BSS/WSS
- **Visualisations** : Dendrogrammes, projections ACP, MDS, heatmaps, courbes du coude

---

##  Installation

### Depuis GitHub

```r
# Installer devtools si nécessaire
if (!requireNamespace("devtools", quietly = TRUE)) {
  install.packages("devtools")
}

# Installer ClusterVariable depuis GitHub
devtools::install_github("Abdourahmane2/ClusterVariable")
```



### Dépendances

```r
install.packages(c("R6", "stats", "FactoMineR", "factoextra", 
                   "cluster", "pheatmap", "ggplot2", "shiny"))
```

---

## 🚀 Utilisation

### Exemple 1 : CAH

```r
library(ClusterVariable)

# Charger les données
data(mtcars)
df <- mtcars[, c("mpg", "disp", "hp", "drat", "wt", "qsec")]

# Initialiser et ajuster le modèle
cah <- CAH$new()
cah$fit(df)

# Partitionnement automatique
cah$cutree()

# Afficher les résultats
cah$print()
cah$summary()

# Visualiser
cah$plot("dendrogramme")
```

### Exemple 2 : K-means

```r
library(ClusterVariable)

# Préparer les données
data(iris)
X <- iris[, 1:4]

# Créer et ajuster le modèle
model <- clusterVariable$new(k = 3)
model$fit(X)

# Résultats
model$print()
model$summary()

# Visualisations
model$plot_clusters()
model$plot_elbow(k_max = 10)
```



---

## 🖥️ Application Shiny

Le package inclut une application Shiny interactive.

### Lancer l'application

```r
library(shiny)
shiny::runApp()
```

### Fonctionnalités de l'application

1. **Importation de données** : Support CSV et Excel, sélection des colonnes
2. **Nettoyage** : Imputation des valeurs manquantes 
3. **Clustering** : Choix de l'algorithme (K-means, CAH, FADM) et configuration des paramètres
4. **Visualisation** : Graphiques interactifs et tableaux de résultats
5. **Prédiction** : Affectation de variables illustratives aux clusters

---

## 📖 Documentation




### Méthodes principales

#### Classe CAH

ici les methodes de milena 

#### Classe clusterVariable(Kmeans)

| Méthode | Description |
|---------|-------------|
| `new(k, max_iter, auto_clean)` | Initialisation K-means |
| `fit(X)` | Ajustement et clustering |
| `predict(X_new)` | Affectation de nouvelles variables |
| `summary()` | Résumé du clustering |
| `plot_clusters()` | Projection ACP |
| `plot_elbow()` | Méthode du coude |
| `cluster_quality_report()` | Rapport de qualité détaillé |

#### Classe CAH_mixtes

| Méthode | Description |
|---------|-------------|
| `new(n_components)` | Initialisation AFDM |
| `fit(df)` | Ajustement sur données mixtes |
| `clustering_hierarchical(n_clusters, method)` | Clustering hiérarchique |
| `predict(new_vars)` | Prédiction pour nouvelles variables |
| `plot_variables()` | Projection des variables |
| `dendo()` | Dendrogramme |

### Champs publics

Accès direct aux résultats via les champs publics :

```r
# CAH
cah$data              # Données nettoyées
cah$hc                # Objet hclust
cah$clusters          # Affectations aux clusters
cah$best_k            # k optimal
cah$compo_latent      # Composantes latentes
cah$predict_result    # Résultats de prédiction
cah$r2_info           # Statistiques R²
cah$silhouette        # Scores de silhouette
cah$eta2              # Valeurs η²

# clusterVariable
model$k               # Nombre de clusters
model$data            # Données d'entraînement
model$cluster_result  # Résultats du clustering
```

---

##  Structure du package

```
ClusterVariable/
├── R/
│   ├── CAH.R                    # CAH (classe R6)
│   ├── clusterVariable.R        # K-means (classe R6)
│   ├── mon_kmeans.R             # Implémentation K-means
│   ├── ACM.R                    # Wrapper ACM (classe R6)
│   └── ACM_Clustering_quali_tierce.R  # Données mixtes (classe R6)
├── man/                         # Documentation
│   ├── CAH.Rd
│   └── clusterVariable.Rd
├── app/                         # Application Shiny
│   ├── ui.R
│   └── server.R
├── tests/                       # Tests unitaires
│   └── testthat/
├── DESCRIPTION                  # Métadonnées du package
├── NAMESPACE                    # Fonctions exportées
└── README.md                    # Ce fichier
```

---

## 👥Auteurs

**Équipe M2 SISE 2025-2026**

- **Abdourahmane** 
- **Milena** 
- **Marvin** 





