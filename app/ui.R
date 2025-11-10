library(shiny)
library(readxl)
library(DT)
library(bslib)
library(shinyjs)

ui <- navbarPage(
  useShinyjs() ,    #pour desactiver les buttons
  id = "onglets",
  #espace entre le nitre et les onglets
  title = div(style = "margin-right: 400px;", "Application de Clustering de variable"),
  theme = bs_theme(bootswatch = "flatly", base_font = font_google("Inter")),
  collapsible = TRUE,


  # =============================== Page d'importation ===============================
  tabPanel(
    "Importation",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title", "Importer vos données"),
            fileInput(
              "fichier", "Fichier (CSV ou Excel)",
              accept = c(".csv", ".xlsx", ".xls")
            ),
            selectInput(
              "separateur", "Séparateur",
              choices = c(Virgule = ",",
                          `Point-virgule` = ";",
                          Tabulation = "\t")
            ),
            actionButton("valider", "Valider l'importation", icon = icon("check"))
        )
      ),
      mainPanel(
        width = 9,
        div(class = "card",
            h4(class = "section-title", "Aperçu des données importées"),
            DTOutput("tableau_import"),
            hr(),
            h4("Résultats du clustering"),
            tableOutput("cluster"),
            verbatimTextOutput("summary")
        )
      )
    )
  ),

  # =============================== Page de nettoyage ===============================
  tabPanel(
    "Nettoyage",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title", "Options de nettoyage"),
            checkboxInput(
              "supprimer_na",
              "Imputation : numériques → moyenne ; catégorielles → « manquant »",
              value = FALSE
            ),
            checkboxInput(
              "normaliser",
              "Normaliser les variables numériques",
              value = FALSE
            ),
            checkboxInput(
              "supprimer_outliers",
              "Supprimer les valeurs aberrantes",
              value = FALSE
            ),
            div(class = "d-grid gap-2",
                actionButton("nettoyer", "Appliquer le nettoyage"),
                br(),
                actionButton("passer_clustering", "Passer au clustering")
            )
        )
      ),
      mainPanel(
        width = 9,
        h4("tableau nettoye"),
        tabsetPanel(
          tabPanel("Aperçu du tableau", DTOutput("tableau_importe_nettoye")),
          tabPanel("Statistiques", uiOutput("statistiques_ui"))
        )
      )
    )
  ),

  # =============================== Page de clustering ===============================
  tabPanel(
    "Clustering",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title", "Paramètres"),
            selectInput(
              "method", "Méthode",
              choices = c("kmeans", "kmodes", "hierarchical")
            ),
            numericInput("k", "Nombre de clusters (k)", value = 2, min = 2, step = 1),
            actionButton("lancer", "Lancer le clustering", icon = icon("play")) ,
            br() ,
            #lacer la coude pour choisir le meilleur k
            actionButton("coude" ,"Afficher la méthode du coude"  , ) ,
            br() ,
            #resume du kmenas
            actionButton("interpreter" ,"interpreter les resultats"  ,)
        )
      ),
      mainPanel(
        width = 9,
        div(class = "card",
            h4(class = "section-title", "Aperçu des données nettoyées"),
            DTOutput("tableau_cluster")
        ),
        div(class = "card",
            h4(class = "section-title", "Résultats du clustering"),
            DTOutput("resultat_cluster"),
            hr() ,
            verbatimTextOutput("Résumé"),
            verbatimTextOutput("summary") ,
            plotOutput("afficher_coude")

        )
      )
    )
  )  ,

  #============  page de resultats du kmenas ==============================
  tabPanel(
    "Résultats du Clustering",
    fluidPage(
      tabsetPanel(id = "tabs_resultats_clustering", type = "tabs",

                   # Onglet 1 : Indicateurs de qualité
                  tabPanel("Qualité du Clustering",
                           br() ,
                           h4("Indicateurs de Qualité"),
                           verbatimTextOutput("qualite"),


                  ),

                  # Onglet 2 : Visualisations
                  tabPanel("Visualisations",
                           br() ,

                           h4("Visualisations du Clustering avec la fonction fviz_cluster"),
                           plotOutput("pca_plot"),
                           br() ,
                           h5("heatmap des centres de clusters"),
                           br()  ,
                           plotOutput("heatmap"),
                           plotOutput("cluster_distribution"),
                           plotOutput("cluster_pca_plot")
                  ),

                  # Onglet 3 : Résumé des Résultats
                  tabPanel("Résumé des Résultats",
                           br() ,
                           h4("Résumé détaillé du clustering"),
                           verbatimTextOutput("clustering_summary"),
                           tableOutput("cluster_centroids"),
                           plotOutput("cluster_distribution")
                  )
      )
    )
  )





  )
