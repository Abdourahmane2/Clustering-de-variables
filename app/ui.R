library(shiny)
library(readxl)
library(DT)
library(bslib)
library(shinyjs)

# Configuration globale pour accepter les gros fichiers (200 MB)
options(shiny.maxRequestSize = 200 * 1024^2)

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
            # Indication de la taille maximale
            helpText("Taille maximale : 1GB", style = "color: #666; font-size: 0.9em;"),
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
              "remplacer les Na par la moyenne ",
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
          tabPanel("Aperçu du tableau", DTOutput("tableau_importe_nettoye_x")),
          tabPanel("Statistiques", uiOutput("statistiques_x"))
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
              choices = c("kmeans", "ACM", "CAH")
            ),
            numericInput("k", "Nombre de clusters (k)", value = 2, min = 2, step = 1),
            actionButton("lancer", "Lancer le clustering", icon = icon("play")) ,
            br() ,
            #lacer la coude pour choisir le meilleur k
            actionButton("coude" ,"Afficher la méthode du coude"  , ) ,
            br() ,
            #resume du kmenas
            # actionButton("interpreter" ,"interpreter les resultats"  ,) ,
            #Predire avec les variable
            actionButton("Importer" ,"Impotrer les variables illustrative") ,
            br()  ,
            actionButton("interpreter" ,"resultats detaille du clustering")
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

  ##============  Importation variables illustrative  ==============================

  tabPanel(
    "Importation_explanatory variable",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title", "Importer vos données"),
            fileInput(
              "fichier_exp", "Fichier (CSV ou Excel)",
              accept = c(".csv", ".xlsx", ".xls")
            ),
            # Indication de la taille maximale
            helpText("Taille maximale : 1GB", style = "color: #666; font-size: 0.9em;"),
            selectInput(
              "separateur_exp", "Séparateur",
              choices = c(Virgule = ",",
                          `Point-virgule` = ";",
                          Tabulation = "\t")
            ),
            actionButton("valider_exp", "Valider l'importation", icon = icon("check"))
        )
      ),
      mainPanel(
        width = 9,
        div(class = "card",
            h4(class = "section-title", "Aperçu des données importées"),
            DTOutput("tableau_import_exp"),
            hr(),
            h4("Résultats du clustering"),
            tableOutput("cluster"),
            verbatimTextOutput("summary")
        )
      )
    )
  ),

  ##============  Nettoyage variables illustrative  ==============================

  tabPanel(
    "Cleaning_explanatory variable",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title", "Options de nettoyage"),
            checkboxInput(
              "supprimer_na_exp",
              "Imputation : numériques → moyenne ; catégorielles → « manquant »",
              value = FALSE
            ),
            checkboxInput(
              "supprimer_outliers_exp",
              "Supprimer les valeurs aberrantes",
              value = FALSE
            ),
            div(class = "d-grid gap-2",
                actionButton("nettoyer_exp", "Appliquer le nettoyage"),
                br(),
                actionButton("Prediction", "Lancer la Prédiction", icon = icon("play"))
            )
        )
      ),
      mainPanel(
        width = 9,
        h4("tableau nettoye"),
        tabsetPanel(
          tabPanel("Aperçu du tableau", DTOutput("tableau_importe_nettoye_exp")),
          tabPanel("Statistiques", uiOutput("statistiques_exp"))
        )
      )
    )
  ),

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
                           plotOutput("pca_plot"),
                           br()  ,
                           plotOutput("heatmap")

                  )


      )
    )
  )
)


