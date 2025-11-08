library(shiny)
library(readxl)
library(DT)
library(bslib)

ui <- navbarPage(
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
            actionButton("lancer", "Lancer le clustering", icon = icon("play"))
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
            h5("Résumé"),
            verbatimTextOutput("summary")
        )
      )
    )
  )
)
