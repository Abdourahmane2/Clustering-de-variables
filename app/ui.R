library(shiny)
library(readxl)
library(DT)

ui <- navbarPage(
  id = "onglets",  # nécessaire pour updateNavbarPage
  title = "Analyse de clustering",

  # ================================= page d'importation de fichier ===========================
  tabPanel("Importation",
           sidebarLayout(
             sidebarPanel(
               fileInput(
                 "fichier", "Importer un fichier CSV ou Excel",
                 accept = c(".csv", ".xlsx", ".xls")
               ),
               selectInput(
                 "separateur", "Séparateur",
                 choices = c(Virgule = ",",
                             `Point-virgule` = ";",
                             Tabulation = "\t")
               ),
               actionButton("valider", "Valider l'importation")
             ),
             mainPanel(
               h4("Aperçu des données importées"),
               DTOutput("tableau_import")
             )
           )
  ),

  #=========================page de nettoyage =============================
  tabPanel("Nettoyage",
           sidebarLayout(
             sidebarPanel(
               checkboxInput("supprimer_na", "Remplacer les variables numeriques par la moyenne et les variables categorielle par manquant", value = FALSE),
               checkboxInput("normaliser", "normaliser les donnes numeriques ", value = FALSE),
               checkboxInput("supprimer_outliers", "Supprimer les valeurs aberantes ", value = FALSE),
               actionButton("nettoyer", "appliquer le nettoyage") ,
               actionButton("passer_clustering", "Passer au clustering")

             ),
             mainPanel(
               h4("tableau nettoye") ,
               tabsetPanel(
                 tabPanel("Aperçu du tableau", DTOutput("tableau_importe_nettoye")),
                 tabPanel("Statistiques", uiOutput("statistiques_ui"))
               )
             )
           )
  ),

  # ================================page de clustering  ==================================

  tabPanel("Clustering",
           sidebarLayout(
             sidebarPanel(
               selectInput("method", "Méthode de clustering",
                           choices = c("kmeans", "hierarchical")),
               numericInput("k", "Nombre de clusters", value = 2, min = 2),
               actionButton("lancer", "Lancer le clustering"),

             ),
             mainPanel(
               h4("Aperçu des données importées"),
               tableOutput("tableau_cluster"),
               hr(),
               h4("Résultats du clustering"),
               tableOutput("cluster"),
               verbatimTextOutput("summary")
             )
           )
  )
)
