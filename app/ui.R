library(shiny)
library(readxl)
library(DT)
library(bslib)
library(shinyjs)

# Configuration globale
options(shiny.maxRequestSize = 1000 * 1024^2)

ui <- navbarPage(
  useShinyjs(),
  id = "onglets",
  title = div(
    style = "margin-right: 50px; font-weight: 600; font-size: 1.3em;",
    icon("project-diagram", style = "margin-right: 10px; color: #3498db;"),
    "Clustering de Variables"
  ),
  theme = bs_theme(
    bootswatch = "flatly",
    base_font = font_google("Poppins"),
    heading_font = font_google("Poppins"),
    primary = "#3498db",
    secondary = "#2ecc71",
    success = "#27ae60",
    info = "#3498db",
    warning = "#f39c12",
    danger = "#e74c3c",
    bg = "#f8f9fa",
    fg = "#2c3e50"
  ),
  collapsible = TRUE,

  # ===== STYLES CSS =====
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Poppins', sans-serif;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        background-attachment: fixed;
      }

      .navbar {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%) !important;
        border: none;
        box-shadow: 0 4px 15px rgba(0,0,0,0.2);
      }

      .navbar-default .navbar-nav > li > a {
        color: white !important;
        font-weight: 500;
        transition: all 0.3s ease;
      }

      .navbar-default .navbar-nav > li > a:hover {
        background-color: rgba(255,255,255,0.2) !important;
        transform: translateY(-2px);
      }

      .navbar-default .navbar-brand {
        color: white !important;
        font-weight: 600;
      }

      .card {
        background: white;
        border-radius: 15px;
        padding: 25px;
        margin: 15px 0;
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
        border: none;
        transition: transform 0.3s ease, box-shadow 0.3s ease;
      }

      .card:hover {
        transform: translateY(-5px);
        box-shadow: 0 15px 40px rgba(0,0,0,0.15);
      }

      .section-title {
        color: #2c3e50;
        font-weight: 600;
        margin-bottom: 20px;
        padding-bottom: 10px;
        border-bottom: 3px solid #3498db;
        display: flex;
        align-items: center;
      }

      .section-title i {
        margin-right: 10px;
        color: #3498db;
      }

      .btn {
        border-radius: 8px;
        padding: 10px 20px;
        font-weight: 500;
        transition: all 0.3s ease;
        border: none;
        box-shadow: 0 4px 15px rgba(0,0,0,0.1);
      }

      .btn:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 20px rgba(0,0,0,0.15);
      }

      .btn-default {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
      }

      .btn-primary {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
      }

      .btn-success {
        background: linear-gradient(135deg, #2ecc71 0%, #27ae60 100%);
        color: white;
      }

      .form-control, .selectize-input {
        border-radius: 8px;
        border: 2px solid #e0e0e0;
        padding: 10px;
        transition: all 0.3s ease;
      }

      .form-control:focus, .selectize-input.focus {
        border-color: #3498db;
        box-shadow: 0 0 0 0.2rem rgba(52, 152, 219, 0.25);
      }

      label {
        color: #2c3e50;
        font-weight: 500;
        margin-bottom: 8px;
      }

      .well {
        background: white;
        border: none;
        border-radius: 15px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
      }

      .dataTables_wrapper {
        padding: 15px;
      }

      table.dataTable thead th {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        font-weight: 500;
        padding: 12px;
      }

      table.dataTable tbody tr:hover {
        background-color: #f0f4ff;
      }

      .help-block {
        color: #7f8c8d;
        font-size: 0.9em;
        font-style: italic;
      }

      .nav-tabs > li > a {
        border-radius: 8px 8px 0 0;
        font-weight: 500;
        color: #555;
      }

      .nav-tabs > li.active > a {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white !important;
        border: none;
      }

      .shiny-plot-output {
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }

      hr {
        border-top: 2px solid #e0e0e0;
        margin: 25px 0;
      }

      .action-buttons {
        display: flex;
        flex-direction: column;
        gap: 10px;
        margin-top: 20px;
      }

      .action-buttons .btn {
        width: 100%;
      }

      .info-box {
        background: #e8f4f8;
        border-left: 4px solid #3498db;
        padding: 15px;
        border-radius: 8px;
        margin: 15px 0;
      }

      .info-box i {
        color: #3498db;
        margin-right: 10px;
      }

      .variables-box {
        background: #f8f9fa;
        border: 2px solid #e0e0e0;
        border-radius: 10px;
        padding: 15px;
        margin: 10px 0;
        max-height: 300px;
        overflow-y: auto;
      }

      .variables-box-active {
        border-color: #2ecc71;
        background: #f0fff4;
      }

      .variables-box-illustrative {
        border-color: #3498db;
        background: #f0f8ff;
      }

      .badge-info {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
        padding: 5px 12px;
        border-radius: 20px;
        font-size: 0.85em;
        font-weight: 500;
      }
    "))
  ),

  # ===== PAGE 1: IMPORTATION =====
  tabPanel(
    title = tagList(icon("upload"), "Importation"),
    value = "Importation",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title",
               icon("file-import"), "Importer vos données"),
            fileInput(
              "fichier",
              label = tagList(icon("file-csv"), "Fichier (CSV ou Excel)"),
              accept = c(".csv", ".xlsx", ".xls"),
              buttonLabel = "Parcourir...",
              placeholder = "Aucun fichier sélectionné"
            ),
            helpText("📊 Taille maximale : 1GB"),
            selectInput(
              "separateur",
              label = tagList(icon("grip-lines"), "Séparateur (CSV uniquement)"),
              choices = c(Virgule = ",",
                          `Point-virgule` = ";",
                          Tabulation = "\t")
            ),
            div(class = "action-buttons",
                actionButton("valider", "Valider l'importation",
                             icon = icon("check-circle"),
                             class = "btn-success")
            )
        )
      ),
      mainPanel(
        width = 9,
        div(class = "card",
            h4(class = "section-title",
               icon("table"), "Aperçu des données importées"),
            DTOutput("tableau_import")
        )
      )
    )
  ),

  # ===== PAGE 2: NETTOYAGE =====
  tabPanel(
    title = tagList(icon("broom"), "Nettoyage"),
    value = "Nettoyage",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title",
               icon("sliders-h"), "Configuration des variables"),

            div(class = "variables-box variables-box-active",
                h5(style = "color: #27ae60; font-weight: 600; margin-bottom: 10px;",
                   icon("check-circle"), " Variables actives"),
                helpText("Variables utilisées pour le clustering"),
                checkboxGroupInput(
                  "colonnes_actives",
                  label = NULL,
                  choices = NULL
                )
            ),

            hr(),

            div(class = "variables-box variables-box-illustrative",
                h5(style = "color: #3498db; font-weight: 600; margin-bottom: 10px;",
                   icon("magic"), " Variables illustratives"),
                helpText("Variables pour la prédiction (optionnel)"),
                checkboxGroupInput(
                  "colonnes_illustratives",
                  label = NULL,
                  choices = NULL
                )
            ),

            hr(),

            checkboxInput(
              "supprimer_na",
              HTML("<strong>Imputer les valeurs manquantes</strong><br>
                   <small>Remplacer les NA par la moyenne (numériques)</small>"),
              value = FALSE
            ),

            div(class = "action-buttons",
                actionButton("nettoyer", "Appliquer le nettoyage",
                             icon = icon("magic"),
                             class = "btn-primary"),
                actionButton("passer_clustering", "Passer au clustering",
                             icon = icon("arrow-right"),
                             class = "btn-success")
            )
        )
      ),
      mainPanel(
        width = 9,
        div(class = "card",
            h4(class = "section-title",
               icon("table"), "Données après nettoyage"),
            tabsetPanel(
              tabPanel(
                tagList(icon("eye"), "Aperçu"),
                br(),
                DTOutput("tableau_nettoye")
              ),
              tabPanel(
                tagList(icon("chart-bar"), "Statistiques"),
                br(),
                uiOutput("statistiques")
              )
            )
        )
      )
    )
  ),

  # ===== PAGE 3: CLUSTERING =====
  tabPanel(
    title = tagList(icon("project-diagram"), "Clustering"),
    value = "Clustering",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title",
               icon("cogs"), "Paramètres du clustering"),
            selectInput(
              "method",
              label = tagList(icon("puzzle-piece"), "Méthode"),
              choices = c("K-means" = "kmeans",
                          "FADM" = "FADM",
                          "CAH" = "CAH")
            ),
            numericInput("k",
                         label = tagList(icon("hashtag"), "Nombre de clusters"),
                         value = 3,
                         min = 2,
                         step = 1),
            hr(),
            div(class = "action-buttons",
                actionButton("lancer", "Lancer le clustering",
                             icon = icon("play-circle"),
                             class = "btn-success"),
                actionButton("coude", "Méthode du coude",
                             icon = icon("chart-line"),
                             class = "btn-primary"),
                actionButton("interpreter", "Voir les résultats",
                             icon = icon("microscope"),
                             class = "btn-primary")
            )
        )
      ),
      mainPanel(
        width = 9,
        div(class = "card",
            h4(class = "section-title",
               icon("database"), "Données utilisées"),
            DTOutput("tableau_cluster")
        ),
        div(class = "card",
            h4(class = "section-title",
               icon("chart-pie"), "Résumé du clustering"),
            verbatimTextOutput("resume_clustering"),
            plotOutput("plot_coude")
        )
      )
    )
  ),

  # ===== PAGE 4: RÉSULTATS =====
  tabPanel(
    title = tagList(icon("chart-line"), "Résultats"),
    value = "Résultats",
    fluidPage(
      div(class = "card",
          h4(class = "section-title",
             icon("trophy"), "Analyse détaillée des résultats"),

          uiOutput("badge_variables"),

          tabsetPanel(
            id = "tabs_resultats",

            # Qualité
            tabPanel(
              tagList(icon("star"), "Qualité"),
              br(),
              div(class = "card",
                  h4(icon("gauge-high"), "Indicateurs de qualité"),
                  verbatimTextOutput("qualite")
              )
            ),

            # Visualisations
            tabPanel(
              tagList(icon("chart-area"), "Visualisations"),
              br(),

              # K-means
              conditionalPanel(
                condition = "input.method == 'kmeans'",
                div(class = "card",
                    plotOutput("pca_plot", height = "500px")
                ),
                br(),
                div(class = "card",
                    plotOutput("heatmap", height = "500px")
                )
              ),

              # CAH
              conditionalPanel(
                condition = "input.method == 'CAH'",
                div(class = "card",
                    plotOutput("dendrogramme", height = "500px")
                ),
                br(),
                div(class = "card",
                    plotOutput("pca_cah", height = "500px")
                ),
                br(),
                div(class = "card",
                    plotOutput("mds_cah", height = "500px")
                ),
                br(),
                div(class = "card",
                    plotOutput("silhouette_cah", height = "500px")
                )
              ),

              # FADM
              conditionalPanel(
                condition = "input.method == 'FADM'",
                div(class = "card",
                    plotOutput("dendrogramme_FADM", height = "500px")
                ),
                br(),
                div(class = "card",
                    plotOutput("pca_FADM", height = "500px")
                ),
                br(),
                div(class = "card",
                    plotOutput("mds_FADM", height = "500px")
                ),
                br(),
                div(class = "card",
                    plotOutput("silhouette_FADM", height = "500px")
                )
              )
            ),

            # Prédictions
            tabPanel(
              tagList(icon("magic"), "Prédictions"),
              br(),
              div(class = "card",
                  h4(icon("wand-magic-sparkles"), "Prédiction avec variables illustratives"),

                  conditionalPanel(
                    condition = "!output.has_illustratives",
                    div(class = "info-box",
                        icon("info-circle"),
                        strong("Aucune variable illustrative sélectionnée."),
                        br(),
                        "Retournez à l'onglet 'Nettoyage' pour en sélectionner."
                    )
                  ),

                  conditionalPanel(
                    condition = "output.has_illustratives",
                    actionButton("lancer_prediction",
                                 "Lancer la prédiction",
                                 icon = icon("rocket"),
                                 class = "btn-success")
                  ),

                  hr(),
                  verbatimTextOutput("resultats_prediction")
              )
            )
          )
      )
    )
  )
)
