library(shiny)
library(readxl)
library(DT)
library(bslib)
library(shinyjs)

# Configuration globale pour accepter les gros fichiers (200 MB)
options(shiny.maxRequestSize = 200 * 1024^2)

ui <- navbarPage(
  useShinyjs(),    #pour desactiver les buttons
  id = "onglets",
  #espace entre le nitre et les onglets
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

  # Styles CSS personnalisés
  tags$head(
    tags$style(HTML("
      /* Style général */
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

      /* Cards améliorées */
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

      /* Titres de sections */
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

      /* Boutons améliorés */
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

      /* Inputs améliorés */
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

      /* Labels */
      label {
        color: #2c3e50;
        font-weight: 500;
        margin-bottom: 8px;
      }

      /* Checkboxes */
      .checkbox label {
        font-weight: 400;
        color: #555;
      }

      /* Sidebar */
      .well {
        background: white;
        border: none;
        border-radius: 15px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.1);
      }

      /* Tables DataTables */
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

      /* Help text */
      .help-block {
        color: #7f8c8d;
        font-size: 0.9em;
        font-style: italic;
      }

      /* Tabs */
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

      /* Outputs */
      .shiny-output-error {
        color: #e74c3c;
        font-weight: 500;
      }

      pre {
        background: #f8f9fa;
        border: 1px solid #e0e0e0;
        border-radius: 8px;
        padding: 15px;
      }

      /* Plots */
      .shiny-plot-output {
        border-radius: 10px;
        overflow: hidden;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
      }

      /* Progress indicators */
      .shiny-notification {
        border-radius: 10px;
        font-weight: 500;
      }

      /* HR */
      hr {
        border-top: 2px solid #e0e0e0;
        margin: 25px 0;
      }

      /* Icon enhancements */
      .fa, .glyphicon {
        margin-right: 8px;
      }

      /* Main panel content */
      .col-sm-9 {
        padding: 20px;
      }

      /* Action button container */
      .action-buttons {
        display: flex;
        flex-direction: column;
        gap: 10px;
        margin-top: 20px;
      }

      .action-buttons .btn {
        width: 100%;
      }

      /* File input styling */
      .btn-file {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
        border-radius: 8px;
      }

      /* Container fluide */
      .container-fluid {
        padding: 30px;
      }

      /* MODAL STYLING */
      .modal-content {
        border-radius: 15px;
        border: none;
        box-shadow: 0 20px 60px rgba(0,0,0,0.3);
      }

      .modal-header {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        color: white;
        border-radius: 15px 15px 0 0;
        padding: 20px 25px;
        border: none;
      }

      .modal-title {
        font-weight: 600;
        font-size: 1.3em;
      }

      .modal-body {
        padding: 30px;
      }

      .modal-footer {
        border-top: 2px solid #e0e0e0;
        padding: 20px 25px;
      }

      .close {
        color: white;
        opacity: 1;
        text-shadow: none;
        font-size: 1.5em;
      }

      .close:hover {
        color: #f0f0f0;
      }

      /* Badge pour indicateur */
      .badge-info {
        background: linear-gradient(135deg, #3498db 0%, #2980b9 100%);
        color: white;
        padding: 5px 12px;
        border-radius: 20px;
        font-size: 0.85em;
        font-weight: 500;
      }

      /* Info box */
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
    "))
  ),

  # =============================== Page d'importation ===============================
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
            # Indication de la taille maximale
            helpText("📊 Taille maximale : 1GB", style = "color: #7f8c8d; font-size: 0.9em;"),
            selectInput(
              "separateur",
              label = tagList(icon("separator"), "Séparateur"),
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
    title = tagList(icon("broom"), "Nettoyage"),
    value = "Nettoyage",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title",
               icon("sliders-h"), "Options de nettoyage"),
            div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px; margin-bottom: 15px;",
                checkboxInput(
                  "supprimer_na",
                  HTML("<strong>Imputation des valeurs manquantes</strong><br>
                       <small style='color: #7f8c8d;'>Remplacer les NA par la moyenne</small>"),
                  value = FALSE
                )
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
               icon("table"), "Tableau nettoyé"),
            tabsetPanel(
              id = "tabs_nettoyage",
              tabPanel(
                tagList(icon("eye"), "Aperçu du tableau"),
                br(),
                DTOutput("tableau_importe_nettoye_x")
              ),
              tabPanel(
                tagList(icon("chart-bar"), "Statistiques"),
                br(),
                uiOutput("statistiques_x")
              )
            )
        )
      )
    )
  ),

  # =============================== Page de clustering ===============================
  tabPanel(
    title = tagList(icon("project-diagram"), "Clustering"),
    value = "Clustering",
    sidebarLayout(
      sidebarPanel(
        width = 3,
        div(class = "card",
            h4(class = "section-title",
               icon("cogs"), "Paramètres"),
            selectInput(
              "method",
              label = tagList(icon("puzzle-piece"), "Méthode de clustering"),
              choices = c("K-means" = "kmeans",
                          "ACM" = "ACM",
                          "CAH" = "CAH")
            ),
            numericInput("k",
                         label = tagList(icon("hashtag"), "Nombre de clusters (k)"),
                         value = 2,
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

                actionButton("interpreter", "Résultats détaillés",
                             icon = icon("microscope"),
                             class = "btn-primary")
            )
        )
      ),
      mainPanel(
        width = 9,
        div(class = "card",
            h4(class = "section-title",
               icon("database"), "Aperçu des données nettoyées"),
            DTOutput("tableau_cluster")
        ),
        div(class = "card",
            h4(class = "section-title",
               icon("chart-pie"), "Résultats du clustering"),
            DTOutput("resultat_cluster"),
            hr(),
            verbatimTextOutput("Résumé"),
            verbatimTextOutput("summary"),
            plotOutput("afficher_coude")
        )
      )
    )
  ),

  #============  page de resultats du kmenas ==============================
  tabPanel(
    title = tagList(icon("chart-line"), "Résultats"),
    value = "Résultats du Clustering",
    fluidPage(
      div(class = "card",
          h4(class = "section-title",
             icon("trophy"), "Analyse des résultats de clustering"),

          # NOUVEAU: Badge d'information si variables illustratives chargées
          uiOutput("badge_variables_exp"),

          tabsetPanel(
            id = "tabs_resultats_clustering",
            type = "tabs",

            # Onglet 1 : Indicateurs de qualité
            tabPanel(
              tagList(icon("star"), "Qualité du Clustering"),
              br(),
              div(class = "card",
                  h4(style = "color: #2c3e50; font-weight: 600;",
                     icon("gauge-high"), "Indicateurs de Qualité"),
                  verbatimTextOutput("qualite")
              )
            ),

            # Onglet 2 : Visualisations
            tabPanel(
              tagList(icon("chart-area"), "Visualisations"),
              br(),
              div(class = "card",
                  plotOutput("pca_plot", height = "500px") ,
                  # plotOutput("visualisation_cah", height = "500px") ,
                  # plotOutput("visualisation_acm", height = "500px")
              ),
              br(),
              div(class = "card",
                  plotOutput("heatmap", height = "500px")
              )
            ),

            # NOUVEAU: Onglet pour les prédictions avec variables illustratives
            tabPanel(
              tagList(icon("magic"), "Prédictions"),
              br(),
              div(class = "card",
                  h4(style = "color: #2c3e50; font-weight: 600;",
                     icon("wand-magic-sparkles"), "Résultats des prédictions"),

                  # Bouton pour ouvrir le modal si pas encore de variables
                  conditionalPanel(
                    condition = "output.has_exp_data == false",
                    div(class = "info-box",
                        icon("info-circle"),
                        strong("Aucune variable illustrative chargée."),
                        br(),
                        "Cliquez sur le bouton ci-dessous pour importer des variables illustratives."
                    ),
                    br(),
                    actionButton("open_modal_from_results",
                                 "Importer des variables illustratives",
                                 icon = icon("plus-circle"),
                                 class = "btn-default")
                  ),

                  # Affichage des résultats si variables chargées
                  verbatimTextOutput("summary_output")
              )
            )
          )
      )
    )
  ),

  # ===============================================
  # MODAL POUR VARIABLES ILLUSTRATIVES
  # ===============================================

  # Script JavaScript pour gérer le modal
  tags$script(HTML("
    $(document).ready(function() {
      // Handlers pour ouvrir/fermer le modal depuis R
      Shiny.addCustomMessageHandler('openModal', function(modalId) {
        $('#' + modalId).modal('show');
      });

      Shiny.addCustomMessageHandler('closeModal', function(modalId) {
        $('#' + modalId).modal('hide');
      });
    });
  ")),

  tags$div(
    id = "modal_variables_exp",
    class = "modal fade",
    tabindex = "-1",
    role = "dialog",
    `data-backdrop` = "true",
    `data-keyboard` = "true",
    tags$div(
      class = "modal-dialog modal-lg",
      role = "document",
      tags$div(
        class = "modal-content",

        # En-tête du modal
        tags$div(
          class = "modal-header",
          tags$button(
            type = "button",
            class = "close",
            `data-dismiss` = "modal",
            `aria-label` = "Close",
            tags$span(`aria-hidden` = "true", "×")
          ),
          tags$h4(
            class = "modal-title",
            icon("plus-square"),
            " Importer des variables illustratives"
          )
        ),

        # Corps du modal
        tags$div(
          class = "modal-body",

          # Étape 1: Importation
          div(
            h5(style = "color: #3498db; font-weight: 600; margin-bottom: 20px;",
               icon("upload"), " Étape 1 : Sélectionner le fichier"),
            fileInput(
              "fichier_exp",
              label = tagList(icon("file-excel"), "Fichier (CSV ou Excel)"),
              accept = c(".csv", ".xlsx", ".xls"),
              buttonLabel = "Parcourir...",
              placeholder = "Aucun fichier sélectionné"
            ),
            helpText("📊 Taille maximale : 1GB",
                     style = "color: #7f8c8d; font-size: 0.9em;"),
            selectInput(
              "separateur_exp",
              label = tagList(icon("separator"), "Séparateur"),
              choices = c(Virgule = ",",
                          `Point-virgule` = ";",
                          Tabulation = "\t")
            ),
            actionButton("valider_exp",
                         "Valider l'importation",
                         icon = icon("check-circle"),
                         class = "btn-success")
          ),

          hr(),

          # Aperçu des données
          div(
            h5(style = "color: #3498db; font-weight: 600; margin-bottom: 20px;",
               icon("eye"), " Aperçu des données"),
            DTOutput("tableau_import_exp")
          ),

          hr(),

          # Étape 2: Nettoyage
          div(
            h5(style = "color: #3498db; font-weight: 600; margin-bottom: 20px;",
               icon("broom"), " Étape 2 : Options de nettoyage"),
            div(style = "background: #f8f9fa; padding: 15px; border-radius: 8px;",
                checkboxInput(
                  "supprimer_na_exp",
                  HTML("<strong>Imputation intelligente</strong><br>
                       <small style='color: #7f8c8d;'>Numériques → moyenne |
                       Catégorielles → « manquant »</small>"),
                  value = FALSE
                )
            ),
            br(),
            actionButton("nettoyer_exp",
                         "Appliquer le nettoyage",
                         icon = icon("magic"),
                         class = "btn-primary")
          ),

          hr(),

          # Aperçu nettoyé
          div(
            h5(style = "color: #3498db; font-weight: 600; margin-bottom: 20px;",
               icon("table"), " Données nettoyées"),
            DTOutput("tableau_importe_nettoye_exp")
          )
        ),

        # Pied du modal
        tags$div(
          class = "modal-footer",
          tags$button(
            type = "button",
            class = "btn btn-default",
            `data-dismiss` = "modal",
            icon("times"),
            " Fermer"
          ),
          actionButton("Prediction",
                       "Lancer la prédiction",
                       icon = icon("rocket"),
                       class = "btn-success")
        )
      )
    )
  )
)
