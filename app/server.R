library(DT)
library(readxl)
library(shiny)
library(shinyjs)
library(ClusterVariable)

server <- function(input, output, session) {
  #fichier lourd
  options(shiny.maxRequestSize = 1000 * 1024^2)

  disable("coude")
  disable("interpreter")
  disable("Importer")


  # MÉMOIRE DU MODÈLE ENTRAÎNÉ (Pour ce souvenir des calculs de cluster)

  model_reactif <- reactiveVal(NULL)

  # 1. IMPORTATION DES DONNÉES X

  data <- reactive({
    req(input$fichier)
    ext <- tools::file_ext(input$fichier$name)

    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$fichier$datapath, sep = input$separateur)
      } else if (ext %in% c("xlsx", "xls")) {
        read_excel(input$fichier$datapath)
      } else {
        stop("Format non pris en charge.")
      }
    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error")
      return(NULL)
    })

    return(df)
  })

  # --- Bouton valider ---
  observeEvent(input$valider, {
    req(data())
    showNotification("Importation réussie !", type = "message")
    updateNavbarPage(session, "onglets", selected = "Nettoyage")
  })

  # --- Tableau page Importation ---
  output$tableau_import <- renderDT({
    req(data())
    datatable(
      head(data(), 10),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE)
  })


  # 2. NETTOYAGE DES DONNÉES X

  cleaned_data <- reactiveVal(NULL)

  observe({
    req(data())
    cleaned_data(data())
  })

  # === Nettoyage avec le bouton ========
  observeEvent(input$nettoyer, {
    req(cleaned_data())
    df <- cleaned_data()

    # --- Imputation des NA ---
    if (input$supprimer_na) {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        }
      }
    }



    cleaned_data(df)
  })


  # === Affichage du tableau nettoyé =====
  output$tableau_importe_nettoye_x <- renderDT({
    req(cleaned_data())
    datatable(
      head(cleaned_data(), 10),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # ==== Statistiques descriptives ====
  output$statistiques_x <- renderUI({
    req(cleaned_data())
    df <- cleaned_data()

    tagList(
      DT::datatable(summary(df)),

      hr(),

      h4("Dimensions"),
      HTML(paste("Nombre de lignes :", nrow(df), "<br>,
		Nombre de colonnes :", ncol(df))),
      hr(),

      h4("Types des colonnes"),
      DT::datatable(
        data.frame(Colonne = names(df), Type = sapply(df, class)),
        rownames = FALSE
      )
    )
  })

  observeEvent(input$passer_clustering, {
    req(cleaned_data())
    updateNavbarPage(session, "onglets", selected = "Clustering")
  })

  #========  passer au clustering ===============
  #===apercu des donne netoyes dans la page clustering===
  # tableOutput("tableau_cluster"),
  output$tableau_cluster <- renderDT({
    req(cleaned_data())
    datatable(
      head(cleaned_data(), 5),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })


  # ===============================
  # 3. LANCER LE CLUSTERING (X)
  # ===============================

  observeEvent(input$lancer, {
    req(cleaned_data())

    disable("interpreter")
    disable("coude")
    disable("Importer")

    # Vérification kmeans
    if (input$method == "kmeans" &&
        !all(sapply(cleaned_data(), is.numeric))) {
      showNotification("kmeans : toutes les colonnes doivent être numériques.", type = "warning")
      return()
    }

    # ---- KMEANS ----
    if (input$method == "kmeans") {
      model <- clusterVariable$new(k = input$k)
      model$fit(cleaned_data())

      model_reactif(model)     # On sauvegarde le modèle

      enable("coude")
      enable("Importer")
      enable("interpreter")

      output$Résumé <- renderPrint(model$print())
    }

    # ---- CAH ----
    if (input$method == "CAH") {

      k_val <- suppressWarnings(as.numeric(input$k))
      if (is.na(k_val) || k_val <= 1) k_val <- NULL

      model <- CAH$new("ward.D2")
      model$fit(cleaned_data())
      model$cutree(k = k_val)

      model_reactif(model)     # On sauvegarde le modele

      enable("coude")
      enable("Importer")
      enable("interpreter")

      output$Résumé <- renderPrint(model$summary())
    }

    if(input$method == "ACM") {
      #Partie de Marvin
    }
  })


  # ===============================
  # 4. MÉTHODE DU COUDE
  # ===============================

  observeEvent(input$coude, {
    req(cleaned_data())

    if (input$method == "kmeans") {
      model <- clusterVariable$new(k = input$k)
      model$fit(cleaned_data())

      output$afficher_coude <- renderPlot({
        model$plot_elbow()
      })
    }
    if (input$method == "CAH"){
      #Miléna

    }
    if (input$method == "ACM"){
      #Partie de Marvin
    }
  })


  # ===============================
  # 5. INTERPRÉTATION (X)
  # ===============================

  # Bouton pour naviguer vers les résultats
  observeEvent(input$interpreter, {
    req(cleaned_data())

    model <- model_reactif()
    if (is.null(model)) {
      showNotification("Veuillez lancer le clustering d'abord.", type = "error")
      return()
    }

    # Changement d'onglet
    updateNavbarPage(session, "onglets", selected = "Résultats du Clustering")
  })

  # === OUTPUTS RÉACTIFS (toujours actifs) ===

  # Qualité du clustering
  output$qualite <- renderPrint({
    req(model_reactif())
    model <- model_reactif()

    if (input$method == "kmeans") {
      model$cluster_quality_report()
    } else if (input$method == "CAH") {
      model$summary()
    } else if (input$method == "ACM") {
      #Partie de Marvin
      cat("Résultats ACM à venir...")
    }
  })

  # === VISUALISATIONS DU CLUSTERING ===

  # Plot PCA (Kmeans)
  output$pca_plot <- renderPlot({
    req(model_reactif())
    req(input$method == "kmeans")

    model <- model_reactif()
    model$plot_clusters()
  })

  # Heatmap (Kmeans)
  output$heatmap <- renderPlot({
    req(model_reactif())
    req(input$method == "kmeans")

    model <- model_reactif()
    model$plot_heatmap()
  })

  # Visualisations CAH (Miléna)

  #{{================du code ici ==============}}

  # Visualisations ACM (Marvin)

  #{{================du code ici ==============}}









  # ==================================================
  # 6. IMPORTATION DES VARIABLES EXPLICATIVES (MODAL)
  # ==================================================

  # NOUVEAU: Le bouton Importer n'ouvre plus un nouvel onglet, mais le modal
  # Pas besoin de observeEvent pour changer d'onglet

  data_exp <- reactive({
    req(input$fichier_exp)
    ext <- tools::file_ext(input$fichier_exp$name)

    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$fichier_exp$datapath, sep = input$separateur_exp)
      } else if (ext %in% c("xlsx", "xls")) {
        read_excel(input$fichier_exp$datapath)
      } else {
        stop("Format non pris en charge.")
      }
    }, error = function(e) {
      showNotification(paste("Erreur explicatives:", e$message), type = "error")
      return(NULL)
    })

    return(df)
  })

  output$tableau_import_exp <- renderDT({
    req(data_exp())
    datatable(head(data_exp(), 10),
              options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE)
  })

  # NOUVEAU: Version UI pour le modal
  output$tableau_import_exp_ui <- renderUI({
    if (is.null(data_exp())) {
      return(p("Aucune donnée importée", style = "color: #7f8c8d; font-style: italic;"))
    }
    DTOutput("tableau_import_exp")
  })

  observeEvent(input$valider_exp, {
    req(data_exp())
    showNotification("Variables explicatives importées.", type = "message")
  })


  # 7. NETTOYAGE EXPLICATIVES

  cleaned_data_exp <- reactiveVal(NULL)

  observe({
    req(data_exp())
    cleaned_data_exp(data_exp())
  })

  observeEvent(input$nettoyer_exp, {
    req(cleaned_data_exp())
    df <- cleaned_data_exp()

    if (input$supprimer_na_exp) {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        } else {
          df[[col]][is.na(df[[col]]) | df[[col]] == "" | df[[col]] == "NA"] <- "manquant"
        }
      }
    }



    cleaned_data_exp(df)
    showNotification("Nettoyage effectué avec succès !", type = "message")
  })

  output$tableau_importe_nettoye_exp <- renderDT({
    req(cleaned_data_exp())
    datatable(head(cleaned_data_exp(), 5),
              options = list(pageLength = 5, scrollX = TRUE),
              rownames = FALSE
    )
  })

  # NOUVEAU: Version UI pour le modal
  output$tableau_importe_nettoye_exp_ui <- renderUI({
    if (is.null(cleaned_data_exp())) {
      return(p("Aucune donnée nettoyée", style = "color: #7f8c8d; font-style: italic;"))
    }
    DTOutput("tableau_importe_nettoye_exp")
  })

  # ==== Statistiques descriptives ====
  output$statistiques_exp <- renderUI({
    req(cleaned_data())
    df <- cleaned_data()

    tagList(
      DT::datatable(summary(df)),

      hr(),

      h4("Dimensions"),
      HTML(paste("Nombre de lignes :", nrow(df), "<br>,
		Nombre de colonnes :", ncol(df))),
      hr(),

      h4("Types des colonnes"),
      DT::datatable(
        data.frame(Colonne = names(df), Type = sapply(df, class)),
        rownames = FALSE
      )
    )
  })

  # NOUVEAU: Indicateur si des variables exp sont chargées
  output$has_exp_data <- reactive({
    !is.null(cleaned_data_exp())
  })
  outputOptions(output, "has_exp_data", suspendWhenHidden = FALSE)

  # NOUVEAU: Badge d'information
  output$badge_variables_exp <- renderUI({
    if (!is.null(cleaned_data_exp())) {
      div(
        style = "margin-bottom: 15px;",
        tags$span(
          class = "badge-info",
          icon("check-circle"),
          paste0(ncol(cleaned_data_exp()), " variables illustratives chargées")
        )
      )
    }
  })


  # 8. PRÉDICTION AVEC VARIABLES EXPLICATIVES
  # Maintenant géré dans observeEvent(input$Prediction_modal) ci-dessus

  # NOUVEAU: Observer pour ouvrir le modal depuis les boutons
  observeEvent(input$Importer, {
    showModal(modalDialog(
      title = tagList(icon("plus-square"), " Importer des variables illustratives"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Fermer"),
        actionButton("Prediction_modal", "Lancer la prédiction",
                     icon = icon("rocket"),
                     class = "btn-success")
      ),

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
    ))
  })

  observeEvent(input$open_modal_from_results, {
    showModal(modalDialog(
      title = tagList(icon("plus-square"), " Importer des variables illustratives"),
      size = "l",
      easyClose = TRUE,
      footer = tagList(
        modalButton("Fermer"),
        actionButton("Prediction_modal", "Lancer la prédiction",
                     icon = icon("rocket"),
                     class = "btn-success")
      ),

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
    ))
  })

  # Gestion du bouton Prediction dans le modal
  observeEvent(input$Prediction_modal, {
    req(cleaned_data(), cleaned_data_exp())

    model <- model_reactif()

    if (is.null(model)) {
      showNotification("Veuillez lancer le clustering d'abord.", type = "error")
      return()
    }

    pred <- model$predict(cleaned_data_exp())

    output$summary_output <- renderPrint({
      cat("=== Résultats de la prédiction ===\n")
       pred
    })

    showNotification("Prédiction effectuée avec succès !", type = "message")
    removeModal()
    updateNavbarPage(session, "onglets", selected = "Résultats du Clustering")
  })

}
