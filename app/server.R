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

  # MÉMOIRE DU MODÈLE ENTRAÎNÉ
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

  # Mise à jour des choix de colonnes après importation
  observe({
    req(data())
    toutes_colonnes <- names(data())

    updateCheckboxGroupInput(session, "colonnes_actives",
                             choices = toutes_colonnes,
                             selected = toutes_colonnes)

    updateCheckboxGroupInput(session, "colonnes_illustratives",
                             choices = toutes_colonnes,
                             selected = NULL)
  })

  # SYNCHRONISATION SIMPLE - Quand ACTIVES changent
  observeEvent(input$colonnes_actives, {
    illus_current <- input$colonnes_illustratives
    actives_current <- input$colonnes_actives

    new_illus <- setdiff(illus_current, actives_current)

   if (!identical(new_illus, illus_current)){
     updateCheckboxGroupInput(session, "colonnes_illustratives", selected = new_illus)
   }
  })

  # SYNCHRONISATION SIMPLE - Quand ILLUSTRATIVES changent
  observeEvent(input$colonnes_illustratives, {
    actives_current <- input$colonnes_actives
    illus_current <- input$colonnes_illustratives

    # Retirer des actives les colonnes qui sont en illustratives
    new_actives <- setdiff(actives_current, illus_current)

    if (!identical(new_actives, actives_current)) {
      updateCheckboxGroupInput(session, "colonnes_actives", selected = new_actives)
    }
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
  cleaned_data_illustratives <- reactiveVal(NULL)

  observe({
    req(data())

    # Filtrer selon les colonnes ACTIVES sélectionnées
    if (!is.null(input$colonnes_actives) && length(input$colonnes_actives) > 0) {
      cleaned_data(data()[, input$colonnes_actives, drop = FALSE])
    } else {
      cleaned_data(data())
    }

    # Filtrer selon les colonnes ILLUSTRATIVES sélectionnées
    if (!is.null(input$colonnes_illustratives) && length(input$colonnes_illustratives) > 0) {
      cleaned_data_illustratives(data()[, input$colonnes_illustratives, drop = FALSE])
    } else {
      cleaned_data_illustratives(NULL)
    }
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

    # Nettoyer aussi les illustratives si elles existent
    if (!is.null(cleaned_data_illustratives())) {
      df_ill <- cleaned_data_illustratives()

      if (input$supprimer_na) {
        for (col in names(df_ill)) {
          if (is.numeric(df_ill[[col]])) {
            df_ill[[col]][is.na(df_ill[[col]])] <- mean(df_ill[[col]], na.rm = TRUE)
          } else {
            df_ill[[col]][is.na(df_ill[[col]]) | df_ill[[col]] == "" | df_ill[[col]] == "NA"] <- "manquant"
          }
        }
      }

      cleaned_data_illustratives(df_ill)
    }
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
      HTML(paste("Nombre de lignes :", nrow(df), "<br>",
                 "Nombre de colonnes :", ncol(df))),
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
    if (input$method == "CAH" &&
        !all(sapply(cleaned_data(), is.numeric))) {
      showNotification("Notre CAH traite que les variables numeriques! pour les variables qualitatives essayer la methode ACM.", type = "warning")
      return()
    }

    # ---- KMEANS ----
    if (input$method == "kmeans") {
      model <- clusterVariable$new(k = input$k)
      model$fit(cleaned_data())

      model_reactif(model)

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

      model_reactif(model)

      enable("coude")
      enable("Importer")
      enable("interpreter")

      output$Résumé <- renderPrint(model$print())
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
      k_val <- suppressWarnings(as.numeric(input$k))
      if (is.na(k_val) || k_val <= 1) k_val <- NULL

      model <- CAH$new("ward.D2")
      model$fit(cleaned_data())
      model$cutree(k = k_val)

      output$afficher_coude <- renderPlot({
        model$plot("elbow")
      })
    }

    if (input$method == "ACM"){
      #Partie de Marvin
    }
  })

  # ===============================
  # 5. INTERPRÉTATION (X)
  # ===============================

  observeEvent(input$interpreter, {
    req(cleaned_data())

    model <- model_reactif()
    if (is.null(model)) {
      showNotification("Veuillez lancer le clustering d'abord.", type = "error")
      return()
    }

    updateNavbarPage(session, "onglets", selected = "Résultats du Clustering")
  })

  # === OUTPUTS RÉACTIFS ===

  output$qualite <- renderPrint({
    req(model_reactif())
    model <- model_reactif()

    if (input$method == "kmeans") {
      model$cluster_quality_report()
    } else if (input$method == "CAH") {
      model$summary()
    } else if (input$method == "ACM") {
      cat("Résultats ACM à venir...")
    }
  })

  # Plot PCA pour Kmeans
  output$pca_plot <- renderPlot({
    req(model_reactif())
    req(input$method == "kmeans")

    model <- model_reactif()
    model$plot_clusters()
  })

  # Plot PCA pour CAH
  output$pca_plot_cah <- renderPlot({
    req(model_reactif())
    req(input$method == "CAH")
    model <- model_reactif()
    model$plot("acp")
  })

  #Plot Dendrogram pour CAH
  output$dendrogramme_cah <- renderPlot({
    req(model_reactif())
    req(input$method == "CAH")
    model <- model_reactif()
    model$plot("dendrogramme")
  })

  # Plot Silhouette pour CAH
  output$silhouette_cah <- renderPlot({
    req(model_reactif())
    req(input$method == "CAH")
    model <- model_reactif()
    model$plot("silhouette")
  })

  # Plot MDS pour CAH
  output$mds_cah <- renderPlot({
    req(model_reactif())
    req(input$method == "CAH")
    model <- model_reactif()
    model$plot("mds")
  })


  # Heatmap
  output$heatmap <- renderPlot({
    req(model_reactif())
    req(input$method == "kmeans")

    model <- model_reactif()
    model$plot_heatmap()
  })

  # ==================================================
  # 6. PRÉDICTION AVEC VARIABLES ILLUSTRATIVES
  # ==================================================

  output$has_exp_data <- reactive({
    !is.null(cleaned_data_illustratives())
  })
  outputOptions(output, "has_exp_data", suspendWhenHidden = FALSE)

  output$badge_variables_exp <- renderUI({
    if (!is.null(cleaned_data_illustratives())) {
      div(
        style = "margin-bottom: 15px;",
        tags$span(
          class = "badge-info",
          icon("check-circle"),
          paste0(ncol(cleaned_data_illustratives()), " variables illustratives sélectionnées")
        )
      )
    }
  })

  observeEvent(input$Importer, {
    req(cleaned_data(), cleaned_data_illustratives())

    model <- model_reactif()

    if (is.null(model)) {
      showNotification("Veuillez lancer le clustering d'abord.", type = "error")
      return()
    }

    pred <- model$predict(cleaned_data_illustratives())

    output$summary_output <- renderPrint({
      cat("=== Résultats de la prédiction ===\n")
      pred <- model$summary()
    })

    showNotification("Prédiction effectuée avec succès !", type = "message")
    updateNavbarPage(session, "onglets", selected = "Résultats du Clustering")
  })
}
