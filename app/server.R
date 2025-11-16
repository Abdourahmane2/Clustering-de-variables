library(DT)
library(readxl)
library(shiny)
library(shinyjs)
library(ClusterVariable)

server <- function(input, output, session) {

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
        } else {
          df[[col]][is.na(df[[col]]) | df[[col]] == "" | df[[col]] == "NA"] <- "manquant"
        }
      }
    }

    # --- Outliers : on NE SUPPRIME PAS DE LIGNES !! ---
    if (input$supprimer_outliers) {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {

          Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
          Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1

          low_bound  <- Q1 - 1.5 * IQR
          high_bound <- Q3 + 1.5 * IQR

          # Remplacement des outliers par NA
          df[[col]][df[[col]] < low_bound | df[[col]] > high_bound] <- NA
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
      model <- clusterVariable$new(k = input$k, data = cleaned_data())
      model$fit()

      model_reactif(model)     # On sauvegarde le modèle

      enable("coude")
      enable("Importer")
      enable("interpreter")

      output$Résumé <- renderPrint(model$summary())
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
        model$tracer_coude()
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

  observeEvent(input$interpreter, {
    req(cleaned_data())

    updateNavbarPage(session, "onglets", selected = "Résultats du Clustering")

    model <- model_reactif()
    if (is.null(model)) {
      showNotification("Veuillez lancer le clustering d'abord.", type = "error")
      return()
    }

    # === KMEANS ===
    if (input$method == "kmeans") {

      output$qualite <- renderPrint({
        sil <- model$indice_silhoute()
        if (is.factor(sil)) sil <- as.numeric(as.character(sil))
        cat("Silhouette =", round(sil, 4))
      })

      output$pca_plot <- renderPlot(model$visualiser_clusters())
      output$heatmap <- renderPlot(model$heatmap_clusters())

      output$summary_output <- renderPrint({
        res <- model$resume_cluster()
        cat("=== Partitions ===\n")
        print(res$partitions)
        cat("\n=== Distances intra ===\n")
        print(res$distances_intra)
        cat("\n=== Distances inter ===\n")
        print(res$dist_inter_cluster)
      })
    }

    # === CAH ===
    if (input$method == "CAH") {
      output$summary_output <- renderPrint({
        model$summary()
      })
    }

    if (input$method == "ACM"){
      #Partie de Marvin
    }
  })


  # ==================================================
  # 6. IMPORTATION DES VARIABLES EXPLICATIVES
  # ==================================================

  observeEvent(input$Importer, {
    updateNavbarPage(session, "onglets", selected = "Importation_explanatory variable")
  })


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
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })

  observeEvent(input$valider_exp, {
    req(data_exp())
    showNotification("Variables explicatives importées.", type = "message")
    updateNavbarPage(session, "onglets", selected = "Cleaning_explanatory variable")
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

    if (input$supprimer_outliers_exp) {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {

          Q1 <- quantile(df[[col]], 0.25, na.rm = TRUE)
          Q3 <- quantile(df[[col]], 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1

          low_bound  <- Q1 - 1.5 * IQR
          high_bound <- Q3 + 1.5 * IQR

          df[[col]][df[[col]] < low_bound | df[[col]] > high_bound] <- NA
        }
      }
    }

    cleaned_data_exp(df)
  })

  output$tableau_importe_nettoye_exp <- renderDT({
    req(cleaned_data_exp())
    datatable(head(cleaned_data_exp(), 10),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE
    )
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



  # 8. PRÉDICTION AVEC VARIABLES EXPLICATIVES
  observeEvent(input$Prediction, {
    req(cleaned_data(), cleaned_data_exp())

    updateNavbarPage(session, "onglets", selected = "Résultats du Clustering")

    model <- model_reactif()

    if (is.null(model)) {
      showNotification("Veuillez lancer le clustering d'abord.", type = "error")
      return()
    }

    pred <- model$predict(X = cleaned_data_exp())

    output$summary_output <- renderPrint({
      cat("=== Résultats de la prédiction ===\n")
      model$summary()
    })

    showNotification("Prédiction effectuée.", type = "message")
  })

}
