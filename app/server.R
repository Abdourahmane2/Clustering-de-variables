library(DT)
library(readxl)
library(shiny)
library(shinyjs)
library(ClusterVariable)

server <- function(input, output, session) {

  # Configuration
  options(shiny.maxRequestSize = 1000 * 1024^2)

  # Désactivation initiale des boutons
  disable("coude")
  disable("interpreter")
  disable("lancer_prediction")

  # ===== VARIABLES RÉACTIVES =====
  data_brute <- reactiveVal(NULL)
  data_actives <- reactiveVal(NULL)
  data_illustratives <- reactiveVal(NULL)
  model <- reactiveVal(NULL)
  model_reactif <- reactiveVal(NULL)  # AJOUT: Variable manquante pour ACM

  # ===== 1. IMPORTATION =====

  # Chargement du fichier
  observeEvent(input$fichier, {
    req(input$fichier)

    ext <- tools::file_ext(input$fichier$name)

    df <- tryCatch({
      if (ext == "csv") {
        read.csv(input$fichier$datapath, sep = input$separateur, stringsAsFactors = FALSE)
      } else if (ext %in% c("xlsx", "xls")) {
        as.data.frame(read_excel(input$fichier$datapath))
      } else {
        stop("Format non supporté. Utilisez CSV ou Excel.")
      }
    }, error = function(e) {
      showNotification(paste("Erreur lors du chargement:", e$message),
                       type = "error", duration = 10)
      return(NULL)
    })

    if (!is.null(df) && nrow(df) > 0) {
      data_brute(df)
      showNotification("Fichier chargé avec succès!", type = "message")
    }
  })

  # Mise à jour des choix de colonnes
  observe({
    req(data_brute())
    colonnes <- names(data_brute())

    updateCheckboxGroupInput(session, "colonnes_actives",
                             choices = colonnes,
                             selected = colonnes)

    updateCheckboxGroupInput(session, "colonnes_illustratives",
                             choices = colonnes,
                             selected = NULL)
  })

  # Affichage aperçu importation
  output$tableau_import <- renderDT({
    req(data_brute())
    datatable(
      head(data_brute(), 20),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
      ),
      rownames = FALSE
    )
  })

  # Bouton valider
  observeEvent(input$valider, {
    req(data_brute())
    showNotification("Données validées! Passez au nettoyage.", type = "message")
    updateNavbarPage(session, "onglets", selected = "Nettoyage")
  })

  # ===== 2. NETTOYAGE =====

  # Synchronisation des sélections (actives <-> illustratives)
  observeEvent(input$colonnes_actives, {
    illus <- input$colonnes_illustratives
    actives <- input$colonnes_actives

    nouveau_illus <- setdiff(illus, actives)

    if (!identical(nouveau_illus, illus)) {
      updateCheckboxGroupInput(session, "colonnes_illustratives",
                               selected = nouveau_illus)
    }
  })

  observeEvent(input$colonnes_illustratives, {
    actives <- input$colonnes_actives
    illus <- input$colonnes_illustratives

    nouveau_actives <- setdiff(actives, illus)

    if (!identical(nouveau_actives, actives)) {
      updateCheckboxGroupInput(session, "colonnes_actives",
                               selected = nouveau_actives)
    }
  })

  # Préparation des données actives
  observe({
    req(data_brute())

    if (!is.null(input$colonnes_actives) && length(input$colonnes_actives) > 0) {
      data_actives(data_brute()[, input$colonnes_actives, drop = FALSE])
    } else {
      data_actives(NULL)
    }
  })

  # Préparation des données illustratives
  observe({
    req(data_brute())

    if (!is.null(input$colonnes_illustratives) && length(input$colonnes_illustratives) > 0) {
      data_illustratives(data_brute()[, input$colonnes_illustratives, drop = FALSE])
    } else {
      data_illustratives(NULL)
    }
  })

  # Bouton nettoyer
  observeEvent(input$nettoyer, {
    req(data_actives())

    df <- data_actives()

    # Imputation des NA pour variables numériques
    if (input$supprimer_na) {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          moyenne <- mean(df[[col]], na.rm = TRUE)
          if (!is.na(moyenne)) {
            df[[col]][is.na(df[[col]])] <- moyenne
          }
        }
      }
    }

    data_actives(df)

    # Nettoyage des illustratives
    if (!is.null(data_illustratives())) {
      df_illus <- data_illustratives()

      if (input$supprimer_na) {
        for (col in names(df_illus)) {
          if (is.numeric(df_illus[[col]])) {
            moyenne <- mean(df_illus[[col]], na.rm = TRUE)
            if (!is.na(moyenne)) {
              df_illus[[col]][is.na(df_illus[[col]])] <- moyenne
            }
          } else {
            df_illus[[col]][is.na(df_illus[[col]]) | df_illus[[col]] == ""] <- "manquant"
          }
        }
      }

      data_illustratives(df_illus)
    }

    showNotification("Nettoyage appliqué avec succès!", type = "message")
  })

  # Affichage tableau nettoyé
  output$tableau_nettoye <- renderDT({
    req(data_actives())
    datatable(
      head(data_actives(), 20),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
      ),
      rownames = FALSE
    )
  })

  # Statistiques
  output$statistiques <- renderUI({
    req(data_actives())
    df <- data_actives()

    stats_df <- data.frame(
      Variable = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      NA_count = sapply(df, function(x) sum(is.na(x))),
      NA_pct = sapply(df, function(x) round(100 * sum(is.na(x)) / length(x), 2))
    )

    tagList(
      h4("Dimensions"),
      p(paste("Lignes:", nrow(df), "| Colonnes:", ncol(df))),
      hr(),
      h4("Informations par variable"),
      DT::datatable(stats_df,
                    options = list(pageLength = 10, scrollX = TRUE),
                    rownames = FALSE)
    )
  })

  # Passer au clustering
  observeEvent(input$passer_clustering, {
    req(data_actives())

    if (is.null(input$colonnes_actives) || length(input$colonnes_actives) == 0) {
      showNotification("Veuillez sélectionner au moins une variable active.",
                       type = "warning")
      return()
    }

    updateNavbarPage(session, "onglets", selected = "Clustering")
  })

  # ===== 3. CLUSTERING =====

  # Affichage données clustering
  output$tableau_cluster <- renderDT({
    req(data_actives())
    datatable(
      head(data_actives(), 10),
      options = list(
        pageLength = 5,
        scrollX = TRUE,
        language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json')
      ),
      rownames = FALSE
    )
  })

  # Lancer le clustering
  observeEvent(input$lancer, {
    req(data_actives())

    disable("interpreter")
    disable("coude")
    disable("lancer_prediction")

    df <- data_actives()
    k_val <- as.integer(input$k)

    # Validation
    if (input$method %in% c("kmeans", "ACM")) {
      if (is.na(k_val) || k_val < 2) {
        showNotification("Le nombre de clusters doit être ≥ 2",
                         type = "error")
        return()
      }


    if (k_val >= nrow(df)) {
      showNotification("Le nombre de clusters doit être < nombre de lignes",
                       type = "error")
      return()
    }
  }

    tryCatch({
      if (input$method == "kmeans") {
        # Vérification numériques
        if (!all(sapply(df, is.numeric))) {
          showNotification("K-means nécessite uniquement des variables numériques.",
                           type = "warning")
          return()
        }

        mod <- clusterVariable$new(k = k_val)
        mod$fit(df)
        model(mod)

        output$resume_clustering <- renderPrint({
          mod$print()
        })

        enable("coude")
        enable("interpreter")
        if (!is.null(data_illustratives())) enable("lancer_prediction")

        showNotification("Clustering K-means terminé!", type = "message")

      } else if (input$method == "CAH") {
        if (!all(sapply(df, is.numeric))) {
          showNotification("CAH nécessite des variables numériques.",
                           type = "warning")
          return()
        }

        # Créer le modèle avec la bonne syntaxe
        mod <- CAH$new(method = "ward.D2")

        # Entraîner
        mod$fit(df)

        # Si l’utilisateur a choisi k → on coupe
        if (!is.na(k_val) && k_val >= 2) {
          mod$cutree(k = k_val)
        }else {
          mod$cutree()
        }

        # Sauvegarder le modèle
        model(mod)

        # Résumé
        output$resume_clustering <- renderPrint(mod$print())

        enable("coude")
        enable("interpreter")
        if (!is.null(data_illustratives())) enable("lancer_prediction")

        showNotification("Clustering CAH terminé!", type = "message")

      } else if (input$method == "ACM") {
        if (!any(sapply(df, function(x) is.factor(x) || is.character(x)))) {
          showNotification("ACM nécessite au moins une variable catégorielle.",
                           type = "warning")
          return()
        }

        mod <- CAH_mixtes$new(n_components = 5)
        mod$fit(df)

        # Effectuer le clustering hiérarchique
        k_val <- suppressWarnings(as.numeric(input$k))
        if (is.na(k_val) || k_val <= 1) k_val <- 3

        labels <- mod$clustering_hierarchical(n_clusters = k_val, method = "ward")

        # Sauvegarder le modèle avec les labels
        model_reactif(mod)
        model(mod)  # CORRECTION: Aussi sauvegarder dans model() pour cohérence

        enable("coude")
        enable("interpreter")
        if (!is.null(data_illustratives())) enable("lancer_prediction")

        output$resume_clustering <- renderPrint({
          mod$summary()
        })

        showNotification("Clustering ACM terminé!", type = "message")
      }

    }, error = function(e) {
      showNotification(paste("Erreur clustering:", e$message),
                       type = "error", duration = 10)
    })
  })

  # Méthode du coude
  observeEvent(input$coude, {
    req(data_actives())

    output$plot_coude <- renderPlot({
      if (input$method == "kmeans") {
        req(model())
        model()$plot_elbow()
      } else if (input$method == "CAH") {
        req(model())
        model()$plot("elbow")
      } else if (input$method == "ACM") {
        req(model_reactif())
        mod <- model_reactif()

        # Vérifier si la méthode existe
        if (!is.null(mod$elbow_method)) {
          return(mod$elbow_method())
        } else {
          showNotification("La méthode du coude n'est pas disponible pour l'ACM.", type = "warning")
          return(NULL)
        }
      }
    })
  })

  # Bouton interpréter
  observeEvent(input$interpreter, {
    req(model())
    updateNavbarPage(session, "onglets", selected = "Résultats")
  })

  # ===== 4. RÉSULTATS =====

  # Badge variables illustratives
  output$badge_variables <- renderUI({
    if (!is.null(data_illustratives())) {
      div(
        style = "margin-bottom: 15px;",
        tags$span(
          class = "badge-info",
          icon("check-circle"),
          paste(ncol(data_illustratives()), "variables illustratives chargées")
        )
      )
    }
  })

  # Indicateur présence illustratives
  output$has_illustratives <- reactive({
    !is.null(data_illustratives())
  })
  outputOptions(output, "has_illustratives", suspendWhenHidden = FALSE)

  # Qualité
  output$qualite <- renderPrint({
    req(model())

    if (input$method == "kmeans") {
      model()$cluster_quality_report()
    } else if (input$method == "CAH") {
      model()$summary()
    } else if (input$method == "ACM") {
      req(model_reactif())
      mod <- model_reactif()
      mod$qualite_clustering()
    }
  })

  # Visualisations K-means
  output$pca_plot <- renderPlot({
    req(model())
    req(input$method == "kmeans")
    model()$plot_clusters()
  })

  output$heatmap <- renderPlot({
    req(model())
    req(input$method == "kmeans")
    model()$plot_heatmap()
  })

  # Visualisations CAH
  output$dendrogramme <- renderPlot({
    req(model())
    req(input$method == "CAH")
    model()$plot("dendrogramme")
  })

  output$pca_cah <- renderPlot({
    req(model())
    req(input$method == "CAH")
    model()$plot("acp")
  })

  output$mds_cah <- renderPlot({
    req(model())
    req(input$method == "CAH")
    model()$plot("mds")
  })

  output$silhouette_cah <- renderPlot({
    req(model())
    req(input$method == "CAH")
    model()$plot("silhouette")
  })

  # Visualisations ACM
  output$dendrogramme_acm <- renderPlot({
    req(model_reactif())
    req(input$method == "ACM")
    mod <- model_reactif()

    # CORRECTION: Utiliser la bonne méthode selon votre package
    if (!is.null(mod$plot_variables)) {
      mod$plot_variables()
    } else if (!is.null(mod$plot)) {
      mod$plot("variables")
    }
  })

  output$pca_acm <- renderPlot({
    req(model_reactif())
    req(input$method == "ACM")
    mod <- model_reactif()

    # CORRECTION: Utiliser la bonne méthode selon votre package
    if (!is.null(mod$dendo)) {
      mod$dendo()
    } else if (!is.null(mod$plot)) {
      mod$plot("dendrogramme")
    }
  })

  # ===== 5. PRÉDICTIONS =====

  observeEvent(input$lancer_prediction, {
    req(model())
    req(data_illustratives())

    tryCatch({

      output$resultats_prediction <- renderPrint({
        cat("=== Results of the  predict ===\n\n")

        if (input$method == "CAH") {

          # On exécute malgré tout predict() pour mettre à jour predict_result
          invisible(model()$predict(data_illustratives()))

          cat("=== Summary of the CAH model ===\n\n")
          return(model()$summary())
        }

        # CORRECTION: Passer les données en paramètre
        predictions <- model()$predict(data_illustratives())
        print(predictions)


        #Il faut que je fasse un cas pour la CAH qui renvoi
        #summary <-model()$summary()
      })

      showNotification("Prédiction effectuée avec succès!", type = "message")

    }, error = function(e) {
      showNotification(paste("Erreur prédiction:", e$message),
                       type = "error", duration = 10)
    })
  })
}
