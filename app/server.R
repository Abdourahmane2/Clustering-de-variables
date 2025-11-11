library(DT)
library(readxl)
library(shiny)
#importer le package creer dans le dossier R
library(ClusterVariable)


server <- function(input, output, session) {
  disable("coude")
  disable("interpreter")

  # --- Importation des données ---
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
    df
  })

  # --- Bouton valider ---
  observeEvent(input$valider, {
    req(data())
    showNotification("✅ Importation réussie !", type = "message")
    updateNavbarPage(session, "onglets", selected = "Nettoyage")
  })

  # --- Tableau page Importation ---
  output$tableau_import <- renderDT({
    req(data())
    datatable(
      head(data(), 10),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # ======== Tableau page Clustering =========
  # output$tableau_cluster <- renderTable({
  #   req(cleaned_data())
  #   datatable(
  #     head(cleaned_data(), 10),
  #     options = list(pageLength = 10, scrollX = TRUE),
  #     rownames = FALSE
  #   )
  # })

  # ======= Données nettoyées réactives ===========
  cleaned_data <- reactiveVal(NULL)

  # Mettre à jour cleaned_data dès que data() change
  observe({
    req(data())
    cleaned_data(data())
  })

  # === Nettoyage avec le bouton ========
  observeEvent(input$nettoyer, {
    req(cleaned_data())
    df <- cleaned_data()

    if (input$supprimer_na) {
      for (col in names(df)) {
        if (is.numeric(df[[col]])) {
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
        } else {
          df[[col]][df[[col]] == "" | df[[col]] == "NA"] <- "manquant"
        }
      }
    }
    if(input$normaliser){
      for (i in names(df)) {
        if (is.numeric(df[[i]])) {
          df[[i]] <- (df[[i]] - mean(df[[i]], na.rm = TRUE)) / sd(df[[i]], na.rm = TRUE)
        }
      }
    }
    if(input$supprimer_outliers){
      for (i in names(df)) {
        if (is.numeric(df[[i]])) {
          Q1 <- quantile(df[[i]], 0.25, na.rm = TRUE)
          Q3 <- quantile(df[[i]], 0.75, na.rm = TRUE)
          IQR <- Q3 - Q1
          lower_bound <- Q1 - 1.5 * IQR
          upper_bound <- Q3 + 1.5 * IQR
          df <- df[df[[i]] >= lower_bound & df[[i]] <= upper_bound | is.na(df[[i]]), ]
        }
      }
    }

    cleaned_data(df)
  })

  # === Affichage du tableau nettoyé =====
  output$tableau_importe_nettoye <- renderDT({
    req(cleaned_data())
    datatable(
      head(cleaned_data(), 10),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })


  # ==== Statistiques descriptives ====
  output$statistiques_ui <- renderUI({
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

  #========  passer au clustering ===============

  observeEvent(input$passer_clustering, {
    req(cleaned_data())
    updateNavbarPage(session, "onglets", selected = "Clustering")
  })


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
  #quand le user clique sur le bouton lancer le clustering
  observeEvent(input$lancer, {
    if (!all(sapply(cleaned_data(), is.numeric)) & input$method == "kmeans") {
      showNotification("Impossible d'appliquer kmeans : toutes les colonnes doivent être numériques.", type =   "warning")
      return()  # stoppe l'exécution du reste
    }

    req(cleaned_data())
    if(input$method == "kmeans"){
      cluster_quanti <- clusterVariable$new(
        k = input$k,
        data = cleaned_data(),

      )
      cluster_quanti$fit()
      enable("coude")
      enable("interpreter")

      output$Résumé <- renderPrint({
        cluster_quanti$summary()
      })

    }

  })


#si le user clique sur visualiser
  observeEvent(input$coude, {
    req(cleaned_data())
    if(input$method == "kmeans"){
    cluster_quanti <- clusterVariable$new(
      k = input$k,
      data = cleaned_data(),

    )
    cluster_quanti$fit()

    output$afficher_coude<- renderPlot({
      print(cluster_quanti$tracer_coude())
  })
}
  })


  #si le user clique sur le bouton resume
  observeEvent(input$interpreter, {
    updateNavbarPage(session, "onglets", selected = "Résultats du Clustering")
    req(cleaned_data())
    if(input$method == "kmeans"){
    cluster_quanti <- clusterVariable$new(
      k = input$k,
      data = cleaned_data(),

    )
    cluster_quanti$fit()

    output$qualite <- renderPrint({
      indice <- cluster_quanti$indice_silhoute()
      if (is.factor(indice)) {
        indice <- as.numeric(as.character(indice))
      }
       cat("la valeur de l'indice de silhoutte est :", round(indice, 4))
    })

    output$pca_plot <- renderPlot({
      cluster_quanti$visualiser_clusters()
    })

    output$heatmap <- renderPlot({
      cluster_quanti$heatmap_clusters()
    })




    # Afficher le résumé des partitions et autres résultats dans le texte
    output$summary_output <- renderPrint({
      results <- cluster_quanti$resume_cluster()
      cat("=== Nature des partitions ===\n")
      print(results$partitions)
      cat("\n=== Degré d'appartenance aux clusters (Silhouette Score) ===\n")
      cat("Score moyen de la silhouette : ", results$silhouette_score, "\n")
      cat("\n=== Distances intra-cluster ===\n")
      print(results$distances_intra)
      cat("\n=== Distances inter-cluster ===\n")
      print(results$dist_inter_cluster)
    })




        }

    })


}
