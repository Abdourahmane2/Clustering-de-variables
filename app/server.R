library(DT)
library(readxl)
library(shiny)

server <- function(input, output, session) {

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
      h4("Statistiques descriptives"),
      DT::datatable(summary(df)),

      h4("Dimensions"),
      HTML(paste("Nombre de lignes :", nrow(df), "<br>",
                 "Nombre de colonnes :", ncol(df))),

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
}
