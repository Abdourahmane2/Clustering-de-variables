library(DT)

server <- function(input, output, session) {
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


  # --- Tableau page Clustering ---
  output$tableau_cluster <- renderTable({
    req(data())
    head(data(), 10)
  })
  #-- t ableau netoyage ---
  output$tableau_importe_nettoye <- renderDT({
    req(data())
    datatable(
      head(data(), 10),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE
    )
  })

  #====affichage des stat =====
  output$statistiques_ui <- renderUI({
    req(data)
    df <- data()

    tagList(
      h4("📊 Statistiques descriptives"),
      datatable(summary(df)),

      h4("📐 Dimensions"),
      HTML(paste("Nombre de lignes :", nrow(df), "<br>",
                 "Nombre de colonnes :", ncol(df))),

      h4("📝 Types des colonnes"),
      #affichage du type  pour chaque colonne
      datatable(data.frame(Colonne = names(df), Type = sapply(df, class)) ,
                options = list(pageLength = 10, scrollX = TRUE),
                choices = list(searching = FALSE, lengthChange = FALSE),
                rownames = FALSE) ,
      #changer le type de chaque colonne si on veut


    )
  })


}
