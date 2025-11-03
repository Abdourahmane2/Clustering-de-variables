server <- function(input, output) {

  data <- reactive({
    req(input$fichier)
    ext <- tools::file_ext(input$fichier$datapath)
    switch(ext,
           csv = read.csv(input$fichier$datapath, header = TRUE, sep = input$separateur),
           xlsx = read_excel(input$fichier$datapath),
           xls = read_excel(input$fichier$datapath),
           validate("Format de fichier non pris en charge")
    )
  })

  # Table pour apercu des données
  output$tableau <- renderTable({
    req(data())
    head(data(), 10)
  })

  # Variable pour rendre le conditionalPanel visible
  output$data_loaded <- reactive({
    req(data())
    TRUE
  })
  outputOptions(output, "data_loaded", suspendWhenHidden = FALSE)
}
