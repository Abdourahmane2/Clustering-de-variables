library(shiny)
library(readxl)

ui <- fluidPage(
  titlePanel("Importation CSV/Excel"),
  sidebarLayout(
    sidebarPanel(
      fileInput(
        "fichier", "Importer un fichier CSV ou Excel",
        accept = c(".csv", ".xlsx", ".xls")
      ),
      selectInput("separateur", "Séparateur", choices = c(Virgule = ",", Point_Virgule = ";", Tabulation = "\t")),
      conditionalPanel(
        condition = "output.data_loaded == true",
        selectInput("method", "Méthode de clustering", choices = c("kmeans", "kmodes", "hierarchical")),
        numericInput("k", "Nombre de clusters", value = 2, min = 2),
        actionButton("lancer", "Lancer le clustering")
      )
    ),
    mainPanel(
      tableOutput("tableau"),
      tableOutput("cluster"),
      verbatimTextOutput("summary")
    )
  )
)
