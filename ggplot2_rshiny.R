load("data/datos.cancer.RData")          # fuera de server: se lee UNA vez

ui <- fluidPage(
  titlePanel("Mortalidad por tumor"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput("tumores", "Tumores",
                         choices = unique(cancer$tumor),
                         selected = c("MAMA", "PROSTATA")),
      sliderInput("anios", "Periodo", min = min(cancer$periodo),
                  max = max(cancer$periodo),
                  value = range(cancer$periodo), sep = "")
    ),
    mainPanel(plotOutput("grafico"))
  )
)

server <- function(input, output, session) {
  output$grafico <- renderPlot({
    d <- cancer[tumor %in% input$tumores & periodo %between% input$anios]
    ggplot(d, aes(periodo, tasa, colour = tumor, linetype = sexo)) +
      geom_line() + theme_minimal()
  })
}

shinyApp(ui, server)