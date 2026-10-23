
library(shiny)
library(ggplot2)
library(ggiraph)
library(data.table)
library(DT)



load("data/datos.cancer.RData")

setDT(cancer)

ui <- fluidPage(
  
  titlePanel("Mortalidad por tumor"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # Selección de tumores
      checkboxGroupInput(
        inputId = "tumores",
        label = "Selecciona los tumores:",
        choices = sort(unique(cancer$tumor)),
        selected = intersect(
          c("MAMA", "PROSTATA"),
          unique(cancer$tumor)
        )
      ),
      
      # Selección del periodo
      sliderInput(
        inputId = "anios",
        label = "Rango de años:",
        min = min(cancer$periodo, na.rm = TRUE),
        max = max(cancer$periodo, na.rm = TRUE),
        value = range(cancer$periodo, na.rm = TRUE),
        sep = ""
      ),
      
      # Variable que representa el color
      selectInput(
        inputId = "color",
        label = "Colorear las líneas por:",
        choices = c(
          "Tumor" = "tumor",
          "Sexo" = "sexo"
        ),
        selected = "tumor"
      )
    ),
    
    mainPanel(
      
      h3("Evolución de la mortalidad"),
      
      girafeOutput(
        outputId = "grafico",
        height = "500px"
      ),
      
      br(),
      
      h3("Datos seleccionados"),
      
      DTOutput("tabla")
    )
  )
)



server <- function(input, output, session) {
  
  # Filtrar los datos según los controles
  datos_filtrados <- reactive({
    
    req(input$tumores)
    req(input$anios)
    
    cancer[
      tumor %in% input$tumores &
        periodo %between% input$anios
    ]
  })
  

  output$grafico <- renderGirafe({
    
    d <- copy(datos_filtrados())
    
    req(nrow(d) > 0)
    
    # Tooltip: tumor, año y tasa
    d[, tooltip := paste0(
      "Tumor: ", tumor,
      "\nAño: ", periodo,
      "\nTasa: ", round(tasa, 2)
    )]
    
    # Identificador de cada observación
    d[, id_punto := paste(tumor, sexo, periodo,sep = "_")]
    
    p <- ggplot(
      d,
      aes(
        x = periodo,
        y = tasa,
        colour = .data[[input$color]],
        group = interaction(tumor, sexo)
      )
    ) +geom_line_interactive(
        aes(
          tooltip = tooltip,
          data_id = id_punto
        ),
        linewidth = 1
      ) + geom_point_interactive(
        aes(
          tooltip = tooltip,
          data_id = id_punto
        ),
        size = 2
      ) +labs(
        title = "Mortalidad por tumor",
        x = "Año",
        y = "Tasa de mortalidad",
        colour = if (input$color == "tumor") {
          "Tumor"
        } else {
          "Sexo"
        }
      ) +
      theme_minimal(base_size = 13) +
      theme(plot.title = element_text(
          face = "bold",
          size = 16
        ),legend.position = "bottom")
    
    # Hacer interactivo el gráfico
    girafe(ggobj = p,
      options = list(
        opts_hover(css = "stroke-width:3px;"
        ),
        opts_tooltip(opacity = 0.95
        ),
        opts_selection(
          type = "single",
          only_shiny = TRUE,
          css = "stroke:red;stroke-width:3px;"
        )
      )
    )
  })
  
  
  output$tabla <- renderDT({
    
    d <- datos_filtrados()
    
    datatable(
      as.data.frame(d),
      rownames = FALSE,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      )
    )
  })
}


shinyApp(ui, server)
