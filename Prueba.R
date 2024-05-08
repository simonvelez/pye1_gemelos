# Cargar las bibliotecas necesarias
library(shiny)
print(twins)
library(ggplot2)

#twins <- as.numeric(twins$DLHRWAGE)


# Definir la interfaz de usuario (UI)
ui <- fluidPage(
  titlePanel("Gemelos"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("variable_x", "Seleccionar variable para el eje X:",
                  choices = c("DLHRWAGE", "DEDUC1", "AGE", "AGESQ", "HRWAGEH", "WHITEH", "MALEH", 
                              "EDUCH", "HRWAGEL", "WHITEL", "MALEL", "EDUCL", "DEDUC2", "DTEN", 
                              "DMARRIED", "DUNCOV")),
      selectInput("variable_y", "Seleccionar variable para el eje Y:",
                  choices = c("DLHRWAGE", "DEDUC1", "AGE", "AGESQ", "HRWAGEH", "WHITEH", "MALEH", 
                              "EDUCH", "HRWAGEL", "WHITEL", "MALEL", "EDUCL", "DEDUC2", "DTEN", 
                              "DMARRIED", "DUNCOV")),
      actionButton("plotBtn", "Generar gráfico")
    ),
    
    mainPanel(
      plotOutput("dotchart")
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  observeEvent(input$plotBtn, {
    selected_variable_x <- input$variable_x
    selected_variable_y <- input$variable_y
    
    # Generar el dotchart
    output$dotchart <- renderPlot({
      dotchart(twins[[selected_variable_x]],
               twins[[selected_variable_y]],
               main = paste("Dotchart de", selected_variable_x, "vs", selected_variable_y),
               xlab = selected_variable_x,
               ylab = selected_variable_y)
    })
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

