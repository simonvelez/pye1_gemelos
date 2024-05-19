library(shiny)
library(shinydashboard)
library(shinylive)
library(httpuv)

# Cargar los datos de la base de datos
twins <- read.csv("twins.txt", header = TRUE)


# Contar las variables de tipo carácter en 'twins'
char_variables <- sum(sapply(twins, is.character))

# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Gemelos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-line")),
      menuItem("Reporte", tabName = "Reporte", icon = icon("search"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "graficos",
        sidebarPanel(
          selectInput("var_x_1", "Seleccionar variable para el eje Y, para el gemelo 1:",
                      choices = c("DEDUC1", "AGE", "HRWAGEH", "WHITEH", "MALEH", "EDUCH","DMARRIED", "DUNCOV")),
          actionButton("plotBtn_1", "Generar Gráfico gemelo 1"),
          
          selectInput("var_x_2", "Seleccionar variable para el eje Y, para el gemelo 2:",
                      choices = c("WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
          actionButton("plotBtn_2", "Generar Gráfico gemelo 2")
        ),
        mainPanel(
          plotOutput("dotchart_1"),
          plotOutput("dotchart_2")
        )
      ),
      tabItem(
        tabName = "Reporte",
        sidebarPanel(
          # Espacio para más contenido del panel lateral
        )
      )
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  
  # Gráfico 1
  observeEvent(input$plotBtn_1, {
    req(input$var_x_1)
    selected_variable_x_1 <- input$var_x_1
    
    output$dotchart_1 <- renderPlot({
      dotchart(
        as.numeric(twins[[selected_variable_x_1]]),
        main = paste("Dotchart de", selected_variable_x_1),
        xlab = selected_variable_x_1,
        ylab = "DLHRWAGE"
      )
    })
  })
  
  # Gráfico 2
  observeEvent(input$plotBtn_2, {
    req(input$var_x_2)
    selected_variable_x_2 <- input$var_x_2
    
    output$dotchart_2 <- renderPlot({
      dotchart(
        as.numeric(twins[[selected_variable_x_2]]),
        main = paste("Dotchart de", selected_variable_x_2),
        xlab = selected_variable_x_2,
        ylab = "HRWAGEL"
      )
    })
  })
  
  # Mostrar la cantidad de variables de tipo carácter
  output$char_data_count <- renderText({
    paste("Columnas de tipo carácter:", char_variables)
  })
  
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)

