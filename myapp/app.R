library(shiny)
library(shinydashboard)
library(shinylive)
library(base)
library(httpuv)

# Calcular las variables necesarias para el reporte
num_registros <- nrow(twins)
num_variables <- ncol(twins)



#convierte las 16 columnas en numeric 
twins$DLHRWAGE<- as.numeric(twins$DLHRWAGE)
twins$DEDUC1<- as.numeric(twins$DEDUC1)
twins$AGE<- as.numeric(twins$AGE)
twins$AGESQ<- as.numeric(twins$AGESQ)
twins$HRWAGEH<- as.numeric(twins$HRWAGEH)
twins$WHITEH<- as.numeric(twins$WHITEH)
twins$MALEH<- as.numeric(twins$MALEH)
twins$EDUCH<- as.numeric(twins$EDUCH)
twins$HRWAGEL<- as.numeric(twins$HRWAGEL)
twins$WHITEL<- as.numeric(twins$WHITEL)
twins$MALEL<- as.numeric(twins$MALEL)
twins$EDUCL<- as.numeric(twins$EDUCL)
twins$DEDUC2<- as.numeric(twins$DEDUC2)
twins$DTEN<- as.numeric(twins$DTEN)
twins$DMARRIED<- as.numeric(twins$DMARRIED)
twins$DUNCOV<- as.numeric(twins$DUNCOV)


#crear una nueva tabla sin NAs
twins_copia1 <- na.omit(twins)


# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  dashboardHeader(title = "Gemelos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduccion", tabName = "Introduccion", icon = icon("pencil")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-line")),
      menuItem("Reporte", tabName = "Reporte", icon = icon("search"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "Introduccion",
        fluidRow(
          box(
            title = "Introduccion",
            width = 12,
            verbatimTextOutput("int")
          )
        )
      ),
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
        fluidRow(
          box(
            title = "Reporte de Datos",
            width = 12,
            verbatimTextOutput("reporte")
          )
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
  output$int <- renderText({
    paste("numero de variables:", num_variables)
    
  })
  
  # Mostrar el reporte de datos en la pestaña de reporte
  output$reporte <- renderText({
    paste("Número de registros:", num_registros, "\n",
          "Número de variables:", num_variables, "\n",
          "Registros con al menos un dato faltante:", char_variables, "\n",
          "Registros con información completa:", registros_completos)
  })
  
  
  
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
