library(shiny)
library(shinydashboard)
library(shinylive)
library(base)
library(httpuv)
library(DT)
library(shinyjs)  # Para usar funciones de JavaScript

# Cargar los datos de la base de datos
twins <- read.csv("twins.txt", header = TRUE)

# Calcular las variables necesarias para el reporte
num_registros <- nrow(twins)
num_variables <- ncol(twins)


# Seleccionar el archivo XLSX interactivamente


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

#calcula las modas
columna_especifica <- "AGE"
moda_columna <- as.numeric(names(sort(table(twins[[columna_especifica]]), decreasing = TRUE))[1])




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
    useShinyjs(),  # Inicializar shinyjs
    tabItems(
      tabItem(
        tabName = "Introduccion",
        fluidRow(
          box(
            title = "Introducción",
            width = 12,
            actionButton("toggleTable", "Mostrar/Ocultar Tabla"),
            hidden(
              div(id = "tabla_container",
                  tableOutput("tabla_twins")
              )
            ),
            verbatimTextOutput("texto_introduccion")
          ),
          box(
            title = "Calcular Moda",
            width = 12,
            selectInput("variable_moda", "Seleccionar variable:",
                        choices = c("DEDUC1", "AGE", "HRWAGEH", "WHITEH", "MALEH", "EDUCH","DMARRIED", "DUNCOV",
                                    "WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
            actionButton("calcular_moda", "Calcular Moda"),
            verbatimTextOutput("moda_resultado")
          ),
          box(
            title = "Calcular Media",
            width = 12,
            selectInput("variable_media", "Seleccionar variable:",
                        choices = c("DEDUC1", "AGE", "HRWAGEH", "WHITEH", "MALEH", "EDUCH","DMARRIED", "DUNCOV",
                                    "WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
            actionButton("calcular_media", "Calcular Media"),
            verbatimTextOutput("media_resultado")
          ),
          box(
            title = "Calcular Mediana",
            width = 12,
            selectInput("variable_mediana", "Seleccionar variable:",
                        choices = c("DEDUC1", "AGE", "HRWAGEH", "WHITEH", "MALEH", "EDUCH","DMARRIED", "DUNCOV",
                                    "WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
            actionButton("calcular_mediana", "Calcular Mediana"),
            verbatimTextOutput("mediana_resultado")
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
  # Leer archivo de Excel y preparar los datos
  data <- eventReactive(input$file, {
    req(input$file)
    
    # Leer los datos del archivo de Excel
    twins <- read_excel(input$file$datapath)
    
    # Convertir las columnas a numérico
    cols_to_convert <- c("DLHRWAGE", "DEDUC1", "AGE", "AGESQ", "HRWAGEH", "WHITEH", "MALEH", 
                         "EDUCH", "HRWAGEL", "WHITEL", "MALEL", "EDUCL", "DEDUC2", "DTEN", 
                         "DMARRIED", "DUNCOV")
    twins[cols_to_convert] <- lapply(twins[cols_to_convert], as.numeric)
    
    # Crear una nueva tabla sin NAs
    twins_copia1 <- na.omit(twins)
    
    list(original = twins, copia1 = twins_copia1)
  })
  
  
  
  output$tabla_twins <- renderTable({
    twins_copia1
  })
  
  # Mostrar texto en la pestaña de Introducción
  output$texto_introduccion <- renderText({
    "Tabla sin registros incompletos"
  })

  # Controlar la visibilidad de la tabla
  observeEvent(input$toggleTable, {
    toggle("tabla_container")
  })
  
  
  
  
  
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
  output$texto_introduccion <- renderText({
    paste("Número de registros:", nrow(twins), "\n",
          "Número de variables:", ncol(twins), "\n",
          "Registros con información completa:", nrow(twins_copia1),
          "moda dato:",moda_columna )
  })
  #calcular la moda de la varibale que se selecciona 
  output$moda_resultado <- renderText({
    req(input$calcular_moda)
    variable <- input$variable_moda
    moda <- as.numeric(names(sort(table(twins_copia1[variable]), decreasing = TRUE))[1])
    paste("Moda de", variable, ":", moda)
  })
  output$media_resultado <- renderText({
    req(input$calcular_media)
    variable1 <- input$variable_media
    media <- mean(twins_copia1[[variable1]], na.rm = TRUE)
    paste("Media de", variable1, ":", media)
  })
  output$mediana_resultado <- renderText({
    req(input$calcular_mediana)
    variable2 <- input$variable_mediana
    mediana <- median(twins_copia1[[variable2]], na.rm = TRUE)
    paste("Mediana de", variable2, ":", mediana)
  })
}

  

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
