library(shinydashboard) # Estructura de la página
library(shinyjs)  # Para usar funciones de JavaScript
library(ggplot2) # Crear gráficos y ponerlos bonitos
library(dplyr) 

# Cargar los datos de la base de datos
twins <- read.csv("twins.txt", header = TRUE)

# Calcular las variables necesarias para el reporte
num_registros <- nrow(twins)#registros en la base original
num_variables <- ncol(twins)#variables


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
twins_copia1 <- na.omit(twins) # datos con registros completos


# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  
  dashboardHeader(title = "Estudio gemelos"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduccion", tabName = "Introduccion", icon = icon("pencil")),
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-line")),
      menuItem("Reporte", tabName = "Reporte", icon = icon("search")),
      menuItem("Análisis", tabName = "analisis", icon = icon("chart-bar"))
    
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
          fluidRow(
            box(
              title = "Calcular Moda gemelo 1 ",
              width = 4,
              selectInput("variable_moda_1", "Seleccionar variable:",
                          choices = c("DEDUC1", "AGE", "HRWAGEH", "WHITEH", "MALEH", "EDUCH","DMARRIED", "DUNCOV")),
              actionButton("calcular_moda_1", "Calcular Moda"),
              verbatimTextOutput("moda_resultado_1")
            ),
            box(
              title = "Calcular Media gemelo 1",
              width = 4,
              selectInput("variable_media_1", "Seleccionar variable:",
                          choices = c("DEDUC1", "AGE", "HRWAGEH", "WHITEH", "MALEH", "EDUCH","DMARRIED", "DUNCOV")),
              actionButton("calcular_media_1", "Calcular Media"),
              verbatimTextOutput("media_resultado_1")
            ),
            box(
              title = "Calcular Mediana gemelo 1",
              width = 4,
              selectInput("variable_mediana_1", "Seleccionar variable:",
                          choices = c("DEDUC1", "AGE", "HRWAGEH", "WHITEH", "MALEH", "EDUCH","DMARRIED", "DUNCOV")),
              actionButton("calcular_mediana_1", "Calcular Mediana"),
              verbatimTextOutput("mediana_resultado_1")
            ),
          ),
          fluidRow(
            box(
              title = "Calcular Moda gemelo 2 ",
              width = 4,
              selectInput("variable_moda_2", "Seleccionar variable:",
                          choices = c("WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
              actionButton("calcular_moda_2", "Calcular Moda"),
              verbatimTextOutput("moda_resultado_2")
            ),
            box(
              title = "Calcular Media gemelo 2",
              width = 4,
              selectInput("variable_media_2", "Seleccionar variable:",
                          choices = c("WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
              actionButton("calcular_media_2", "Calcular Media"),
              verbatimTextOutput("media_resultado_2")
            ),
            box(
              title = "Calcular Mediana gemelo 2",
              width = 4,
              selectInput("variable_mediana_2", "Seleccionar variable:",
                          choices = c("WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
              actionButton("calcular_mediana_2", "Calcular Mediana"),
              verbatimTextOutput("mediana_resultado_2")
            )
          )
        )
      ),
      
        
      tabItem(
        tabName = "graficos",
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
      ),
      tabItem(
        tabName = "analisis",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Medidas de Dispersión del Gemelo 1",
              width = 12,
              selectInput("variable_gemelo1", "Seleccionar variable:",
                          choices = c("DEDUC1", "AGE", "HRWAGEH", "WHITEH", "MALEH", "EDUCH", "DMARRIED", "DUNCOV")),
              uiOutput("dispersion_gemelo1")
            )
          ),
          column(
            width = 6,
            box(
              title = "Medidas de Dispersión del Gemelo 2",
              width = 12,
              selectInput("variable_gemelo2", "Seleccionar variable:",
                          choices = c("WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
              uiOutput("dispersion_gemelo2")
            )
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
 
    # Convertir las columnas a numérico
    cols_to_convert <- c("DLHRWAGE", "DEDUC1", "AGE", "AGESQ", "HRWAGEH", "WHITEH", "MALEH", 
                         "EDUCH", "HRWAGEL", "WHITEL", "MALEL", "EDUCL", "DEDUC2", "DTEN", 
                         "DMARRIED", "DUNCOV")
    twins[cols_to_convert] <- lapply(twins[cols_to_convert], as.numeric)
    
  
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
  
  output$dotchart_1 <- renderPlot({
    ggplot(twins_copia1, aes(x = EDUCL, y = HRWAGEL)) + #Se usa para crear el dotchart
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, color = "blue") +
      labs(title = "Dotchart de EDUCH vs HRWAGEH", x = "Años de educación", y = "Salario en dólares") +
      theme_minimal() + stat_summary(fun.y=mean, geom="point", shape=18,
                                     size=3, color="red")
  })
  #grafico 2
  
  
  
  output$dotchart_2 <- renderPlot({
    ggplot(twins_copia1, aes(x = EDUCH, y = HRWAGEH)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "red") +
      labs(title = "Dotchart de EDUCL vs HRWAGEL", x = "Años de educación", y = "Salario  en dólares") +
      theme_minimal()
    
  })
  
  # Mostrar la cantidad de variables de tipo carácter
  output$int <- renderText({
    paste("numero de variables:", num_variables)
    
  })
  
  # Mostrar el reporte de datos en la pestaña de reporte
  output$texto_introduccion <- renderText({
    paste("Número de registros:", nrow(twins), "\n",
          "Número de variables:", ncol(twins), "\n",
          "Registros con información completa:", nrow(twins_copia1), "\n",
          "Registros con información incompleta:",nrow(twins)-nrow(twins_copia1))
  })
  
  # Calcular medidas de dispersión para gemelo 1
  output$dispersion_gemelo1 <- renderText({
    req(input$variable_gemelo1)
    var <- input$variable_gemelo1
    dispersion_result <- twins_copia1[[var]]
    
    dispersion <- sprintf("Dispersión: %f", sd(dispersion_result, na.rm = TRUE))
    rango <- sprintf("Rango: %f", diff(range(dispersion_result, na.rm = TRUE)))
    varianza <- sprintf("Varianza: %f", var(dispersion_result, na.rm = TRUE))
    desviacion_estandar <- sprintf("Desviación Estándar: %f", sd(dispersion_result, na.rm = TRUE))
    coeficiente_variacion <- sprintf("Coeficiente de Variación: %f", sd(dispersion_result, na.rm = TRUE) / mean(dispersion_result, na.rm = TRUE))
    
    paste(dispersion, rango, varianza, desviacion_estandar, coeficiente_variacion, sep = "\n")
  })
  
  
  # Calcular medidas de dispersión para gemelo 2
  output$dispersion_gemelo2 <- renderText({
    req(input$variable_gemelo2)
    var <- input$variable_gemelo2
    dispersion_result <- twins_copia1[[var]]
    
    dispersion <- sprintf("Dispersión: %f", sd(dispersion_result, na.rm = TRUE))
    rango <- sprintf("Rango: %f", diff(range(dispersion_result, na.rm = TRUE)))
    varianza <- sprintf("Varianza: %f", var(dispersion_result, na.rm = TRUE))
    desviacion_estandar <- sprintf("Desviación Estándar: %f", sd(dispersion_result, na.rm = TRUE))
    coeficiente_variacion <- sprintf("Coeficiente de Variación: %f", sd(dispersion_result, na.rm = TRUE) / mean(dispersion_result, na.rm = TRUE))
    
    paste(dispersion, rango, varianza, desviacion_estandar, coeficiente_variacion, sep = "\n")
  })
  
  
# moda para el gemelo 1   
  output$moda_resultado_1 <- renderText({
    req(input$calcular_moda_1)
    variable <- input$variable_moda_1
    moda <- as.numeric(names(sort(table(twins_copia1[variable]), decreasing = TRUE))[1])
    paste("Moda de", variable, ":", moda)
  })
# media para el gemelo 1
  output$media_resultado_1 <- renderText({
    req(input$calcular_media_1)
    variable1 <- input$variable_media_1
    media <- mean(twins_copia1[[variable1]], na.rm = TRUE)
    paste("Media de", variable1, ":", media)
  })
  
#mediana para el gemelo 1
  output$mediana_resultado_1 <- renderText({
    req(input$calcular_mediana_1)
    variable2 <- input$variable_mediana_1
    mediana <- median(twins_copia1[[variable2]], na.rm = TRUE)
    paste("Mediana de", variable2, ":", mediana)
  })
  
#moda para el gemelo 2
  output$moda_resultado_2 <- renderText({
    req(input$calcular_moda_2)
    variable <- input$variable_moda_2
    moda <- as.numeric(names(sort(table(twins_copia1[variable]), decreasing = TRUE))[1])
    paste("Moda de", variable, ":", moda)
  })
  
#media para el gemelo 2
  output$media_resultado_2 <- renderText({
    req(input$calcular_media_2)
    variable1 <- input$variable_media_2
    media <- mean(twins_copia1[[variable1]], na.rm = TRUE)
    paste("Media de", variable1, ":", media)
  })
#mediana para el gemelo 2
  output$mediana_resultado_2 <- renderText({
    req(input$calcular_mediana_2)
    variable2 <- input$variable_mediana_2
    mediana <- median(twins_copia1[[variable2]], na.rm = TRUE)
    paste("Mediana de", variable2, ":", mediana)
  })

}
  

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)


