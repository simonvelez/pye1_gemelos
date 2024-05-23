library(shinydashboard) # Estructura de la página
library(shinyjs)  # Para usar funciones de JavaScript
library(ggplot2) # Crear gráficos y ponerlos bonitos
library(dplyr) #calcular media, moda, mediana y demas

# Cargar los datos de la base de datos
twins <- read.csv("twins.txt", header = TRUE)

# Calcular las variables necesarias para el reporte
num_registros <- nrow(twins) # registros en la base original
num_variables <- ncol(twins) # variables

# Convierte las 16 columnas en numeric 
cols_to_convert <- c("DLHRWAGE", "DEDUC1", "AGE", "AGESQ", "HRWAGEH", "WHITEH", "MALEH", 
                     "EDUCH", "HRWAGEL", "WHITEL", "MALEL", "EDUCL", "DEDUC2", "DTEN", 
                     "DMARRIED", "DUNCOV")
twins[cols_to_convert] <- lapply(twins[cols_to_convert], as.numeric)

# Crear una nueva tabla sin NAs
twins_comp <- na.omit(twins) # datos con registros completos

# Seleccionar solo las columnas útiles para el proyecto


# Discretizar años de educación del gemelo 1 y el gemelo 2 
twins_comp$EDUCH_disc <- cut(twins_comp$EDUCH, breaks = c(0,10, 12, 15, 18, 21), labels = c("0-10","10-12", "13-15", "16-18", "19-21"))

twins_comp$EDUCL_disc <- cut(twins_comp$EDUCL, breaks = c(0,10, 12, 15, 18, 21), labels = c("0-10","10-12", "13-15", "16-18", "19-21"))




# Calcular los cuartiles, mínimos y máximos
summary_stats <- function(data, var) {
  q <- quantile(data[[var]], na.rm = TRUE)
  list(
    Min = min(data[[var]], na.rm = TRUE),
    Q1 = q[2],
    Median = q[3],
    Q3 = q[4],
    Max = max(data[[var]], na.rm = TRUE)
  )
}

educh_stats <- summary_stats(twins_comp, "EDUCH")
hrwageh_stats <- summary_stats(twins_comp, "HRWAGEH")

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
            )
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
          plotOutput("dotchart_2"),
          plotOutput("dotchart_discretizado_gemelo1"),
          plotOutput("dotchart_discretizado_gemelo2") 
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
              verbatimTextOutput("dispersion_gemelo1"),
              verbatimTextOutput("cuartiles_gemelo1") # Agregar salida para cuartiles
            )
          ),
          column(
            width = 6,
            box(
              title = "Medidas de Dispersión del Gemelo 2",
              width = 12,
              selectInput("variable_gemelo2", "Seleccionar variable:",
                          choices = c("WHITEL", "AGESQ", "MALEL", "EDUCL", "DEDUC2", "DTEN", "DMARRIED", "DUNCOV")),
              verbatimTextOutput("dispersion_gemelo2"),
              verbatimTextOutput("cuartiles_gemelo2") # Agregar salida para cuartiles
            )
          )
        )
      )
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  output$tabla_twins <- renderTable({
    twins_comp
  })
  
  # Mostrar texto en la pestaña de Introducción
  output$texto_introduccion <- renderText({
    paste("Número de registros:", nrow(twins), "\n",
          "Número de variables:", ncol(twins), "\n",
          "Registros con información completa:", nrow(twins_comp), "\n",
          "Registros con información incompleta:", nrow(twins) - nrow(twins_comp))
  })
  
  # Controlar la visibilidad de la tabla
  observeEvent(input$toggleTable, {
    toggle("tabla_container")
  })
  
  # Gráfico 1
  output$dotchart_1 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCL, y = HRWAGEL)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, color = "blue") +
      labs(title = "Dotchart de EDUCL vs HRWAGEL", x = "Años de educación", y = "Salario en dólares") +
      theme_minimal() 
  })
  
  # Gráfico 2
  output$dotchart_2 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCH, y = HRWAGEH)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "red") +
      labs(title = "Dotchart de EDUCH vs HRWAGEH", x = "Años de educación", y = "Salario en dólares") +
      theme_minimal()
  })
  
  # Gráfico discretizado gemelo 2
  output$dotchart_discretizado_gemelo2 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCH_disc, y = HRWAGEH)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "purple") +
      labs(title = "Dotchart Discretizado de EDUCH vs HRWAGEH", x = "Años de educación (Discretizado)", y = "Salario en dólares") +
      theme_minimal()+stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red")
  })
  
  # grafico discretizado gemelo 1
  output$dotchart_discretizado_gemelo1 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCL_disc, y = HRWAGEL)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "green") +
      labs(title = "Dotchart Discretizado de EDUCH vs HRWAGEH", x = "Años de educación (Discretizado)", y = "Salario en dólares") +
      theme_minimal()+stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red")
  })
  
  # Calcular medidas de dispersión para gemelo 1
  output$dispersion_gemelo1 <- renderText({
    req(input$variable_gemelo1)
    var <- input$variable_gemelo1
    dispersion_result <- twins_comp[[var]]
    
    dispersion <- sprintf("Dispersión: %.2f unidades", round(sd(dispersion_result, na.rm = TRUE), 2))
    rango <- sprintf("Rango: %.2f unidades", round(diff(range(dispersion_result, na.rm = TRUE)), 2))
    varianza <- sprintf("Varianza: %.2f unidades^2", round(var(dispersion_result, na.rm = TRUE), 2))
    desviacion_estandar <- sprintf("Desviación Estándar: %.2f unidades", round(sd(dispersion_result, na.rm = TRUE), 2))
    coeficiente_variacion <- sprintf("Coeficiente de Variación: %.2f %%", round(sd(dispersion_result, na.rm = TRUE) / mean(dispersion_result, na.rm = TRUE), 2))
    
    paste(dispersion, rango, varianza, desviacion_estandar, coeficiente_variacion, sep = "\n")
  })
  
  # Calcular medidas de dispersión para gemelo 2
  output$dispersion_gemelo2 <- renderText({
    req(input$variable_gemelo2)
    var <- input$variable_gemelo2
    dispersion_result <- twins_comp[[var]]
    
    dispersion <- sprintf("Dispersión: %.2f unidades", round(sd(dispersion_result, na.rm = TRUE), 2))
    rango <- sprintf("Rango: %.2f unidades", round(diff(range(dispersion_result, na.rm = TRUE)), 2))
    varianza <- sprintf("Varianza: %.2f unidades^2", round(var(dispersion_result, na.rm = TRUE), 2))
    desviacion_estandar <- sprintf("Desviación Estándar: %.2f unidades", round(sd(dispersion_result, na.rm = TRUE), 2))
    coeficiente_variacion <- sprintf("Coeficiente de Variación: %.2f %%", round(sd(dispersion_result, na.rm = TRUE) / mean(dispersion_result, na.rm = TRUE), 2))
    
    paste(dispersion, rango, varianza, desviacion_estandar, coeficiente_variacion, sep = "\n")
  })
  
  # Mostrar cuartiles para gemelo 1
  output$cuartiles_gemelo1 <- renderText({
    req(input$variable_gemelo1)
    var <- input$variable_gemelo1
    stats <- summary_stats(twins_comp, var)
    
    paste("Mínimo:", stats$Min, "Q1:", stats$Q1,"Q2:", stats$Median,"Q3:", stats$Q3, 
          "Máximo:", stats$Max, sep = "\n")
  })
  
  # Mostrar cuartiles para gemelo 2
  output$cuartiles_gemelo2 <- renderText({
    req(input$variable_gemelo2)
    var <- input$variable_gemelo2
    stats <- summary_stats(twins_comp, var)
    
    paste("Mínimo:", stats$Min, "Q1:", stats$Q1,"Mediana:", stats$Median, "Q3:", stats$Q3, 
          "Máximo:", stats$Max, sep = "\n")
  })
  
  # Moda para el gemelo 1
  output$moda_resultado_1 <- renderText({
    req(input$calcular_moda_1)
    variable <- input$variable_moda_1
    moda <- as.numeric(names(sort(table(twins_comp[[variable]]), decreasing = TRUE))[1])
    paste("Moda de", variable, ":", moda)
  })
  
  # Media para el gemelo 1
  output$media_resultado_1 <- renderText({
    req(input$calcular_media_1)
    variable1 <- input$variable_media_1
    media <- mean(twins_comp[[variable1]], na.rm = TRUE)
    paste("Media de", variable1, ":", media)
  })
  
  # Mediana para el gemelo 1
  output$mediana_resultado_1 <- renderText({
    req(input$calcular_mediana_1)
    variable2 <- input$variable_mediana_1
    mediana <- median(twins_comp[[variable2]], na.rm = TRUE)
    paste("Mediana de", variable2, ":", mediana)
  })
  
  # Moda para el gemelo 2
  output$moda_resultado_2 <- renderText({
    req(input$calcular_moda_2)
    variable <- input$variable_moda_2
    moda <- as.numeric(names(sort(table(twins_comp[[variable]]), decreasing = TRUE))[1])
    paste("Moda de", variable, ":", moda)
  })
  
  # Media para el gemelo 2
  output$media_resultado_2 <- renderText({
    req(input$calcular_media_2)
    variable1 <- input$variable_media_2
    media <- mean(twins_comp[[variable1]], na.rm = TRUE)
    paste("Media de", variable1, ":", media)
  })
  
  # Mediana para el gemelo 2
  output$mediana_resultado_2 <- renderText({
    req(input$calcular_mediana_2)
    variable2 <- input$variable_mediana_2
    mediana <- median(twins_comp[[variable2]], na.rm = TRUE)
    paste("Mediana de", variable2, ":", mediana)
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)


