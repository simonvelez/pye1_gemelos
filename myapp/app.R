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

#calcular la dimension de la tabla
dim_datos<- paste("Número de filas:", dim(twins_comp)[1], "\nNúmero de columnas:", dim(twins_comp)[2])

# Seleccionar solo las columnas útiles para el proyecto
twins_comp %>%
  select(EDUCH, EDUCL, HRWAGEL, HRWAGEH)


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
      menuItem("Resumen De Estadísticos", tabName = "analisis", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    useShinyjs(),  # Inicializar shinyjs
    tabItems(
      tabItem(
        tabName = "Introduccion",
        fluidRow(
          box(
            title = "Visualización de datos con DotChart: ¿Influye la escolaridad en el salario? Un estudio con gemelos Monocigóticos",
            width = 12,
            actionButton("toggleTable", "Mostrar/Ocultar Tabla"),
            hidden(
              div(id = "tabla_container",
                  tableOutput("tabla_twins")
              )
            ),
            uiOutput("texto_introduccion") # Mostrar el texto con la información
          ),
        )
      ),
      tabItem(
        tabName = "graficos",
        fluidRow(
          box(
            title = "Gráficos Discretizados",
            width = 6,
            actionButton("botongraficosdiscretizados", "Mostrar/Ocultar Gráficos Discretizados"),
            hidden(
              div(id = "graficas_discretizadas",
                  plotOutput("dotchart_discretizado_gemelo1"),
                  plotOutput("dotchart_discretizado_gemelo2")
              )
            )
          ),
          box(
            title = "Gráficos sin Discretizar",
            width = 6,
            actionButton("botongraficos", "Mostrar/Ocultar Gráficos Continuos"),
            hidden(
              div(id = "graficos_sindiscretizar",
                  plotOutput("dotchart_1"),
                  plotOutput("dotchart_2")
              )
            )
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
                          choices = c("EDUCL","HRWAGEL")),
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
                          choices = c("EDUCH","HRWAGEH")),
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
  output$texto_introduccion <- renderUI({
    fluidRow(
      box(
        width = 12,
        textOutput("intro_paragraph"),
        footer = tagList(
          tableOutput("data_summary")
        )
      )
    )
  })
  
  output$intro_paragraph <- renderText({
    "Se desea estudiar la incidencia de los años de escolaridad en el ingreso por hora, para lo cual, se cuenta con la información de pares de gemelos monocigóticos mayores de 18 años respecto a diversas variables sociodemográficas de interés."
  })
  
  output$data_summary <- renderTable({
    data.frame(
      "Número de registros" = nrow(twins),
      "Número de variables" = ncol(twins),
      "Registros con información completa" = nrow(twins_comp),
      "Registros con información incompleta" = nrow(twins) - nrow(twins_comp),
      "Dimensión de la base de datos con la información completa" = dim_datos
    )
  })
  

  
  # Controlar la visibilidad de la tabla ___introduccion 
  observeEvent(input$toggleTable, {
    toggle("tabla_container")
  })
  
  # Controlar la visibilidad de las gráficas sin discretizar ___graficos
  observeEvent(input$botongraficos, {
    toggle("graficos_sindiscretizar")
  })
  
  # Controlar la visibilidad de las gráficas discretizadas ___graficos
  observeEvent(input$botongraficosdiscretizados, {
    toggle("graficas_discretizadas")
  })
  
  
  
  # Gráfico 1
  output$dotchart_1 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCL, y = HRWAGEL)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, color = "blue") +
      labs(title = "Dotchart de gemelo1", x = "Años de educación", y = "Salario en dólares") +
      theme_minimal() 
  })
  
  # Gráfico 2
  output$dotchart_2 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCH, y = HRWAGEH)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "red") +
      labs(title = "Dotchart de gemelo2", x = "Años de educación", y = "Salario en dólares") +
      theme_minimal()
  })
  
  
  # grafico discretizado gemelo 1
  output$dotchart_discretizado_gemelo1 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCL_disc, y = HRWAGEL)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "green") +
      labs(title = "Dotchart Discretizado de gemelo1", x = "Años de educación (Discretizado)", y = "Salario en dólares") +
      theme_minimal()+stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red")
  })
  # Gráfico discretizado gemelo 2
  output$dotchart_discretizado_gemelo2 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCH_disc, y = HRWAGEH)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "purple") +
      labs(title = "Dotchart Discretizado de gemelo2", x = "Años de educación (Discretizado)", y = "Salario en dólares") +
      theme_minimal()+stat_summary(fun = mean, geom = "point", shape = 18, size = 3, color = "red")
  })
  
  # Calcular medidas de dispersión para gemelo 1
  output$dispersion_gemelo1 <- renderText({
    req(input$variable_gemelo1)
    var <- input$variable_gemelo1
    dispersion_result <- twins_comp[[var]]
    units <- get_units(var)
    
    dispersion <- sprintf("Dispersión: %.2f %s", round(sd(dispersion_result, na.rm = TRUE), 2), units)
    rango <- sprintf("Rango: %.2f %s", round(diff(range(dispersion_result, na.rm = TRUE)), 2), units)
    varianza <- sprintf("Varianza: %.2f %s^2", round(var(dispersion_result, na.rm = TRUE), 2), units)
    desviacion_estandar <- sprintf("Desviación Estándar: %.2f %s", round(sd(dispersion_result, na.rm = TRUE), 2), units)
    coeficiente_variacion <- sprintf("Coeficiente de Variación: %.2f %%", round(sd(dispersion_result, na.rm = TRUE) / mean(dispersion_result, na.rm = TRUE), 2) * 100)
    
    paste(dispersion, rango, varianza, desviacion_estandar, coeficiente_variacion, sep = "\n")
  })
  # Calcular medidas de dispersión para gemelo 2
  output$dispersion_gemelo2 <- renderText({
    req(input$variable_gemelo2)
    var <- input$variable_gemelo2
    dispersion_result <- twins_comp[[var]]
    units <- get_units(var)
    
    dispersion <- sprintf("Dispersión: %.2f %s", round(sd(dispersion_result, na.rm = TRUE), 2), units)
    rango <- sprintf("Rango: %.2f %s", round(diff(range(dispersion_result, na.rm = TRUE)), 2), units)
    varianza <- sprintf("Varianza: %.2f %s^2", round(var(dispersion_result, na.rm = TRUE), 2), units)
    desviacion_estandar <- sprintf("Desviación Estándar: %.2f %s", round(sd(dispersion_result, na.rm = TRUE), 2), units)
    coeficiente_variacion <- sprintf("Coeficiente de Variación: %.2f %%", round(sd(dispersion_result, na.rm = TRUE) / mean(dispersion_result, na.rm = TRUE), 2) * 100)
    
    paste(dispersion, rango, varianza, desviacion_estandar, coeficiente_variacion, sep = "\n")
  })
  
  
  # Mostrar cuartiles para gemelo 1
  output$cuartiles_gemelo1 <- renderText({
    req(input$variable_gemelo1)
    variable_gemelo1 <- input$variable_gemelo1
    stats <- summary_stats(twins_comp, variable_gemelo1)
    
    paste("Mínimo:", stats$Min, "Q1:", stats$Q1,"Q2:", stats$Median,"Q3:", stats$Q3, 
          "Máximo:", stats$Max, sep = "\n")
  })
  
  # Mostrar cuartiles para gemelo 2
  output$cuartiles_gemelo2 <- renderText({
    req(input$variable_gemelo2)
    variable_gemelo2 <- input$variable_gemelo2
    stats <- summary_stats(twins_comp, variable_gemelo2)
    
    paste("Mínimo:", stats$Min, "Q1:", stats$Q1,"Mediana:", stats$Median, "Q3:", stats$Q3, 
          "Máximo:", stats$Max, sep = "\n")
  })
  
  # Moda para el gemelo 1
  output$moda_resultado_1 <- renderText({
    req(input$calcular_moda_1)
    variable <- input$variable_moda_1
    moda <- as.numeric(names(sort(table(twins_comp[[variable]]), decreasing = TRUE))[1])
    units <- get_units(variable)
    paste("Moda de", variable, ":", moda,units)
  })
  
  # Media para el gemelo 1
  output$media_resultado_1 <- renderText({
    req(input$calcular_media_1)
    variable1 <- input$variable_media_1
    media <- mean(twins_comp[[variable1]], na.rm = TRUE)
    units <- get_units(variable1)
    paste("Media de", variable1, ":", media,units)
  })
  
  # Mediana para el gemelo 1
  output$mediana_resultado_1 <- renderText({
    req(input$calcular_mediana_1)
    variable2 <- input$variable_mediana_1
    mediana <- median(twins_comp[[variable2]], na.rm = TRUE)
    units <- get_units(variable2)
    paste("Mediana de", variable2, ":", mediana,units)
  })
  
  # Moda para el gemelo 2
  output$moda_resultado_2 <- renderText({
    req(input$calcular_moda_2)
    variable <- input$variable_moda_2
    moda <- as.numeric(names(sort(table(twins_comp[[variable]]), decreasing = TRUE))[1])
    units <- get_units(variable)
    paste("Moda de", variable, ":", moda,units)
  })
  
  # Media para el gemelo 2
  output$media_resultado_2 <- renderText({
    req(input$calcular_media_2)
    variable1 <- input$variable_media_2
    media <- mean(twins_comp[[variable1]], na.rm = TRUE)
    units <- get_units(variable1)
    paste("Media de", variable1, ":", media)
  })
  # Función para obtener las unidades adecuadas
  get_units <- function(variable) {
    if (variable %in% c("EDUCH", "EDUCL")) {
      return("Años")
    } else if (variable %in% c("HRWAGEH", "HRWAGEL")) {
      return("pesos/h")
    } else {
      return("unidades")
    }
  }
  
  # Mediana para el gemelo 2
  output$mediana_resultado_2 <- renderText({
    req(input$calcular_mediana_2)
    variable2 <- input$variable_mediana_2
    mediana <- median(twins_comp[[variable2]], na.rm = TRUE)
    units <- get_units(variable2)
    paste("Mediana de", variable2, ":", mediana, units)
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)


