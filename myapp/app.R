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


# Discretizar años de educación del gemelo 1 y el gemelo 2 

#Según nivel educativo:
twins_comp$EDUCH_disc_edu <- cut(twins_comp$EDUCH, breaks = c(0,6,12,16,22), labels = c("Primaria (0-6)", "Secundaria (6-12)", "Pregrado (12-16)", "Posgrado (16-22)"))
twins_comp$EDUCL_disc_edu <- cut(twins_comp$EDUCL, breaks = c(0,6,12,16,22), labels = c("Primaria (0-6)", "Secundaria (6-12)", "Pregrado (12-16)", "Posgrado (16-22)"))

#Según cuartiles:
twins_comp$EDUCH_disc_cua <- cut(twins_comp$EDUCH, breaks = c(0,12,14,16,20), labels = c("Q1 (0-12)", "Q2 (12-14)", "Q3 (14-16)", "Q4 (16-20)"))
twins_comp$EDUCL_disc_cua <- cut(twins_comp$EDUCL, breaks = c(0,12,14,16,20), labels = c("Q1 (0-12)", "Q2 (12-14)", "Q3 (14-16)", "Q4 (16-20)"))


# Función para calcular los cuartiles, mínimos y máximos
summary_stats <- function(data, var) {
  q <- quantile(data[[var]], na.rm = TRUE)  # Calcular los cuantiles (cuartiles) del conjunto de datos para la variable especificada, ignorando los valores NA
  list(
    Min = min(data[[var]], na.rm = TRUE),  # Calcular el valor mínimo de la variable, ignorando los valores NA
    Q1 = q[2],  # Primer cuartil (Q1)
    Median = q[3],  # Mediana (Q2)
    Q3 = q[4],  # Tercer cuartil (Q3)
    Max = max(data[[var]], na.rm = TRUE)  # Calcular el valor máximo de la variable, ignorando los valores NA
  )
}

# Función para obtener las unidades de medida de la variable
get_units <- function(variable) {
  if (variable %in% c("EDUCH", "EDUCL")) {  # Si la variable es EDUCH o EDUCL
    return("Años")  # La unidad es "Años"
  } else if (variable %in% c("HRWAGEH", "HRWAGEL")) {  # Si la variable es HRWAGEH o HRWAGEL
    return("$")  # La unidad es "$" (dólares)
  } else {
    return("unidades")  # Para otras variables, la unidad es "unidades"
  }
}

# Calcular las estadísticas resumen para la variable EDUCH en el conjunto de datos twins_comp
educh_stats <- summary_stats(twins_comp, "EDUCH")

# Calcular las estadísticas resumen para la variable HRWAGEH en el conjunto de datos twins_comp
hrwageh_stats <- summary_stats(twins_comp, "HRWAGEH")


# Definir la interfaz de usuario (UI)
ui <- dashboardPage(
  # Definir la cabecera del dashboard
  dashboardHeader(title = "Visualización de datos"),
  
  # Definir la barra lateral del dashboard
  dashboardSidebar(
    sidebarMenu(
      menuItem("Introduccion", tabName = "Introduccion", icon = icon("pencil")),  # Menú para la sección de introducción
      menuItem("Gráficos", tabName = "graficos", icon = icon("chart-line")),      # Menú para la sección de gráficos
      menuItem("Resumen De Estadísticos", tabName = "analisis", icon = icon("chart-bar"))  # Menú para la sección de análisis estadístico
    )
  ),
  
  # Definir el cuerpo del dashboard
  dashboardBody(
    useShinyjs(),  # Inicializar shinyjs para utilizar funciones JavaScript en la aplicación
    tabItems(
      # Contenido de la pestaña de introducción
      tabItem(
        tabName = "Introduccion",
        fluidRow(
          box(
            h2("Visualización de datos con DotChart: ¿Influye la escolaridad en el salario? Un estudio con gemelos Monocigóticos"),  # Título del cuadro
            width = 12,  # Ancho del cuadro
            actionButton("toggleTable", "Mostrar Base de datos usada"),  # Botón para mostrar u ocultar la tabla
            hidden(
              div(id = "tabla_container",  # Contenedor de la tabla, inicialmente oculto
                  tableOutput("tabla_twins")  # Salida de la tabla que muestra los datos de los gemelos
              )
            ),
            uiOutput("texto_introduccion")  # Salida del texto con la información de introducción
          )
        )
      ),
      
      # Contenido de la pestaña de gráficos
      tabItem(
        tabName = "graficos",
        fluidRow(
          box(
            title = "Discretizados según nivel educativo",  # Título del cuadro para gráficos discretizados por nivel educativo
            width = 6,  # Ancho del cuadro
            actionButton("botongraficosdiscretizados", "Mostrar Gráficos"),  # Botón para mostrar u ocultar los gráficos discretizados por nivel educativo
            hidden(
              div(id = "graficos_nivedu",  # Contenedor de los gráficos discretizados por nivel educativo, inicialmente oculto
                  plotOutput("dotchart_discretizadoedu_gemelo1"),  # Salida del gráfico discretizado por nivel educativo para el gemelo 1
                  plotOutput("dotchart_discretizadoedu_gemelo2")   # Salida del gráfico discretizado por nivel educativo para el gemelo 2
              )
            )
          ),
          box(
            title = "Discretizados según cuartiles",  # Título del cuadro para gráficos discretizados por cuartiles
            width = 6,  # Ancho del cuadro
            actionButton("botongraficos", "Mostrar Gráficos"),  # Botón para mostrar u ocultar los gráficos discretizados por cuartiles
            hidden(
              div(id = "graficos_cuartiles",  # Contenedor de los gráficos discretizados por cuartiles, inicialmente oculto
                  plotOutput("dotchart_discretizadocua_gemelo1"),  # Salida del gráfico discretizado por cuartiles para el gemelo 1
                  plotOutput("dotchart_discretizadocua_gemelo2")   # Salida del gráfico discretizado por cuartiles para el gemelo 2
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Análisis",  # Título del cuadro de análisis
            width = 12,  # Ancho del cuadro
            actionButton("botongraficossindisc", "Mostrar Gráficos sin discretizar"),  # Botón para mostrar u ocultar los gráficos discretizados por cuartiles
            hidden(
              div(id = "graficos_sindisc",  # Contenedor de los gráficos discretizados por cuartiles, inicialmente oculto
                  fluidRow(
                    column(width = 6, plotOutput("dotchart_1")),  # Primera columna con el primer gráfico
                    column(width = 6, plotOutput("dotchart_2"))   # Segunda columna con el segundo gráfico
                  )
              )
            )
          )
        )
      ),
      
      # Contenido de la pestaña de análisis estadístico
      tabItem(
        tabName = "analisis",
        fluidRow(
          column(
            width = 6,
            box(
              title = "Medidas de Dispersión del Gemelo 1",  # Título del cuadro para medidas de dispersión del gemelo 1
              width = 12,
              selectInput("variable_gemelo1", "Seleccionar variable:",  # Selector de variable para el gemelo 1
                          choices = c("Años de educación (EDUCL)" = "EDUCL", "Salario por Hora (HRWAGEL)" = "HRWAGEL")),  # Opciones del selector
              verbatimTextOutput("centros_gemelo1"),  # Salida de medidas centrales para el gemelo 1
              verbatimTextOutput("dispersion_gemelo1"),  # Salida de medidas de dispersión para el gemelo 1
              verbatimTextOutput("cuartiles_gemelo1")  # Salida de cuartiles para el gemelo 1
            )
          ),
          column(
            width = 6,
            box(
              title = "Medidas de Dispersión del Gemelo 2",  # Título del cuadro para medidas de dispersión del gemelo 2
              width = 12,
              selectInput("variable_gemelo2", "Seleccionar variable:",  # Selector de variable para el gemelo 2
                          choices = c("Años de educación (EDUCH)" = "EDUCH", "Salario por Hora (HRWAGEH)" = "HRWAGEH")),  # Opciones del selector
              verbatimTextOutput("centros_gemelo2"),  # Salida de medidas centrales para el gemelo 2
              verbatimTextOutput("dispersion_gemelo2"),  # Salida de medidas de dispersión para el gemelo 2
              verbatimTextOutput("cuartiles_gemelo2")  # Salida de cuartiles para el gemelo 2
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
    fluidRow(  # Crear una fila fluida para organizar el contenido
      box(  # Caja para contener el contenido
        width = 12,  # Ancho de la caja (12 columnas, ocupando toda la fila)
        h4("Objetivo:"),
        textOutput("intro_paragraph"),
        h4("Información de los registros:"),
        tableOutput("data_summary"),  # Salida de tabla para mostrar el resumen de datos
        br(),
        p("Isabella Gutiérrez, Samuel Rojas y Simón Vélez"), 
        p("Universidad del Rosario"), 
        p("Probabilidad y Estadística I"),
        p("2024-I"),
        footer = tagList(  # Pie de página de la caja que contendrá una lista de elementos
          br(),
          h4("¿De dónde vienen los datos?"),
          p("Ashenfelter, Orley and Krueger, Alan. Estimates of the Economic Return to Schooling from a New Sample of Twins.
        The American Economic Review 84.5 (Dic. 1994) 1157-1173.")
        )
      )
    )
  })
  
  # Generar el texto introductorio
  output$intro_paragraph <- renderText({
    "Se desea estudiar la incidencia de los años de escolaridad en el ingreso por hora, para lo cual, se cuenta con la información de pares de gemelos monocigóticos mayores de 18 años respecto a diversas variables sociodemográficas de interés."
  })
  
  # Generar la tabla resumen de los datos
  output$data_summary <- renderTable({
    data.frame(
      "Número de registros" = nrow(twins),  # Número total de registros en el conjunto de datos
      "Número de variables" = ncol(twins),  # Número total de variables en el conjunto de datos
      "Registros con información completa" = nrow(twins_comp),  # Número de registros con información completa
      "Registros con información incompleta" = nrow(twins) - nrow(twins_comp),  # Número de registros con información incompleta
      "Dimensión de la base de datos con la información completa" = dim_datos  # Dimensiones del conjunto de datos con información completa
    )
  })
  
  

  
  # Controlar la visibilidad de la tabla ___introduccion 
  observeEvent(input$toggleTable, {
    toggle("tabla_container")
  })
  
  # Controlar la visibilidad de las gráficas discretizadas por cuartiles
  observeEvent(input$botongraficos, {
    toggle("graficos_cuartiles")
  })
  
  # Controlar la visibilidad de las gráficas discretizadas por nivel educativo
  observeEvent(input$botongraficosdiscretizados, {
    toggle("graficos_nivedu")
  })
  
  # Controlar la visibilidad de las gráficas sin discretizar
  observeEvent(input$botongraficossindisc, {
    toggle("graficos_sindisc")
  })
  
  
  # Gráfico 1
  output$dotchart_1 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCL, y = HRWAGEL)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 1, color = "blue") +
      labs(title = "Dotchart de gemelo 1 sin discretizar", x = "Años de educación", y = "Salario por hora ($)") +
      theme_minimal() 
  })
  
  # Gráfico 2
  output$dotchart_2 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCH, y = HRWAGEH)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "red") +
      labs(title = "Dotchart de gemelo 2 sin discretizar", x = "Años de educación", y = "Salario por hora ($)") +
      theme_minimal()
  })
  
  
  # Gráfico discretizado según educación gemelo 1
  output$dotchart_discretizadoedu_gemelo1 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCL_disc_edu, y = HRWAGEL)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "green") +
      labs(title = "Dotchart Discretizado según nivel educativo de Gemelo 1", x = "Años de educación (Discretizado según nivel educativo)", y = "Salario por hora ($)") +
      theme_minimal()+stat_summary(fun.data=data_summary, color="red")
  })
  # Gráfico discretizado según educación gemelo 2
  output$dotchart_discretizadoedu_gemelo2 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCH_disc_edu, y = HRWAGEH)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "purple") +
      labs(title = "Dotchart Discretizado según nivel educativo de Gemelo 2", x = "Años de educación (Discretizado según nivel educativo)", y = "Salario por hora ($)") +
      theme_minimal()+stat_summary(fun.data=data_summary, color="red")
  })
  
  # Gráfico discretizado por cuartiles gemelo 1
  output$dotchart_discretizadocua_gemelo1 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCL_disc_cua, y = HRWAGEL)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "blue") +
      labs(title = "Dotchart Discretizado por intervalos de Gemelo 1", x = "Años de educación (Discretizado por cuartiles)", y = "Salario por hora ($)") +
      theme_minimal()+stat_summary(fun.data=data_summary, color="red")
  })
  # Gráfico discretizado por cuartiles gemelo 2
  output$dotchart_discretizadocua_gemelo2 <- renderPlot({
    ggplot(twins_comp, aes(x = EDUCH_disc_cua, y = HRWAGEH)) +
      geom_dotplot(binaxis = 'y', stackdir = 'center', dotsize = 0.5, color = "orange") +
      labs(title = "Dotchart Discretizado por intervalos de Gemelo 2", x = "Años de educación (Discretizado por cuartiles)", y = "Salario por hora ($)") +
      theme_minimal()+stat_summary(fun.data=data_summary, color="red")
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
    units <- get_units(variable_gemelo1)
    
    paste("Mínimo:", stats$Min, units, "\n",
          "Cuartil 1:", stats$Q1, units, "\n",
          "Cuartil 2:", stats$Median, units, "\n",
          "Cuartil 3:", stats$Q3, units, "\n",
          "Máximo:", stats$Max, units
          )
  })
  
  # Mostrar cuartiles para gemelo 2
  output$cuartiles_gemelo2 <- renderText({
    req(input$variable_gemelo2)
    variable_gemelo2 <- input$variable_gemelo2
    stats <- summary_stats(twins_comp, variable_gemelo2)
    units <- get_units(variable_gemelo2)
    
    paste("Mínimo:", stats$Min, units, "\n",
          "Cuartil 1:", stats$Q1, units, "\n",
          "Cuartil 2:", stats$Median, units, "\n",
          "Cuartil 3:", stats$Q3, units, "\n",
          "Máximo:", stats$Max, units
          )
  })
  
  # Función unificada para moda, media y mediana del gemelo 1
  output$centros_gemelo1 <- renderText({
    req(input$variable_gemelo1)
    
    variable <- input$variable_gemelo1
    moda <- as.numeric(names(sort(table(twins_comp[[variable]]), decreasing = TRUE))[1])
    media <- mean(twins_comp[[variable]], na.rm = TRUE)
    mediana <- median(twins_comp[[variable]], na.rm = TRUE)
    units <- get_units(variable)
    
    paste("Moda de", variable, ":", moda, units, "\n",
          "Media de", variable, ":", round(media, 2), units, "\n",
          "Mediana de", variable, ":", round(mediana, 2), units)
  })
  
  # Función unificada para moda, media y mediana del gemelo 1
  output$centros_gemelo2 <- renderText({
    req(input$variable_gemelo1)
    
    variable <- input$variable_gemelo2
    moda <- as.numeric(names(sort(table(twins_comp[[variable]]), decreasing = TRUE))[1])
    media <- mean(twins_comp[[variable]], na.rm = TRUE)
    mediana <- median(twins_comp[[variable]], na.rm = TRUE)
    units <- get_units(variable)
    
    paste("Moda de", variable, ":", moda, units, "\n",
          "Media de", variable, ":", round(media, 2), units, "\n",
          "Mediana de", variable, ":", round(mediana, 2), units)
})
  
  # Función para mostrar la media y la meida +/- sd
  data_summary <- function(x) {
    m <- mean(x)
    ymin <- m-sd(x)
    ymax <- m+sd(x)
    return(c(y=m,ymin=ymin,ymax=ymax))
  }
  
}
# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)


