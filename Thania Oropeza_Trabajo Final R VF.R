#### TRABAJO FINAL - Thania Oropeza ####

rm(list = ls())  
## Instalación de paquetes necesarios:

install.packages("shiny")
install.packages("ggplot2")
install.packages("gridExtra")
install.packages("gridtext")

library(shiny)
library(ggplot2)
library(gridExtra)
library(gridtext)

## Dataset a tomar para el ejercicio MTCARS:
str(mtcars)
summary(mtcars)

## Se define la interfaz de usuario para el app:
ui <- fluidPage(
  titlePanel("MTCARS ShinyApp - TRABAJO FINAL R - Thania Oropeza"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Seleccionar opciones:"),
      selectInput(inputId = "var_select", "1. Seleccionar Variable 1:", choices = names(mtcars)),
      selectInput(inputId = "var_select2", "2. Seleccionar Variable 2:", choices = names(mtcars)),
      
      splitLayout(
        cellWidths = c("50%", "50%"),
        sliderInput(inputId = "bins", "3. Para Histograma: N° de BINS", min = 5, max = 25, value = 15),
        sliderInput(inputId = "tamaño_punto", "4. Dispersión: Tamaño de los puntos", min = 1, max = 5, value = 3)
      ),
      
      radioButtons(inputId = "color", label = "5. Seleccionar el color de los gráficos", 
                   choices = c("red", "purple", "orange"), selected = "red"),

      # Seleccionar formato de descarga
      selectInput(inputId = "download_format", "6. Seleccionar formato de descarga gráficos", 
                  choices = c("png", "pdf"), selected = "pdf"),
      
      # Botón de descarga de gráficos:
      downloadButton("exportarplot", "Descargar gráficos"),
      
      p("Warning: Click en 'ajustar modelo' antes de descargar reporte completo:"),
      actionButton("fit", "Ajustar modelo"),
      
      # Botón para descargar el reporte completo:
      downloadButton("downloadPlot", "Descargar reporte completo (PDF)")
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Panel 1: Descripción", 
                           h3("Descripción del App"),
                           p("Esta aplicación visualiza datos del dataset MTCARS con gráficos interactivos y un modelo de regresión."),
                           h3("Vista Previa de los Datos"),
                           tableOutput("data_preview")
                  ),
                  
                  tabPanel("Panel 2: Gráficos", 
                           h3("Visualización de Gráficos"),
                           fluidRow(
                             column(6, plotOutput("plot1")),
                             column(6, plotOutput("plot2")),
                             column(6, plotOutput("plot3")),
                             column(6, plotOutput("plot4"))
                           )
                  ),
                  
                  tabPanel("Panel 3: Resultados", 
                           h3("Resultados y Modelo de Regresión"),
                           tableOutput("summary_table"),
                           plotOutput("regression_plot")
                  )
      )
    )
  )
)

# Definir la lógica del servidor
server <- function(input, output) {
  
  # Vista previa de los datos
  output$data_preview <- renderTable({
    head(mtcars)
  })
  
  # Primer gráfico - Histograma
  output$plot1 <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var_select)) +
      geom_histogram(bins = input$bins, fill = input$color, color = "black") +
      theme_minimal() +
      labs(title = paste("Histograma de", input$var_select))
  })
  
  # Segundo gráfico - Burbujas
  output$plot2 <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var_select, y = input$var_select2, size = "cyl", color = "factor(am)")) +
      geom_point(alpha = 0.7) +
      labs(title = "Gráfico de Burbujas",
           x = input$var_select, 
           y = input$var_select2, 
           size = "Cilindros", 
           color = "Transmisión") +
      theme_minimal()
  })
  
  # Tercer gráfico - Boxplot
  output$plot3 <- renderPlot({
    ggplot(mtcars, aes_string(y = input$var_select)) +
      geom_boxplot(fill = input$color) +
      labs(title = paste("Diagrama de caja de", input$var_select)) +
      theme_minimal()
  })
  
  # Cuarto gráfico - Dispersión
  output$plot4 <- renderPlot({
    ggplot(mtcars, aes_string(x = input$var_select, y = input$var_select2)) +
      geom_point(size = input$tamaño_punto, color = input$color) +
      labs(title = "Gráfico de dispersión", 
           x = input$var_select, 
           y = input$var_select2) +
      theme_minimal()
  })
  
  # Tabla de resumen
  output$summary_table <- renderTable({
    req(input$var_select)
    summary(mtcars[, input$var_select, drop = FALSE])
  })
  
  # Modelo de regresión - Regresión lineal simple
  model <- eventReactive(input$fit, {
    req(input$var_select, input$var_select2)
    lm(as.formula(paste(input$var_select2, "~", input$var_select)), data = mtcars)
  })
  
  output$regression_plot <- renderPlot({
    req(model())
    ggplot(mtcars, aes_string(x = input$var_select, y = input$var_select2)) +
      geom_point() +
      geom_smooth(method = "lm", color = "blue") +
      labs(title = "Regresión Lineal",
           x = input$var_select,
           y = input$var_select2)
  })
  
  #Descargar gráficos:
  
  output$exportarplot <- downloadHandler(
    filename = function() {
      paste("graficos_mtcars", input$download_format, sep = ".")
    },
    content = function(file) {
      if (input$download_format == "png") {
        png(file, width = 800, height = 600)
      } else {
        pdf(file, width = 8, height = 6)
      }
      
      # Crear gráficos
      hist <- ggplot(mtcars, aes_string(x = input$var_select)) +
        geom_histogram(bins = input$bins, fill = input$color, color = "black") +
        theme_minimal() +
        labs(title = paste("Histograma de", input$var_select))
      
      burb <- ggplot(mtcars, aes_string(x = input$var_select, y = input$var_select2, size = "cyl", color = "factor(am)")) +
        geom_point(alpha = 0.7) +
        labs(title = "Gráfico de Burbujas",
             x = input$var_select, 
             y = input$var_select2, 
             size = "Cilindros", 
             color = "Transmisión") +
        theme_minimal()
      
      boxp <- ggplot(mtcars, aes_string(x = input$var_select)) +
        geom_boxplot(fill = input$color) +
        labs(title = paste("Diagrama de caja de", input$var_select)) +
        theme_minimal()
      
      disp <- ggplot(mtcars, aes_string(x = input$var_select, y = input$var_select2)) +
        geom_point(size = input$tamaño_punto, color = input$color) +
        labs(title = "Gráfico de dispersión",
             x = input$var_select,
             y = input$var_select2) +
        theme_minimal()
      
      # Imprimir todos los gráficos en un solo archivo
      grid.arrange(hist, burb, boxp, disp, ncol = 2)
      
      dev.off()
    }
  )      
  
  # Descargar el reporte completo:
  output$downloadPlot <- downloadHandler(
    filename = function() { 
      paste("Reporte_MTCARS_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      pdf(file, width = 8, height = 11)
      
      # Panel 1: Descripción
      plot.new()
      title("Panel 1: Descripción")
      text(0.1, 0.9, "Esta aplicación visualiza datos del dataset MTCARS", cex = 1.2)
      
      # Mostrar la tabla de vista previa
      grid.table(head(mtcars))
      
      # Panel 2: Gráficos
      hist <- ggplot(mtcars, aes_string(x = input$var_select)) +
        geom_histogram(bins = input$bins, fill = input$color, color = "black") +
        labs(title = paste("Histograma de", input$var_select))
      print(hist)
      
      burb <- ggplot(mtcars, aes_string(x = input$var_select, y = input$var_select2, size = "cyl", color = "factor(am)")) +
        geom_point(alpha = 0.7) +
        labs(title = "Gráfico de Burbujas", x = input$var_select, y = input$var_select2)
      print(burb)
      
      boxp <- ggplot(mtcars, aes_string(y = input$var_select)) +
        geom_boxplot(fill = input$color) +
        labs(title = paste("Diagrama de caja de", input$var_select))
      print(boxp)
      
      disp <- ggplot(mtcars, aes_string(x = input$var_select, y = input$var_select2)) +
        geom_point(size = input$tamaño_punto, color = input$color) +
        labs(title = "Gráfico de dispersión", x = input$var_select, y = input$var_select2)
      print(disp)
      
      # Panel 3: Resultados y Modelo de Regresión
      plot.new()
      title("Panel 3: Resultados y Modelo de Regresión")
      
      # Resumen de la variable seleccionada
      grid.table(summary(mtcars[, input$var_select, drop = FALSE]))
      
      # Modelo de regresión si es factible:
      if (!is.null(model())) {
        print(summary(model()))
        reg_plot <- ggplot(mtcars, aes_string(x = input$var_select, y = input$var_select2)) +
          geom_point() +
          geom_smooth(method = "lm", color = "blue") +
          labs(title = "Regresión Lineal", x = input$var_select, y = input$var_select2)
        print(reg_plot)
      }
      
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)


