# Cargar las bibliotecas necesarias
library(shiny)
library(readxl)
library(ggplot2)
library(dplyr)

# Definir la interfaz de usuario de la aplicación
ui <- fluidPage(
  titlePanel("App de Linealidad"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Selecciona un archivo Excel con datos de calibración"),
      selectInput("y_column", "Selecciona la columna Y:", choices = NULL),
      actionButton("regression_button", "Realizar Regresión")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Gráficas", plotOutput("calibration_plot")),
        tabPanel("Tabla de Parámetros", tableOutput("parameter_table")),
        tabPanel("Gráfica de Residuales", plotOutput("residual_plot"))
      )
    )
  )
)

# Definir el servidor de la aplicación
server <- function(input, output) {
  # Leer los datos del archivo Excel
  data <- reactive({
    req(input$file)
    df <- read_excel(input$file$datapath)
    choices <- colnames(df)[-1] # Excluir la primera columna (X) de las opciones
    updateSelectInput(session, "y_column", choices = choices)
    return(df)
  })
  
  # Realizar la regresión lineal
  regression_results <- reactive({
    req(input$regression_button)
    y_column <- sym(input$y_column)
    df <- data()
    model <- lm(!!y_column ~ X, data = df)
    summary_data <- summary(model)
    residuals <- model$residuals
    return(list(model = model, summary_data = summary_data, residuals = residuals))
  })
  
  # Crear la gráfica de calibración
  output$calibration_plot <- renderPlot({
    req(input$regression_button)
    df <- data()
    regression <- regression_results()
    
    ggplot(df, aes(x = X, y = !!sym(input$y_column))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(title = "Curva de Calibración",
           x = "X",
           y = input$y_column)
  })
  
  # Crear la tabla de parámetros
  output$parameter_table <- renderTable({
    req(input$regression_button)
    regression <- regression_results()
    summary_data <- regression$summary_data
    data.frame(
      "Parámetro" = c("Intercepto", "Pendiente", "R-squared", "P-value"),
      "Valor" = c(summary_data$coefficients["(Intercept)"],
                  summary_data$coefficients["X"],
                  summary_data$r.squared,
                  summary_data$coefficients["X", "Pr(>|t|)"])
    )
  })
  
  # Crear la gráfica de residuales
  output$residual_plot <- renderPlot({
    req(input$regression_button)
    regression <- regression_results()
    
    ggplot(data.frame(X = data()$X, Residuals = regression$residuals), aes(x = X, y = Residuals)) +
      geom_point() +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Gráfica de Residuales",
           x = "X",
           y = "Residuales")
  })
}

# Ejecutar la aplicación Shiny
shinyApp(ui = ui, server = server)
