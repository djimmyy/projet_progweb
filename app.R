library(shiny)

ui <- fluidPage(
  titlePanel("Test R Shiny - Projet Web"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins", "Nombre de classes :", min = 5, max = 50, value = 20)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

server <- function(input, output, session) {
  output$distPlot <- renderPlot({
    x <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    hist(x, breaks = bins, col = 'steelblue', border = 'white',
         xlab = "Temps d'attente (min)", main = "Histogramme de test")
  })
}

shinyApp(ui = ui, server = server)
