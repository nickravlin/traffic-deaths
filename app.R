#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

accident_data <- read.csv("accident.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("U.S. Car Accident Data"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 20),
      checkboxInput("logTransform", "Log Transform People Involved", value = FALSE)
    ),
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$distPlot <- renderPlot({
    # Generate bins based on input$bins from ui.R
    x <- accident_data[, 9]
    
    # Log transform the data if the checkbox is selected
    if (input$logTransform) {
      x <- log(x + 1)
    }
    
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    hist_color <- ifelse(input$logTransform, 'blue', 'red')
    
    # Draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = hist_color, border = 'white',
         xlab = 'People Involved (Log Transformed if checked)',
         ylab = 'Frequency',
         main = 'Distribution of People Involved in Car Accident (Log Transformed if checked)')
  })
}

shinyApp(ui = ui, server = server)