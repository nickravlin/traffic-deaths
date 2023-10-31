#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(dplyr)

accident <- read.csv("accident.csv")

# Get unique state names from the dataset
state_names <- unique(accident$STATENAME)

# Get unique month names and day values from the dataset
month_names <- unique(accident$MONTHNAME)
day_values <- unique(accident$DAY)

# Define a bounding box for the continental United States
us_bounding_box <- list(
  min_lng = -125,  # Minimum longitude
  max_lng = -65,   # Maximum longitude
  min_lat = 24,   # Minimum latitude
  max_lat = 49    # Maximum latitude
)

ui <- fluidPage(
  titlePanel("U.S. Fatal Car Accidents 2021"),
  
  fluidRow(
    column(12,  # Make map column larger
           leafletOutput("mymap", width = "100%", height = "600px")  # Increase map size
    ),
    column(3,
           selectInput("select", h3("Select State"),
                       choices = c("All", state_names),  # Include all state names
                       selected = "South Dakota",
                       multiple = TRUE)  # Allow multiple selections
    ),
    column(3,
           selectInput("month_select", h3("Select Month"),
                       choices = c("All", month_names),  # Include all month names
                       selected = "All",
                       multiple = TRUE)  # Allow multiple selections
    ),
    column(3,
           uiOutput("county_select")
    ),
    column(3,
           sliderInput("fatalities_slider", h3("Select Minimum Fatalities"),
                       min = 0, max = max(accident$FATALS), value = 0, step = 1)
    )
  )
)

server <- function(input, output, session) {
  output$county_select <- renderUI({
    if (any(input$select == "All")) {
      # If "All" states selected, allow selection of all counties
      selectInput("county_select", h3("Select County"), choices = c("All", unique(accident$COUNTYNAME)),
                  multiple = TRUE)  # Allow multiple selections
    } else {
      # Filter counties based on the selected state
      filtered_counties <- accident %>%
        filter(STATENAME %in% input$select) %>%
        pull(COUNTYNAME) %>%
        unique()
      selectInput("county_select", h3("Select County"), choices = c("All", filtered_counties),
                  multiple = TRUE)  # Allow multiple selections
    }
  })
  
  output$mymap <- renderLeaflet({
    filtered_data <- if (any(input$select == "All")) {
      accident
    } else {
      accident %>%
        filter(STATENAME %in% input$select)
    }
    
    # Filter data based on selected state and county
    if (any(input$county_select == "All")) {
      filtered_data <- filtered_data
    } else {
      filtered_data <- filtered_data %>%
        filter(COUNTYNAME %in% input$county_select)
    }
    
    # Apply a bounding box filter to keep points within the continental United States
    filtered_data <- filtered_data %>%
      filter(LATITUDE >= us_bounding_box$min_lat,
             LATITUDE <= us_bounding_box$max_lat,
             LONGITUD >= us_bounding_box$min_lng,
             LONGITUD <= us_bounding_box$max_lng)
    
    # Filter data based on selected month
    if (any(input$month_select == "All")) {
      filtered_data <- filtered_data
    } else {
      filtered_data <- filtered_data %>%
        filter(MONTHNAME %in% input$month_select)
    }
    
    # Filter data based on selected minimum fatalities
    filtered_data <- filtered_data %>%
      filter(FATALS >= input$fatalities_slider)
    
    leaflet(filtered_data) %>%
      addTiles() %>%
      addMarkers(lat = ~LATITUDE, lng = ~LONGITUD, popup = ~paste("Fatalities: ", FATALS, "<br>Month: ", MONTHNAME, "<br>Day: ", DAY))
  })
}

shinyApp(ui, server)

