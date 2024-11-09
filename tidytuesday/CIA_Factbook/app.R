#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)
library(maps)

cia <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-22/cia_factbook.csv') |> 
  mutate(
    iso_a3 = countrycode::countrycode(
      country,
      origin = "country.name",
      destination = "iso3c"
    )) |> 
  drop_na(iso_a3)

world_data <- map_data("world") |> 
  mutate(
    iso_a3 = countrycode::countrycode(
      region,
      origin = "country.name",
      destination = "iso3c")) |> 
  drop_na(iso_a3)

data4map <- world_data |> 
  left_join(cia, by = "iso_a3")

vars <- setdiff(names(cia), c("country", "iso_a3"))

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),
  
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput('xcol', 'color Variable', vars)),

        # Show a plot of the generated distribution
        mainPanel(plotOutput("mapPlot"))
    ))
    
# Define server logic required to draw a histogram
server <- function(input, output) {

    
    output$mapPlot <- renderPlot({
      data4map |> 
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(aes_string(fill = input$xcol), color = "white", size = 0.5) +
        theme_void() +
        scale_fill_gradient(low = "blue", high = "red", trans = "log")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
