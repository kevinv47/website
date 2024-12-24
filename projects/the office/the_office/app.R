library(shiny)
library(tidyverse)

# Load data and filter for main characters
main_characters <- c("Michael", "Jim", "Pam", "Dwight", "Ryan", "Angela", 
                     "Andy", "Stanley", "Phyllis", "Kelly", "Toby", 
                     "Kevin", "Oscar", "Creed", "Meredith")

palette <- c("Michael" = "#7FB3D5", "Jim" = "#8B8B8B", "Pam" = "#FF8DAA", 
             "Dwight" = "#FFD700", "Ryan" = "#28A745", "Angela" = "#5B2C6F", 
             "Andy" = "#FF6F61", "Stanley" = "#5C3A1D", "Phyllis" = "#A0522D", 
             "Kelly" = "#FF1493", "Toby" = "#4A90E2", "Kevin" = "#FFA500", 
             "Oscar" = "#1E4D2B", "Creed" = "#C0C0C0", "Meredith" = "#A50000")

office <- read_csv("the-office_lines.csv") |> 
  filter(Character %in% main_characters)

# Define UI
ui <- fluidPage(
  titlePanel("Shiny Plot"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("word", "Enter a word to search:", ""), # Word input field
      actionButton("submit", "Search")                 # Submit button
    ),
    mainPanel(
      plotOutput("wordPlot")                           # Plot output
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  regular_expression <- reactiveVal("") # Reactive variable for the regex
  
  # Update the reactive value when the submit button is clicked
  observeEvent(input$submit, {
    regular_expression(paste0("(?i)\\b", input$word, "\\b")) # Add word boundaries
  })
  
  # Render the plot
  output$wordPlot <- renderPlot({
    req(regular_expression()) # Ensure a word is provided
    
    office |> 
      mutate(on_task = case_when(
        str_detect(Line, regular_expression()) ~ 1,
        TRUE ~ 0
      )) |> 
      group_by(Character) |> 
      summarise(prop = sum(on_task) / n()) |> 
      mutate(percentage = prop * 100) |> 
      ggplot(aes(x = reorder(Character, prop), y = prop, fill = Character)) +
      geom_col(show.legend = FALSE) +
      geom_text(aes(label = paste0(round(percentage, 1), "%")), 
                vjust = 0.5, 
                hjust = 1.05,
                size = 5,
                color = "white",
                fontface = "bold") +
      scale_fill_manual(values = palette) +
      labs(y = "Percentage of Lines with the Word",
           x = "Character",
           subtitle = "Rounded to the nearest tenth") +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            panel.grid = element_blank(),
            plot.title = element_text(hjust = 0.6)) +
      coord_flip()
  })
}

# Run the app
shinyApp(ui = ui, server = server)
