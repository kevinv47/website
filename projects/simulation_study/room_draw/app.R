library(shiny)
library(tidyverse)

room_pref <- function(red_p, blue_p, green_p) {
  dorms <- c("red", "blue", "green")
  prob <- c(red_p, blue_p, green_p)
  sample(dorms, size = 3, replace = FALSE, prob = prob)
}

space_identifier <- function(color_pref, room_occ, r_rooms, b_rooms, g_rooms) {
  occ <- c(r_rooms, b_rooms, g_rooms)
  colors <- c("red", "blue", "green")
  color_num <- match(color_pref, colors)
  if (occ[color_num] < room_occ[color_num]) return(colors[color_num])
  return("no")
}

room_assignment <- function(pref_order, room_occ, r_rooms, b_rooms, g_rooms) {
  for (pref in pref_order) {
    room <- space_identifier(pref, room_occ, r_rooms, b_rooms, g_rooms)
    if (room != "no") return(room)
  }
}

my_room <- function(lottery_num, pref_probs, room_occ) {
  r_rooms <- b_rooms <- g_rooms <- 0
  for (num in 1:lottery_num) {
    if (num == lottery_num) {
      return(room_assignment(c("red", "blue", "green"), room_occ, r_rooms, b_rooms, g_rooms))
    }
    pref_order <- room_pref(pref_probs[1], pref_probs[2], pref_probs[3])
    room <- room_assignment(pref_order, room_occ, r_rooms, b_rooms, g_rooms)
    if (room == "red") r_rooms <- r_rooms + 1
    if (room == "blue") b_rooms <- b_rooms + 1
    if (room == "green") g_rooms <- g_rooms + 1
  }
  return("no")
}

room_draws <- function(draws, pref_probs, room_occ, lott_num) {
  set.seed(47)
  room_given <- replicate(draws, my_room(lott_num, pref_probs, room_occ))
  room_table <- prop.table(table(factor(room_given, levels = c("red", "blue", "green"))))
  tibble(
    room = names(room_table),
    proportion = as.numeric(room_table),
    draws = draws
  )
}

# Define UI
ui <- fluidPage(
  titlePanel("Room Assignment Simulation"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("lott_num", "Lottery Number:", min = 1, max = 100, value = 50),
      tableOutput("probTable")
    ),
    mainPanel(
      sliderInput("draws", "Number of Draws For Graphing:", min = 10, max = 100, value = 50),
      plotOutput("distPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  pref_probs <- c(0.6, 0.3, 0.1)
  room_occ <- c(30, 30, 40)
  palette <- c("Red Room" = "#e74c3c", "Blue Room" = "#3498db", "Green Room" = "#2ecc71")
  
  room_data <- reactive({
    map_df(1:input$draws, ~ room_draws(.x, pref_probs, room_occ, input$lott_num))
  })
  
  final_probabilities <- reactive({
    room_draws(100, pref_probs, room_occ, input$lott_num)
  })
  
  output$distPlot <- renderPlot({
    ggplot(room_data(), aes(x = draws, y = proportion, color = case_when(
      room == "red" ~ "Red Room",
      room == "blue" ~ "Blue Room",
      room == "green" ~ "Green Room"
    ))) +
      geom_point(alpha = 0.2) +
      geom_smooth(se = FALSE) +
      labs(color = "Room Color",
           x = "Number of Draws",
           y = "Proportion of Room Assignments",
           title = "Proportion of Room Colors Assigned",
           subtitle = "Proportion of each room color as draws increase") +
      scale_color_manual(values = palette) +
      theme_minimal()
  })
  
  output$probTable <- renderTable({
    final_probabilities() %>%
      select(room, proportion) %>%
      mutate(proportion = scales::percent(proportion)) %>%
      rename(`Room Color` = room, `Proportion at 100 Draws` = proportion)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
