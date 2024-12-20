
library(shiny)
library(tidyverse)
library(maps)

cia <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2024/2024-10-22/cia_factbook.csv') |> 
  mutate(
    birth_rate = round(birth_rate, digits = 2),
    death_rate = round(death_rate, digits = 2),
    infant_mortality_rate = round(infant_mortality_rate, digits = 2),
    internet_users = round(internet_users, digits = 2) / population,
    life_exp_at_birth = round(life_exp_at_birth, digits = 2),
    maternal_mortality_rate = round(maternal_mortality_rate, digits = 2),
    net_migration_rate = round(net_migration_rate, digits = 2),
    population = round(population, digits = 2),
    population_growth_rate = round(population_growth_rate, digits = 2)
  ) |> 
  mutate(
    iso_a3 = countrycode::countrycode(
      country,
      origin = "country.name",
      destination = "iso3c")) |> 
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

label_mapping <- function(input_col) {
  case_when(
    input_col == "birth_rate" ~ "Birth rate",
    input_col == "death_rate" ~ "Death Rate",
    input_col == "infant_mortality_rate" ~ "Infant Mortality Rate",
    input_col == "internet_users" ~ "% of Population",
    input_col == "life_exp_at_birth" ~ "Years",
    input_col == "maternal_mortality_rate" ~ "Maternal Mortality Rate",
    input_col == "net_migration_rate" ~ "Net Migration Rate",
    input_col == "population" ~ "Population",
    input_col == "population_growth_rate" ~ "Population Growth Rate",
    TRUE ~ input_col  # Default case if no match
  )
}

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("The 2014 CIA Factobook (Global Indicators)"),
  
  
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput('xcol', 'World Indicator', choices = list(
            "Birth Rate" = "birth_rate",
            "Death Rate" = "death_rate",
            "Infant Mortality Rate" = "infant_mortality_rate",
            "Internet Users as a % of Population" = "internet_users",
            "Life Expectancy at Birth" = "life_exp_at_birth",
            "Maternal Mortality Rate" = "maternal_mortality_rate",
            "Net Migration Rate" = "net_migration_rate",
            "Population" = "population",
            "Population Growth Rate" = "population_growth_rate"
          )),
          textOutput("reactivePrint")),

        # Show a plot of the generated distribution
        mainPanel(plotOutput("mapPlot"))
    ))
    
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$reactivePrint <- renderText({
      case_when(
        input$xcol == "birth_rate" ~ "Birth rate (number of live births per 1,000 people).",
        input$xcol == "death_rate" ~ "Death rate (number of deaths per 1,000 people).",
        input$xcol == "infant_mortality_rate" ~ "Infant mortality rate (number of deaths of infants under one year old per 1,000 live births).",
        input$xcol == "internet_users" ~ "Number of internet users divided by total country population.",
        input$xcol == "life_exp_at_birth" ~ "Life expectancy at birth (in years).",
        input$xcol == "maternal_mortality_rate" ~ "Maternal mortality rate (number of maternal deaths per 100,000 live births).",
        input$xcol == "net_migration_rate" ~ "Net migration rate (number of migrants per 1,000 people).",
        input$xcol == "population" ~ "Total population of the country.",
        input$xcol == "population_growth_rate" ~ "Population growth rate (multiplier)."
      )
    })
    
    output$mapPlot <- renderPlot({
      data4map |> 
        ggplot(aes(x = long, y = lat, group = group)) +
        geom_polygon(aes_string(fill = input$xcol), color = "white", size = 0.5) +
        theme_void() +
        scale_fill_gradient(low = "blue", high = "red", trans = "log", 
                            labels = scales::label_comma(accuracy = 0.01)) +
        labs(fill = label_mapping(input$xcol))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
