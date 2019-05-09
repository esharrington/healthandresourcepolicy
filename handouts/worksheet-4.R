# Libraries
library(ggplot2)
library(dplyr)

# Data
species <- read.csv('data/species.csv',
  stringsAsFactors = FALSE)
animals <- read.csv('data/animals.csv',
  na.strings = '',
  stringsAsFactors = FALSE)

# User Interface
in1 <- selectInput(
  inputId = 'pick_species',
  label = 'Pick a Species',
  choices = unique(species[['id']]))
in2 <- ...(
  ... = 'slider_months',
  ...,
  ...,
  ...,
  ...)
side <- sidebarPanel(
  'Options', ...)
out1 <- textOutput('species_label')
out2 <- plotOutput('species_plot')
main <- mainPanel(out2)
tab <- tabPanel(title = 'Species',
                sidebarLayout(side, main))
ui <- navbarPage('Portal Project', tab)

# Server
server <- function(input, output) {

  slider_months <- reactive({
    ...
    ...
  })
  output[['species_label']] <- renderText(
    species %>%
      filter(id == input[['pick_species']]) %>%
      select(genus, species) %>%
      paste(collapse = ' ')
  )
  output[['species_plot']] <- renderPlot({
    df <- animals %>%
      filter(id == input[['pick_species']]) %>%
      filter(month %in% ...)
    ggplot(df, aes(year)) +
      geom_bar()
  })
 ... <- renderDataTable({
    ...
    ...
    ...
 })
}

# Create the Shiny App
shinyApp(ui = ui, server = server)
