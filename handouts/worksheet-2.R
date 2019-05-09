# Data
species <- ...('data/species.csv',
  stringsAsFactors = FALSE)
... <- read.csv(...,
  na.strings = '',
  stringsAsFactors = FALSE)

# User Interface
in1 <- selectInput(
  ...,
  label = 'Pick a species',
  choices = ...)
...
tab1 <- ...(
  ...
  ...)
ui <- navbarPage(
  title = 'Portal Project',
  ...)

# Server
server <- function(...) {
  ... <- ...(
    input[[...]])
}

# Create the Shiny App
shinyApp(ui = ui, server = server)
