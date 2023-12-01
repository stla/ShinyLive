library(shiny)

ui <- fluidPage(
  tags$head(tags$script("test.js")),
  fileInput("file", "Upload")
)

server <- function(input, output, session) {}

shinyApp(ui = ui, server = server)