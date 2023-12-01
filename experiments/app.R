library(shiny)

ui <- fluidPage(
  tags$head(tags$script("test.js")),
  fileInput("file", "Upload"),
  verbatimTextOutput("wd"),
  verbatimTextOutput("file")
)

server <- function(input, output, session) {
  
  output$wd <- renderPrint({
    getwd()
  })
  
  output$file <- renderPrint({
    input$file
  })
}

shinyApp(ui = ui, server = server)