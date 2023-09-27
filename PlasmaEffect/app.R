webr::install("RcppColors")

library(RcppColors)
library(shiny)


plasma <- function(x, y, w, h) {
  (128 + (128 * sin(x/16))
   + 128 + (128 * sin(y/32))
   + 128 + (128 * sin(sqrt((x - w/2)*(x - w/2) + (y - h/2)*(y - h/2)) / 8))
   + 128 + (128 * sin(sqrt(x*x + y*y) / 8))) / 4
}

x_ <- y_ <- seq(-127, 127, length.out = 512L)
Zimg <- function(w1, h1, w2, h2) {
  Mre <- outer(x_, y_, plasma, w = w1, h = h1)
  Mim <- outer(x_, y_, plasma, w = w2, h = h2)
  Z <- complex(real = Mre, imaginary = Mim)
  dim(Z) <- c(512L, 512L)
  Z  
}


#### SHINY APP ----####

ui <- fluidPage(
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "colormap", "Color map",
        choices = paste0("colorMap", c(1L, 2L, 4L:14L))
      ),
      sliderInput(
        "nrmlz", "normalization", min = 1, max = 256, step = 5, value = 256
      ),
      splitLayout(
        sliderInput("w1", "w1", min = 0, max = 200, step = 4, value = 40),
        sliderInput("h1", "h1", min = 0, max = 200, step = 4, value = 40)
      ),
      splitLayout(
        sliderInput("w2", "w2", min = 0, max = 200, step = 4, value = 160),
        sliderInput("h2", "h2", min = 0, max = 200, step = 4, value = 160)
      ),
      width = 6
    ),
    mainPanel(
      plotOutput("plasma"),
      width = 6
    )
  )
) 


server <- function(input, output, session) {
  
  Z0 <- reactive({
    Zimg(
      input[["w1"]], input[["h1"]], input[["w2"]], input[["h2"]]
    )
  })
  
  Z1 <- reactive({
    Z0() / input[["nrmlz"]]
  })
  
  Img <- reactive({
    switch(
      input[["colormap"]],
      colorMap1 = colorMap1(Z1()), 
      colorMap2 = colorMap2(Z1()), 
      colorMap4 = colorMap4(Z1()), 
      colorMap5 = colorMap5(Z1()), 
      colorMap6 = colorMap6(Z1()), 
      colorMap7 = colorMap7(Z1()), 
      colorMap8 = colorMap8(Z1()), 
      colorMap9 = colorMap9(Z1()), 
      colorMap10 = colorMap10(Z1()), 
      colorMap11 = colorMap11(Z1()), 
      colorMap12 = colorMap12(Z1()), 
      colorMap13 = colorMap13(Z1()), 
      colorMap14 = colorMap14(Z1()) 
    )
  })
  
  output[["plasma"]] <- renderPlot({
    par(mar = c(0, 0, 0, 0))
    plot(
      NULL, xlim = c(0, 1), ylim = c(0, 1), asp = 1,
      xlab = NA, ylab = NA, axes = FALSE, xaxs = "i", yaxs = "i"
    )
    rasterImage(Img(), 0, 0, 1, 1)
  }, width = 512, height = 512)
  
}


shinyApp(ui, server)
