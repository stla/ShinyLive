webr::install("deldir")
webr::install("polyclip")
library(deldir)

n <- 256L
x_ <- y_ <- seq(0, 1, length.out = n)
M <- matrix(NA_real_, nrow = n, ncol = n)

voronoi <- function(x, y) {
  z <- deldir(x, y, rw = c(0, 1, 0, 1))
  vrn <- tile.list(z, clipp = list(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1)))
  sites <- vapply(vrn, function(cell) {
    cell$pt
  }, FUN.VALUE = numeric(2L))
  list(vrn, sites)
}

colorMatrix <- function(vrn) {
  w <- vrn[[1L]]; sites <- vrn[[2L]]
  nearestSite <- function(xy) {
    dists <- apply(sites, 2L, function(pt) {
      c(crossprod(pt - xy))
    })
    which.min(dists)
  }
  maxDists <- vapply(w, function(cell) {
    plg <- rbind(cell$x, cell$y)
    sqrt(max(apply(plg, 2L, function(xy) {
      c(crossprod(xy - cell$pt))
    })))
  }, FUN.VALUE = numeric(1L))
  clr <- function(x, y) {
    xy <- c(x, y)
    siteIndex <- nearestSite(xy)
    maxd <- maxDists[siteIndex]
    site <- w[[siteIndex]][["pt"]]
    min(1, sqrt(c(crossprod(xy - site))) / maxd)
  }
  #
  for(i in 1L:n) {
    for(j in 1L:n) {
      M[i, j] <- clr(x_[i], 1-y_[j])
    }
  }
  M
}

isDark <- function(color) {
  rgb <- col2rgb(color)
  rgb[1]*0.299 + rgb[2]*0.587 + rgb[3]*0.114 < 186
}

plotRaster <- function(M, vrn, paltt = "Grays", bias = 1) {
  colors <- hcl.colors(100L, paltt)
  borderColor <- ifelse(isDark(colors[length(colors)]), "black", "white") 
  fcol <- colorRamp(colors, bias = bias, interpolate = "spline")
  img_rgb <- fcol(t(M))
  img <- rgb(img_rgb[, 1L], img_rgb[, 3L], img_rgb[, 3L], maxColorValue = 255)
  img <- matrix(img, nrow = n, ncol = n)
  w <- vrn[[1L]]; sites <- vrn[[2L]]
  par(mar = c(0, 0, 0, 0))
  plot(
    NULL, asp = 1, xlim = c(0, 1), ylim = c(0, 1), lwd =5,  
    xlab = NA, ylab = NA, axes = FALSE, xaxs = "i", yaxs = "i"
  )
  rasterImage(img, 0, 0, 1, 1)
  points(t(sites), col = "red", pch = 19)
  plot(w, add = TRUE, border = borderColor)
}


####---- SHINY APP ----####
library(shiny)
library(bslib)

ui <- page_sidebar(
  plotOutput("tessellation", width = "512px", height = "512px", fill = FALSE),
  sidebar = sidebar(
    splitLayout(
      actionButton("generate", "New sites"),
      numericInput(
        "nsites", "# sites", value = 10, min = 5, step = 5
      )
    ),
    selectInput(
      "palette", "Palette", choices = hcl.pals(), selected = "Grays"
    ),
    numericInput(
      "bias", "Bias", value = 0.5, min = 0.2, step = 0.05
    ),
    width = "30%"
  ),
  theme = bs_theme(bootswatch = "darkly")
)

server <- function(input, output, session) {
  
  XY <- eventReactive(input[["generate"]], {
    nsites <- input[["nsites"]]
    list(x = runif(nsites), y = runif(nsites))
  }, ignoreNULL = FALSE)
  
  Voronoi <- eventReactive(XY(), {
    x <- XY()[["x"]]; y <- XY()[["y"]]
    voronoi(x, y)
  })
  
  ColorMatrix <- eventReactive(Voronoi(), {
    x <- XY()[["x"]]; y <- XY()[["y"]]
    vrn <- voronoi(x, y)
    colorMatrix(vrn)
  })
  
  output[["tessellation"]] <- renderPlot({
    plotRaster(
      ColorMatrix(), Voronoi(), input[["palette"]], input[["bias"]]
    )
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)
