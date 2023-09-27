library(shiny)

# shiny UI ####
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "alpha", "alpha", min = 2, max = 4, value = 3, step = 0.1
      )
    ),
    mainPanel(
      r3jsOutput("surface", height = "512px")
    )
  )
)

# shinyServer ####
server <- function(input, output, session) {

  Mesh <- reactive({
    # voxel
    alpha <- input[["alpha"]] / 4
    voxel <- with(G, array(h(Rho, Theta, Phi, alpha), dim = c(nρ, nθ, nϕ)))
    # 3d contour
    surf <- contour3d(voxel, level = 0, x = ρ0, y = θ0, z = ϕ0)
    # transform to Cartesian coordinates
    ρθϕ <- surf[["vertices"]]
    ρ <- ρθϕ[, 1L]; θ <- ρθϕ[, 2L]; ϕ <- ρθϕ[, 3L] 
    vertices <- cbind(
      ρ * cos(θ) * sin(ϕ),
      ρ * sin(θ) * sin(ϕ),
      ρ * cos(ϕ)
    )
    triangles <- surf[["triangles"]]
    list(vertices, triangles)
  })
  
  output[["surface"]] <- renderR3js({
    mesh <- Mesh()
    vertices <- mesh[[1L]]
    r3js(
      shape3js(
        plot3js.window(
          plot3js.new(),
          xlim = range(vertices[, 1L]),
          ylim = range(vertices[, 2L]),
          zlim = range(vertices[, 3L]),
          aspect = c(1, 1, 1)
        ),
        vertices = vertices,
        faces = mesh[[2L]],
        normals = NULL,
        col = "maroon"
      )
    )
  })
  
}

# shiny app object ----
shinyApp(ui, server)
