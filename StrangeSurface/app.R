webr::install("rmarchingcubes")
webr::install("r3js")

library(rmarchingcubes)
library(r3js)

f <- function(x, y, z, alpha) {
  A <- cospi(alpha)
  B <- sinpi(alpha)
  B2 <- B*B
  AB2 <- A*B2
  z2 <- z*z
  z4 <- z2*z2
  z4*(B2-2*A-2) + 4*x*y^2*(AB2-4*B2) + x*z2*(AB2-B2) + 
    3*z2*(AB2-2*A+2) + x*(B2-AB2) + AB2 - B2
}

# library(cOde)
# jacobianSymb(
#   c(f = "z^4*(B2-2*A-2) + 4*x*y^2*(AB2-4*B2) + x*z^2*(AB2-B2) + 3*z^2*(AB2-2*A+2) + x*(B2-AB2) + AB2 - B2"),
#   c("x", "y", "z")
# )

grad <- function(x, y, z, alpha) {
  A <- cospi(alpha)
  B <- sinpi(alpha)
  B2 <- B*B
  AB2 <- A*B2
  z2 <- z*z
  z3 <- z*z2
  cbind(
    4*(AB2-4*B2)*y^2 + (AB2-B2)*(z2 - 1), 
    8*(AB2-4*B2)*x*y,
    4**(B2-2*A-2)*z3 + 2*(AB2-B2)*x*z + 6*(AB2-2*A+2)*z
  )
}

h <- function(ρ, θ, ϕ, alpha){
  x <- ρ * cos(θ) * sin(ϕ)
  y <- ρ * sin(θ) * sin(ϕ)
  z <- ρ * cos(ϕ)
  f(x, y, z, alpha)
}

# make grid
nρ <- 100L; nθ <- 200L; nϕ <- 100L
ρ_ <- seq(0, 3.05, length.out = nρ) # ρ runs from 0 to the desired radius
θ_ <- seq(0, 2*pi, length.out = nθ)
ϕ_ <- seq(0, pi, length.out = nϕ) 
G <- expand.grid(Rho = ρ_, Theta = θ_, Phi = ϕ_)


#--- SHINY APP ----####
library(shiny)

css <- "
#info {
  background-color: #ddd;
}
"

# shiny UI ####
ui <- fluidPage(
  tags$head(
    tags$style(HTML(css))
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "alpha", "alpha", min = 2.2, max = 3.8, value = 3, step = 0.1
      ),
      br(),
      textOutput("info")
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
    surf <- contour3d(voxel, level = 0, x = ρ_, y = θ_, z = ϕ_)
    # transform to Cartesian coordinates
    ρθϕ <- surf[["vertices"]]
    ρ <- ρθϕ[, 1L]; θ <- ρθϕ[, 2L]; ϕ <- ρθϕ[, 3L] 
    vertices <- cbind(
      ρ * cos(θ) * sin(ϕ),
      ρ * sin(θ) * sin(ϕ),
      ρ * cos(ϕ)
    )
    triangles <- surf[["triangles"]]
    normals <- -grad(vertices[, 1L], vertices[, 2L], vertices[, 3L], alpha)
    list(vertices, triangles, normals)
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
        normals = mesh[[3L]],
        col = "maroon"
      )
    )
  })
  
  output[["info"]] <- renderText({
    if(packageVersion("r3js") <= "0.0.2") {
      paste0(
        "When you change `alpha`, the new plot is added to the previous one. ",
        "You have to upgrade the 'r3js' package to get rid of this bug." 
      )
    }
  })
  
}

# shiny app object ----
shinyApp(ui, server)
