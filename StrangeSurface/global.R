webr::install("rmarchingcubes")
webr::install("r3js")

library(rmarchingcubes)
library(r3js)

f <- function(x, y, z, alpha) {
  A <- cospi(alpha)
  B <- sinpi(alpha)
  B2 <- B*B
  z2 <- z*z
  z4 <- z2*z2
  z4*(B2-2*A-2) + 4*x*y^2*(A*B2-4*B2) + x*z2*(A*B2-B2) + 
    3*z2*(A*B2-2*A+2) + x*(B2-A*B^2) + A*B2 - B2
}

h <- function(ρ, θ, ϕ, alpha){
  x <- ρ * cos(θ) * sin(ϕ)
  y <- ρ * sin(θ) * sin(ϕ)
  z <- ρ * cos(ϕ)
  f(x, y, z, alpha)
}

# make grid
nρ <- 150L; nθ <- 250L; nϕ <- 150L
ρ0 <- seq(0, 3.05, length.out = nρ) # ρ runs from 0 to the desired radius
θ0 <- seq(0, 2*pi, length.out = nθ)
ϕ0 <- seq(0, pi, length.out = nϕ) 
G <- expand.grid(Rho = ρ0, Theta = θ0, Phi = ϕ0)
