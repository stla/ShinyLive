webr::install("ggplot2")
webr::install("grid")
webr::install("htmlTable")

library(shiny)
library(ggplot2)
library(grid)
library(htmlTable)

#####
##### AUXILIARY FUNCTIONS #### 
#####
lengthIC <- function(n, sigma.hat, alpha = 5/100) {
  2 * qt(1 - alpha/2, n-1) * sigma.hat/sqrt(n)
}
# credibility interval for standard deviation
ICsigma <- function(n, sigma.hat, alpha = 5/100) {
  bmin <- sqrt(qchisq(alpha/2, n-1))
  bmax <- sqrt(qchisq(1 - alpha/2, n-1))
  sqrt.scr <- sigma.hat * sqrt(n-1)
  c(lwr = sqrt.scr/bmax, upr = sqrt.scr/bmin)
}
# square root of inverse gamma distribution
digamma <- function(y, a, b) {
  dgamma(1/y, a, b) / y^2
}
dsqrtigamma <- function(y, a, b) {
  digamma(y^2, a, b) * 2*y
}
qsqrtigamma <- function(p, a, b) {
  sqrt(1/qgamma(1 - p, a, b))
}


# Shiny UI ####
ui <- pageWithSidebar(
  
  # Application title
  headerPanel("Sample size determination for estimating a mean"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    h3("Standard deviation uncertainty:"),
    helpText(
      "Note: the distribution plotted at right shows the uncertainty 
about the standard deviation resulting from your preliminary data. 
      It depends on the sample size and the standard deviation estimate of your preliminary sample."),
    # 
    h4("Set the size of the sample you used to estimate the standard deviation:"),
    numericInput("n", 
                 "",
                 min = 2,
                 max = 200,
                 value = 5,
                 step = 1),
    
    h4("Set estimated standard deviation:"),
    numericInput("sigma.hat", 
                 "", 
                 min = 0.5, 
                 max = 50, 
                 value = 5, 
                 step = 0.5),
    helpText(
      "Set here the standard deviation estimated from your preliminary experiments"
    ), 
    br(), 
    h3("Half-width of the confidence interval about the mean:"), 
    h4("Set confidence level (%):"),
    sliderInput(
      "level", "", min = 50, max = 99, value = 95, step = 1
    ),
    helpText(
      "Set here the confidence level of the confidence interval you want to predict"
    ),
    #	h4("Confidence interval about the mean:"), 
    # 
    h4("Set prediction error  (%):"), 
    sliderInput(
      "credibility", "", min = 1, max = 50, value = 40, step = 0.5
    ),  
    helpText(
      "Set here the error probability of the predicted half-width"
    ),
    br(), 
    numericInput("nmin", 
                 "Minimum sample size", 
                 min = 2, 
                 max = 15, 
                 value = 5,
                 step = 1),
    numericInput("nmax", 
                 "Maximum sample size", 
                 min = 16, 
                 max = 200, 
                 value = 30,
                 step = 1)
  ),
  mainPanel(
    #h2("Standard deviation uncertainty"),
    h4(
      "Warning: the uncertainty about the standard deviation estimate plays a serious role in the determination of the sample size"
    ),
    div(class="container",
        #div(class="span6",
        #	h5("Confidence interval about the standard deviation: ")
        #),
        #div(class="span3", 
        plotOutput("PlotSigmaIC")
        #),
        #div(class="span3", 
        #	""
        #)
    ),
    br(),
    h2("Half-width of the confidence interval about the mean"),
    helpText(
      "The blue line shows the predicted half-width: this is the 
half-width you would get if the 
estimated standard deviation of your future data is exactly", 
      span(id="sigma", class="shiny-text-output")
    ), 
    helpText(
      "The shaded area shows the uncertainty about the blue line 
resulting from the uncertainty about your standard deviation estimate", 
      "More precisely, the half-width has", 
      span(id="credibilitybis", class="shiny-text-output"), 
      "% probability to lie outside the shaded area", 
      "(", 
      span(id="halfcredibility", class="shiny-text-output"), 
      "% below the lower line as well as beyond the upper line).", 
      "You can set this error preditcion level at the bottom in the sidebar."
    ), 
    plotOutput("PlotLengthIC"),
    br(),
    htmlOutput("htmltab")
  )
)


# Shiny server ####
server <- function(input, output, session) {
  #
  output$n <- reactive({ input$n })
  output$credibility <- reactive({ 100-input$credibility })
  output$credibilitybis <- reactive({ input$credibility })
  output$halfcredibility <- reactive( {input$credibility/2 })
  output$sigma <- reactive({ input$sigma.hat })
  #
  DF1 <- reactive({
    level <- input$level
    alpha <- (100-level)/200
    sigma <- input$sigma.hat
    nmin <- input$nmin
    nmax <- input$nmax
    ll <- sapply(
      nmin:nmax, function(n) lengthIC(n, sigma.hat = sigma, alpha = alpha)
    ) / 2
    df1 <- data.frame(n = nmin:nmax, hwidth = ll)
    list(df1 = df1, level = level, sigma = sigma)    
  })
  PlotLengthIC0 <- reactive({
    # 
    loadDF1 <- DF1()
    df1 <- loadDF1$df1
    level <- loadDF1$level
    cred <- 100 - input$credibility
    #sigma <- loadDF1$sigma
    titl <- bquote(paste(
      "Predicted half-width of the ", 
      .(level),  "%-confidence interval about the mean ", 
      "and ", .(cred), "%-prediction band"
    ))
    p <- ggplot(df1, aes(x = n,y = hwidth)) + geom_line(colour = "blue") + 
      geom_point(colour = "orange") + ggtitle(titl) +
      xlab("sample size") + ylab("half-width") 
    list(p = p, nn = df1$n)
  })
  #
  DFband <- reactive({
    loadDF1 <- DF1()
    df1 <- loadDF1$df1
    alpha <- (100 - loadDF1$level) / 200
    sigma <- loadDF1$sigma
    nn <- df1$n
    cred <- 100 - input$credibility
    n <- input$n
    band <- data.frame(n = nn, lcl = NA_real_, ucl = NA_real_)
    lcl.sig <- sigma * sqrt(qf((100-cred)/200, nn-1, n-1)) 
    band$lcl <- sapply(1:nrow(band), function(i) { 
      lengthIC(nn[i], lcl.sig[i], alpha = alpha) / 2
    })
    ucl.sig <- sigma * sqrt(qf((100+cred)/200, nn-1, n-1)) 
    band$ucl <- sapply(1:nrow(band), function(i) { 
      lengthIC(nn[i], ucl.sig[i], alpha = alpha) / 2
    })
    list(df1 = df1, band = band)
  })
  
  output$PlotLengthIC <- renderPlot({
    P0 <- PlotLengthIC0()
    p <- P0$p
    loadband <- DFband()
    band <- loadband$band
    p <- p + 
      geom_ribbon(
        data = band, aes(x = n, y = NULL, ymin = lcl, ymax = ucl), 
        color = "white", linetype = 0, alpha = 0.3
      ) 
    print(p)
  })
  #
  # TABLE PREDICTIONS
  #
  output$htmltab <- reactive({
    loadband <- DFband()
    df1 <- loadband$df1
    band <- loadband$band
    df.predict <- cbind(df1, band[, -1])
    rnd <- max(1-floor(log10(min(df1$hw))), 0)
    df.predict <- sapply(df.predict, function(x) round(x, rnd))
    #rownames(df.predict) <- df1$n 
    colnames(df.predict) <- 
      c("Sample size", "half-width", "lower bound", "upper bound")
    cred <- 100 - input$credibility 
    htmlTable(
      df.predict, 
      title = "", #rowlabel="aa", rowname="iii", 
      #n.rgroup=c(nrow(df1)), rgroup=c("Sample size"),
      n.cgroup = c(1, 1, 2), 
      cgroup = c("", "predicted value", paste0(cred, "%-prediction interval")), 
      #cgroup.just = c("l","r"), 
      ctable = TRUE, output = FALSE,
      caption = "Predicted half-width in function of sample size"
    )
  })
  #
  plotSigmaIC <- reactive({
    n <- input$n
    sigma <- input$sigma.hat
    bounds0 <- qsqrtigamma(c(1e-10, .99), (n-1)/2, (n-1)*sigma^2/2) #ICsigma(n,sigma.hat,.01)
    # QUARTILES : 
    Quar <- qsqrtigamma(c(25, 50, 75) / 100, (n-1)/2, (n-1)*sigma^2/2)     
    Qdf <- data.frame(
      Quartiles = paste0(c(25, 50, 75), "%"), 
      x = Quar, 
      xend = Quar, 
      y = 0, 
      yend = dsqrtigamma(Quar, (n-1)/2, (n-1)*sigma^2/2))
    list(n = n, sigma.hat = sigma, bounds0 = bounds0, Qdf = Qdf)
  })
  #
  output$PlotSigmaIC <- renderPlot({
    params <- plotSigmaIC()
    bounds0 <- params$bounds0
    sigma.hat <- params$sigma.hat
    Qdf <- params$Qdf
    n <- params$n
    x <- seq(bounds0[1], bounds0[2], length.out = 100)
    df <- data.frame(x, y = dsqrtigamma(x, (n-1)/2, (n-1)*sigma.hat^2/2))  	
    p <- ggplot(data = df, aes(x = x, y = y)) + 
      geom_line(colour = "blue", linewidth = 1.4) + 
      geom_segment(
        data = Qdf, 
        aes(x = x, xend = xend, y = y, yend = yend, colour = Quartiles), 
        linetype="twodash"
      ) +
      xlab("standard deviation") + ylab(NULL) + 
      ggtitle("Standard deviation uncertainty") +
      theme(
        axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(face = "bold", size = 14),
        #axis.ticks.margin=unit(0, "mm"), 
        panel.background = element_rect(fill = "grey95"),
        #panel.grid=element_blank(),
        plot.title = element_text(face = "italic", size = 23)
        #panel.border = element_rect(fill = "grey90", colour = NA)
      ) +
      scale_y_continuous(expand = c(0, 0))
    print(p)
  }, width = 525)
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
