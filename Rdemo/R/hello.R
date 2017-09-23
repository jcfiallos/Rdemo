# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

library(shiny)

myData <- c()

myFluidPage <- fluidPage(

  # Application title
  titlePanel("Demo de JDV"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)




myShinyServer <- function(input, output, sesion) {

  output$distPlot <- renderPlot({

    # generate bins based on input$bins from ui.R
    x    <- myData
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # draw the density with the specified
    lines(x, col  = 'blue', lwd = 2)

  })

}


mydemofunction <- function(x)
{
  myData <<- x
  runApp(shinyApp(
    ui = myFluidPage,
    server = myShinyServer
  ))

}


mydemofunction(x=rnorm(1000))
