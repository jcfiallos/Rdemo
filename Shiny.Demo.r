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

#ENTORNO UI.(INTERFAZ)

myFluidPage <- fluidPage(

  #title
  titlePanel("DEMO"),

  #sidebarLayaout
  sidebarLayout(
    sidebarPanel(
      checkboxInput(inputId = "density",
                    label = strong("Show density"),
                    value = FALSE),
      
      checkboxInput(inputId = "theoretical",
                    label = strong("Show theoretical"),
                    value = FALSE),
    
    br(),  
    
    sliderInput("bins","Number of bins:",
                  min = 1,
                  max = 50,
                  value = 10)
    ),
    
    #Gráfico
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

#ENTORNO SERVER.
myShinyServer <- function(input,output,sesion) {

  output$distPlot <- renderPlot({

    #Obtener las bins basados en las bin del UI.R
    x <- myData
    bins <- seq(min(x), max(x), length.out = input$bins + 1) #**

    #Obtener el histograma modificado
    hist(x, breaks = bins, col = "grey86", probability =  TRUE, ylab = "Probability", xlab = "Observations", main = "Histogram") #Breaks muestra el número de bins tan el histograma
    
    if (input$density){
      dens <- density(x)
      lines(dens, col = "blue", lty=1, lwd=2)
    }
    
    if (input$theoretical){
      teorico <- dnorm(x,mean=0, sd=1 ,log = FALSE)
      lines(teorico, col = "red", lty=1, lwd=2)
    }
    
    })
}


myfunction <- function(x)
{
  myData <<- x
  runApp(shinyApp(
    ui = myFluidPage,
    server = myShinyServer
  ))
}


myfunction(x=rnorm(100000))
#myfunction(x=sample(1:100, 1000, replace=TRUE))

