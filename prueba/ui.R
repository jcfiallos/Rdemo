
library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

  navbarPage(
    # theme = "cerulean",  # <--- To use a theme, uncomment this
    "Aplicacion para Series de Tiempo",
    tabPanel("Cargar Archivo",
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Elija Archivo',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 radioButtons('quote', 'Quote',
                              c(None='',
                                'Double Quote'='"',
                                'Single Quote'="'"),
                              '"'),
                 radioButtons('deci', 'Separador decimal',
                              c('Coma'=",",
                                'Punto'="."),
                              '"'),
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = ""),
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 plotOutput('MyPlot'),
                 tableOutput('contents'),
                 verbatimTextOutput("text1")
               )
             )
             
    ),
    tabPanel("Analisis Inicial", 
             sidebarPanel(
               
               radioButtons("type", "Select a trend:",
                            list("Linear up" = "linear.up",
                                 "Linear down" = "linear.down",
                                 "Curved up" = "curved.up",
                                 "Curved down" = "curved.down",
                                 "Fan-shaped" = "fan.shaped")),
               br(),
               
               checkboxInput("show.resid", "Show residuals", FALSE),
               
               br(),
               
               helpText("Texto de ayuda."),
               br()),
             mainPanel(
               plotOutput("scatter"),
               br(),
               br(),
               plotOutput("residuals")
                       
             )
    ),
    tabPanel("Resultados", "Este panel muestra los resultados")
  )
))
