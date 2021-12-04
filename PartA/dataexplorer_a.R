#  The data
library(ggplot2)
library(shiny)
library(RColorBrewer)


dataset <- read.csv('trgn.demo.csv',header = TRUE, sep = ",", quote = "\"", dec = ".", fill = TRUE, row.names = 1)
headerNames=colnames(dataset)

ui <- fluidPage(
  pageWithSidebar(
    headerPanel("Data Explorer"),
    sidebarPanel(
      selectInput('x', 'X', c("None"=FALSE,headerNames),headerNames[2]),
      selectInput('y', 'Y', c("None"=FALSE,headerNames),headerNames[3]),
      selectInput('c', 'Colour', c("None"=FALSE,headerNames),headerNames[3]),
      selectInput('size', 'Size', c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), selected = 5),
      checkboxInput('geom_point', 'geom_point',TRUE)
    ),
    mainPanel(
      plotOutput('plot')
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$plot <- renderPlot({
    p <- ggplot(dataset, aes_string(x=input$x, fill=input$c))
    if (input$geom_point)
      p <- p + geom_point(aes_string(x=input$x,y=input$y, color=input$c, size=as.numeric(input$size)))
    else
      p <- p + geom_point(aes_string(y = 0, size=as.numeric(input$size)))
    print(p)
    
  }, height=700)
  
}

# Run the application 
shinyApp(ui = ui, server = server)
