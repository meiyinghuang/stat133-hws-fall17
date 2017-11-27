# Shiny app for hw04
#Author: Meiying Huang

library(shiny)
library(ggplot2)

#source("./../code/functions.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Grade Visualizer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("select", label = h3("Select box"), 
                  choices = list("HW1" = "HW1", 
                                 "waiting" = "waiting"), 
                  selected = "eruptions"),
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      radioButtons("scale", label = h3("choose scale"),
                   choices = list("none" = 1, 
                                  "std units" = 2, 
                                  "scale" = 3), 
                   selected = 1)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[ ,input$select]
    if (input$scale == 1) {
      x
    } else if (input$scale == 2) {
      x <- standardize(x)
    } else {
      x <- rescale(x)
    }
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'blue', border = 'white',
         main = input$select, xlab = input$select)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


----------
  #' @title standardize
#' @description convert x to standard units
#' @param x numeric vector
#' @param na.rm whether to remove missing values
#' @return vector in standardized units
#' @example 
#' standardize(1:5)
standardize <- function(x, na.rm = FALSE) {
  if (!is.numeric(x)) {
    stop("non-numeric argument")
  }
  x_bar <- mean(x, na.rm = na.rm)
  x_sd <- sd(x, na.rm = na.rm)
  z <- (x - x_bar) / x_sd
  return(z)
}


#' @title rescale
#' @description rescale a vector by its range
#' @param y numeric vector
#' @param na.rm whether to remove missing values
#' @return vector in rescaled units
#' @example 
#' rescale(1:5)
rescale <- function(y, na.rm = FALSE) {
  if (!is.numeric(y)) {
    stop("'y' is not numeric")
  }
  ymin <- min(y, na.rm = na.rm)
  ymax <- max(y, na.rm = na.rm)
  (y - ymin) / (ymax - ymin)
}

